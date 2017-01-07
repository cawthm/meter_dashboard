## arcola meter
# turning this into more DRY set of code to talk to ET meters

library("httr")
library("xml2")
library("dplyr")
library("readr")
library("lubridate")
library("fasttime") # need to deprecate this to lubridate

arcola_base_url <- "http://arcolacovenant.synology.me:2856"

url1 <- paste0(arcola_base_url, "/billing.htm")
url2 <- paste0(arcola_base_url, "/rtdata.htm")

my_xml1

xml_find_first(my_xml1, ".//kwh_del") %>% xml_contents() # total cum kwh, uses url1

xml_find_first(my_xml2, ".//i_kw_net") %>% xml_contents() # instantaneous kw; uses url2

# takes a single url and returns a block of xml
get_xml_block <- function(my_url, login, password) {
  url_response <- RETRY("GET",url, pause_cap = 6000, authenticate(login, password))
  if (http_error(url_response) == FALSE) {
    content(url_response, "parsed")
  }
}

#takes two blocks of xml and turns them into one line of data
create_data_line <- function(my_xml1, my_xml2, tz = "US/Eastern") {
  formatted_time <- xml_find_first(my_xml1, ".//meter_time") %>% xml_contents() #timestamp, uses xml1
  data_frame(
    date_time_utc = formatted_time,
    age = my_xml2[[4]][[1]]  %>% as.numeric(),
    inst_kw = attr(my_xml2[[27]], "value") %>% as.numeric(),
    total_kw = attr(my_xml2[[7]], "value") %>% as.numeric()#,
    #    date_time_est = as.character(fastPOSIXct(formatted_time, tz = "America/New_York"))
  )
}



get_and_write_the_data <- function(url, output_file_path, login, password) {
  url_response <- RETRY("GET",url, pause_cap = 6000, authenticate(login, password))
  if (http_error(url_response) == FALSE) {
    my_xml <- content(url_response, "parsed")
    my_xml2 <- as_list(my_xml)$devices$device$records$record
    line <- create_data_line(my_xml2)
    write_csv(line, output_file_path, append = TRUE)
  } else {
    Sys.sleep(3)
  }
}