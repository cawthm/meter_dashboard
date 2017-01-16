## from the johnston meters

library("httr")
library("xml2")
library("dplyr")
library("readr")
library("lubridate")
library("fasttime")

## Base url of the meters

url_siel<- "http://64.206.121.30/setup/devicexml.cgi?ADDRESS=250&TYPE=DATA"

url_growatt <- "http://64.206.121.31/setup/devicexml.cgi?ADDRESS=250&TYPE=DATA"

create_data_line <- function(my_xml2, tz = "US/Eastern") {
  formatted_time <- lubridate::ymd_hms(my_xml2[[1]][[1]]) # time of observation
  data_frame(
    date_time_utc = formatted_time,
    age = my_xml2[[2]][[1]]  %>% as.numeric(), # How many seconds old the data is
    inst_kw = attr(my_xml2[[11]], "value") %>% as.numeric(), # "Power Instantaneous, total all phases"
    total_kw = attr(my_xml2[[4]], "value") %>% as.numeric(), # "Energy Net"
    date_time_local = date_time_utc %>% with_tz(tz = tz)
  )
}

get_and_write_the_data <- function(my_url, output_file_path, login, password) {
  url_response <- RETRY("GET", my_url, pause_cap = 6000, authenticate(login, password))
  if (http_error(url_response) == FALSE) {
    my_xml <- content(url_response, "parsed")
    my_xml2 <- as_list(my_xml)$devices$device$records$record
    line <- create_data_line(my_xml2)
    #print(line)
    readr::write_csv(line, output_file_path, append = TRUE)
  } else {
    Sys.sleep(10)
  }
}

while(month(Sys.time()) < 10) {
  get_and_write_the_data(url_siel, "data/johnston_siel_data.csv", "admin", "admin")
  Sys.sleep(5)
  get_and_write_the_data(url_growatt, "data/johnston_growatt_data.csv", "admin", "admin")
  Sys.sleep(5)
}
