## from the laurita meter

library("httr")
library("xml2")
library("dplyr")
library("readr")
library("lubridate")
library("fasttime")

## Base url of the meter
url <- "http://50.248.181.209:1005/setup/devicexml.cgi?ADDRESS=10&TYPE=DATA&ts_cache=1462902068560"

create_data_line <- function(my_xml2) {
  formatted_time <- lubridate::ymd_hms(my_xml2[[2]][[1]])
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

while(month(Sys.time()) < 10) {
 get_and_write_the_data(url, "./data/laurita_data.csv", "admin", "deck0B71")
 Sys.sleep(10)
}
