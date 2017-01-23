library("httr")
library("xml2")
library("dplyr")
library("readr")
library("StreamMetabolism")
library("lubridate")

source("helper_funcs.R")

aina_location <- list(address = "660 Waine'e St", 
                        city = "Lahaina, Maui", 
                        state = "HI", 
                        tz = "US/Hawaii", # use OlsonNames() to see list of valis tz's
                        lat = 20.874239, 
                        lng = -156.675356)

meter1_base_url <- "http://166.248.205.38:80"
meter2_base_url <- "http://166.248.205.37:80"

url1_a <- paste0(meter1_base_url, "/billing.htm")
url1_b <- paste0(meter1_base_url, "/rtdata.htm")

url2_a <- paste0(meter2_base_url, "/billing.htm")
url2_b <- paste0(meter2_base_url, "/rtdata.htm")

while(year(Sys.Date()) <= 2017) {
  daylight_span <- sunrise.set(aina_location$lat, aina_location$lng, date = ymd("2016-01-01", tz = aina_location$tz))
  
  if (floor(difftime(Sys.Date(), daylight_span$sunset ,tz = aina_location$tz ,units = "days")) != 0) {
    daylight_span <- sunrise.set(aina_location$lat, aina_location$lng, date = Sys.Date() %>% ymd(.,tz = aina_location$tz)) #sunrise.set gives info for yesterday, so we increment 1 day
  }
  
  this_day <- interval(daylight_span$sunrise, daylight_span$sunset, tzone = aina_location$tz)
  right_now <- ymd_hms(Sys.time(), tz = "US/Eastern")
  
  while(right_now %within% this_day) {
    xml1_a <- get_xml_block(url1_a, "eM200", "PW")
    xml1_b <- get_xml_block(url1_b, "eM200", "PW")
    one_line_of_data <- create_data_line(xml1_a, xml1_b, tz = aina_location$tz)
    write_csv(one_line_of_data, "data/aina1_data.csv", append = TRUE)
    xml2_a <- get_xml_block(url2_a, "eM200", "PW")
    xml2_b <- get_xml_block(url2_b, "eM200", "PW")
    one_line_of_data2 <- create_data_line(xml2_a2, xml2_b, tz = aina_location$tz)
    write_csv(one_line_of_data, "data/aina2_data.csv", append = TRUE)
    Sys.sleep(15)
  }
}
