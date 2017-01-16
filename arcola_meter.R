## arcola meter
# turning this into more DRY set of code to talk to ET meters

library("httr")
library("xml2")
library("dplyr")
library("readr")
library("StreamMetabolism")
library("lubridate")

source("helper_funcs.R")

arcola_location <- list(address = "52 Paramus Rd", 
                        city = "Paramus", 
                        state = "NJ", 
                        tz = "US/Eastern", # use OlsonNames() to see list of valis tz's
                        lat = 40.923459, 
                        lng = -74.086748) 


arcola_base_url <- "http://arcolacovenant.synology.me:2856"

url_a <- paste0(arcola_base_url, "/billing.htm")
url_b <- paste0(arcola_base_url, "/rtdata.htm")


while(year(Sys.Date()) <= 2017) {
  daylight_span <- sunrise.set(arcola_location$lat, arcola_location$lng, date = ymd("2016-01-01", tz = arcola_location$tz))

  if (floor(difftime(Sys.Date(), daylight_span$sunset ,tz = arcola_location$tz ,units = "days")) != 0) {
    daylight_span <- sunrise.set(arcola_location$lat, arcola_location$lng, date = Sys.Date() %>% ymd(.,tz = arcola_location$tz)) #sunrise.set gives info for yesterday, so we increment 1 day
  }

  this_day <- interval(daylight_span$sunrise, daylight_span$sunset, tzone = arcola_location$tz)
  right_now <- ymd_hms(Sys.time(), tz = "US/Eastern")
  
  while(right_now %within% this_day) {
    xml1 <- get_xml_block(url_a, "eM200", "PW", tz = arcola_location$tz)
    xml2 <- get_xml_block(url_b, "eM200", "PW", tz = arcola_location$tz)
    one_line_of_data <- create_data_line(xml1, xml2)
    write_csv(one_line_of_data, "data/arcola_data.csv", append = TRUE)
    Sys.sleep(15)
  }
}




