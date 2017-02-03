# laurita meters

source("helper_funcs.R")

location <- list(address = "52 Paramus Rd", 
                         city = "New Egypt", 
                         state = "NJ", 
                         tz = "US/Eastern", # use OlsonNames() to see list of valis tz's
                         lat = 40.068883,
                         lng = -74.500641,
                         creds_name = "laurita_obvius") 

.creds <- read_csv("data/.creds.csv") %>% filter(device == location$creds_name)

while(year(Sys.Date()) <= 2017) { # everything in UTC
  daylight_span <- sunrise.set(location$lat, location$lng, date = ymd("2016-01-01", tz = "UTC")) #
  
  if (abs(difftime(Sys.time() %>% with_tz("UTC"), daylight_span$sunset ,tz = location$tz ,units = "hours")) >= 24) {
    daylight_span <- sunrise.set(location$lat, location$lng, date = Sys.Date() %>% ymd(.,tz = "UTC")) #
  }
  
  this_day <- interval(daylight_span$sunrise, daylight_span$sunset, tzone = "UTC")
  right_now <- Sys.time() %>% with_tz("UTC")
  
  while( right_now %within% this_day ) {
    try(
      meter_info_snapshot(.creds$url, login = creds$login, password = .creds$password, tz = location$tz, meter_type = .creds$type) %>% 
        write_csv(.,"data/laurita_obvius.csv", append = TRUE)
    )
    Sys.sleep(7)
  }
  
}