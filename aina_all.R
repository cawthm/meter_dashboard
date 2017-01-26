# aina_meters

source("helper_funcs.R")

aina_location <- list(address = "660 Waine'e St", 
                        city = "Lahaina, Maui", 
                        state = "HI", 
                        tz = "US/Hawaii", # use OlsonNames() to see list of valis tz's
                        lat = 20.874239, 
                        lng = -156.675356)

.creds <- read_csv("data/.creds.csv")

aina1 <- .creds %>% filter(device == "aina1_meter") %>% .$url # meter ID is 1111111100409D4760C7  
login1 <- .creds %>% filter(device == "aina1_meter") %>% .$login
password1 <- .creds %>% filter(device == "aina1_meter") %>% .$pass
type1 <- .creds %>% filter(device == "aina1_meter") %>% .$type

aina2 <- .creds %>% filter(device == "aina2_meter") %>% .$url # meter ID is 1111111100409D47614E
login2 <- .creds %>% filter(device == "aina2_meter") %>% .$login
password2 <- .creds %>% filter(device == "aina2_meter") %>% .$pass
type2 <- .creds %>% filter(device == "aina2_meter") %>% .$type

while(year(Sys.Date()) <= 2017) { # everything in UTC
  daylight_span <- sunrise.set(aina_location$lat, aina_location$lng, date = ymd("2016-01-01", tz = "UTC")) # in local time
  
  if (abs(difftime(Sys.time() %>% with_tz("UTC"), daylight_span$sunset ,tz = aina_location$tz ,units = "hours")) >= 24) {
    daylight_span <- sunrise.set(aina_location$lat, aina_location$lng, date = Sys.Date() %>% ymd(.,tz = "UTC")) #sunrise.set gives info for yesterday, so we increment 1 day
  }
  
  this_day <- interval(daylight_span$sunrise, daylight_span$sunset, tzone = "UTC")
  right_now <- Sys.time() %>% with_tz("UTC")
  
  while(right_now %within% this_day) {
    try(
      meter_info_snapshot(aina1, login = login1, password = password1, tz = aina_location$tz, meter_type = type1) %>% 
      write_csv(.,"data/aina1_data.csv", append = TRUE)
      )
    try(
    meter_info_snapshot(aina2, login = login2, password = password2, tz = aina_location$tz, meter_type = type2) %>% 
      write_csv(.,"data/aina2_data.csv", append = TRUE)
    )
   
    Sys.sleep(7)
  }
    
}
  
