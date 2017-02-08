## aina meter 2
# meter ID is 1111111100409D47614E

source("helper_funcs.R")

location <- list(address = "660 Waine'e St", 
                 city = "Lahaina, Maui", 
                 state = "HI", 
                 tz = "US/Hawaii", # use OlsonNames() to see list of valis tz's
                 lat = 20.874239, 
                 lng = -156.675356,
                 creds_name = "aina2_meter")

.creds <- read_csv("data/.creds.csv") %>% filter(device == location$creds_name)

while(year(Sys.Date()) <= 2017) { # everything in UTC
  daylight_span <- sunrise.set(location$lat, location$lng, date = ymd("2016-01-01", tz = "UTC")) #
  
  if (abs(difftime(Sys.time() %>% with_tz("UTC"), daylight_span$sunset ,tz = location$tz ,units = "hours")) >= 24) {
    daylight_span <- sunrise.set(location$lat, location$lng, date = as.character((Sys.Date() %>% with_tz("UTC")))) #
    source("helper_funcs.R") # refresh once per day
  }
  
  this_day <- interval(daylight_span$sunrise, daylight_span$sunset)
  right_now <- Sys.time() %>% with_tz("UTC")
  
  while(right_now %within% this_day) {
    try(
      x <- meter_info_snapshot(.creds$url, login = .creds$login, password = .creds$pass, tz = location$tz, meter_type = .creds$type),
      silent = TRUE
      
    )
    
    write_csv(x, paste0("data/",location$creds_name,".csv"), append = TRUE)
    
    Sys.sleep(15)
    
    right_now <- Sys.time() %>% with_tz("UTC")
  }
  