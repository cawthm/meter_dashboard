## aina meter 1 
# meter ID is 1111111100409D4760C7
# turning this into more DRY set of code to talk to ET meters

source("helper_funcs.R")

creds_name <- "aina1_meter"

.creds <- read_csv("data/.creds.csv") %>% filter(device == creds_name)

while(year(Sys.Date()) <= 2017) { # everything in UTC
  daylight_span <- sunrise.set(.creds$lat, .creds$lng, date = ymd("2016-01-01", tz = "UTC")) #
  
  if (abs(difftime(Sys.time() %>% with_tz("UTC"), daylight_span$sunset ,tz = .creds$tz ,units = "hours")) >= 24) {
    daylight_span <- sunrise.set(.creds$lat, .creds$lng, date = as.character((Sys.Date() %>% with_tz("UTC")))) #
    source("helper_funcs.R") # refresh once per day
  }
  
  this_day <- interval(daylight_span$sunrise, daylight_span$sunset)
  right_now <- Sys.time() %>% with_tz("UTC")
  
  while(right_now %within% this_day) {
    try(
      x <- meter_info_snapshot(.creds$url, login = .creds$login, password = .creds$pass, tz = .creds$tz, meter_type = .creds$type),
      silent = TRUE
      
    )
    
    try( write_csv(x, paste0("data/", creds_name,".csv"), append = TRUE) )
    
    Sys.sleep(15)
    
    right_now <- Sys.time() %>% with_tz("UTC")
  }
  Sys.sleep(60) # go to sleep for two minutes in the outer loop; basically, waiting for the sun to come up

}
