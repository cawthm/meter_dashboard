##should turn these functions into an R package


# takes a single url and returns a block of xml
get_xml_block <- function(my_url, login, password) {
  url_response <- RETRY("GET",my_url, pause_cap = 6000, authenticate(login, password))
  if (http_error(url_response) == FALSE) {
    content(url_response, "parsed", encoding = "UTF-8")
  }
}

#takes two blocks of xml (from 2 diff parts on the meter) and turns them into one line of data
create_data_line <- function(my_xml1, my_xml2, meter_type = "em") { 
  meter_type <- tolower(meter_type)
  stopifnot(meter_type %in% c("em", "obvius"))
  
  machine_time <-   Sys.time() %>% lubridate::with_tz("UTC")

    if (meter_type == "em") {
      formatted_time <- xml_find_first(my_xml1, ".//meter_time") %>% xml_contents() %>% lubridate::mdy_hm(tz = "UTC") #timestamp, uses xml1.
      # the time on the machine running the script, translated to UTC
      date_time_utc <- formatted_time %>% lubridate::with_tz(tz = "UTC")
      age <- difftime(machine_time, formatted_time, units = "secs") %>% as.numeric() # in seconds
      inst_kw <- xml_find_first(my_xml2, ".//i_kw_net") %>% xml_contents() %>% xml_text() %>% as.numeric() # instantaneous kw; uses url2
      total_kw <- xml_find_first(my_xml1, ".//kwh_del") %>% xml_contents() %>% xml_text() %>% as.numeric() # total cum kwh, uses url1
   
    } else {
      
      date_time_utc <- formatted_time
      age <- my_xml1[[2]][[1]]  %>% as.numeric() # How many seconds old the data is
      inst_kw <- attr(my_xml1[[11]], "value") %>% as.numeric() # "Power Instantaneous, total all phases"
      total_kw <- attr(my_xml1[[4]], "value") %>% as.numeric() # "Energy Net"
    }
  
  tibble(
    date_time_utc,
    age,
    inst_kw,
    total_kw
  )

}