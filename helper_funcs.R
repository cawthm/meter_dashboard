##should turn these functions into an R package
library("tidyverse")
library("StreamMetabolism")
library("anytime")
library("httr")
library("xml2")
library("stringr")
library("lubridate")

anytime::addFormats("%A,%B %d %Y %H:%M:%S")

# takes a single url and returns a block of xml
get_xml_block <- function(my_url, login, password) {
  url_response <- RETRY("GET",my_url, pause_cap = 6000, authenticate(login, password))
  if (http_error(url_response) == FALSE) {
    content(url_response, "parsed", encoding = "UTF-8")
  }
}

# new function.  take a url + login/pwd credentials, return df

meter_info_snapshot <- function(my_url, login, password, meter_type, tz) {
  meter_type <- tolower(meter_type)
  assertthat::assert_that(meter_type %in% c("em", "obvius", "laurita_obvius"))
  machine_time <-   Sys.time() %>% lubridate::with_tz("UTC")
  
  if (meter_type == "em") {
    my_xml1 <- get_xml_block(paste0(my_url, "/billing.htm"), login, password)
    my_xml2 <- get_xml_block(paste0(my_url, "/rtdata.htm"), login, password)
    date_time_utc <- xml2::xml_find_first(my_xml1, ".//meter_time") %>% xml_contents() %>% lubridate::mdy_hm(tz = tz) %>% with_tz("UTC")
    inst_kw <- xml_find_first(my_xml2, ".//i_kw_net") %>% xml_contents() %>% xml_text() %>% as.numeric() # instantaneous kw; uses url2
    total_kw <- xml_find_first(my_xml1, ".//kwh_del") %>% xml_contents() %>% xml_text() %>% as.numeric()
    
  }  
  if (meter_type == "obvius") {
    my_xml1 <- get_xml_block(my_url, login, password)
    df <- tibble(field = my_xml1 %>% xml_find_all(".//point") %>% xml_attr("name"),
                 value = my_xml1 %>% xml_find_all(".//point") %>% xml_attr("value") %>% as.numeric()
    )
    
    date_time_utc <- xml2::xml_find_first(my_xml1, ".//time") %>% xml_contents() %>% lubridate::ymd_hms(tz = tz) %>% with_tz("UTC")
    inst_kw <- df %>% filter(field == "Power Instantaneous, total all phases") %>% .$value
    total_kw <- df %>% filter(field == "Energy Net") %>% .$value 
  }
  if (meter_type == "laurita_obvius") {
    my_xml1 <- get_xml_block(my_url, login, password)
    date_time_utc <- my_xml1 %>% xml_find_all(".//font") %>% xml_find_all(".//span") %>% xml_contents() %>% as.character() %>% anytime(asUTC = T) %>% with_tz("UTC")
    inst_kw <- my_xml1 %>% xml_find_all(".//font") %>% xml_contents() %>% .[15] %>% as.character() %>% str_extract("[0-9]+.[0-9]{2}") %>% as.numeric()
    total_kw <- my_xml1 %>% xml_find_all(".//font") %>% xml_contents() %>% .[5] %>% as.character() %>% str_extract("[0-9]+.[0-9]{2}") %>% as.numeric()
  }
  tibble(
    date_time_utc,
    age = difftime(machine_time, date_time_utc, units = "secs")  %>% as.numeric() %>% round(2),
    inst_kw,
    total_kw
  )
}

#takes two blocks of xml (from 2 diff parts on the meter) and turns them into one line of data
# create_data_line <- function(my_xml1, meter_type, my_xml2) { 
#   meter_type <- tolower(meter_type)
#   stopifnot(meter_type %in% c("em", "obvius"))
#   
#   machine_time <-   Sys.time() %>% lubridate::with_tz("UTC")
# 
#     if (meter_type == "em") {
#       #timestamp, uses xml1; the time on the machine running the script, translated to UTC
#       date_time_utc <- xml2::xml_find_first(my_xml1, ".//meter_time") %>% xml_contents() %>% lubridate::mdy_hm(tz = "UTC") 
#       inst_kw <- xml_find_first(my_xml2, ".//i_kw_net") %>% xml_contents() %>% xml_text() %>% as.numeric() # instantaneous kw; uses url2
#       total_kw <- xml_find_first(my_xml1, ".//kwh_del") %>% xml_contents() %>% xml_text() %>% as.numeric() # total cum kwh, uses url1
#    
#     } else {
#       # we create df from the obvius' xml that we then reference
#       
#       df <- tibble(field = my_xml1 %>% xml_find_all(".//point") %>% xml_attr("name"),
#                    value = my_xml1 %>% xml_find_all(".//point") %>% xml_attr("value") %>% as.numeric()
#                      )
#       
#       date_time_utc <- xml2::xml_find_first(my_xml1, ".//time") %>% xml_contents() %>% lubridate::ymd_hms(tz = "UTC") %>% with_tz("UTC")
#       inst_kw <- df %>% filter(field == "Power Instantaneous, total all phases") %>% .$value
#       total_kw <- df %>% filter(field == "Energy Net") %>% .$value 
#       
#     }
#   
#   tibble(
#     date_time_utc,
#     age = difftime(machine_time, date_time_utc, units = "secs"),
#     inst_kw,
#     total_kw
#   )
# 
# }

