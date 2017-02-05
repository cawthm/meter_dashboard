##should turn these functions into an R package
library("tidyverse")
library("StreamMetabolism")
library("httr")
library("xml2")
library("stringr")
library("lubridate")
library("anytime")

anytime::addFormats("%A,%B %d %Y %H:%M:%S")

# takes a single url and returns a block of xml
get_xml_block <- function(my_url, login, password) {
  url_response <- RETRY("GET",my_url, times = 30, pause_base = 10, pause_cap = 6000, authenticate(login, password))
  if (http_error(url_response) == FALSE) {
    content(url_response, "parsed", encoding = "UTF-8")
  }
}

# new function.  take a url + login/pwd credentials, return df

meter_info_snapshot <- function(my_url, login, password, tz, meter_type) {
  meter_type <- tolower(meter_type)
  assertthat::assert_that(meter_type %in% c("em", "obvius", "laurita_obvius"))

  machine_time <-   Sys.time() %>% lubridate::with_tz("UTC")
  
  if (meter_type == "em") {
    my_xml1 <- get_xml_block(paste0(my_url, "/billing.htm"), login, password)
    my_xml2 <- get_xml_block(paste0(my_url, "/rtdata.htm"), login, password)
    date_time_utc <- xml2::xml_find_first(my_xml1, ".//meter_time") %>% xml_contents() %>% lubridate::mdy_hm(tz = tz) %>% with_tz("UTC")
    inst_kw <- xml_find_first(my_xml2, ".//i_kw_net") %>% xml_contents() %>% xml_text() %>% as.numeric() # instantaneous kw; uses url2
    cum_kwh <- xml_find_first(my_xml1, ".//kwh_del") %>% xml_contents() %>% xml_text() %>% as.numeric()
    
  }  
  if (meter_type == "obvius") {
    my_xml1 <- get_xml_block(my_url, login, password)
    df <- data_frame(field = my_xml1 %>% xml_find_all(".//point") %>% xml_attr("name"),
                 value = my_xml1 %>% xml_find_all(".//point") %>% xml_attr("value") %>% as.numeric()
    )
    
    date_time_utc <- xml2::xml_find_first(my_xml1, ".//time") %>% xml_contents() %>% ymd_hms() %>% with_tz("UTC")
    inst_kw <- df %>% filter(field == "Power Instantaneous, total all phases") %>% .$value
    cum_kwh <- df %>% filter(field == "Energy Net") %>% .$value 
  }
  
  if (meter_type == "laurita_obvius") {
    my_xml1 <- get_xml_block(my_url, login, password)
    date_time_utc <- my_xml1 %>% xml_find_all(".//font") %>% xml_find_all(".//span") %>% xml_contents() %>% as.character() %>% anytime(asUTC = T) %>% with_tz("UTC")
    inst_kw <- my_xml1 %>% xml_find_all(".//font") %>% xml_contents() %>% .[15] %>% as.character() %>% str_extract("[0-9]+.[0-9]{2}") %>% as.numeric()
    cum_kwh <- my_xml1 %>% xml_find_all(".//font") %>% xml_contents() %>% .[5] %>% as.character() %>% str_extract("[0-9]+.[0-9]{2}") %>% as.numeric()
  }
  data_frame(
    date_time_utc,
    age = 0,
    inst_kw,
    cum_kwh
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

# ### everything below here should be cleaned up
# temp <- function(filelist) {
#   y <- tibble(x = character(),y= character(),z= character()) 
#   for (i in filelist) {
#     x <- read_csv(i)[,c(1,2,4)] %>% as_tibble()
#     names(y) <- names(x)
#     y = rbind(x,y)
#   }
#   y
# }
# 
# # just using the table for the names
# ch <- odbcConnect("gse_db")
# aina <- sqlFetch(ch, "DATA") %>% as_tibble()
# obvius <- sqlFetch(ch, "Obvius") %>% as_tibble()
# 
# aina_all <- read_csv("~/Dropbox/Green Street Energy/mc/DATA.txt", col_names = FALSE)
# 
# obvius_all <- read_csv("~/Dropbox/Green Street Energy/mc/Obvius.txt", col_names = FALSE)
# 
# names(aina_all) <- names(aina)
# 
# names(obvius_all) <- names(obvius)
# 
# # get meter 1 first
# 
# aina1 <- aina_all %>% filter(METER_ID == "1111111100409D47614E") %>% select(date_time_utc = METER_TIME, kw = I_KW_DEL, cum_kwh = KWH_DEL) %>% mutate(date_time_utc = mdy_hms(date_time_utc, tz = "US/Hawaii")) %>% arrange(date_time_utc)
# 
# 
# aina2 <- aina1 %>% group_by(date_time_utc) %>% summarise(kw = max(kw), cum_kwh = max(cum_kwh))
# 
# 
# aina3 <- aina2 %>% mutate(date_time_utc = with_tz(date_time_utc,"UTC"))
# 
# # then meter 2
# 
# aina_start <- min(aina1$date) %>% floor_date("day")
# aina_last <- max(aina1$date) %>% ceiling_date("day")
# difftime(aina_start, aina_last, "days")
# 
# all_days <- tibble(date = aina_start + days(0:1866))
# 
# aina_obs_per_day <- aina1 %>% group_by(date) %>% summarise(n = n()) %>% arrange(desc(n))
# 
# all_days <- left_join(all_days, aina_obs_per_day)
