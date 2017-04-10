library(shiny)
library(shinydashboard)
library(tidyverse)
library(fasttime)
library(ggplot2)
library(lubridate)
library(readr)
library(C3)
#library("sunreturn")
########################## UI CODE ###########################

header <-  dashboardHeader(title = "GSE Dash")
 
sideBar <-  dashboardSidebar(disable = TRUE,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              numericInput("srec_price", label = "SREC px", value = 225, step = 5)
            ),
            fluidRow(
              column(h2("Laurita"), width = 2),
              column(C3GaugeOutput("inst_kw_laurita_meter", height = 90), width = 2),
              valueBoxOutput("time_live_laurita_meter", width = 2),
              valueBoxOutput("money_today_laurita_meter", width = 2),
              valueBoxOutput("money_month_laurita_meter", width = 2),
              valueBoxOutput("money_year_laurita_meter", width = 2)
            ),
            br(),
            fluidRow(
              column(h2("Arcola"), width = 2),
              column(C3GaugeOutput("inst_kw_arcola_meter", height = 90), width = 2),
              valueBoxOutput("time_live_arcola_meter", width = 2),
              valueBoxOutput("money_today_arcola_meter", width = 2),
              valueBoxOutput("money_month_arcola_meter", width = 2),
              valueBoxOutput("money_year_arcola_meter", width = 2)
            ),
            br(),
            fluidRow(
              column(h2("Aina 1"), width = 2),
              column(C3GaugeOutput("inst_kw_aina1_meter", height = 90), width = 2),
              valueBoxOutput("time_live_aina1_meter", width = 2),
              valueBoxOutput("money_today_aina1_meter", width = 2),
              valueBoxOutput("money_month_aina1_meter", width = 2),
              valueBoxOutput("money_year_aina1_meter", width = 2)
            ),
            br(),
            fluidRow(
              column(h2("Aina 2"), width = 2),
              column(C3GaugeOutput("inst_kw_aina2_meter", height = 90), width = 2),
              valueBoxOutput("time_live_aina2_meter", width = 2),
              valueBoxOutput("money_today_aina2_meter", width = 2),
              valueBoxOutput("money_month_aina2_meter", width = 2),
              valueBoxOutput("money_year_aina2_meter", width = 2)
            ),
            br(),
            fluidRow(
              column(h2("Johnston Siel"), width = 2),
              column(C3GaugeOutput("inst_kw_johnston_siel_obvius", height = 90), width = 2),
              valueBoxOutput("time_live_johnston_siel_obvius", width = 2),
              valueBoxOutput("money_today_johnston_siel_obvius", width = 2),
              valueBoxOutput("money_month_johnston_siel_obvius", width = 2),
              valueBoxOutput("money_year_johnston_siel_obvius", width = 2)
            ),
            br(),
            fluidRow(
              column(h2("Johnston Growatts"), width = 2),
              column(C3GaugeOutput("inst_kw_johnston_growatt_obvius", height = 90), width = 2),
              valueBoxOutput("time_live_johnston_growatt_obvius", width = 2),
              valueBoxOutput("money_today_johnston_growatt_obvius", width = 2),
              valueBoxOutput("money_month_johnston_growatt_obvius", width = 2),
              valueBoxOutput("money_year_johnston_growatt_obvius", width = 2)
            )
# 
#     tabItem(tabName = "irr",
#             h2("IRR tab content")
    )
  )
)

ui <- dashboardPage(header, sideBar, body)



### The "Run once section" #####################
.creds <- read_csv("data/.creds.csv", col_types = cols(switch_on = col_date("%m/%d/%y"), rate_as_of = col_date("%m/%d/%y")))

##first, load them

sites <- lapply(paste0("data/", .creds$device, ".csv"), read_csv)

sites <- setNames(sites, .creds$device)

##then, add a column with local time


sites2 <- map2(sites, .creds$tz, ~ mutate(.x, date_time_local = with_tz(date_time_utc, tz = .y)))


starts <- function(my_df, unit) { my_df %>% filter(date(date_time_local) <= (floor_date(now(), unit = unit) - days(1))) %>% distinct() %>% top_n(1, date_time_local) }

start_day <- lapply(sites2, starts, "day")
start_month <- lapply(sites2, starts, "month")
start_year <- lapply(sites2, starts, "year")


## compute first switch date from as_of
creds2 <- .creds %>% mutate(first_switch = if_else(month(esc_mon) < month(rate_as_of), rate_as_of + months(12 - month(rate_as_of) + month(esc_mon)), rate_as_of + months(month(esc_mon) - month(rate_as_of))))
##compute years since and today's rate
                               
creds2 <- creds2 %>% mutate(years_since = (difftime(Sys.Date(),first_switch, units = "weeks")/52.24) %>% as.numeric() %>% floor() %>% pmax(.,0), today_rate = cur_rate * (1 + esc) ^ years_since)

###

######################## SERVER CODE ########################

server <- function(input, output, session) { 
  
  # for each device, create a list of reactive objects
  # 
  
  reacts <- lapply(names(sites2), function(i) { #names(sites2)
    reactiveFileReader(
      intervalMillis = 10000, # every 5 seconds
      session = session,
      filePath = paste0("data/", i, ".csv"),
      readFunc = read_csv
    )
  }
  )
  
  # reacts <- reactiveFileReader(
  #   intervalMillis = 10000, # every 10 seconds
  #   session = session,
  #   filePath = paste0("data/", "arcola_meter", ".csv"),
  #   readFunc = read_csv
  # )
  
  #reacts2 <- reactive( reacts() %>% mutate(date_time_local = with_tz(date_time_utc, tz = "US/Eastern")))
  
  #reacts2 <- reactive( reacts() %>% mutate(date_time_local = with_tz(date_time_utc, "US/Eastern")) )
  
  
  names(reacts) <- names(sites2)
  
 #### 
 lapply(names(reacts), function(i) {
   output[[paste0("time_live_", i)]] <- renderInfoBox({
     my_time <- reacts[[i]]()[["date_time_utc"]] %>% tail(1)
     tz <- creds2 %>% filter(device == i) %>% select(tz) %>% as.character()
     my_time <- with_tz(my_time, tz)
     valueBox("as of", paste("Last: ", format(my_time, "%H:%M:%S")), icon = icon("clock"), color = "lime")
   })
  })
 ####
 
 lapply(names(reacts), function(i) {
   output[[paste0("inst_kw_", i)]] <- renderC3Gauge({
     my_kw <- reacts[[i]]()[["inst_kw"]] %>% tail(1) %>% as.numeric() 
     size <- creds2 %>% filter(device == i) %>% select(size_kw) %>% as.numeric()# Gauge is a % and must take into account meter kw size
     C3Gauge(sprintf("%1.0f", my_kw/size * 100))
   })
 })
 
 lapply(names(reacts), function(i) {
   output[[paste0("money_today_", i)]] <- renderInfoBox({
     
     last_kwh <- reacts[[i]]()[["cum_kwh"]] %>% tail(1) %>% as.numeric()
     kwh_today <- last_kwh - start_day[[i]][["cum_kwh"]][[1]]
     rate <- creds2 %>% filter(device == i) %>% select(today_rate) %>% as.numeric()
     srec_px <- input$srec_price * (creds2 %>% filter(device == i) %>% select(srecs)) 
     money_today <- kwh_today * (rate + input$srec_price/100) 
     valueBox(
         "today",paste0("$",format(money_today, decimal.mark = ".", big.mark = ",", digits = 2, nsmall = 2)) , icon = icon("dollar"), color = "aqua")
   })
 })
 
 lapply(names(reacts), function(i) {
   output[[paste0("money_month_", i)]] <- renderInfoBox({
     
     last_kwh <- reacts[[i]]()[["cum_kwh"]] %>% tail(1) %>% as.numeric()
     kwh_month <- last_kwh - start_month[[i]][["cum_kwh"]][[1]]
     rate <- creds2 %>% filter(device == i) %>% select(today_rate) %>% as.numeric()
     srec_px <- input$srec_price * (creds2 %>% filter(device == i) %>% select(srecs)) 
     money_month <- kwh_month * (rate + input$srec_price/100) 
     valueBox(
       "mtd",paste0("$",format(money_month, decimal.mark = ".", big.mark = ",", digits = 2, nsmall = 2)) , icon = icon("dollar"), color = "blue")
   })
 })
 
 lapply(names(reacts), function(i) {
   output[[paste0("money_year_", i)]] <- renderInfoBox({
     
     last_kwh <- reacts[[i]]()[["cum_kwh"]] %>% tail(1) %>% as.numeric()
     kwh_year <- last_kwh - start_year[[i]][["cum_kwh"]][[1]]
     rate <- creds2 %>% filter(device == i) %>% select(today_rate) %>% as.numeric()
     srec_px <- input$srec_price * (creds2 %>% filter(device == i) %>% select(srecs)) 
     money_year <- kwh_year * (rate + input$srec_price/100) 
     valueBox(
       "ytd",paste0("$",format(money_year, decimal.mark = ".", big.mark = ",", digits = 2, nsmall = 2)) , icon = icon("dollar"), color = "black")
   })
 })
 
# lapply(names(reacts), function(i) {
#   output[[paste0("money_today_", i)]] <- renderC3Gauge({
#     
#     last_kwh <- reacts[[i]]()[["cum_kwh"]] %>% tail(1) %>% as.numeric()
#     kwh_today <- last_kwh - start_day[[i]][["cum_kwh"]]
#     base_money <- kwh_today * cur_rates
#     size <- .creds %>% filter(device == i) %>% select(size_kw) %>% as.numeric()# Gauge is a % and must take into account meter kw size
#     C3Gauge(sprintf("%1.0f", my_kw/size * 100))
#   })
# })
 
  
  
  #output$gauge_live <- renderC3Gauge({
  #  x <- fileReaderData() %>% do(tail(., 1)) %>% select(inst_kw) %>% as.numeric()
  #  C3Gauge(sprintf("%1.0f", x/255 * 100))
  #})
  
  #last 15 minutes
  #output$plot_15_mins <- renderPlot({
  #  set <- fileReaderData() %>% filter(date_time_utc >= Sys.time() - minutes(200))
  #  ggplot(set) + geom_step(aes(x = date_time_utc, y = inst_kw), color = 'darkblue', alpha = .75) + theme_minimal()
  #})
  
  # Today
  #output$plot_today <- renderPlot({
  #set <- fileReaderData() %>% filter(date_time_utc >= floor_date(Sys.time(), unit = "day"))
  #ggplot(set) + geom_step(aes(x = date_time_utc, y = inst_kw), color = 'darkblue', alpha = .75) + theme_minimal()
  #})
  
  #output$money_today <- renderInfoBox({
  #  kwh_today <- last_cum_kwh() - start_day_cum_kwh
  #  price <- .104 * .95 + .28
  #  valueBox(
  #    "today",paste0("$",format(kwh_today * price, decimal.mark = ".", big.mark = ",", digits = 2, nsmall = 2)) , icon = icon("dollar"),
  #    color = "aqua"
  #  )
  #})

  #output$money_mtd <- renderInfoBox({
  #  kwh_mtd <- last_cum_kwh() - start_month_cum_kwh
  #  price <- .104 * .95 + .28
  #  valueBox(
  #    "MTD", paste0("$",format(kwh_mtd * price, decimal.mark = ".", big.mark = ",", digits = 0, nsmall = 0, scientific = FALSE)), icon = icon("dollar"),
  #    color = "light-blue"
  #  )
  #})
  
  #output$money_ytd <- renderInfoBox({
  #  kwh_ytd <- last_cum_kwh() - start_year_cum_kwh
  #  price <- .104 * .95 + .28
  #  valueBox(
  #    "YTD", paste0("$",format(kwh_ytd * price, decimal.mark = ".", big.mark = ",", digits = 0, nsmall = 0, scientific = FALSE)), icon = icon("money"),
  #    color = "blue"
  #  )
  #})
  
  
  
  }

shinyApp(ui, server)
