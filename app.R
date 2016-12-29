library(shiny)
library(shinydashboard)
library(dplyr)
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
              column(h2(" Laurita"), width = 2),
              column(C3GaugeOutput("gauge_live", height = 90), width =2),
              box(title = textOutput("time_live"),background = "teal", solidHeader = TRUE, width = 2, height = 100, h3(tags$strong(textOutput("kw_live")))),
              valueBoxOutput("money_today", width = 2),
              valueBoxOutput("money_mtd", width = 2),
              valueBoxOutput("money_ytd", width = 2)
            ),
            br(),
            fluidRow(
              column(h2(" Johnston"), width = 2),
              column(C3GaugeOutput("gauge_live_john", height = 90), width =2),
              box(title = textOutput("time_live_john"),background = "teal", solidHeader = TRUE, width = 2, height = 100, h3(tags$strong(textOutput("kw_live_john")))),
              valueBoxOutput("money_today_john", width = 2),
              valueBoxOutput("money_mtd_john", width = 2),
              valueBoxOutput("money_ytd_john", width = 2)
            )
    ),

    tabItem(tabName = "irr",
            h2("IRR tab content")
    )
  )
)

ui <- dashboardPage(header, sideBar, body)



### The "Run once section"

infile_lau <- "./data/laurita_data.csv"
infile_siel <- "data/johnston_siel_data.csv"
infile_gro <- "./data/johnston_growatt_data.csv"

#### Laurita calculations ####
# some static calcs that don't need to be updated so often useful constants
static_data <- read_csv(infile_lau)
static_data <- static_data %>% mutate(date_time_est = fastPOSIXct(date_time_utc, tz = "America/New_York")) 

## Future Michael: the following code will be difficult to reason about
# what you're doing:
# having added 'date_time_est' to static date above, 
# you are then filtering on the last entry on the previous day
# by taking only values where the day == ceiling date of two days ago
# aka "yesterday"  
# ie test 'ceiling_date(Sys.time() - days(2), unit = "day")
start_day_cum_kwh <- static_data %>% filter(day(date_time_est) == day(ceiling_date(Sys.time() - days(2), unit = "day"))) %>% select(cum_kwh) %>% summarise(last(cum_kwh)) %>% as.numeric()
start_month_cum_kwh <- static_data %>% filter(date_time_est >= floor_date(Sys.time(), unit = "month")) %>% select(cum_kwh) %>% summarise(first(cum_kwh)) %>% as.numeric()
start_year_cum_kwh <- static_data %>% filter(date_time_est >= floor_date(Sys.time(), unit = "year")) %>% select(cum_kwh) %>% summarise(first(cum_kwh)) %>% as.numeric()


###
#### Johnston calculations ####
# some static calcs that don't need to be updated so often useful constants
static_data_siel <- read_csv(infile_siel)
static_data_gro <- read_csv(infile_gro)

static_data_siel <- static_data_siel %>% mutate(date_time_est = fastPOSIXct(date_time_utc, tz = "America/New_York")) 

static_data_gro <- static_data_gro %>% mutate(date_time_est = fastPOSIXct(date_time_utc, tz = "America/New_York")) 

start_day_cum_kwh_john <- static_data_siel %>% filter(day(date_time_est) == day(ceiling_date(Sys.time() - days(2), unit = "day"))) %>% select(cum_kwh) %>% summarise(last(cum_kwh)) %>% as.numeric() +
  static_data_gro %>% filter(day(date_time_est) == day(ceiling_date(Sys.time() - days(2), unit = "day"))) %>% select(cum_kwh) %>% summarise(last(cum_kwh)) %>% as.numeric()

start_month_cum_kwh_john <- static_data_siel %>% filter(date_time_est >= floor_date(Sys.time(), unit = "month")) %>% select(cum_kwh) %>% summarise(first(cum_kwh)) %>% as.numeric() + 
  static_data_gro %>% filter(date_time_est >= floor_date(Sys.time(), unit = "month")) %>% select(cum_kwh) %>% summarise(first(cum_kwh)) %>% as.numeric()

start_year_cum_kwh_john <- static_data_siel %>% filter(date_time_est >= floor_date(Sys.time(), unit = "year")) %>% select(cum_kwh) %>% summarise(first(cum_kwh)) %>% as.numeric() + 
  static_data_gro %>% filter(date_time_est >= floor_date(Sys.time(), unit = "year")) %>% select(cum_kwh) %>% summarise(first(cum_kwh)) %>% as.numeric()

######################## SERVER CODE ########################

server <- function(input, output, session) { 


  
  # reactive calcs that will update often
  fileReaderData <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = infile_lau,
    readFunc = read_csv
  )
  
  last_cum_kwh <- reactive({
    fileReaderData() %>% filter(date_time_utc == max(date_time_utc)) %>% select(cum_kwh) %>% tail(1) %>% as.numeric() })

    last_kw_time <- reactive({
    fileReaderData() %>% filter(date_time_utc == max(date_time_utc)) %>% mutate(date_time_est = fastPOSIXct(date_time_utc), tz = "America/New_York") %>% tail(1) %>% select(date_time_est)})
    
    last_kw <- reactive({
      fileReaderData() %>% filter(date_time_utc == max(date_time_utc)) %>% mutate(date_time_est = fastPOSIXct(date_time_utc), tz = "America/New_York") %>% select(kw) %>% tail(1) %>% as.numeric() })

### End Laurita Calcs ###
    
### Laurita Server funcs ###
  
  ###live
  output$time_live <- renderText({
    paste("Last: ", format(last_kw_time()[[1]], "%H:%M:%S"))
  })
  
  output$kw_live <- renderText({
    paste(last_kw(), "kW")
  })
  
  
  output$gauge_live <- renderC3Gauge({
    x <- fileReaderData() %>% do(tail(., 1)) %>% select(kw) %>% as.numeric()
    C3Gauge(sprintf("%1.0f", x/255 * 100))
  })
  
  #last 15 minutes
  output$plot_15_mins <- renderPlot({
    set <- fileReaderData() %>% filter(date_time_est >= Sys.time() - minutes(200))
    ggplot(set) + geom_step(aes(x = date_time_est, y = kw), color = 'darkblue', alpha = .75) + theme_minimal()
  })
  
  # Today
  output$plot_today <- renderPlot({
  set <- fileReaderData() %>% filter(date_time_est >= floor_date(Sys.time(), unit = "day"))
  ggplot(set) + geom_step(aes(x = date_time_est, y = kw), color = 'darkblue', alpha = .75) + theme_minimal()
  })
  
  output$money_today <- renderInfoBox({
    kwh_today <- last_cum_kwh() - start_day_cum_kwh
    price <- .104 * .95 + .28
    valueBox(
      "today",paste0("$",format(kwh_today * price, decimal.mark = ".", big.mark = ",", digits = 2, nsmall = 2)) , icon = icon("dollar"),
      color = "aqua"
    )
  })

  output$money_mtd <- renderInfoBox({
    kwh_mtd <- last_cum_kwh() - start_month_cum_kwh
    price <- .104 * .95 + .28
    valueBox(
      "MTD", paste0("$",format(kwh_mtd * price, decimal.mark = ".", big.mark = ",", digits = 0, nsmall = 0, scientific = FALSE)), icon = icon("dollar"),
      color = "light-blue"
    )
  })
  
  output$money_ytd <- renderInfoBox({
    kwh_ytd <- last_cum_kwh() - start_year_cum_kwh
    price <- .104 * .95 + .28
    valueBox(
      "YTD", paste0("$",format(kwh_ytd * price, decimal.mark = ".", big.mark = ",", digits = 0, nsmall = 0, scientific = FALSE)), icon = icon("money"),
      color = "blue"
    )
  })
  
  
  # reactive calcs that will update often
  fileReaderData_siel <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = infile_siel,
    readFunc = read_csv
  )
  
  fileReaderData_gro <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = infile_gro,
    readFunc = read_csv
  )
  
  last_cum_kwh_john <- reactive({
    fileReaderData_siel() %>% filter(date_time_utc == max(date_time_utc)) %>% select(cum_kwh) %>% tail(1) %>% as.numeric() +
    fileReaderData_gro() %>% filter(date_time_utc == max(date_time_utc)) %>% select(cum_kwh) %>% tail(1) %>% as.numeric()
      })
  
  last_kw_john <- reactive({
    fileReaderData_siel() %>% filter(date_time_utc == max(date_time_utc)) %>% mutate(date_time_est = fastPOSIXct(date_time_utc), tz = "America/New_York") %>% select(kw) %>% tail(1) %>% as.numeric() +
      fileReaderData_gro() %>% filter(date_time_utc == max(date_time_utc)) %>% mutate(date_time_est = fastPOSIXct(date_time_utc), tz = "America/New_York") %>% select(kw) %>% tail(1) %>% as.numeric() })
  
  ### End Johnston Calcs ###
  ###live
  output$time_live_john <- renderText({
    format(last_kw_time()[[1]], "%H:%M:%S")
  })
  
  output$kw_live_john <- renderText({
    paste(last_kw_john(), "kW")
  })
  
  
  output$gauge_live_john <- renderC3Gauge({
    x <- fileReaderData_siel() %>% do(tail(., 1)) %>% select(kw) %>% as.numeric() +
      fileReaderData_gro() %>% do(tail(., 1)) %>% select(kw) %>% as.numeric()
    C3Gauge(sprintf("%1.0f", x/170.4 * 100))
  })
  
  #last 15 minutes
  output$plot_15_mins <- renderPlot({
    set <- fileReaderData() %>% filter(date_time_est >= Sys.time() - minutes(200))
    ggplot(set) + geom_step(aes(x = date_time_est, y = kw), color = 'darkblue', alpha = .75) + theme_minimal()
  })
  
  # Today
  output$plot_today <- renderPlot({
    set <- fileReaderData() %>% filter(date_time_est >= floor_date(Sys.time(), unit = "day"))
    ggplot(set) + geom_step(aes(x = date_time_est, y = kw), color = 'darkblue', alpha = .75) + theme_minimal()
  })
  
  output$money_today_john <- renderInfoBox({
    kwh_today <- last_cum_kwh_john() - start_day_cum_kwh_john
    price <- .120
    valueBox(
      "today",paste0("$",format(kwh_today * price, decimal.mark = ".", big.mark = ",", digits = 2, nsmall = 2)) , icon = icon("dollar"),
      color = "aqua"
    )
  })
  
  output$money_mtd_john <- renderInfoBox({
    kwh_mtd <- last_cum_kwh_john() - start_month_cum_kwh_john
    price <- .12
    valueBox(
      "MTD", paste0("$",format(kwh_mtd * price, decimal.mark = ".", big.mark = ",", digits = 0, nsmall = 0, scientific = FALSE)), icon = icon("dollar"),
      color = "light-blue"
    )
  })
  
  output$money_ytd_john <- renderInfoBox({
    kwh_ytd <- last_cum_kwh_john() - start_year_cum_kwh_john
    price <- .12
    valueBox(
      "YTD", paste0("$",format(kwh_ytd * price, decimal.mark = ".", big.mark = ",", digits = 0, nsmall = 0, scientific = FALSE)), icon = icon("money"),
      color = "blue"
    )
  })
  
  }

shinyApp(ui, server)
