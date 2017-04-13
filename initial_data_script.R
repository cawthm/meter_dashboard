### When starting the app, a time consuming step is to compute the starting values
##   used in computing daily, mtd, and ytd figures.
##   this script is run once per day (at 6am UTC, 2/1am EST, 11/10pm HST) to 
##   pre-emptively generate these figures, which are then saved into lists of tibbles

starts <- function(my_df, unit) { my_df %>% filter(date(date_time_local) <= (floor_date(now(), unit = unit) - days(1))) %>% distinct() %>% top_n(1, date_time_local) }