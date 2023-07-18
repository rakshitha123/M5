# Helper functions to preprocess external variables

get_day_of_month <- function(x){
  x <- strsplit(as.character(x), "-")
  as.numeric(x[[1]][3])
}


get_is_weekday <- function(x){
  weekday <- 1
  
  if(as.character(x) %in% c("Saturday", "Sunday"))
    weekday <- 0
  
  weekday
}


get_event_data <- function(x, all_events){
  which(all_events == as.character(x))
}