# A function to create the embedded matrix and final lags to train the global models 
#
# Parameters
# sales_dataset - the competition dataset at the evaluation phase
# date_info - a dataframe containing the date related external variables
# cluster_series_numbers - series indexes (numbers) corresponding with series in the current cluster
# lag - number of past lags required for model training
create_input_matrix <- function(sales_dataset, date_info, cluster_series_numbers, lag) {
  
  embedded_series <- NULL
  final_lags <- NULL
  series_means <- NULL
  
  dataset <- sales_dataset[cluster_series_numbers, 7:ncol(sales_dataset)]
  
  
  for (i in 1:nrow(dataset)) {
    # print(i)
    time_series <- dataset[i,]
    time_series <- as.numeric(time_series)
    
    # Perform mean scale normalisation 
    mean <- mean(time_series)
    
    if(mean == 0)
      mean <- 1 # Avoid division by zero
    
    series_means <- c(series_means, mean)
    time_series <- time_series / mean
    
    # Embedding the series
    embedded <- embed(time_series, lag + 1)
    
    # Adding external variables
    state <- as.character(sales_dataset[cluster_series_numbers[i], "state_id"])
    weekday_vec <- as.numeric(date_info[(lag+1):length(time_series), "isweekday"]) # is weekday day or not
    month_vec <- as.numeric(date_info[(lag+1):length(time_series), "month"]) # month
    snap_vec <- as.numeric(date_info[(lag+1):length(time_series), paste0("snap_", state)]) # is a day with snap or not
    wday_vec <- as.numeric(date_info[(lag+1):length(time_series), "wday"]) # day of week
    day_vec <- as.numeric(date_info[(lag+1):length(time_series), "day"]) # day of month
    event_name_1_vec <- date_info[(lag+1):length(time_series), "event_name_1_converted"] # event name 1
    event_name_2_vec <- date_info[(lag+1):length(time_series), "event_name_2_converted"] # event name 2
    event_type_1_vec <- date_info[(lag+1):length(time_series), "event_type_1_converted"] # event type 1
    event_type_2_vec <- date_info[(lag+1):length(time_series), "event_type_2_converted"] # event type 2
    
    embedded <- cbind(embedded, weekday_vec, month_vec, snap_vec, wday_vec, day_vec, event_name_1_vec, event_name_2_vec, event_type_1_vec, event_type_2_vec)
    
    if (!is.null(embedded_series)) 
      embedded_series <- as.matrix(embedded_series)
    
    embedded_series <- rbind(embedded_series, embedded)
    
    if (!is.null(final_lags)) 
      final_lags <- as.matrix(final_lags)
    
    # Creating the test set
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))
    
    # Adding external features to the test set
    current_series_final_lags <- cbind(current_series_final_lags,
                                       date_info[(length(time_series)+1), "isweekday"],
                                       date_info[(length(time_series)+1), "month"],
                                       date_info[(length(time_series)+1), paste0("snap_", state)],
                                       date_info[(length(time_series)+1), "wday"],
                                       date_info[(length(time_series)+1), "day"],
                                       date_info[(length(time_series)+1), "event_name_1_converted"],
                                       date_info[(length(time_series)+1), "event_name_2_converted"],
                                       date_info[(length(time_series)+1), "event_type_1_converted"],
                                       date_info[(length(time_series)+1), "event_type_2_converted"])
    
    final_lags <- rbind(final_lags, as.data.frame(current_series_final_lags))
  }
  
  # Changing the column types of embedded matrix - lags are numeric and external variables are factors
  embedded_series <- as.data.frame(embedded_series)
  colnames(embedded_series)[1] <- "y"
  colnames(embedded_series)[2:(lag+1)] <- paste("Lag", 1:lag, sep = "")
  indx <- 1:(lag+1)
  embedded_series[indx] <- lapply(embedded_series[indx], function(x) as.numeric(x))
  indx <- (lag+2):ncol(embedded_series)
  embedded_series[indx] <- lapply(embedded_series[indx], function(x) as.factor(x))
  
  
  # Changing the column type of test set - lags are numeric and external variables are factors
  final_lags <- as.data.frame(final_lags)
  colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep = "")
  colnames(final_lags)[(lag+1):ncol(final_lags)] <- colnames(embedded_series)[(lag+2): ncol(embedded_series)]
  indx <- 1:lag
  final_lags[indx] <- lapply(final_lags[indx], function(x) as.numeric(x))
  indx <- (lag+1):ncol(final_lags)
  final_lags[indx] <- lapply(final_lags[indx], function(x) as.factor(x))
  
  list(embedded_series, final_lags, series_means)
}
