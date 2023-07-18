# Implementation of global models: pooled regression and LightGBM

library(glmnet)
library(lightgbm)



# A wrapper function to obtain forecasts

# Parameters
# embedded_series - embeded matrix (training data)
# lag - no. of past lags that are used to train the model
# final_lags - test set
# series_means - the calculated mean values of the series in the cluster
# method - pooled_regression or lightgbm
# cluster_series_numbers - series indexes (numbers) corresponding with the series in the required cluster
# forecast_horizon - the number of forecasts that need to be obtained (default value = 28)
calculate_forecasts <- function(embedded_series, lag, final_lags, series_means, method, cluster_series_numbers, forecast_horizon = 28){
  
  # Model Fitting
  model <- fit_model(embedded_series, lag, method)
  
  # Forecasting
  forec_recursive(model, final_lags, lag, series_means, method, cluster_series_numbers, forecast_horizon)
}



# Fit and forecast from a global model

# Parameters
# fitting_data - embeded matrix (training data)
# lag - no. of past lags that are used to train the model
# method - pooled_regression or lightgbm
fit_model <- function(fitting_data, lag, method) {
  # Create the formula
  formula <- "y ~ "
  for(predictor in 2:ncol(fitting_data)){
    if(predictor != ncol(fitting_data)){
      formula <- paste0(formula, colnames(fitting_data)[predictor], " + ")
    }else{
      formula <- paste0(formula, colnames(fitting_data)[predictor])
    }
  }
  
  formula <- paste(formula, "+ 0", sep="")
  formula <- as.formula(formula)
  
  # Fit global models
  if(method == "pooled_regression"){
    model <- glm(formula = formula, data = fitting_data)
  }else if(method == "lightgbm"){
    # Defining hyperparameters
    lgb.grid <- list(objective = "poisson",
                     metric = "rmse",
                     bagging_freq = 1,
                     lambda_l2 = 0.1,
                     learning_rate = 0.075,
                     num_leaves = 128,
                     min_data_in_leaf = 100,
                     boosting_type = 'gbdt',
                     force_row_wise = TRUE,
                     sub_row = 0.75,
                     verbosity = -1,
                     num_iterations = 1200)
    
    train <- Matrix(as.matrix(fitting_data[-1]), sparse = TRUE)
    y_train <- as.numeric((fitting_data[,1]))
    dtrain <- lgb.Dataset(data = train, label = y_train, free_raw_data = FALSE)
    
    categoricals.vec <- colnames(fitting_data)[-(1:(lag+1))]
    
    model <- lgb.train(params = lgb.grid, data = dtrain, categorical_feature = categoricals.vec )
  }
  
  model
}



# Recursive forecasting of the series until a given horizon

# Parameters
# model - a trained pooled regression or lightgbm model
# final_lags - test set
# lag - no. of past lags that are used to train the model
# series_means - the calculated mean values of the series in the cluster
# method - pooled_regression or lightgbm
# cluster_series_numbers - series indexes (numbers) corresponding with the series in the required cluster
# train_length - the length of a sales series
# forecast_horizon - the number of forecasts that need to be obtained (default value = 28)
forec_recursive <- function(model, final_lags, lag, series_means, method, cluster_series_numbers, train_length = 1941, forecast_horizon = 28){
  
  # This will store the predictions corresponding with each horizon
  predictions <- NULL
  
  for (ho in 1:forecast_horizon){
    # Get predictions for the current horizon
    if(method == "pooled_regression")
      new_predictions <- predict.glm(object = model, newdata = as.data.frame(final_lags)) 
    else if(method == "lightgbm")
      new_predictions <- predict(model, Matrix(as.matrix(final_lags), sparse=TRUE))
    
    # Adding the current forecasts to the final predictions matrix
    predictions <- cbind(predictions, new_predictions)
    
    # Updating the test set for the next horizon
    if(ho < forecast_horizon){
      final_lags <- final_lags[-lag]
      final_lags <- cbind(new_predictions, final_lags)
      colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
      
      for(col in (lag+1):ncol(final_lags))
        final_lags[col] <- lapply(final_lags[col], as.character)[[1]]
      
      # Updating the external variables for the next horizon
      state <- as.character(sales_dataset[cluster_series_numbers[1], "state_id"]) # Based on the way I have clustered, the state of a given cluster is the same for all its series
      final_lags["weekday_vec"] <- date_info[(train_length+ho+1), "isweekday"]
      final_lags["month_vec"] <- date_info[(train_length+ho+1), "month"]
      final_lags["snap_vec"] <- date_info[(train_length+ho+1), paste0("snap_", state)]
      final_lags["wday_vec"] <- date_info[(train_length+ho+1), "wday"]
      final_lags["day_vec"] <- date_info[(train_length+ho+1), "day"]
      final_lags["event_name_1_vec"] <- date_info[(train_length+ho+1), "event_name_1_converted"]
      final_lags["event_name_2_vec"] <- date_info[(train_length+ho+1), "event_name_2_converted"]
      final_lags["event_type_1_vec"] <- date_info[(train_length+ho+1), "event_type_1_converted"]
      final_lags["event_type_2_vec"] <- date_info[(train_length+ho+1), "event_type_2_converted"]
      
      for(col in (lag+1):ncol(final_lags))
        final_lags[col] <- lapply(final_lags[col], as.factor)[[1]]
      
      final_lags <- as.data.frame(final_lags)
    }
  }
  
  # Renormalise the predictions
  true_predictions <- predictions * as.vector(series_means)
  true_predictions
}