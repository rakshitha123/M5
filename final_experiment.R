BASE_DIR <- "M5" # Change this to the checkout location


# Execute configs
source(file.path(BASE_DIR, "configs", "configs.R"))


# A wrapper function to run the full modelling framework used for the competition solution
run_model <- function(lag){
  # Obtain forecasts for all clusters
  for(clus in 1:length(all_clusters)){
    print(paste0("Starting cluster ", clus))
    processing_cluster <- as.numeric(unlist(strsplit(all_clusters[[clus]], " "))) + 1
    
    # Preprocessing
    input_matrices <- create_input_matrix(sales_dataset, date_info, processing_cluster, lag)
    embedded_series <- input_matrices[[1]] # Embedded matrix
    all_final_lags <- input_matrices[[2]] # Test set
    series_means <- input_matrices[[3]] # Mean value of each series
    
    # Get pooled regression forecasts  
    pr_forecasts <- calculate_forecasts(embedded_series, lag, all_final_lags, series_means, "pooled_regression", processing_cluster)
    pr_forecasts[pr_forecasts < 0] <- 0
    
    # Get LightGBM forecasts
    lightgbm_forecasts <- calculate_forecasts(embedded_series, lag, all_final_lags, series_means, "lightgbm", processing_cluster)
    lightgbm_forecasts[lightgbm_forecasts < 0] <- 0
    
    # Average the sub-model forecasts
    final_forecasts <- (pr_forecasts + lightgbm_forecasts)/2
    
    # Clusters contain series in the same order as in original data file. Hence, can directly write forecasts into the output file.
    dir.create(file.path(BASE_DIR, "results"), showWarnings = FALSE, recursive=TRUE)
    write.table(final_forecasts, output_file_name, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",", append = TRUE)
    
    print(paste0("Finished cluster ", clus))
  }


  # Prepare submission in the format required by the M5 competition. 
  # The submission contains the forecasts for both validation and evaluation phases.
  validation_results <- sales_dataset[,(ncol(sales_dataset)-27):ncol(sales_dataset)]
  evaluation_results <- read.csv(output_file_name, header = F)
  
  validation_results$id <- paste0(sales_dataset$item_id,"_",sales_dataset$store_id,"_validation")
  evaluation_results$id <-  paste0(sales_dataset$item_id,"_",sales_dataset$store_id,"_evaluation")
  
  final_validation_results <- validation_results[,c(29,1:28)]
  final_evaluation_results <- evaluation_results[,c(29,1:28)]
  
  names <- c("id", sprintf("F%s",seq(1:28)))
  colnames(final_validation_results) <- names
  colnames(final_evaluation_results) <- names
  
  final_submission_results <- rbind(final_validation_results, final_evaluation_results)
  
  write.csv(final_submission_results, file.path(BASE_DIR, "results", "final_submision.csv"), row.names = FALSE)
}


# Get forecasts
run_model(20) # Change the lags as required. For the solution, I used 400 lags.
