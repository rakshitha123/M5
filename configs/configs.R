# Execute helper scripts
source(file.path(BASE_DIR, "models", "global_models.R"))
source(file.path(BASE_DIR, "utils", "global_model_helper.R"))
source(file.path(BASE_DIR, "utils", "date_helper.R"))


# Set mode to "comp" (run with the competition dataset and original clusters) or demo (run with a demo dataset and demo clusters) 
mode <- "demo"


# Load datasets
date_info <- read.csv(file.path(BASE_DIR, "dataset", "calendar.csv"), header = TRUE)

if(mode == "demo"){
  all_clusters <- readLines(file.path(BASE_DIR, "dataset", "demo_data", "demo_clusters.txt"))
  sales_dataset <- read.csv(file.path(BASE_DIR, "dataset", "demo_data", "demo_sales_train_evaluation.csv"), header = TRUE)
}else{
  all_clusters <- readLines(file.path(BASE_DIR, "dataset", "m5_clusters_70.txt"))
  
  if (file.exists(file.path(BASE_DIR, "dataset", "sales_train_evaluation.zip"))){
    unzip(file.path(BASE_DIR, "dataset", "sales_train_evaluation.zip"), exdir = file.path(BASE_DIR, "dataset"))
    file.remove(file.path(BASE_DIR, "dataset", "sales_train_evaluation.zip"))
  }
  sales_dataset <- read.csv(file.path(BASE_DIR, "dataset", "sales_train_evaluation.csv"), header = TRUE)
}


# Results file name
output_file_name <- file.path(BASE_DIR,"results", "m5_forecasts.txt")


# Process external variables
event_name_one_vals <- unique(date_info$event_name_1)
event_name_two_vals <- unique(date_info$event_name_2)
event_type_one_vals <- unique(date_info$event_type_1)
event_type_two_vals <- unique(date_info$event_type_2)

date_info$day <- sapply(date_info$date, get_day_of_month)
date_info$isweekday <- sapply(date_info$weekday, get_is_weekday)
date_info$event_name_1_converted <- sapply(date_info$event_name_1, get_event_data, event_name_one_vals)
date_info$event_name_2_converted <- sapply(date_info$event_name_2, get_event_data, event_name_two_vals)
date_info$event_type_1_converted <- sapply(date_info$event_type_1, get_event_data, event_type_one_vals)
date_info$event_type_2_converted <- sapply(date_info$event_type_2, get_event_data, event_type_two_vals)