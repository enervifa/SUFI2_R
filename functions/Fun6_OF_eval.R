# Function to calculate performance metrics 
## now take performance and if goalfun_name is "Nash_Sutcliff" use NSE and find best iteartion
# Function to find the iteration with maximum NSE or weighted NSE

# Define weights 
# Function to assign weights based on user input
assign_weights <- function(data, flow_weight, ET_weight, LAI_weight,nombre_run=nombre_run,directory=directory) {
  
  distinct_catchments_by_metric <- data %>%
  group_by(metric) %>%
  summarise(n_distinct_catchments = n_distinct(catch))

  # Create a weight column in the data
   data <- data %>%
    # Join the distinct counts back to the main dataset
    left_join(distinct_catchments_by_metric, by = "metric") %>%
    # Assign weights based on metric and variable
    mutate(weight = case_when(
      metric == "flow" ~ flow_weight / n_distinct_catchments,  # 2 for flow
      metric == "ET"   ~ ET_weight / n_distinct_catchments,    # 8 for ET
      metric == "LAI"  ~ LAI_weight / n_distinct_catchments,   # 8 for LAI
      TRUE ~ 1  # Default weight if none of the metrics match
    )) %>%
    select(-n_distinct_catchments)
    
    
    ##save the data
    #write_csv(data, paste0(directory,"/SUFI2.IN/",nombre_run,"/_performance_weighted.csv"))
    
    # data <- data %>%
    #   slice(1)
    return(data)
}


##testing 
# top_row <- assign_weights(combined_performance_all, flow_weight=1, ET_weight=0, LAI_weight=0)
# top_row <- assign_weights(combined_performance_all, flow_weight=0, ET_weight=1, LAI_weight=0)
# top_row <- assign_weights(combined_performance_all, flow_weight=0.33, ET_weight=0.33, LAI_weight=0.33)

#metric_type can take values 
#Objective function type, 1=mult,2=sum,3=r2,4=chi2,5=NS,6=br2,7=ssqr,8=PBIAS,9=KGE,10=RSR,11=MNS

find_best_iteration <- function(data, metric_type,nombre_run=nombre_run,directory=directory) {
  
  metric_column <- metric_type
  
  # Filter the data to include only rows where the metric matches the derived metric_type
  filtered_df <- data %>%
    mutate(weighted_metric = .data[[metric_column]] * weight)
  return(filtered_df)
} 

find_best_iteration_2 <- function(data, metric_type,nombre_run=nombre_run,directory=directory) {
  metric_column <- metric_type
  metric_column <- metric_type
  
  # Filter the data to include only rows where the metric matches the derived metric_type
  filtered_df <- data %>%
    mutate(weighted_metric = .data[[metric_column]] * weight)
  
  filtered_df <-filtered_df %>%
    # Group by 'iter' and sum the weighted metrics
    group_by(iter) %>%
    summarise(weighted_metric = sum(weighted_metric, na.rm = TRUE)) %>%
    # Order the results by weighted metric in descending order
    arrange(desc(weighted_metric)) %>%
    mutate(metric_type = metric_type)
  
  return(filtered_df)
} 

find_best_iteration_dist <- function(data, metrics, OF) {
  filtered_df <- list()  # Initialize an empty list to store filtered data
  
  # Loop through each metric in the metrics vector
  for (i in metrics) {
    # Filter data based on the current metric
    data_m <- data %>%
      filter(metric == i) %>%
      # Arrange by the OF value and select the top 340 rows
      arrange(desc(.data[[OF]])) %>%
      slice(1:340)
    
    # Store the filtered data in the list, using the metric as the list index
    filtered_df[[i]] <- data_m
  }
  
  return(filtered_df)  # Return the list of filtered data frames
}

  
  
  
  
find_best_iteration_2 <- function(data, metric_type,nombre_run=nombre_run,directory=directory) {
  metric_column <- metric_type
  metric_column <- metric_type
  
  # Filter the data to include only rows where the metric matches the derived metric_type
  filtered_df <- data %>%
    mutate(weighted_metric = .data[[metric_column]] * weight)
  
  filtered_df <-filtered_df %>%
    # Group by 'iter' and sum the weighted metrics
    group_by(iter) %>%
    summarise(weighted_metric = sum(weighted_metric, na.rm = TRUE)) %>%
    # Order the results by weighted metric in descending order
    arrange(desc(weighted_metric)) %>%
    mutate(metric_type = metric_type)
  
  return(filtered_df)
} 


find_best_iteration_3 <- function(data, metric_type,nombre_run=nombre_run,directory=directory) {
  metric_column <- metric_type
  metric_column <- metric_type
  
  # Filter the data to include only rows where the metric matches the derived metric_type
  filtered_df <- data %>%
    mutate(weighted_metric = .data[[metric_column]] * weight)
  
  filtered_df <-filtered_df %>%
    # Group by 'iter' and sum the weighted metrics
    group_by(iter) %>%
    summarise(weighted_metric = sum(weighted_metric, na.rm = TRUE)) %>%
    # Order the results by weighted metric in descending order
    arrange(desc(weighted_metric)) %>%
    mutate(metric_type = metric_type) %>%
    ##select the best iteration
    slice(1)
  
  return(filtered_df)
} 

#testing

#flow_best <- find_best_iteration(top_row , 'KGE')
#flow_best <- find_best_iteration_2(top_row , 'KGE')

