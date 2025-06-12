library(dplyr)
library(lubridate)
library(readr)


file_list<-c()
#For ET data
#setwd("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/rawetdata")

##For LAI data
# setwd("C:/Users/Lenovo/Desktop/LC_subbasins/LAIdata_subsLC/LAI_subsLC") ## old modis data
 setwd("C:/Users/Lenovo/Desktop/LC_subbasins/LAIHIQ_data_newdel")
list_of_files <- list.files(pattern = ".csv$")
# 
# ##otracuenca_borracho 41
# #CHANGE NAME IN DATE AND FORMAT TO DMYYYY
# 
# # loop over files to read them and change date format
# for (i in 1:length(list_of_files)) {
#   file_list[[i]] <- read_csv(paste0("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/", list_of_files[i]), 
#                              col_types = cols('system:time_start' = col_date(format = "%b %d, %Y")), na="")
#   
# }

# Initialize an empty list to store processed dataframes
processed_data <- list()

# Loop over each file
for (file in list_of_files) {
  # Read the file
  data <- read_csv(file, col_types = cols('system:time_start' = col_date(format = "%b %d, %Y")), na="")
  
  # Perform necessary data manipulations
  input_data <- data %>%
    mutate(Date = `system:time_start`) %>%
    mutate(month = month(Date), year = year(Date))
  
  # Create a sequence of dates from min_date to max_date
  min_date <- min(input_data$Date)
  max_date <- max(input_data$Date)
  date_seq <- seq(min_date, max_date, by = "day")
  
  # Interpolate LAI values to daily frequency
  interpolated_data <- data.frame(Date = date_seq) %>%
    left_join(input_data, by = "Date") %>%
    mutate(LAdata = undefined * 0.1) %>%
    mutate(LAI = if_else(is.na(LAdata), approx(x = Date, y = LAdata, xout = Date)$y, LAdata))
  
  
  # Calculate monthly mean LAI values
  monthly_mean <- interpolated_data %>%
    mutate(month = month(Date), year = year(Date)) %>%
    group_by(month, year) %>%
    summarise(monthly_mean = mean(LAI, na.rm = TRUE)) %>%
    arrange(year, month)
  
  
  # Append processed dataframe to the list
  processed_data[[file]] <-  monthly_mean
  
  # Save processed data to a CSV file
  write.csv(monthly_mean, paste0("processed_", file), row.names = FALSE) # Save in the current directory
}

saveRDS(processed_data, file = "processed_data.rds")
######################

# Combine processed dataframes into a single dataframe
combined_data <- bind_rows(processed_data)

# Sort combined_data by year and month
combined_data <- combined_data %>%
  arrange(year, month)

# Save combined_data to CSV
write.csv(combined_data, "combined_data2.csv", row.names = FALSE)
