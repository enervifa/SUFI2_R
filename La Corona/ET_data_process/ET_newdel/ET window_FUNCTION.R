library(dplyr)
library(lubridate)



file_list<-c()
#For ET data
#setwd("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/rawetdata")

For LAI data
setwd("C:/Users/Lenovo/Desktop/LC_subbasins/LAIdata_subsLC/LAI_subsLC")
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
  #browser()
  # Perform necessary data manipulations
  input_data <- data %>%
    mutate(Date = `system:time_start`) %>%
    mutate(month = month(Date), year = year(Date)) %>%
    mutate(value = undefined / 8)
  #browser()
  # Create a sequence of dates from min_date to max_date
  min_date <- min(input_data$Date)
  max_date <- max(input_data$Date)
  date_seq <- seq(min_date, max_date, by = "day")
  
  # Filter dates at the start and end of each month
  start_of_month <- date_seq[day(date_seq) == 1]
  end_of_month <- date_seq[day(date_seq) == days_in_month(date_seq)]
  
  # Check if the Date matches any date in the end_of_month vector
  input_data2 <- input_data %>%
    mutate(monthly_sum = ifelse(Date %in% end_of_month, undefined, NA))
  
  # Filter out NA values in monthly_sum ##de aca necesitamos columna monthly_sum
  input_data3 <- input_data2 %>%
    select(month,year,monthly_sum)%>%
    na.omit()
  
    
  #now use a for to read every date in date_seq and given the date, calculate the window (seq(the_date - 8, the_date - 1, by = "day") and assing that value
  #to the previous 8 days in the date_seq vector
  # Create a dataframe with the date sequence
  date_seq <- c(seq(min_date - 8, min_date - 1, by = "day"), date_seq)
  date_seq<-as.data.frame(date_seq)
  
  #rename the column
  colnames( date_seq)<-c("Date")
  date_seq_withdata <- date_seq %>%
    mutate(ET_day = NA)
  
  for (i in 1:length(input_data$Date)) {
    the_date <- input_data$Date[i]
    date_lag <- seq(the_date - 8, the_date - 1, by = "day")
    
    # Find indices of date_seq_withdata$Date that match the dates in date_lag
    indices <- which(date_seq_withdata$Date %in% date_lag)
    
    # Assign the corresponding values from input_data2 to ET_day in date_seq_withdata
    date_seq_withdata$ET_day[indices] <- input_data$value[i]
  }
  
  ##now calculate monthly average on ET_day but only add monthly data if month has not na values.
  # Calculate monthly average for non-NA months
  monthly_sum<- date_seq_withdata%>%
    mutate(month = month(Date), year = year(Date)) %>%
    group_by(month, year) %>%
    filter(all(!is.na(ET_day))) %>%
    summarise(monthly_sum = sum(ET_day))%>%
    arrange(year,month)
  
    # Merge monthly_avg with input_data to add missing months
  final_data <- left_join(monthly_sum,  input_data3)
  
  # Append processed dataframe to the list
  processed_data[[file]] <- final_data
  #save the processed_data list as rds object
 
  # Save processed data to a CSV file
  write.csv(final_data, paste0("processed_", file), row.names = FALSE) ## in same directory then need to move files to process data folder
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
