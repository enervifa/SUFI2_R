# Load necessary libraries
library(tidyverse)
library(lubridate)

# 1. Read the input dataset containing system_time_start and the data values (B).
# Replace 'input_data.csv' with the path to your input dataset
input_data <- read_csv("C:/Users/Lenovo/Documents/input_data.csv",
                       col_types = cols('system:time_start' = col_date(format = "%d-%b-%y")),
                       na="")

# 2. Create a new column to extract the month from system_time_start.
input_data2 <- input_data %>%
  rename(Date='system:time_start')%>%
  mutate(month = month(Date))%>%
  mutate(year = year(Date))

# Get the minimum and maximum dates
min_date <- min(input_data2$Date)
max_date <- max(input_data2$Date)

# Generate a sequence of dates from min_date to max_date
date_seq <- seq(min_date, max_date, by = "day")

# Filter dates at the start and end of each month, considering leap years
start_of_month <- date_seq[day(date_seq) == 1]
end_of_month <- date_seq[day(date_seq) == days_in_month(date_seq)]

# Create a new column 'cum_month' and assign NA to all rows initially
input_data3 <- input_data2 %>%
  mutate(cum_month = NA)

# Check if the Date matches any date in the end_of_month vector
for (i in 1:length(end_of_month)) {
    input_data3$cum_month <- ifelse(input_data2$Date == end_of_month[i],
                                 monthly_cumulative_sum[i],
                                 input_data3$cum_month)
}


# Get the first month and year from the input data
first_month <- month(input_data2$Date[1])
first_year <- year(input_data2$Date[1]) 

# Filter the dataset to select values that match the first month and year, and fall between the 9th and the end of the month
filtered_data <- input_data2 %>%
  filter(month(Date) == first_month & 
           year(Date) == first_year &
           day(Date) >= 9)
# Extract values from the 'undefined' and 'cum_month' columns
extracted_values <- filtered_data%>%
  summarise(ET=sum(undefined))

###add that in a new column
input_data3 <- input_data3 %>%
  mutate(cum_month = ifelse(input_data2$Date %in% filtered_data$Date, extracted_values$ET, input_data3$cum_month))


# Add the corresponding cumulative sum values to the cum_month column
values <- ifelse(input_data2$Date %in% filtered_data$Date,,
                               input_data$cum_month)

# 3. Initialize an empty vector to store the monthly cumulative sum.
monthly_cumulative_sum <- c()

# 4. Loop over each unique date
unique_months <- unique(input_data2$month)
for (date in unique_dates) {
  # 5. Subset the data for the current month.
  current_month_data <- filter(input_data2 , input_data2$Date == date)
  browser()
  # Initialize cumulative sum for the current month
  cumulative_sum <- 0
  
  # 6. Loop over each observation in the current month
  for (i in 1:nrow(current_month_data)) {
    # 7. Calculate the previous 8-day window based on the system_time_start date.
    previous_8_days <- current_month_data$B[i-7:i-1]
    browser()
    # 8. Calculate the number of days in the 8-day window that belong to the actual month.
    days_in_month <- sum(month(current_month_data$system_time_start[i-7:i-1]) == month(current_month_data$system_time_start[i]))
    
    # 9. Divide the data value (B) by the number of days to have the average by day in the window.
    #    Then multiply this value by the number of days and add it to the cumulative sum for the actual month.
    cumulative_sum <- cumulative_sum + (sum(previous_8_days) / days_in_month)
    browser()
    # 10. Subtract the computed value from the data value (B) and store it for the next month.
    current_month_data$B[i] <- current_month_data$B[i] - ((sum(previous_8_days) / days_in_month) * days_in_month)
  }
  
  # 11. Store the cumulative sum for the last observation of the month.
  monthly_cumulative_sum <- c(monthly_cumulative_sum, cumulative_sum)
}

# 12. Create a dataframe with the monthly cumulative sum.
monthly_cumulative_df <- data.frame(system_time_start = unique_dates, monthly_cumulative_sum = monthly_cumulative_sum)

# 13. Print or export the resulting dataframe.
print(monthly_cumulative_df)
