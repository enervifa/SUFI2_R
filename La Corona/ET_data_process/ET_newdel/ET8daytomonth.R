# Load necessary libraries
library(tidyverse)
library(lubridate)


file_list<-c()
setwd("C:/Users/Lenovo/Desktop/LC_subbasins/ET_newdel")
list_of_files <- list.files(pattern = ".csv$")

##otracuenca_borracho 41
#CHANGE NAME IN DATE AND FORMAT TO DMYYYY

# loop over files to read them and change date format
for (i in 1:length(list_of_files)) {
  file_list[[i]] <- read_csv(paste0("C:/Users/Lenovo/Desktop/LC_subbasins/ET_newdel/", list_of_files[i]), 
                             col_types = cols('system:time_start' = col_date(format = "%b %d, %Y")), na="")
  
}


####write a for to apply this to every file

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

## 3. As each day in the dataset corresponds to 8 day observation in 8 previous days,
## we need to calculate the 8 day series and they agregate it. This means to map every obs in input_data dataset and divide it into 8, and assing
##this value to the previous 8 days of that date. 
## For example, if we have a value of 100 in 2020-01-01, we need to assign this value/8 to 2019-12-31, 2020-01-30, 2020-01-28, 2020-01-29, 2020-01-28, 2020-01-27, 2020-01-26 2020-01-25.
## We will use the function map to do this.




#######################

### eeach date in input_data2 will be divided by 8 and replicated for 8 previous days in date_seq vector
## we will use the function map to do this

# Divide each value by 8 and replicate it for 8 days
input_data3 <- input_data2 %>%
  mutate(value = input_data2$undefined/8)

##now read every date in date_seq and given the date, calculate the window (seq(the_date - 8, the_date - 1, by = "day") and assing that value
##to the previous 8 days in the date_seq vector

# Create a dataframe with the date sequence

date_seq<-as.data.frame(date_seq)

#rename the column
colnames( date_seq)<-c("Date")

#now use a for to read every date in date_seq and given the date, calculate the window (seq(the_date - 8, the_date - 1, by = "day") and assing that value
#to the previous 8 days in the date_seq vector

date_seq_withdata <- date_seq %>%
  mutate(ET_day = NA)

for (i in 1:length(input_data3$Date)) {
  the_date <- input_data3$Date[i]
  date_lag <- seq(the_date - 8, the_date - 1, by = "day")
  
  # Find indices of date_seq_withdata$Date that match the dates in date_lag
  indices <- which(date_seq_withdata$Date %in% date_lag)
  
  # Assign the corresponding values from input_data2 to ET_day in date_seq_withdata
  date_seq_withdata$ET_day[indices] <- input_data3$value[i]
}

##now calculate monthly average on ET_day but only add monthly data if month has not na values.
# Calculate monthly average for non-NA months
monthly_avg <- date_seq_withdata%>%
  mutate(month = month(Date), year = year(Date)) %>%
  group_by(month, year) %>%
  filter(all(!is.na(ET_day))) %>%
  summarise(monthly_sum = sum(ET_day))

## now chech in the input_data3 if there is data at the end of a month and add that month if it is not in the monthly_avg dataframe
## we will use the function map to do this

# Check if the Date matches any date in the end_of_month vector
##first create a vector of dates with start and end date

# Get the minimum and maximum dates
min_date <- min(input_data2$Date)
max_date <- max(input_data2$Date)

# Generate a sequence of dates from min_date to max_date
date_seq <- seq(min_date, max_date, by = "day")

# Filter dates at the start and end of each month, considering leap years
start_of_month <- date_seq[day(date_seq) == 1]
end_of_month <- date_seq[day(date_seq) == days_in_month(date_seq)]

# Create a new column 'cum_month' and assign NA to all rows initially This accounts for months that do not need analysis
input_data4 <- input_data2 %>%
  mutate(monthly_sum = NA)

# Check if the Date matches any date in the end_of_month vector
for (i in 1:length(end_of_month)) {
  input_data4$monthly_sum <- ifelse(input_data4$Date == end_of_month[i],
                                  input_data4$undefined,
                                  input_data4$monthly_sum)
}

input_data5 <- input_data4 %>%
  select(month,year,monthly_sum)%>%
  na.omit()
##now left join the monthly_avg dataframe with the input_data4 dataframe to add the months that were not in the monthly_avg dataframe

# Left join the monthly_avg dataframe with the input_data4 dataframe

final_data <- full_join(input_data5, monthly_avg)
#arrange final data by month,year
final_data<-final_data%>%   ##removing the first month is necessary
  arrange(year,month)

##save this into a .csv file
