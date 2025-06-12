##this code is to read .csv files from MODIS download, edit them in SWATCUP format, 
#save them in csv and then create a concatenated numbered file to input

library(readr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(readr)
library(tidyr)

file_list<-c()
setwd("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/process data")
list_of_files <- list.files(pattern = ".csv$")

##otracuenca_borracho 41
#CHANGE NAME IN DATE AND FORMAT TO DMYYYY

# loop over files to read them and change date format
for (i in 1:length(list_of_files)) {
  file_list[[i]] <- read_csv(paste0("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/process data/", list_of_files[i]), 
                             col_types = cols('system:time_start' = col_date(format = "%b %d, %Y")), na="")

}

########
# 
# for (i in 1:length(list_of_files)) {
#   file_list[[i]] <- read_csv(paste0("C:/Users/Lenovo/Downloads/SubETbo/", list_of_files[i]), 
#                              col_types = cols(Date = col_date(format = "%m/%d/%Y")))
# }

processed_files_swatcup <- list()

  # loop over files to read them and change date format
#this works well
for (i in 1:length(file_list)) {
    first_file <- file_list[[i]]
    modified_file <- first_file[-1, ] %>%
      mutate(Date= make_date(year=year,month=month)) %>%
      mutate(Mon_ET = monthly_sum) %>%
      mutate(Mon_ET = Mon_ET * 0.1) %>% ##this setp will only compute sum for months that have complete data
      mutate(row_number = row_number())%>%  ##now filter time period for the data
      filter(Date>'2002-12-31' ,Date<'2016-01-01')
    
    #create time series vector
    min_date <- min(modified_file$Date)
    max_date <- max(modified_file$Date)
    month_year_seq <- data.frame(month = rep(1:12, length.out = 12 * (year(max_date) - year(min_date) + 1)),
                                 year = rep(year(min_date):year(max_date), each = 12))%>%
    mutate(Date= make_date(year=year,month=month))%>%
      mutate(row_number = row_number())
    
    ##left join with vector to account for NAs row id
    modified_file3<-left_join(month_year_seq, modified_file, by = 'Date')
    
      # Shift row numbers for NAs
      modified_file4<-modified_file3%>%
      na.omit()%>%
      select(month.y,year.y,row_number.x,Mon_ET)%>%
      mutate(CUP = paste0("ET","_",list_of_files[i],"_",month.y,"_",year.y))%>%
      select(row_number.x,CUP,Mon_ET)
     # select(row_number.y,row_number.x,CUP,Mon_ET)
    
# 	row_number	CUP	Mon_ET
# 1	2	ET_aet_sub8.csv_2_2003	84.2
# 2	11	ET_aet_sub8.csv_11_2003	74.6


####prepare input for SWATCUP
#first read them and add row number, descirption and value as SWATCUP format

      #append the file to a list processed_files
      processed_files_swatcup[[i]] <- modified_file4
  write.csv(modified_file4 , paste0("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/swatcup_LC_ET/CUP",list_of_files[i]))
}

##save list porcessed_files_swatcup to .RDS

saveRDS(processed_files_swatcup, "processed_files_swatcup.RDS")

file_list2<-c()
setwd("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/swatcup_LC_ET")
list_of_files3 <- list.files(pattern = ".csv$")

# Get the subbasin numbers from the file names
subbasin_numbers <- as.numeric(gsub(".*sub(\\d+).*", "\\1", list_of_files3))

# Sort the list of files based on subbasin numbers
sorted_files <- list_of_files3[order(subbasin_numbers)]


######################### create input with all files together and the descriptive text from observed.txt (except fist line with number of stations)

######################### create input with all files together and the descriptive text from observed.txt (except fist line with number of stations)
text_between_files <- c("  : this is the name of the variable and the subbasin number to be included in the objective function",
                        "    : number of data points for this variable as it follows below. First column is a sequential number from beginning",
                        "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value.")  # Specify the text between files

# now put it all together
# Loop over files to read them, select columns, and change date format
for (i in 1:length(sorted_files)) {
  # Read the current file using read_csv()
  file_path <- paste0("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/swatcup_LC_ET/", sorted_files[[i]])
  file_content <- read_csv(file_path)
  
  # Select columns 2, 3, and 4
  file_content <- file_content %>% select(2:4)
  
  # Extract subbasin number from file name
  subbasin_number <- gsub(".*sub(\\d+).*", "\\1", sorted_files[i])
  
  # Get the number of rows in the file_content
  num_rows <- nrow(file_content)
  
  # Create header text with modified information
  header_text <- c(paste0("Sub", subbasin_number, text_between_files[1]),
                   paste0(num_rows, text_between_files[2]),
                   text_between_files[3])
  
  # Write header text to SWAT_CUP_Input.txt
  if (i == 1) {
    write_lines(header_text, "SWAT_CUP_Input.txt")
  } else {
    write_lines(header_text, "SWAT_CUP_Input.txt", append = TRUE)
  }
  
  # Write file content to SWAT_CUP_Input.txt
  write_delim(file_content, "SWAT_CUP_Input.txt", append = TRUE, delim = "  ")
  
  # Add a blank line between files
  write_lines("", "SWAT_CUP_Input.txt", append = TRUE)
}

#######################################
##############Observed.txt


file_list2<-c()
setwd("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/swatcup_LC_ET")
list_of_files3 <- list.files(pattern = ".csv$")

# Get the subbasin numbers from the file names
subbasin_numbers <- as.numeric(gsub(".*sub(\\d+).*", "\\1", list_of_files3))

# Sort the list of files based on subbasin numbers
sorted_files <- list_of_files3[order(subbasin_numbers)]

######################### create input with all files together and the descriptive text from observed.txt (except fist line with number of stations)

######################### create input with all files together and the descriptive text from observed.txt (except fist line with number of stations)
text_between_files <- c("  : this is the name of the variable and the subbasin number to be included in the objective function",
"    1     : weight of the variable in the objective function",
"    -1    : Dynamic flow separation. Not considered if -1. If 1, then values should be added in the forth column below after observations",
"    -1    : constant flow separation, threshold value. (not considered if -1)",
"    1     : if separation of signal is considered, this is weight of the smaller values in the objective function",
"    1     : if separation of signal is considered, this is weight of the larger values in the objective function",
"    10    : percentage of measurement error",
"    : number of data points for this variable as it follows below. First column is a sequential number from beginning",
"    : of the simulation, second column is variable name and date (format arbitrary), third column is variable value.")

# now put it all together
# Loop over files to read them, select columns, and change date format
for (i in 1:length(sorted_files)) {
  # Read the current file using read_csv()
  file_path <- paste0("C:/Users/Lenovo/Desktop/LC_subbasins/ETdata_subs_LC/swatcup_LC_ET/", sorted_files[[i]])
  file_content <- read_csv(file_path)
  
  # Select columns 2, 3, and 4
  file_content <- file_content %>% select(2:4)
  
  # Extract subbasin number from file name
  subbasin_number <- gsub(".*sub(\\d+).*", "\\1", sorted_files[i])
  
  # Get the number of rows in the file_content
  num_rows <- nrow(file_content)
  
  # Create header text with modified information
  header_text <- c(paste0("Sub", subbasin_number, text_between_files[1]),
                   text_between_files[2:7],paste0(num_rows, text_between_files[8]),text_between_files[9])
  
  # Write header text to SWAT_CUP_Input.txt
  if (i == 1) {
    write_lines(header_text, "SWAT_CUP_Input2.txt")
  } else {
    write_lines(header_text, "SWAT_CUP_Input2.txt", append = TRUE)
  }
  
  # Write file content to SWAT_CUP_Input.txt
  write_delim(file_content, "SWAT_CUP_Input2.txt", append = TRUE, delim = "  ")
  
  # Add a blank line between files
  write_lines("", "SWAT_CUP_Input2.txt", append = TRUE)
}


##############