

Output_extract_simET_cal <- function(directory, sub_no, station_name,
                                     Date_start, Date_end, obs_data = et_df3, nsub)
{
  #directory is the output directory
  # sub_nois the sub no (numeric vector)
  # station_name is character vector of station identifiers
  # Date_start and Date_end are beginning and end dates
  # for observed data period
  # obs_data, defaults to et_df3 if not other name given
  # # Dates, set up for daily: change for monthly
  #Date_start = "2003-01-01"
  # Date_end = "2015-12-31"
  Dates <- seq.Date(as.Date(Date_start),
                    as.Date(Date_end),by="month")
  # generate Julian days and years
  # browser()
  Julian <- yday(Dates)
  Years <- year(Dates)
  #browser()
  # read in output.rch
  # browser()
  
  suppressWarnings(# Read the data skipping the header rows
    data <- read_table2(paste0(directory,"/output.sub"), skip = 9, col_names = FALSE))
  
  data <- separate(data, X4, into = c("X4", "X5"), sep = "\\.")
  Dates = rep(seq.Date(as.Date(Date_start), as.Date(Date_end), by = "month"),each=nsub)
  
  # Convert MON and AREA columns to character type
  data$X4<- as.numeric(data$X4)
  data$X5<- as.numeric(data$X5)
  data_f<-data%>% filter(X4<13)
  out_sub <- data_f %>%
    mutate(Date = Dates[1:n()])
  #     #browser()
  #     filtered_outsub <- out_sub %>%
  #   filter(!(grepl("^20(03|04|05|06|07|08|09|10|11|12|13|14|15)|^13^20^201", as.character(X4)))) %>%
  #   filter(!is.na(X4))
  
  for (k in 1:length(sub_no)) {
    # read in the observed
    # read in the observed
    # browser()
    ET_obs<- obs_data[[station_name[k]]] %>%
      select(Date,ET)%>%
      mutate(Type = "observed")
    colnames(ET_obs)<- c("Date", "ET", "Type")
    
    #browser()
    # read in the predicted
    # here the X2 is the ET
    suppressWarnings(ET_pred <- out_sub %>%
                       select(X2,X4,X8,Date) %>%
                       filter(X2 == sub_no[k]))
     
    colnames(ET_pred) <- c("SUB", "MON", "ET","Date")
    # do some manipulation to create dates  
    
    ET_pred <- ET_pred %>%
      mutate(Type = "Predicted") %>%
      # select only a few columns
      select(Date,ET,SUB,Type) %>%
      filter(Date >= Date_start & Date <= Date_end)
    # mutate(Date = as.Date(paste(Years,"-01-01",sep=""))) %>%
    # select(-c(Julian,Years))
    # merge using join so dates match
    
    ET_obs_filtered <- ET_obs %>%
      filter(Date >= Date_start & Date <= Date_end)
    

    #browser()
    
    ET_all <- tryCatch({suppressWarnings(left_join(ET_pred,ET_obs_filtered,
                                          by=c("Date")) %>%
                       mutate(catch = station_name[k]))
      }, error = function(e) {
      message("Error in right_join for station: ", station_name[k])
      ET_all<- ET_pred  # Save the ET_pred to the list
      
    })
    #browser()
    
    # rbind with other stations if exist (k > 1)
    if (k > 1) {
      suppressWarnings(ET_out <- rbind(ET_out,ET_all))
    } else {
      suppressWarnings(ET_out <- ET_all)
    }
  }
  return(ET_out)    
} 

##testing the function
# 
# iteration_dir<-("C:/Users/Lenovo/Documents/Calib y Valid La Corona Sydney/LCnewsoil/exorty/LCnewsoil_forR2.Sufi2.SwatCup")
#  Stations <- c("Sub12.txt","Sub15.txt",'Sub3.txt','Sub8.txt','Sub10.txt','Sub11.txt','Sub14.txt',"Sub4.txt","Sub6.txt","Sub7.txt","Sub9.txt","Sub13.txt")
#   subs <- c(12,15,3,8,10,11,14,4,6,7,9,13)
#    Date_start = "2003-01-01"
#   Date_end = "2015-12-31"
#  
# #browser()
#   # run the output_extract function
#   ET_all <- Output_extract_simET_cal (directory = iteration_dir,
#                            sub_no = subs,
#                            station_name = Stations,
#                            Date_start, Date_end)
#   ET_plot <- ggplot(ET_all, aes(x = Date, y = ET)) +
#   geom_line() +
#   facet_wrap(~ catch, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "ET over Time by sub", x = "Date", y = "ETmm")
