Output_extract_simLAI_cal <- function(directory, station_name, sub_number,#hru_number,
                                      Date_start, Date_end, obs_data = LAI_df3)
{
  #directory is the output directory
  # reach_no is the reach no (numeric vector) ## DELETED THIS INPUT AS I WILL USE AVERAGE BY CATCH AND BY LANDUSE
  #sub_number is the subbasin number in same order as station names
  # station_name is character vector of station identifiers
  # Date_start and Date_end are beginning and end dates
  # for observed data period
  # obs_data, defaults to flow_df if not other name given
  # # Dates, set up for daily: change for monthly
  # Date_start = "2003-01-01"
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
    
    c_names <- read_fwf(paste0(directory,"/output.hru"),
                        fwf_widths(c(4,5,10,5,5,5,rep(10,79))),
                        skip = 8, n_max=1))
  
  output_hru <- read_fwf(paste0(directory,"/output.hru"),
                         fwf_widths(c(4,5,10,5,5,5,rep(10,79)),
                                    col_names = as.character(c_names)),
                         skip = 9)
  
  Dates = rep(seq.Date(as.Date(Date_start), as.Date(Date_end), by = "month"), each=170)
  
  # Convert MON and AREA columns to character type
  
  data_f<-output_hru%>% filter(MON<13)
  data_f_dates <- data_f %>%
    mutate(Date = Dates[1:n()])%>%
    select("HRU","SUB","LAI", "Date")
  #     #browser()
  #     filtered_outsub <- out_sub %>%
  #   filter(!(grepl("^20(03|04|05|06|07|08|09|10|11|12|13|14|15)|^13^20^201", as.character(X4)))) %>%
  #   filter(!is.na(X4))
  
  for (k in 1:length(station_name)) {
    # read in the observed
    # browser()
    LAI_obs<- obs_data%>% 
      filter(catch == station_name[k]) %>%
      mutate(Date = as.Date(Date))%>%
      filter(year(Date) >= year(Date_start) & year(Date) <= year(Date_end))%>%
      mutate(Type = "observed")
    colnames(LAI_obs)<- c("Date", "LAI", "catch","Type")
    
    # browser()
    # read in the predicted
    # here the X2 is the ET
   # browser()
    suppressWarnings(LAI_pred <- data_f_dates %>%
                      
                       ## calculate weigthed value LAI given HRU number and macthing this number in hru_frac_sub_sel dataset 
                       left_join(hru_frac_sub_sel, by = c("HRU")) %>% 
                       filter(SUB == sub_number[k]) %>%
                  
                       #filter(HRU== hru_number[k]))# %>%
                       # Calculate weighted LAI using the weights from hru_frac_sub_sel
                       group_by(SUB,Date) %>%  # Grouping by SUB to calculate the mean LAI
                      # summarise(LAI = mean(LAI, na.rm = TRUE)))
                       summarise(LAI = sum(LAI * HRU_Fraction, na.rm = TRUE)))
    # do some manipulation to create dates  and add a type column
    LAI_pred <- LAI_pred %>%
      mutate(Type = "Predicted")%>% #catch = station_name[k]
      ungroup()
    
    LAI_pred_list<-list()
#browser()
      # merge using join so dates match
    LAI_all<- tryCatch({suppressWarnings(left_join(LAI_pred,LAI_obs,
                                                    by=c("Date")))
    }, error = function(e) {
      message("Error in right_join for station: ", station_name[k])
      LAI_all<- LAI_pred  # Save the LAI_pred to the list
      
    })
    
      # browser()
    
    # rbind with other stations if exist (k > 1)
    if (k > 1) {
      suppressWarnings(LAI_out <- rbind(LAI_out,LAI_all))
    } else {
      suppressWarnings(LAI_out <- LAI_all)
    }
  }
  return(LAI_out = LAI_out)    
} 



# ##testing the function

#iteration_dir<-("C:/Users/Lenovo/Documents/Calib y Valid La Corona Sydney/LCnewsoil/exorty/LC_newsoil_newdelim_R.Sufi2.SwatCup")
# iteration_dir<-("C:/Users/Lenovo/Documents/Calib y Valid La Corona Sydney/LCnewsoil/LaCorona_OK_newdelineation/LaCorona_OK/Scenarios/Default/TxtInOut")
#  Stations <- c("sub3","sub4","sub6","sub7","sub8","sub9","sub10","sub11")
# # hru_number <- c(8,21,58,66,74,98,116,124) ## hru witch max representation in subbasin
#  sub_number <- c(3,4,6,7,8,9,10,11)
#    Date_start = "2003-01-01"
#   Date_end = "2017-12-31"
# #  land_uses_swat_code = c('PAST','PINE','GRAS')
# #browser()
# #   # run the output_extract function
#   LAI_all <- Output_extract_simLAI_cal (directory = iteration_dir,
#                         #   hru_no = hrus,
#                            station_name = Stations,sub_number = sub_number,#hru_number = hru_number,
#                             Date_start, Date_end)
  
#  
# ##plot LAI_all
#   LAI_plot_1 <- ggplot(LAI_all) +
#         geom_line( aes(x = Date, y = LAI.x, color='obs'), alpha=0.5) +
#         geom_line( aes(x = Date, y = LAI.y, color= 'sim'), alpha=0.6) +
#         facet_wrap(~ SUB, scales = "free_y") +
#         theme_bw() +
#         labs(title = "LAI simulated test ", x = "Date", y = "LAI")+
#         ##legend in bottom
#         theme(legend.position = "bottom")
#  # LAI_all_hru,  LAI_all_mean,  LAI_all_wei
#   
#   ## add tag and plot in one plot only
  # LAI_all_hru_<- LAI_all%>%
  #   mutate(Type = "hrumax")
  # LAI_all_wei_<-   LAI_all%>%
  #   mutate(Type = "weigthed")
  # LAI_all_mean_<-   LAI_all_mean%>%
  #   mutate(Type = "mean")
#   # 
#   
#   ###bind
#   LAI_all_plot<- bind_rows(LAI_all_hru_, LAI_all_mean_, LAI_all_wei_)
# #  
#   LAI_plot_1 <- ggplot(  LAI_all_plot) +
#     geom_line( aes(x = Date, y = LAI.x, color='obs'), alpha=0.5) +
#     geom_line( aes(x = Date, y = LAI.y, color= Type), alpha=0.6) +
#     facet_wrap(~ SUB, scales = "free_y") +
#     theme_bw() +
#     labs(title = "LAI simulated calculated different methods ", x = "Date", y = "LAI")+
#     ##legend in bottom
#     theme(legend.position = "bottom")
#   
#   ##save
#ggsave("C:/Users/Lenovo/Documents/Calib y Valid La Corona Sydney/LCnewsoil/exorty/LC_newsoil_newdelim_R.Sufi2.SwatCup/LAI_plot_strat.png", width = 20, height = 20)
#   
#   
#  # LAI_plot_182
#   
#   ## this function to read observed data and save all hrus in a list
#   
  Output_extract_allhrussimLAI_cal <- function(directory, station_name, sub_number,#hru_number,
                                               Date_start, Date_end)
  {
    #directory is the output directory
    # reach_no is the reach no (numeric vector) ## DELETED THIS INPUT AS I WILL USE AVERAGE BY CATCH AND BY LANDUSE
    #sub_number is the subbasin number in same order as station names
    # station_name is character vector of station identifiers
    # Date_start and Date_end are beginning and end dates
    # for observed data period
    # obs_data, defaults to flow_df if not other name given
    # # Dates, set up for daily: change for monthly
    # Date_start = "2003-01-01"
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
    
    c_names <- read_fwf(paste0(directory,"/output.hru"),
                        fwf_widths(c(4,5,10,5,5,5,rep(10,79))),
                        skip = 8, n_max=1))
  
  output_hru <- read_fwf(paste0(directory,"/output.hru"),
                         fwf_widths(c(4,5,10,5,5,5,rep(10,79)),
                                    col_names = as.character(c_names)),
                         skip = 9)
  
  Dates = rep(seq.Date(as.Date(Date_start), as.Date(Date_end), by = "month"), each=170)
  
  # Convert MON and AREA columns to character type
  
  data_f<-output_hru%>% filter(MON<13)
  data_f_dates <- data_f %>%
    mutate(Date = Dates[1:n()])%>%
    select("HRU","SUB","LAI", "Date")
  #     #browser()
  #     filtered_outsub <- out_sub %>%
  #   filter(!(grepl("^20(03|04|05|06|07|08|09|10|11|12|13|14|15)|^13^20^201", as.character(X4)))) %>%
  #   filter(!is.na(X4))
  
  for (k in 1:length(station_name)) {
    # read in the observed
    # browser()
       # browser()
    # read in the predicted
    # here the X2 is the ET
    suppressWarnings(LAI_pred <- data_f_dates %>%
                       filter(SUB == sub_number[k]) %>%
                       
                       
                       ## calculate weigthed value LAI given HRU number and macthing this number in hru_frac_sub_sel dataset 
                       left_join(hru_frac_sub_sel, by = c("HRU")) )#%>% 
                       # filter(HRU== hru_number[k]))# %>%
                       # Calculate weighted LAI using the weights from hru_frac_sub_sel
                       #group_by(SUB,Date) %>%  # Grouping by SUB to calculate the mean LAI
                       # summarise(LAI = mean(LAI, na.rm = TRUE)))
                       #summarise(LAI = sum(LAI * HRU_Fraction, na.rm = TRUE)))
    # do some manipulation to create dates  and add a type column
    LAI_pred <- LAI_pred %>%
      mutate(Type = "Predicted")%>% #catch = station_name[k]
      ungroup()
    
    #browser()
    # merge using join so dates match
    LAI_all<-  LAI_pred
    
    # browser()
    
    # rbind with other stations if exist (k > 1)
    if (k > 1) {
      suppressWarnings(LAI_out <- rbind(LAI_out,LAI_all))
    } else {
      suppressWarnings(LAI_out <- LAI_all)
    }
  }
  return(LAI_out = LAI_out)    
  } 
  # 
  # ##testing the function
  # LAI_all <- Output_extract_allhrussimLAI_cal (directory = iteration_dir,
  #                                               #   hru_no = hrus,
  #                                               station_name = Stations,sub_number = sub_number,#hru_number = hru_number,
  #                                               Date_start, Date_end)
  # 
  # 
  # 