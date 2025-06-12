
# write a function to extract output reach variables

#output reach file
# this assumes the identifier string in observed.txt includes the day and year
Output_extract_flow_cal <- function(directory, reach_no, station_name,
                                    Date_start, Date_end, 
                                    obs_data = flow_df_date2)
{
  #directory is the output directory
  # reach_no is the reach no (numeric vector)
  # station_name is character vector of station identifiers
  # Date_start and Date_end are beginning and end dates
  # for observed data period
  # obs_data, defaults to flow_df if not other name given
  # # Dates, set up for daily: change for monthly
  Dates <- seq.Date(as.Date(Date_start),
                    as.Date(Date_end),by="month")
  # generate Julian days and years
  # browser()
  Julian <- yday(Dates)
  Years <- year(Dates)
  #browser()
  # read in output.rch
  out_rch <- read_table2(paste0(directory,"/output.rch"), skip=9, col_names = F)
  #browser()
  for (k in 1:length(reach_no)) {
    # read in the observed
    # browser()
    flow_obs<- obs_data[[station_name[k]]] %>%
      mutate(Type = "observed")
    colnames(flow_obs)<- c("Date", "Flow", "Type")
    
    # read in the predicted
    # here the X2 is the Reach
    #browser()
    suppressWarnings(flow_pred <- out_rch %>%
                       select(X2,X4,X7) %>%
                       filter(X4<13)%>%
                       filter(X2 == reach_no[k]))
    colnames(flow_pred) <- c("Reach", "month", "Flow")
    # do some manipulation to create dates  
    #browser()
    flow_pred <- flow_pred %>%
      mutate(Date = Dates[1:n()],
             Type = "Predicted") %>%
      # select only a few columns
      select(Date,Flow, Type) #%>%
    # mutate(Date = as.Date(paste(Years,"-01-01",sep=""))) %>%
    # select(-c(Julian,Years))
    # merge using join so dates match
    suppressWarnings(flow_all <- tryCatch({suppressWarnings(right_join(flow_obs,flow_pred,
                                   by=c("Date")) %>%
                                    mutate(catch = station_name[k]))
    }, error = function(e) {
      message("Error in right_join for station: ", station_name[k])
      flow_all<- flow_pred  # Save the ET_pred to the list
    }))
    # rbind with other stations if exist (k > 1)
    if (k > 1) {
      suppressWarnings(flow_out <- rbind(flow_out,flow_all))
    } else {
      suppressWarnings(flow_out <- flow_all)
    }
  }
  return(flow_out)    
} 

##testing the function
# # stations (in order of observed.txt)
#   Stations <- c("FLOW_OUT_14", "FLOW_OUT_15")
#   reaches <- c(14,15)
#   Date_start = "2003-01-01"
#   Date_end = "2015-12-31"
#  iteration_dir<-("C:/Users/Lenovo/Documents/Calib y Valid La Corona Sydney/LCnewsoil/exorty/LCnewsoil_forR2.Sufi2.SwatCup")
#   # run the output_extract function
#   flow_all <- Output_extract_flow_cal(directory = iteration_dir,
#                              reach_no = reaches,
#                              station_name = Stations,
#                              Date_start, Date_end)
# 
#   flow_plot <- ggplot(flow_all, aes(x = Date, y = Flow.y)) +
#   geom_line() +
#   facet_wrap(~ catch, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "flow over Time by sub", x = "Date", y = "Flowpredmm")
