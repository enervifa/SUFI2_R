
## now based on result, calculate performance
performance_flow <- function(flow_all) {
  flow_all %>%
    group_by(catch) %>%
    dplyr::summarize(NSE=NSE(Flow.x,Flow.y),
                     KGE = KGE(Flow.x,Flow.y),
                     R2 = Rsquared(Flow.x, Flow.y),
                     PBIAS = Pbias(Flow.x, Flow.y),
                     Vol_frac = Vol_frac(Flow.x, Flow.y),
                     MSE = MSE(Flow.x, Flow.y),
                     SSQR = SSQR(Flow.x, Flow.y),
                     bR2 = bR2(Flow.x, Flow.y)
    )
}

# Define a function to calculate performance metrics for ET
performance_ET <- function(ET_all) {
  ET_all %>%
    group_by(catch) %>%
    dplyr::summarize(
      NSE = NSE(ET.y, ET.x),
      KGE = KGE(ET.y, ET.x),
      R2 = Rsquared(ET.y, ET.x),
      PBIAS = Pbias(ET.y, ET.x),
      Vol_frac = Vol_frac(ET.y, ET.x),
      MSE = MSE(ET.y, ET.x),
      SSQR = SSQR(ET.y, ET.x),
      bR2 = bR2(ET.y, ET.x)
    )
}

# Define a function to calculate performance metrics for LAI
performance_LAI <- function(LAI_all) {
  na.omit(LAI_all) %>%
    group_by(catch) %>%
    dplyr::summarize(
      NSE = NSE(LAI.y, LAI.x),
      KGE = KGE(LAI.y, LAI.x),
      R2 = Rsquared(LAI.y, LAI.x),
      PBIAS = Pbias(LAI.y, LAI.x),
      Vol_frac = Vol_frac(LAI.y, LAI.x),
      MSE = MSE(LAI.y, LAI.x),
      SSQR = SSQR(LAI.y, LAI.x),
      bR2 = bR2(LAI.y, LAI.x)
    )
}