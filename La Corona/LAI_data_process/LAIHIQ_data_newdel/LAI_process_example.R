df <- data.frame(
  date = seq.Date(from = as.Date("2023-01-01"), by = "4 days", length.out = 10),
  mean = rnorm(10, 5, 1),  # Replace with your actual mean values
  std = rnorm(10, 0.5, 0.1)  # Replace with your actual std values
)

# Create a sequence of daily dates covering the entire period
daily_dates <- seq.Date(from = min(df$date), to = max(df$date) + 3, by = "day")

# Perform linear interpolation for mean values
mean_interpolated <- approx(x = df$date, y = df$mean, xout = daily_dates, rule = 2)$y

# Perform linear interpolation for standard deviation values
std_interpolated <- approx(x = df$date, y = df$std, xout = daily_dates, rule = 2)$y

# Create a data frame with interpolated daily values
daily_df <- data.frame(
  date = daily_dates,
  mean = mean_interpolated,
  std = std_interpolated
)

# Inspect the resulting daily data frame
#print(daily_df)


# Assuming you have daily data with columns: date, mean, std

# Create upper and lower bounds for the interpolation
upper_bound <- df$mean + df$std
lower_bound <- df$mean - df$std

# Perform interpolation for upper bound
upper_interpolated <- approx(x = df$date, y = upper_bound, xout = daily_dates, rule = 2)$y

# Perform interpolation for lower bound
lower_interpolated <- approx(x = df$date, y = lower_bound, xout = daily_dates, rule = 2)$y

# Compute mean of upper and lower bounds as the interpolated mean
mean_interpolated <- (upper_interpolated + lower_interpolated) / 2

# Compute standard deviation of upper and lower bounds as the interpolated std
std_interpolated <- (upper_interpolated - lower_interpolated) / 2

# Create a data frame with interpolated daily values
daily_df_std <- data.frame(
  date = daily_dates,
  mean = mean_interpolated,
  std = std_interpolated
)


# Plot optios given the example
ggplot() +
  geom_point(data = df, aes(x = date, y = mean, color = "MODIS LAI")) +
  theme_minimal()+
  geom_line(data = daily_df, aes(x = date, y = mean, color = "LI"), alpha = 0.3)+
  geom_point(data = daily_df_std, aes(x = date , y = mean, color = "MODIS LAI consideing stdev"), alpha = 0.3)+
  ##add std_interpolated
  geom_ribbon(data = daily_df_std, aes(x = date, ymin = mean - std, ymax = mean + std), alpha = 0.1) 

