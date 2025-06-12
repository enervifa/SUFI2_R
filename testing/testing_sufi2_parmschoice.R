
set.seed(123)
params <- matrix(rnorm(20 * 3, mean = 5, sd = 10), nrow = 20)
colnames(params) <- c("Param1", "Param2", "Param3")
goal_values <- rnorm(20, mean = 0.6, sd = 0.4)
data <- data.frame(Sim_No. = 1:20, goal_value = goal_values, params)


params_matrix <- data %>% select(starts_with("Param"))

best_data<-data

J <- matrix(0, nrow = choose(nrow(params_matrix), 2), ncol = ncol(params_matrix ))
index <- 1
for (i in 1:(nrow(best_data) - 1)) {
  for (k in (i + 1):nrow(best_data)) {
    delta_g <- data$goal_value[k] - data$goal_value[i]
    params_i <- as.numeric(params_matrix[i, ])
    params_k <- as.numeric(params_matrix[k, ])
    delta_b <- params_k - params_i
    J[index, ] <- delta_g / delta_b
    index <- index + 1
  }
}

J[is.na(J)] <- 0
J_T <- t(J)
H <- J_T %*% J
sigma_g2 <- var(data$goal_value)
C <- sigma_g2 * solve(H)


s_j <- sqrt(diag(C))
t_critical <- qt(0.975, df = nrow(best_data) - ncol(best_data))

##choose either best or median and run code behind
median_params <- best_data %>%
  select(-c(Sim_No., goal_value)) %>% #, metric
  summarise(across(everything(), ~ median(.x, na.rm = TRUE)))%>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Median")


median_params <-best_data %>%
  arrange(desc(goal_value)) %>%
  select(-c(Sim_No.,goal_value)) %>% #,metric
  slice(1)%>%
  ##select parms for max goal_value
  pivot_longer(everything(), names_to = "Parameter", values_to = "Median")



#################################
# Compute Confidence Intervals
confidence_intervals <- median_params %>%
  mutate(
    SD = s_j[match(Parameter, colnames(params_matrix))],  # Match standard deviations to parameters
    CI_Lower = Median - t_critical * SD,
    CI_Upper = Median + t_critical * SD
  )

#  Calculate b'_j min and b'_j max based on the formulas provided

confidence_intervals_final <- confidence_intervals %>%
  rowwise() %>%
  mutate(
    param_name = Parameter,  # Identify parameter name for each row
    # Get the min and max from the correct column in params_matrix
    param_min = min(params_matrix[, param_name]),  
    param_max = max(params_matrix[, param_name]),  
    b_j_min = CI_Lower - max((CI_Lower - param_min) / 2, (param_max - CI_Upper) / 2),
    b_j_max = CI_Upper + max((CI_Lower - param_min) / 2, (param_max - CI_Upper) / 2)
  ) %>%
  ungroup() %>%
  select(-param_name)%>%  # Remove name
  select(Parameter, b_j_min, b_j_max)  # Reorder columns)
#########


# Convert the parameter matrix to a long format for plotting
params_long <- data %>%
  select(-c(Sim_No., goal_value)) %>% #,metric
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value")

# Plot distributions of initial parameters
ggplot(params_long, aes(x = Value, fill = Parameter)) +
  geom_histogram(position = "dodge", binwidth = 0.5, alpha = 0.7) +
  facet_wrap(~ Parameter, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Initial Parameters", x = "Value", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot confidence intervals
ggplot(confidence_intervals, aes(x = Parameter, y = Median)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "red") +
  theme_minimal() +
  labs(title = "Updated Parameters with Confidence Intervals", x = "Parameter", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

initial_ranges <- params_long %>%
  group_by(Parameter) %>%
  summarise(Min = min(Value, na.rm = TRUE),
            Max = max(Value, na.rm = TRUE))
# Combine initial ranges with confidence intervals
combined_data <- confidence_intervals %>%
  rename(Median = Median, CI_Lower = CI_Lower, CI_Upper = CI_Upper) %>%
  left_join(initial_ranges, by = c("Parameter" = "Parameter"))%>%
  left_join(confidence_intervals_final, by = c("Parameter" = "Parameter"))


# Plot initial parameter ranges with updated parameters and confidence intervals
best<-ggplot(combined_data, aes(x = Parameter)) +
  geom_errorbar(aes(ymin = Min, ymax = Max, color = "Initial Range"), width = 0.2, linetype = "dashed") +
  geom_errorbar(aes(ymin = b_j_min, ymax = b_j_max, color = "Updated Range"), width = 0.2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.2) +
  geom_point(aes(y = Median, color = "95% CI"), size = 3) +
  theme_minimal() +
  theme_bw() +
  labs(title = "Initial Parameter Ranges vs Updated Parameters with Confidence Intervals using Best",
       x = "Parameter",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(values = c("Initial Range" = "red", "Updated Range" = "green", "95% CI" = "blue"))+ylim(-20,40)

####################test ust with flow NSEs "C:\Users\Lenovo\Documents\Calib y Valid La Corona Sydney\LCnewsoil\exorty\LC_newsoil_newdelim.Sufi2.SwatCup\SUFI2.IN\1_all_flow\goal_1_all_flowmetrics.txt"

prueba_goal<-goal_with_OF_indgoal1500%>%
  filter(metric == 'flow')
data <-prueba_goal
params_matrix <- data %>% select(-c(Sim_No.,goal_value,metric,"21:v__CANMX.hru______PINE"))
initial_ranges <-params_matrix%>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value")%>%
  group_by(Parameter) %>%  
  summarise(
    Min = min(Value, na.rm = TRUE),  
    Max = max(Value, na.rm = TRUE)
  )
best_data<-data

J <- matrix(0, nrow = choose(nrow(params_matrix), 2), ncol = ncol(params_matrix ))
index <- 1
for (i in 1:(nrow(best_data) - 1)) {
  for (k in (i + 1):nrow(best_data)) {
    delta_g <- data$goal_value[k] - data$goal_value[i]
    params_i <- as.numeric(params_matrix[i, ])
    params_k <- as.numeric(params_matrix[k, ])
    delta_b <- params_k - params_i
    J[index, ] <- delta_g / delta_b
    index <- index + 1
  }
}

J[is.na(J)] <- 0
J_T <- t(J)
H <- J_T %*% J
sigma_g2 <- var(data$goal_value)
C <- sigma_g2 * solve(H)


s_j <- sqrt(diag(C))
t_critical <- qt(0.975, df = nrow(best_data) - ncol(best_data))

##choose either best or median HERE and run code behind
median_params <- best_data %>%
  select(-c(Sim_No., goal_value,"21:v__CANMX.hru______PINE",metric)) %>% #, metric
  summarise(across(everything(), ~ median(.x, na.rm = TRUE)))%>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Median")


median_params <-best_data %>%
  arrange(desc(goal_value)) %>%
  select(-c(Sim_No.,goal_value,"21:v__CANMX.hru______PINE",metric)) %>% #,metric
  slice(1)%>%
  ##select parms for max goal_value
  pivot_longer(everything(), names_to = "Parameter", values_to = "Median")



#################################
# Compute Confidence Intervals
confidence_intervals <- median_params %>%
  mutate(
    SD = s_j[match(Parameter, colnames(params_matrix))],  # Match standard deviations to parameters
    CI_Lower = Median - t_critical * SD,
    CI_Upper = Median + t_critical * SD
  )

#  Calculate b'_j min and b'_j max based on the formulas provided

confidence_intervals_final <- confidence_intervals %>%
  rowwise() %>%
  mutate(
    param_name = Parameter,  # Identify parameter name for each row
    # Get the min and max from the correct column in params_matrix
    param_min = min(params_matrix[, param_name]),  
    param_max = max(params_matrix[, param_name]),  
    b_j_min = CI_Lower - max((CI_Lower - param_min) / 2, (param_max - CI_Upper) / 2),
    b_j_max = CI_Upper + max((CI_Lower - param_min) / 2, (param_max - CI_Upper) / 2)
  ) %>%
  ungroup() %>%
  select(-param_name)%>%  # Remove name
  select(Parameter, b_j_min, b_j_max)  # Reorder columns)



# Combine initial ranges with confidence intervals
combined_data <- confidence_intervals %>%
  rename(Median = Median, CI_Lower = CI_Lower, CI_Upper = CI_Upper) %>%
  left_join(initial_ranges, by = c("Parameter" = "Parameter"))%>%
  left_join(confidence_intervals_final, by = c("Parameter" = "Parameter"))%>%
  ##add column to plot values with max les than 1 separately
  mutate(categ=ifelse(Max<1,"one","two"))

combined_data_b<-combined_data%>%
  mutate(cenetr='best sim')


combined_data_m<-combined_data%>%
  mutate(cenetr='median')

combined_data_flow<-bind_rows(combined_data_b,combined_data_m)%>%
  mutate(metric='flow')

####################test ust with LAI NSEs
prueba_goal<-goal_with_OF_indgoal1500%>%
  filter(metric == 'LAI')

data <-prueba_goal
params_matrix <- data %>% select(-c(Sim_No.,goal_value,metric,"21:v__CANMX.hru______PINE"))
initial_ranges <-params_matrix%>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value")%>%
  group_by(Parameter) %>%  
  summarise(
    Min = min(Value, na.rm = TRUE),  
    Max = max(Value, na.rm = TRUE)
  )
best_data<-data

J <- matrix(0, nrow = choose(nrow(params_matrix), 2), ncol = ncol(params_matrix ))
index <- 1
for (i in 1:(nrow(best_data) - 1)) {
  for (k in (i + 1):nrow(best_data)) {
    delta_g <- data$goal_value[k] - data$goal_value[i]
    params_i <- as.numeric(params_matrix[i, ])
    params_k <- as.numeric(params_matrix[k, ])
    delta_b <- params_k - params_i
    J[index, ] <- delta_g / delta_b
    index <- index + 1
  }
}

J[is.na(J)] <- 0
J[!is.finite(J)] <- 0 
J_T <- t(J)
H <- J_T %*% J

data$goal_value[is.infinite(data$goal_value)] <- NA
sigma_g2 <- var(na.omit(data$goal_value))

C <- sigma_g2 * solve(H)


s_j <- sqrt(diag(C))
t_critical <- qt(0.975, df = nrow(best_data) - ncol(best_data))

##choose either best or median and run code behind
median_params <- best_data %>%
  select(-c(Sim_No., goal_value,"21:v__CANMX.hru______PINE",metric)) %>% #, metric
  summarise(across(everything(), ~ median(.x, na.rm = TRUE)))%>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Median")


median_params <-best_data %>%
  arrange(desc(goal_value)) %>%
  select(-c(Sim_No.,goal_value,"21:v__CANMX.hru______PINE",metric)) %>% #,metric
  slice(1)%>%
  ##select parms for max goal_value
  pivot_longer(everything(), names_to = "Parameter", values_to = "Median")



#################################
# Compute Confidence Intervals
confidence_intervals <- median_params %>%
  mutate(
    SD = s_j[match(Parameter, colnames(params_matrix))],  # Match standard deviations to parameters
    CI_Lower = Median - t_critical * SD,
    CI_Upper = Median + t_critical * SD
  )

#  Calculate b'_j min and b'_j max based on the formulas provided

confidence_intervals_final <- confidence_intervals %>%
  rowwise() %>%
  mutate(
    param_name = Parameter,  # Identify parameter name for each row
    # Get the min and max from the correct column in params_matrix
    param_min = min(params_matrix[, param_name]),  
    param_max = max(params_matrix[, param_name]),  
    b_j_min = CI_Lower - max((CI_Lower - param_min) / 2, (param_max - CI_Upper) / 2),
    b_j_max = CI_Upper + max((CI_Lower - param_min) / 2, (param_max - CI_Upper) / 2)
  ) %>%
  ungroup() %>%
  select(-param_name)%>%  # Remove name
  select(Parameter, b_j_min, b_j_max)  # Reorder columns)



# Combine initial ranges with confidence intervals
combined_data <- confidence_intervals %>%
  rename(Median = Median, CI_Lower = CI_Lower, CI_Upper = CI_Upper) %>%
  left_join(initial_ranges, by = c("Parameter" = "Parameter"))%>%
  left_join(confidence_intervals_final, by = c("Parameter" = "Parameter"))%>%
  ##add column to plot values with max les than 1 separately
  mutate(categ=ifelse(Max<1,"one","two"))

combined_data_b<-combined_data%>%
  mutate(cenetr='best sim')


combined_data_m<-combined_data%>%
  mutate(cenetr='median')

combined_data_lai<-bind_rows(combined_data_b,combined_data_m)%>%
  mutate(metric='LAI')

### plor covariance results from matrix C

# Convert matrix C to data frame
C_df <- as.data.frame(C)
C_df$Parameter1 <- rownames(C_df)
C_long <- C_df %>%
  pivot_longer(-Parameter1, names_to = "Parameter2", values_to = "Covariance")

# Create the heatmap
ggplot(C_long, aes(x = Parameter1, y = Parameter2, fill = Covariance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Covariance Matrix Heatmap",
       x = "Parameter",
       y = "Parameter",
       fill = "Covariance")

#Covariance measures how much two variables change together. 
#A positive covariance indicates that as one parameter increases, the other parameter tends to increase as well.
#A negative covariance indicates that as one parameter increases, the other tends to decrease.
#he magnitude of covariance indicates the strength of the relationship. Larger values (in absolute terms) indicate a stronger relationships
#Variance: The diagonal elements of the covariance matrix represent the variance of each parameter. Variance measures the spread of each parameter's values around its mean. High variance indicates that the parameter’s values are spread out widely,
#while low variance indicates that the values are clustered closely around the mean

####################test ust with ET NSE
prueba_goal<-goal_with_OF_indgoal1500%>%
  filter(metric == 'ET')

data <-prueba_goal
params_matrix <- data %>% select(-c(Sim_No.,goal_value,metric,"21:v__CANMX.hru______PINE"))
initial_ranges <-params_matrix%>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value")%>%
  group_by(Parameter) %>%  
  summarise(
    Min = min(Value, na.rm = TRUE),  
    Max = max(Value, na.rm = TRUE)
  )
best_data<-data

J <- matrix(0, nrow = choose(nrow(params_matrix), 2), ncol = ncol(params_matrix ))
index <- 1
for (i in 1:(nrow(best_data) - 1)) {
  for (k in (i + 1):nrow(best_data)) {
    delta_g <- data$goal_value[k] - data$goal_value[i]
    params_i <- as.numeric(params_matrix[i, ])
    params_k <- as.numeric(params_matrix[k, ])
    delta_b <- params_k - params_i
    J[index, ] <- delta_g / delta_b
    index <- index + 1
  }
}

J[is.na(J)] <- 0
J[!is.finite(J)] <- 0 
J_T <- t(J)
H <- J_T %*% J

#data$goal_value[is.infinite(data$goal_value)] <- NA
sigma_g2 <- var(na.omit(data$goal_value))

C <- sigma_g2 * solve(H)


s_j <- sqrt(diag(C))
t_critical <- qt(0.975, df = nrow(best_data) - ncol(best_data))

##choose either best or median and run code behind
median_params <- best_data %>%
  select(-c(Sim_No., goal_value,"21:v__CANMX.hru______PINE",metric)) %>% #, metric
  summarise(across(everything(), ~ median(.x, na.rm = TRUE)))%>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Median")


median_params <-best_data %>%
  arrange(desc(goal_value)) %>%
  select(-c(Sim_No.,goal_value,"21:v__CANMX.hru______PINE",metric)) %>% #,metric
  slice(1)%>%
  ##select parms for max goal_value
  pivot_longer(everything(), names_to = "Parameter", values_to = "Median")



#################################
# Compute Confidence Intervals
confidence_intervals <- median_params %>%
  mutate(
    SD = s_j[match(Parameter, colnames(params_matrix))],  # Match standard deviations to parameters
    CI_Lower = Median - t_critical * SD,
    CI_Upper = Median + t_critical * SD
  )

#  Calculate b'_j min and b'_j max based on the formulas provided

confidence_intervals_final <- confidence_intervals %>%
  rowwise() %>%
  mutate(
    param_name = Parameter,  # Identify parameter name for each row
    # Get the min and max from the correct column in params_matrix
    param_min = min(params_matrix[, param_name]),  
    param_max = max(params_matrix[, param_name]),  
    b_j_min = CI_Lower - max((CI_Lower - param_min) / 2, (param_max - CI_Upper) / 2),
    b_j_max = CI_Upper + max((CI_Lower - param_min) / 2, (param_max - CI_Upper) / 2)
  ) %>%
  ungroup() %>%
  select(-param_name)%>%  # Remove name
  select(Parameter, b_j_min, b_j_max)  # Reorder columns)



# Combine initial ranges with confidence intervals
combined_data <- confidence_intervals %>%
  rename(Median = Median, CI_Lower = CI_Lower, CI_Upper = CI_Upper) %>%
  left_join(initial_ranges, by = c("Parameter" = "Parameter"))%>%
  left_join(confidence_intervals_final, by = c("Parameter" = "Parameter"))%>%
  ##add column to plot values with max les than 1 separately
  mutate(categ=ifelse(Max<1,"one","two"))

combined_data_b<-combined_data%>%
  mutate(cenetr='best sim')


combined_data_m<-combined_data%>%
  mutate(cenetr='median')

combined_data_et<-bind_rows(combined_data_b,combined_data_m)%>%
  mutate(metric='ET')

# Modify x-axis labels to appear twice
combined_data <- combined_data %>%
  mutate(Parameter_Label = factor(paste(Parameter, cenetr, sep = " - ")))

############## combine and plot results

### plot all ranges 
combined_data_flow_lai_et<-bind_rows(combined_data_flow,combined_data_lai,combined_data_et)%>%
  group_by(Parameter)%>%
  mutate(category = case_when(
    max(Max) <= 1 ~ "one",
    max(Max) <= 5 ~ "two",
    max(Max) <= 100 ~ "three",
    TRUE ~ "High"
  )) %>%
  ungroup()
# Modify x-axis labels to appear twice
combined_data_flow_lai_et_plot<- combined_data_flow_lai_et %>%
  mutate(Parameter_Label = factor(paste(Parameter, metric, sep = " - ")))

# Plot with customized x-axis labels
c_one <- ggplot(combined_data_flow_lai_et_plot %>% filter(category  == "one",cenetr == "best sim"), aes(x = Parameter_Label)) +
  geom_errorbar(aes(ymin = Min, ymax = Max, color = "Initial Range"), width = 0.1, linetype = "dashed") +
  geom_errorbar(aes(ymin = b_j_min, ymax = b_j_max, color = "Updated Range"), width = 0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.4) +
  geom_point(aes(y = Median, color = as.factor(metric)), size = 2, alpha=0.2) +
  theme_minimal() +
  theme_bw() +
  #  facet_grid(metric ~ ., scales = "fixed") +
  labs(title = "Initial Parameter Ranges vs Updated Parameters with Confidence Intervals using Best",
       x = "Parameter",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("Initial Range" = "red", "Updated Range" = "green", "95% CI" = "blue")) #+
#scale_x_discrete(labels = function(x) gsub(" - ", "\n", x))  # Customize x-axis labels

c_one
# Plot with customized x-axis labels
c_two <- ggplot(combined_data_flow_lai_et_plot %>% filter(category == "two",cenetr == "best sim"), aes(x = Parameter_Label)) +
  geom_errorbar(aes(ymin = Min, ymax = Max, color = "Initial Range"), width = 0.1, linetype = "dashed") +
  geom_errorbar(aes(ymin = b_j_min, ymax = b_j_max, color = "Updated Range"), width = 0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.4) +
  geom_point(aes(y = Median, color = cenetr), size = 2, alpha=0.2) +
  theme_minimal() +
  theme_bw() +
 # facet_grid(categ ~ ., scales = "free") +
  labs(title = "Initial Parameter Ranges vs Updated Parameters with Confidence Intervals using Best",
       x = "Parameter",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("Initial Range" = "red", "Updated Range" = "green", "95% CI" = "blue", "best sim" = "black", "median" = "orange")) +
  scale_x_discrete(labels = function(x) gsub(" - ", "\n", x))  # Customize x-axis labels

c_two

c_three <- ggplot(combined_data_flow_lai_et_plot %>% filter(category == "three",cenetr == "best sim"), aes(x = Parameter_Label)) +
  geom_errorbar(aes(ymin = Min, ymax = Max, color = "Initial Range"), width = 0.1, linetype = "dashed") +
  geom_errorbar(aes(ymin = b_j_min, ymax = b_j_max, color = "Updated Range"), width = 0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.4) +
  geom_point(aes(y = Median, color = cenetr), size = 2, alpha=0.2) +
  theme_minimal() +
  theme_bw() +
 # facet_grid(categ ~ ., scales = "free") +
  labs(title = "Initial Parameter Ranges vs Updated Parameters with Confidence Intervals using Best",
       x = "Parameter",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("Initial Range" = "red", "Updated Range" = "green", "95% CI" = "blue", "best sim" = "black", "median" = "orange")) +
  scale_x_discrete(labels = function(x) gsub(" - ", "\n", x))  # Customize x-axis labels

c_three


c_high <- ggplot(combined_data_flow_lai_et_plot %>% filter(category == "High",cenetr == "best sim"), aes(x = Parameter_Label)) +
  geom_errorbar(aes(ymin = Min, ymax = Max, color = "Initial Range"), width = 0.1, linetype = "dashed") +
  geom_errorbar(aes(ymin = b_j_min, ymax = b_j_max, color = "Updated Range"), width = 0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.4) +
  geom_point(aes(y = Median, color = cenetr), size = 2, alpha=0.2) +
  theme_minimal() +
  theme_bw() +
  # facet_grid(categ ~ ., scales = "free") +
  labs(title = "Initial Parameter Ranges vs Updated Parameters with Confidence Intervals using Best",
       x = "Parameter",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("Initial Range" = "red", "Updated Range" = "green", "95% CI" = "blue", "best sim" = "black", "median" = "orange")) +
  scale_x_discrete(labels = function(x) gsub(" - ", "\n", x))  # Customize x-axis labels

c_high


####################median

# Plot with customized x-axis labels
c_one <- ggplot(combined_data_flow_lai_et_plot %>% filter(category  == "one",cenetr == "median"), aes(x = Parameter_Label)) +
  geom_errorbar(aes(ymin = Min, ymax = Max, color = "Initial Range"), width = 0.1, linetype = "dashed") +
  geom_errorbar(aes(ymin = b_j_min, ymax = b_j_max, color = "Updated Range"), width = 0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.4) +
  geom_point(aes(y = Median, color = as.factor(metric)), size = 2, alpha=0.2) +
  theme_minimal() +
  theme_bw() +
  #  facet_grid(metric ~ ., scales = "fixed") +
  labs(title = "Initial Parameter Ranges vs Updated Parameters with Confidence Intervals using median",
       x = "Parameter",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("Initial Range" = "red", "Updated Range" = "green", "95% CI" = "blue")) #+
#scale_x_discrete(labels = function(x) gsub(" - ", "\n", x))  # Customize x-axis labels

c_one
# Plot with customized x-axis labels
c_two <- ggplot(combined_data_flow_lai_et_plot %>% filter(category == "two",cenetr == "median"), aes(x = Parameter_Label)) +
  geom_errorbar(aes(ymin = Min, ymax = Max, color = "Initial Range"), width = 0.1, linetype = "dashed") +
  geom_errorbar(aes(ymin = b_j_min, ymax = b_j_max, color = "Updated Range"), width = 0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.4) +
  geom_point(aes(y = Median, color = cenetr), size = 2, alpha=0.2) +
  theme_minimal() +
  theme_bw() +
  # facet_grid(categ ~ ., scales = "free") +
  labs(title = "Initial Parameter Ranges vs Updated Parameters with Confidence Intervals using median",
       x = "Parameter",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("Initial Range" = "red", "Updated Range" = "green", "95% CI" = "blue", "median" = "black", "median" = "orange")) +
  scale_x_discrete(labels = function(x) gsub(" - ", "\n", x))  # Customize x-axis labels

c_two

c_three <- ggplot(combined_data_flow_lai_et_plot %>% filter(category == "three",cenetr == "median"), aes(x = Parameter_Label)) +
  geom_errorbar(aes(ymin = Min, ymax = Max, color = "Initial Range"), width = 0.1, linetype = "dashed") +
  geom_errorbar(aes(ymin = b_j_min, ymax = b_j_max, color = "Updated Range"), width = 0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.4) +
  geom_point(aes(y = Median, color = cenetr), size = 2, alpha=0.2) +
  theme_minimal() +
  theme_bw() +
  # facet_grid(categ ~ ., scales = "free") +
  labs(title = "Initial Parameter Ranges vs Updated Parameters with Confidence Intervals using median",
       x = "Parameter",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("Initial Range" = "red", "Updated Range" = "green", "95% CI" = "blue", "median" = "black", "median" = "orange")) +
  scale_x_discrete(labels = function(x) gsub(" - ", "\n", x))  # Customize x-axis labels

c_three


c_high <- ggplot(combined_data_flow_lai_et_plot %>% filter(category == "High",cenetr == "median"), aes(x = Parameter_Label)) +
  geom_errorbar(aes(ymin = Min, ymax = Max, color = "Initial Range"), width = 0.1, linetype = "dashed") +
  geom_errorbar(aes(ymin = b_j_min, ymax = b_j_max, color = "Updated Range"), width = 0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.4) +
  geom_point(aes(y = Median, color = cenetr), size = 2, alpha=0.2) +
  theme_minimal() +
  theme_bw() +
  # facet_grid(categ ~ ., scales = "free") +
  labs(title = "Initial Parameter Ranges vs Updated Parameters with Confidence Intervals using median",
       x = "Parameter",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("Initial Range" = "red", "Updated Range" = "green", "95% CI" = "blue", "median" = "black", "median" = "orange")) +
  scale_x_discrete(labels = function(x) gsub(" - ", "\n", x))  # Customize x-axis labels

c_high
