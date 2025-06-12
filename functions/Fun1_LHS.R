#fucntion1 Define function for Latin Hypercube Sampling 
lhsRange <- function(nIter, paramRange) {
  nParam <- nrow(paramRange)
  paramSampling <- lhs::randomLHS(nIter, nParam)
  
  for (i in 1:nParam) {
    paramSampling[, i] <- paramRange$min_value[i] + paramSampling[, i] * 
      (paramRange$max_value[i] - paramRange$min_value[i])
  }
  
  paramSampling <- as.data.frame(paramSampling)
  colnames(paramSampling) <- as.character(paramRange$param)
  paramSampling <- cbind(Simulation = 1:nrow(paramSampling), paramSampling)
  return(paramSampling)
}



#testing
# Example parameter range
# param_forgoal_lhs <- tibble(
#   param = c("param1", "param2"),
#   min_value = c(0, 20),
#   max_value = c(10, 30)
# )
# 
# # Perform LHS sampling
# nIter <- 3
# param_sampling <- lhsRange(nIter, param_forgoal_lhs)
# 
# # Print sampled parameters
# print(param_sampling)