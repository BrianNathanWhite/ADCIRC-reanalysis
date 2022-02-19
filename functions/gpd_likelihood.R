gpd_likelihood <- function(parameters, data) {
  
  #check parameter constraint
  if(all(1 + parameters[2]*data/parameters[1] > 0) == T & parameters[1] > 0) {
    #check if shape parameter not about 0
    if(abs(parameters[2]) >= 0.01){
      #compute log-likelihood when parameter not about zero
      summend_0 <- log(1 + parameters[2]*data/parameters[1])
      result <- -length(data)*log(parameters[1]) - (1+1/parameters[2])*sum(summend_0)
    } else {
      #compute log-likelihood when parameter is about zero
      result <- -length(data)*log(parameters[1]) - (1/parameters[1])*sum(data)
    }
  } #set log-likelihood to large negative number if parameter constraint violated
  else {
    result <- -10^10
  }
  #return the appropriate log-likelihood for the given parameters and data
  return(result)
}

#set the threshold, those observations which exceed this are considered extreme
threshold <- 0.5

#create data set containing the threshold exceedences
sandy_hook %>%
  filter(ADC_POST_Detided > 0.5) %>%
  mutate(exceedence = ADC_POST_Detided - threshold) %>%
  select(TIME, exceedence) -> sandy_hook_exceedence

#initial guess
guess_gpd <- c(0.5, 0.5)

#find the MLEs
result_gpd <- optim(guess_gpd, gpd_likelihood, data = sandy_hook_exceedence$exceedence, method = 'BFGS', hessian = T, control=list(maxit = 10000, fnscale = -1))

#compare result with extRemes function fevd
result_gpd$par
model2$results[1]