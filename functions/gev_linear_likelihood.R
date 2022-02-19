gev_linear_likelihood <- function(parameters, data, predictor) {
  
  #check parameter constraint
  if(all(1 + parameters[4]*(data[,1] - (parameters[1] + parameters[2]*data[,predictor]))/parameters[3] > 0) == TRUE & parameters[3] > 0) {
    #check if shape parameter about zero
    if(abs(parameters[4]) >= 0.001){
      #compute log-likelihood when parameter not zero
      summend_0 <- (1 + parameters[4]*(data[,1] - (parameters[1] + parameters[2]*data[,predictor]))/parameters[3])^(-1/parameters[4])
      summend_1 <- log(1 + parameters[4]*(data[,1] - (parameters[1] + parameters[2]*data[,predictor]))/parameters[3])
      result <- -sum(summend_0) -(1+1/parameters[4])*sum(summend_1) - length(data[,1])*log(parameters[3])
    } else {
      #compute log-likelihood when parameter is about zero
      summend_2 <- exp(-(data[,1] - (parameters[1] + parameters[2]*data[,predictor]))/parameters[3])
      summend_3 <- (data[,1] - (parameters[1] + parameters[2]*data[,predictor]))/parameters[3]
      result <- -sum(summend_2) - sum(summend_3) - length(data[,1])*log(parameters[3])
    }
  } #set log-likelihood to large negative number if parameter constraint violated
  else {
    result <- -10^10
  }
  #return the appropriate log-likelihood for the given parameters and data
  return(result)
}

#set initial guess, results are sensitive to changes
guess_gev_linear <- c(.5, .5, .5, .5)

#use BFGS algorithm via the optim function to obtain MLEs
result_gev_linear <- optim(guess_gev_linear, gev_linear_likelihood, data = sandy_hook_max, predictor = 3, method = 'BFGS', hessian = T, control=list(maxit = 10000, fnscale = -1))

#compare MLEs with those obtained via the extREMES packages function fevd
result_gev_linear$par
model1$results[1]