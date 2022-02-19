gev_likelihood <- function(parameters, data) {
  
  #check parameter constraint
  if(all(1 + parameters[3]*(data - parameters[1])/parameters[2] > 0) == TRUE & parameters[2] > 0) {
    #check if shape parameter about zero
      if(abs(parameters[3]) >= 0.001){
        #compute log-likelihood when parameter not zero
        summend_0 <- (1 + parameters[3]*(data - parameters[1])/parameters[2])^(-1/parameters[3])
        summend_1 <- log(1 + parameters[3]*(data - parameters[1])/parameters[2])
        result <- -sum(summend_0) -(1+1/parameters[3])*sum(summend_1) - length(data)*log(parameters[2])
    } else {
        #compute log-likelihood when parameter is about zero
        summend_2 <- exp(-(data - parameters[1])/parameters[2])
        summend_3 <- (data - parameters[1])/parameters[2]
        result <- -sum(summend_2) - sum(summend_3) - length(data)*log(parameters[2])
    }
  } #set log-likelihood to large negative number if parameter constraint violated
    else {
      result <- -10^10
    }
  #return the appropriate log-likelihood for the given parameters and data
  return(result)
}

#set initial guess, results are sensitive to changes
guess_gev <- c(mean(sandy_hook_max$max), sd(sandy_hook_max$max), 0.001)

#use BFGS algorithm via the optim function to obtain MLEs
result_gev <- optim(initial_guess, gev_likelihood, data = sandy_hook_max$max, method = 'BFGS', hessian = T, control=list(maxit = 10000, fnscale = -1))

#compare MLEs with those obtained via the extREMES packages function fevd
result_gev$par
model0$results[1]

#extract Hession, find inverse then compute covariance matrix of MLEs
V <- -solve(test_result$hessian)
V

#from R. Smith's code, computes matrix consisting of t-test information for each parameter estimate
res <-matrix(nrow=3,ncol=4)
for(i in 1:3){
  res[i,1] <- result_gev$par[i]
  res[i,2] <- sqrt(V[i,i])
  res[i,3] <- res[i,1]/res[i,2]
  res[i,4] <- 2*pnorm(-abs(res[i,3]))
}

res <- cbind(c('mu','sigma','xi'),round(res,4))
res <- rbind(c('Parameter','Estimate','SE','t value','p value'),res)