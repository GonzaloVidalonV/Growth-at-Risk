
standardizationFunctions <- list()


method_a_cdf <- function( values, timeFrame = 120){
  
  #Inverse empirical cdf transformation based on Duprey 2019
  
  output <- ecdf(values[1:timeFrame])(values[1:timeFrame])
  
  for(k in (timeFrame+1):length(values)){
    output <- append(output, ecdf(values[1:k])(values[k]))
  }
  return(output)
  
}



method_b_max <- function( values, timeFrame = 120){
  
  #Only entrying values are scaled by the maximum of the new value and the previous ones
  
  output <- values[1:timeFrame]/max(values[1:timeFrame])
  
  for(k in (timeFrame+1):length(values)){
    output <- append(output, values[k]/max(values[1:k]))
  }
  
  return(output)
}



cdf_winsorized <- function(values, timeFrame = 120, sigmas = 3){
  
  winsor_values <- values
  
  for(r in ncol(values)){
    s = sd(winsor_values[,r])
    m = mean(winsor_values[,r])
    threshold = m + sigmas * s
    winsor_values[winsor_values[ ,r]>threshold ,r] = threshold
  }
  
  output <- method_a_cdf(winsor_values, timeFrame)
  return(output)
}



max_winsorized <- function(values, timeFrame = 120, sigmas = 3){
  
  winsor_values <- values
  
  for(r in ncol(values)){
    s = sd(winsor_values[,r])
    m = mean(winsor_values[,r])
    threshold = m + sigmas * s
    winsor_values[winsor_values[ ,r]>threshold ,r] = threshold
  }
  
  output <- method_b_max(winsor_values, timeFrame)
  return(output)
}
