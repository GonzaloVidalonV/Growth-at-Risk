### CLEAN ENVIRONMENT AND COMMAND LINE

cat("\014")
rm(list = ls())


### Working Directory

workingDirectory <- "C:/cemla/Documents/GaR/Portal Update/CGARP/Code"



### CODE

setwd(workingDirectory)

source("GaR_packages.R")
source("GaR_parameters.R")
source("GaR_wrapper.R")
source("GaR_output.R")