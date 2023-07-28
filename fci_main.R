### Optional: CLEAN ENVIRONMENT AND COMMAND LINE

cat("\014")
rm(list=ls())


### Mandatory: Set Working Directory

workingDirectory <- "C:/cemla/Documents/GaR/Portal Update/CGARP/Code"
#--------------------------------------------------------------


### CODE

setwd(workingDirectory)

source("fci_packages.R")
source("fci_parameters.R")
source("fci_wrapper.R")

if(saveXLSX){
  source("fci_output.R")
}

rm(list=setdiff(ls(),
                c("aggregatedFCI",
                "iData","mData","dData",
                "parametersDF")
                ))