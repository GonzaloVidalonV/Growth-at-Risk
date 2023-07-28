################################################################################
### Necessary packages
################################################################################

packages <- c("xlsx",
              "tidyverse",
              "imputeTS",
              "lubridate",
              "rJava",
              "plotly")


################################################################################
### Packages setup
################################################################################

for(p in packages){
  if(! p %in% installed.packages()){
    install.packages(p)
  }
}

for(p in packages){
  if(! p %in% tolower((.packages()))){
    library(p,character.only = TRUE)
  }
}

rm(p)
