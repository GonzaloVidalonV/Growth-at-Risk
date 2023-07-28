################################################################################
### Necessary packages
################################################################################

packages <- c(
  'readxl',
  'xlsx',
  'lubridate',
  'ggplot2',
  'dplyr',
  'ggpubr',
  'plotly',
  'latex2exp',
  'caTools',
  'rqpd',
  'broom',
  'snpar',
  'tidyverse'
)

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
