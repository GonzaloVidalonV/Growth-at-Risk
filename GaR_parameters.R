
################################################################################
### Preamble parameters
################################################################################


source_file = "../Source/GaR_panel.xlsx"
sheet = "Panel"

dependent_variable <- "g_GDP"

independent_variables <- c("g_GDP",
                           "VIX")

orth_var_dep <- c("FCI")
orth_var_ind <- c("VIX")


#Forecast horizon
h <- 1

#Number of quantiles (19 -> tau in {0.05,0.10,...,0.95})
n_tau <- 19


#Enable to introduce interaction terms
#interaction_term <- 


selected_date_v = c('2021/09/01')

selected_date_next_v = c('2021/12/01')

# a = 0.05 -> 5% GaR
GaR_level <- 0.05

#Expected Shortfall and Expected Longrise quantile level
probability <- 0.05

#Confidence level of coefficient intervals
alpha_CI <- 0.05

### Saving output options

outputPath <- "../output"
fileTag <- "test"