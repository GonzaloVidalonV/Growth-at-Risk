

################################################################################
### Packages setup
################################################################################

print("Packages loading...")

for (p in packages) {
  if (!p %in% installed.packages()) {
    install.packages(p)
  }
}

for (p in packages) {
  if (!p %in% tolower((.packages()))) {
    library(p, character.only = TRUE)
  }
}

rm(list = c("p",
            "packages"))



################################################################################
### Preamble v.0 variables
################################################################################

print("Preamble setting...")

flag_output <- TRUE

tau_w <- NA

n_date <- 1

fixed_df_headers <- c("Date",
                      "Country",
                      "N_Country")


orth_vars <- paste(orth_var_dep, orth_var_ind, "res", sep = "_")


df_selected_date <- data.frame(
  date = character(),
  country = character(),
  model = character(),
  predictive_score = double(),
  expected_shortfall = double(),
  expected_longrise = double(),
  entropy_d = double(),
  entropy_u = double(),
  outturn = double(),
  GaR_alpha = double(),
  prob_negative_growth = double(),
  mean = double(),
  std_dev = double(),
  skewness = double(),
  kurt = double(),
  
  stringsAsFactors = FALSE
)


ls_functions <- list()

ls_idx_x_grid <- list()

vec_r <- 1

# Date for saving excel files
hoy <- format(today(tzone = ""), '%y%m%d')



variables <- c("dependent_variable",
               "independent_variables",
               "orth_var_dep ",
               "orth_var_ind",
               "h",
               "n_tau",
               "selected_date_v",
               "selected_date_next_v",
               "GaR_level",
               "probability",
               "alpha_CI")

variablesValues <- c(dependent_variable,
                     paste(independent_variables,collapse =", "),
                     paste(orth_var_dep,collapse = ", ") ,
                     paste(orth_var_ind,collapse = ", "),
                     h,
                     n_tau,
                     selected_date_v,
                     selected_date_next_v,
                     GaR_level,
                     probability,
                     alpha_CI)

parametersDF <- data.frame(Variables = variables,
                           Values = variablesValues)



################################################################################
### Preamble functions
################################################################################

z_score <- function(data) {
  return((data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE))
}



get_q_coeff <- function(aux_data_frame){
  
  n = nrow(aux_data_frame)
  
  aux_data_frame$rowresult <- row.names(aux_data_frame)
  aux_data_frame$q <- NA
  aux_data_frame$coefficient <- NA
  
  q1 = regex("\\[\\d+\\.\\d+\\]")
  q2 = regex("\\d+\\.\\d+")
  
  for(k in 1:n){
    aux_data_frame$coefficient[k] <- gsub(pattern = q1,
                                          replacement = "",
                                          x = aux_data_frame$rowresult[k])
    aux_data_frame$q[k] <- as.numeric(str_extract(pattern =  q2,
                                                  string =  aux_data_frame$rowresult[k]))
  }
  return(aux_data_frame)
}



################################################################################
### Main code
################################################################################


print("Data loading...")

for (k_date in 1:n_date) {
  selected_date <- selected_date_v[k_date]
  selected_date_next <- selected_date_next_v [k_date]
  
  selected_date_format = as.character.Date(x = selected_date)
  
  ## Load the PANEL Data
  my_data <-
    read_excel(path = source_file, sheet = sheet) #Data read
  
  original_my_data <- my_data
  
  my_data <-
    my_data[my_data$Date <= as.Date.character(selected_date), ]
  
  #Nice Date formatting
  my_data$Date <- ymd(my_data$Date)
  
  
  
  # Main Model Parameters ----
  
  print("Model setting...")
  
  countries <- unique(my_data$N_Country) #Indices of panel members
  n_countries <- length(countries) #Number of members of the panel
  country_names <-
    as.character(unique(unlist(my_data$Country))) #Labels
  
  # Preparing the Data ----
  
  future_data <- my_data #data initialization
  
  lead_dep_var_aux <- vector()
  
  auxiliary_independent_variables <-
    setdiff(names(future_data), fixed_df_headers)
  
  for (df_var in union(independent_variables, orth_vars)) {
    assign(paste0(df_var, "_aux"), vector())
  }
  
  #loop for each country
  for (i in 1:n_countries) {
    local_data <- my_data[my_data$N_Country == countries[i], ]
    
    # Obtaining orthogonalized variables
    for (k_orth in 1:length(orth_vars)) {
      orth_formula = paste(orth_var_dep[k_orth], orth_var_ind[k_orth],  sep = "~")
      model_aux <-
        lm(as.formula(orth_formula), local_data, na.action = na.omit)
      local_data[, orth_vars[k_orth]] <- 1
      local_data[, orth_vars[k_orth]] <-
        local_data[, orth_vars[k_orth]] * NA
      local_data[which(!is.na(local_data[orth_var_dep[k_orth]])
                       &!is.na(local_data[orth_var_ind[k_orth]])),
                 orth_vars[k_orth]] <-
        residuals(model_aux) #attach residuals
    }
    
    # z_score of independent variables
    
    for (df_var in union(independent_variables, orth_vars)) {
      assign(paste0(df_var, "_aux"),
             cbind(get(paste0(
               df_var, "_aux"
             )),
             z_score(as.vector(local_data[[df_var]]))))
    }
    
    
    #future GDP (dependent variable). NOTE: no z_score for better interpretation
    lead_dep_var_aux <-
      cbind(lead_dep_var_aux, lead(local_data[[dependent_variable]], n = h, default = NA))
  }
  
  
  #attach as panel data
  future_data$future_dep_var <- c(lead_dep_var_aux)
  for (df_var in union(independent_variables, orth_vars)) {
    future_data[[df_var]] <-
      c(get(paste0(df_var, "_aux")))
  }
  
  
  remove(local_data)
  remove(model_aux)
  remove(lead_dep_var_aux)
  
  remove(list = paste0(union(independent_variables, orth_vars), "_aux"))
  
  
  
  
  # Panel Quantile Regression ----
  
  # Quantile regression parameters
  tau_list <- 1:(n_tau) / (n_tau + 1) #Quantile list
  
  if(is.na(tau_w)) {
    tau_w <-
      rep(1 / length(tau_list), length(tau_list)) #Option for weights, we use equally weighted
  }
  
  # Panel specification after "|" are the country fixed effects
  dep_var <- 'future_dep_var'
  indep_var <- union(independent_variables, orth_vars)
  fixed_effects <- 'N_Country'
  
  if (exists('interaction_term')) {
    k_model <- paste(
      dep_var,
      " ~ ",
      paste(indep_var, collapse = "+"),
      '+',
      paste(interaction_term, collapse = "+"),
      '| as.factor(',
      fixed_effects,
      ')'
    )
    qr_form <-
      as.formula(k_model)
  }else{
    k_model <- paste(
      dep_var,
      " ~ ",
      paste(indep_var, collapse = "+"),
      '| as.factor(',
      fixed_effects,
      ')'
    )
    qr_form <-
      as.formula(k_model)
  }
  
  print("Model estimation...")
  
  #main function
  rqfit <-
    rqpd(qr_form,
         panel(
           method = "pfe",
           taus = tau_list,
           tauw = tau_w
         ),
         data = future_data,
         na.omit)
  
  #Unconditional model
  rqfit_0 <-
    rqpd(
      future_dep_var ~ 1 |
        as.factor(N_Country),
      panel(
        method = "pfe",
        taus = tau_list,
        tauw = tau_w
      ),
      data = future_data,
      na.omit
    )
  
  
  #coefficient extraction for Excel saving
  summ_rqfit <-
    summary.rqpd(rqfit, R = 500)$coefficients #R = 1000 (number of bootstrap)
  
  
  #pseudo-R
  rho <- function(u, tau) {
    R_aux <- vector()
    u <- head(u, length(u) - n_countries)
    u <- matrix(u, nrow = length(u) / length(tau), byrow = FALSE)
    
    for (t in 1:length(tau)) {
      u_aux <- u[, t]
      R_aux <- rbind(R_aux, sum(u_aux * (tau[t] - (u_aux < 0))))
    }
    
    return(R_aux)
  }
  
  R1 <-
    1 - rho(rqfit$residuals, rqfit$panel$taus) / rho(rqfit_0$residuals, rqfit$panel$taus)
  print("Pseudo R-squared...")
  print(t(R1))
  
  #quantile coefficients estimates
  beta_taus <- coef(rqfit)
  
  FE_terms <- tail(beta_taus, n_countries) #extract fixed effects
  
  #rearranging for obtaining estimated quantiles
  beta_taus <- head(beta_taus, length(beta_taus) - n_countries)
  beta_taus <-
    matrix(beta_taus,
           nrow = n_tau,
           byrow = length(beta_taus) / n_tau)
  beta_taus <- t(beta_taus)
  
  print("Distribution results...")
  #Estimated quantiles including FE
  
  future_data_top <-
    future_data[future_data$Date == selected_date, ]
  
  Q_aux <- rep(1, dim(future_data_top)[1])
  for (vr in indep_var) {
    Q_aux <- cbind(Q_aux, future_data_top[, vr])
  }
  
  if (exists('interaction_term')) {
    int_var <- unlist(strsplit(interaction_term, '\\:'))
    
    Q_aux <-
      cbind(Q_aux, future_data_top[, int_var[1]] * future_data_top[, int_var[2]])
  }
  
  Q_hat <-
    data.matrix(Q_aux) %*% beta_taus + FE_terms[future_data_top$N_Country]
  
  #Attach on panel data? OPTIONAL
  future_data_top$Q_hat <- Q_hat
  
  
  # Future Distribution Fitting ----
  #  We use a non-parametric approach based on kernel estimates of CDF
  
  n_grid <-
    5000 # grid size for the kernel distributions (for the support)
  
  #initialization of variables
  PDF_mat <- vector()
  CDF_mat <- vector()
  x_mat <- vector()
  
  promedio <- vector()
  desv_est <- vector()
  coef_asim <- vector()
  curtosis <- vector()
  
  GaR <- vector()
  below_zero <- vector()
  q_realized <- vector()
  predictive_score <- vector()
  
  
  for (n in 1:dim(Q_hat)[1]) {
    # NA cases -> NA outputs
    if (is.na(Q_hat[n, 1])) {
      aux_na <- rep(NA, n_grid)
      PDF_mat <- rbind(PDF_mat, aux_na)
      CDF_mat <- rbind(CDF_mat, aux_na)
      x_mat <- rbind(x_mat, aux_na)
      
      promedio <- rbind(promedio, NA)
      desv_est <- rbind(desv_est, NA)
      coef_asim <- rbind(coef_asim, NA)
      curtosis <- rbind(curtosis, NA)
      
      GaR <- rbind(GaR, NA)
      below_zero <- rbind(below_zero, NA)
      
      q_realized <- rbind(q_realized, NA)
      predictive_score <- rbind(predictive_score, NA)
      
      #non-NA cases
    } else{
      #same support for plotting purposes, one can use other options
      
      x_min <- -100
      x_max <- 100
      
      #support grid
      x_grid <- seq(from = x_min,
                    to = x_max,
                    length.out = n_grid)
      
      #Kernel estimates
      
      dist_kernel <-
        kde(Q_hat[n, ], kernel = "triw", xgrid = x_grid)
      
      PDF <- splinefun(x_grid, dist_kernel$fhat) #PDF
      CDF <- splinefun(x_grid, dist_kernel$Fhat) #CDF
      
      
      #Finding GaR value based on unitroot estimates
      
      #function to search the zero at 'a' level
      GaR_fun <- function(x, a) {
        CDF(x) - a
      }
      
      
      # a = 0.05 -> 5% GaR
      GaR_find <- uniroot(
        GaR_fun,
        interval = c(x_min, x_max),
        tol = 0.0001,
        a = probability
      )
      
      
      GaR_median <- uniroot(
        GaR_fun,
        interval = c(x_min, x_max),
        tol = 0.0001,
        a = 0.5
      )
      
      #integral functions for moment estimates
      int <- function(x,
                      c = 0,
                      g = PDF,
                      n = 1)
        (x - c) ^ n * g(x)
      
      #mean
      m_1 <- integrate(int,
                       lower = x_min,
                       upper = x_max,
                       n = 1)
      mu <- m_1$value
      
      #variance -> second centered moment
      sigma_2 <- integrate(
        int,
        lower = x_min,
        upper = x_max,
        c = mu,
        n = 2
      )
      #standard deviation
      std_dev <- sqrt(sigma_2$value)
      
      #third centered moment
      m_3 <- integrate(
        int,
        lower = x_min,
        upper = x_max,
        c = mu,
        n = 3
      )
      #skewness
      skew_coef <- m_3$value / std_dev ^ 3
      
      #fourth centered moment
      m_4 <- integrate(
        int,
        lower = x_min,
        upper = x_max,
        c = mu,
        n = 4
      )
      #kurtosis
      kurt_coef <- m_4$value / std_dev ^ 4
      
      
      #probability mass below the 0% growth
      p_below_zero <- CDF(0)
      
      
      #asignation of variables of interest
      
      PDF_mat <- rbind(PDF_mat, dist_kernel$fhat)
      CDF_mat <- rbind(CDF_mat, dist_kernel$Fhat)
      x_mat <- rbind(x_mat, x_grid)
      
      promedio <- rbind(promedio, mu)
      desv_est <- rbind(desv_est, std_dev)
      coef_asim <- rbind(coef_asim, skew_coef)
      curtosis <- rbind(curtosis, kurt_coef)
      
      below_zero <- rbind(below_zero, p_below_zero)
      GaR <- rbind(GaR, GaR_find$root)
      
      q_realized <-
        rbind(q_realized, CDF(future_data_top$future_dep_var[n]))
      predictive_score <-
        rbind(predictive_score,
              PDF(future_data_top$future_dep_var[n]))
    }
    
    
    observed_value <-
      original_my_data$g_GDP[original_my_data$Country == future_data_top$Country[n]  &
                               original_my_data$Date == as.Date.character(selected_date_next)]
    
    if (length(observed_value) == 0) {
      observed_value =  which(PDF(x_grid) > 10 ^ -13)[1]
    }
    
    aux_support = which(PDF(x_grid) > 10 ^ -13 |
                          abs(x_grid - observed_value) < 2.5)
    aux_support = seq(min(aux_support), max(aux_support))
    
    idx_x_grid = aux_support
    
    idx_selected_date <- nrow(df_selected_date) + 1
    
    df_selected_date[idx_selected_date, ] <- NA
    
    df_selected_date$outturn[idx_selected_date] <- observed_value
    
    df_selected_date$date[idx_selected_date] <- selected_date_next
    
    df_selected_date$country[idx_selected_date] <-
      future_data_top$Country[n]
    
    df_selected_date$predictive_score[idx_selected_date] <-
      PDF(observed_value)
    
    df_selected_date$GaR_alpha[idx_selected_date] <-
      GaR_find$root
    
    df_selected_date$prob_negative_growth[idx_selected_date] <-
      p_below_zero
    
    df_selected_date$mean[idx_selected_date] <- mu
    df_selected_date$std_dev[idx_selected_date] <- std_dev
    df_selected_date$skewness[idx_selected_date] <- skew_coef
    df_selected_date$kurt[idx_selected_date] <- kurt_coef
    
    ES <-
      integrate(f = int,
                lower = x_grid[1],
                upper = GaR_find$root)
    
    probability = 0.05
    df_selected_date$expected_shortfall[idx_selected_date] <-
      (1 / probability) * ES$value
    
    upper_find <- uniroot(
      GaR_fun,
      interval = c((x_min+ x_max)/2, x_max),
      tol = 0.0001,
      a = 0.95
    )

    LR <-
      integrate(
        f = int,
        lower = upper_find$root,
        upper = tail(x_grid, n = 1)
      )
    
    df_selected_date$expected_longrise[idx_selected_date] <-
      (1 / probability) * LR$value
    
    df_selected_date$model[idx_selected_date] <- k_model
    
    #list_entry <-
    #  paste(selected_date, k_model, future_data_top$Country[n],
    #        sep = "_")
    
    #ls_functions[[list_entry]] <- PDF
    
    #ls_idx_x_grid[[list_entry]] <- idx_x_grid
  }
}

