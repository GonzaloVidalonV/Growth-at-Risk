print("Output setting...")

auxTime <-
  str_remove_all(as.character(Sys.time()) , pattern = "[-: ]")
auxTime <-
  paste (substr(auxTime, 1, 8), "_", substr(auxTime, 9, 14), sep = "")

outputFile <-
  paste(outputPath, "/", "GaR_", auxTime, "_", fileTag, ".xlsx", sep = "")


### Saving data information

write.xlsx(
  x = future_data,
  file = outputFile,
  sheet = "Data",
  append = FALSE
)


### Saving quantile information

aux_data_frame <- as.data.frame(summ_rqfit)

aux_data_frame <- get_q_coeff(aux_data_frame)

colnames(aux_data_frame) = c('Value',
                             'Standard_Error',
                             't',
                             'Prob',
                             'Var_quantile',
                             'Quantile',
                             'Variable')

aux_data_frame$Confidence_Interval <-
  aux_data_frame$Standard_Error * qnorm(1 - alpha_CI / 2)

write.xlsx(
  x = aux_data_frame[, c(
    'Quantile',
    'Variable',
    'Value',
    'Standard_Error',
    't',
    'Prob',
    'Confidence_Interval'
  )],
  file = outputFile,
  sheet = "Quantile coefficients",
  append = TRUE
)





### Saving plots

plot_row <- 1

wb <- loadWorkbook(outputFile)

Plots_sheet <- createSheet(wb, sheetName = "Plots")

for (ind_var in union(independent_variables, orth_vars)) {
  filename = paste0(ind_var, ".png")
  
  png(
    filename,
    height = 800,
    width = 800,
    res = 250,
    pointsize = 8
  )
  
  sub_aux_df <- aux_data_frame[aux_data_frame$Variable == ind_var, ]
  
  y_min = min(sub_aux_df$Value - sub_aux_df$Confidence_Interval)
  y_max = max(sub_aux_df$Value + sub_aux_df$Confidence_Interval)
  
  plot(
    x = sub_aux_df$Quantile,
    y = sub_aux_df$Value,
    xlim = c(0, 1),
    ylim = c(y_min, y_max),
    xlab = "Quantiles",
    ylab = "Estimated coefficients",
    main = ind_var,
    pch = 3,
    cex = 1
  )
  
  abline(h = 0, col = rgb(0.5, 0.5, 0.5))
  
  for (q1 in tau_list) {
    if (!is.na(q1)) {
      k_q1 <- which(sub_aux_df$Quantile == q1)
      top_CI <-
        sub_aux_df$Value[k_q1] + sub_aux_df$Confidence_Interval[k_q1]
      bottom_CI <-
        sub_aux_df$Value[k_q1] - sub_aux_df$Confidence_Interval[k_q1]
      if (top_CI * bottom_CI <= 0) {
        aux_color <- rgb(1, 0, 0)
      } else{
        aux_color <- rgb(0, 0, 1)
      }
      lines(
        c(q1, q1),
        c(bottom_CI, top_CI),
        col = aux_color,
        lty = 3,
        type = "b",
        pch = 3,
        cex = .7
      )
    }
  }
  
  dev.off()
  
  addPicture(
    filename,
    Plots_sheet,
    scale = 1,
    startRow = plot_row,
    startColumn = 1
  )
  
  plot_row <- plot_row + 16
  saveWorkbook(wb, outputFile)
  res <- file.remove(filename)
  
}


filename = paste0("pseudoRsquared", ".png")

png(
  filename,
  height = 800,
  width = 800,
  res = 250,
  pointsize = 8
)

plot(
  tau_list,
  R1,
  type = "p",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "Quantiles",
  ylab = "Pseudo-R squared",
  main = "Pseudo-R squared"
)

dev.off()

addPicture(
  filename,
  Plots_sheet,
  scale = 1,
  startRow = plot_row,
  startColumn = 1
)
saveWorkbook(wb, outputFile)
res <- file.remove(filename)


write.xlsx(
  x = df_selected_date[, c(
    'date',
    'country',
    'GaR_alpha',
    'prob_negative_growth',
    'expected_shortfall',
    'expected_longrise'
  )],
  file = outputFile,
  sheet = "Distribution results",
  append = TRUE
)

write.xlsx(
  x = parametersDF,
  file = outputFile,
  sheet = "Parameters",
  append = TRUE
)


print("Process completed.")

rm(list = setdiff(
  ls(),
  c(
    "alpha_CI",
    "beta_taus",
    "CDF",
    "dep_var",
    "indep_var",
    "independent_variables",
    "my_data",
    "n_countries",
    "n_grid",
    "n_tau",
    "orth_formula",
    "orth_var_dep",
    "orth_var_ind",
    "outputPath",
    "PDF",
    "probability",
    "Q_aux",
    "q_realized",
    "qr_form",
    "R1",
    "res",
    "rqfit",
    "rqfit_0",
    "selected_date",
    "selected_date_format",
    "selected_date_next",
    "selected_date_next_v",
    "summ_rqfit",
    "upper_find",
    "aux_data_frame",
    "df_selected_date",
    "h"
  )
))