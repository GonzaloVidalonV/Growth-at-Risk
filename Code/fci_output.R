#STORING XLSX TIME AND PARAMETER STAMPED FILE OUTPUT

auxTime <-
  str_remove_all(as.character(Sys.time()) , pattern = "[-: ]")
auxTime <-
  paste (substr(auxTime, 1, 8), "_", substr(auxTime, 9, 14), sep = "")

outputFile <- paste(outputPath, "/", "FCI_", auxTime, "_", fileTag,".xlsx", sep = "")


# Saving long data frame
if (saveLong) {
  write.xlsx(
    x = iData[, c('DATES', 'country', 'fcis')],
    file = outputFile,
    row.names = FALSE,
    append = FALSE,
    sheetName = "FCI"
  )
} else{
  write.xlsx(
    x = aggregatedFCI,
    file = outputFile,
    row.names = FALSE,
    append = FALSE,
    sheetName = "FCI"
  )
}


if(saveIndexRelatedData){
  write.xlsx(x=iData, file = outputFile, row.names = FALSE,
             append = TRUE, sheetName = "Index Data")
}

if(saveMonthlyRelatedData){
  write.xlsx(x=mData, file = outputFile, row.names = FALSE,
             append = TRUE, sheetName = "Monthly Data")
}

if(saveDailyRelatedData){
  write.xlsx(x=dData, file = outputFile, row.names = FALSE,
             append = TRUE, sheetName = "Daily Data")
}


write.xlsx(
  x = parametersDF,
  file = outputFile,
  row.names = FALSE,
  append = TRUE,
  sheetName = "Parameters"
)


#Output to clipboard
# write.table(x=dData, file="clipboard-16384", sep="\t")
# write.table(x=iData, file="clipboard-16384", sep="\t")
# write.table(x=mData, file="clipboard-16384", sep="\t")