#### Parameters

initialDate <- "2000-01-01"
finalDate   <- "2021-12-31"

countries <- c("BRAZIL",
               "CHILE",
               "COLOMBIA",
               "MEXICO",
               "PERU")

referenceRyrCountry <- "US"

lagTilde  <- 260*10-1
lagPeriod <- 20
lagWindow <- 260*2
lagYear   <- 261

lagTildeMonth <- 119
lagMonth <- 1
lagWindowMonth <- 6

standardizationMethod <- "method_b_max"

lambda = 0.85


### Saving output options

saveXLSX <- TRUE
saveLong <- TRUE
saveIndexRelatedData <- TRUE
saveMonthlyRelatedData <- FALSE
saveDailyRelatedData <- FALSE

outputPath <- "C:/Users/gonza/Documents/Growth at RIsk/Growth-at-Risk"
fileTag <- "test"


