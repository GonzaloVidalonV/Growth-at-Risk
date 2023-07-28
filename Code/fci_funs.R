##########################

range_business_days <- function(from, to){
  #Function to create a range of weekdays 
  #Input
  #   from: initial date (inclusive)
  #   to: final date (inclusive)
  #Output
  #   output: range of weekdays
  
  aux = as.Date.character("2000-12-22") #This date is a Friday for reference
  aux <- weekdays(aux-(4:0))
  output <- (0:(to-from)+from)
  output <- output[weekdays(output) %in% aux]
  output <- output[length(output):1]
  output <- output[order(output,decreasing = TRUE)]
  return(output)
}

#For the daily information in the first part of the process, the convention is 
#to arrange dates in descending order according to their row position in the data frame.
#The idea behind this arrangement is that the end date of analysis are congruous,
#whereas the available information for the entry data tends to not be uniform in time.


range_eom <- function(from,to){
  output <- c()
  aux <- to
  while(aux>=from){
    output <- append(x = output, values = aux)
    aux <- aux-1
    if((aux %% 100) == 0){
      aux <- ((aux %/% 100)-1)*100 + 12
    }
  }
  output <- data.frame(DATES = output)
  return(output)
}


eomInt2Date <- function(intDates){
  intYear  <- intDates %/% 100
  intMonth <- (intDates  %% 100)+1
  intDays  <- rep(1,length(intDates))
  intYear[intMonth==13] = intYear[intMonth==13]+1
  intMonth[intMonth==13] = 1
  output <- as.Date(ISOdate(year=intYear,month=intMonth, day=intDays))-1
  return(output)
}

##########################

#Functions to integrate daily data into a range of weekdays
#Input
# dData: data frame with a date column DATES.
# Daily Variables: The corresponding data in a data frame with the column DATES,
# and another column with one of the following names:
#   STX  - capital market index
#   CPI  - consumer price index
#   Ryr  - government yields of a given node
#   Rbs  - government yields of a given node of a reference economy
#   CPIbs- consumer price index of the reference economy
#
#Output
# output: data frame with the daily variable integrated and interpolated linearly
#         for missing data on the variables

join_STX <- function(dData, STX){
  output <- dData
  output <- merge.data.frame(x = output, y = STX, by="DATES",
                             all.x = TRUE, sort = FALSE)
  output <- output[order(output$DATES, decreasing = TRUE ),]
  output$STX <- na_interpolation(output$STX)
  output$STX[output$DATES < min(STX$DATES)] <- NA
  return(output)
}

join_CPI <- function(dData, CPI){
  output <- dData
  aux_CPI <- merge.data.frame(x = data.frame(DATES=output$DATES),
                              y = CPI,by="DATES",
                              all = TRUE, sort = FALSE)
  aux_CPI <- aux_CPI[order(aux_CPI$DATES),]
  aux_CPI <- na_interpolation(aux_CPI)
  output <- merge.data.frame(x = output, y = aux_CPI,by="DATES",
                             all.x = TRUE, sort = FALSE)
  output$CPI[output$DATES < min(CPI$DATES)] <- NA
  return(output)
}

join_Rbs <- function(dData, Rbs){
  output <- dData
  output <- merge.data.frame(x = output, y = Rbs, by="DATES",
                             all.x = TRUE, sort = FALSE)
  output <- output[order(output$DATES, decreasing = TRUE ),]
  output$Rbs <- na_interpolation(output$Rbs)
  output$Rbs[output$DATES < min(Rbs$DATES)] <- NA
  return(output)
}

join_CPIbs <- function(dData, CPIbs){
  output <- dData
  output <- merge.data.frame(x = output, y = CPIbs, by="DATES",
                             all.x = TRUE, sort = FALSE)
  output <- output[order(output$DATES, decreasing = TRUE ),]
  output$CPIbs <- na_interpolation(output$CPIbs)
  output$CPIbs[output$DATES < min(CPIbs$DATES)] <- NA
  return(output)
}

join_Ryr <- function(dData, Ryr){
  output <- dData
  output <- merge.data.frame(x = output, y = Ryr,by="DATES",
                             all.x = TRUE, sort = FALSE)
  output <- output[order(output$DATES, decreasing = TRUE ),]
  output$Ryr <- na_interpolation(output$Ryr)
  output$Ryr[output$DATES < min(Ryr$DATES)] <- NA
  return(output)
}

join_rRbs <- function(dData, rRbs){
  output <- dData
  output <- merge.data.frame(x = output, y = rRbs,by="DATES",
                             all.x = TRUE, sort = FALSE)
  output <- output[order(output$DATES, decreasing = TRUE ),]
  output$rRbs <- na_interpolation(output$rRbs)
  output$rRbs[output$DATES < min(rRbs$DATES)] <- NA
  return(output)
}

join_FIX <- function(dData, FIX){
  output <- dData
  output <- merge.data.frame(x = output, y = FIX,by="DATES",
                             all.x = TRUE, sort = FALSE)
  output <- output[order(output$DATES, decreasing = TRUE ),]
  output$FIX <- na_interpolation(output$FIX)
  output$FIX[output$DATES < min(FIX$DATES)] <- NA
  return(output)
}

##########################

get_min_NA <- function(v){
  #Function to obtain the first non NA entry in the vector v (it will be used to
  #obtain the earliest date for which there is available data)
  #input
  # v: vector.
  #output
  # aux: integer.
  aux <- which(is.na(v))[1]
  return(aux)
}

##########################

#Functions to obtain the stress variables for each segment.

get_STX <- function(dailyData, lagTilde = 2609, lagPeriod = 20,
                    lagWindow = 520, originalLag = FALSE){
  n = length(dailyData$DATES)
  dailyData$rSTX  <- dailyData$STX / dailyData$CPI
  
  dailyData$lnSTX <- NA
  dailyData$lnSTX[1:(n-1)] <- log(dailyData$rSTX[1:(n-1)]/dailyData$rSTX[2:n])
  
  dailyData$auxSdSTX <- NA
  dailyData$lnSTX_tilde <- NA
  for(k in 1:(n-lagTilde)){
    dailyData$auxSdSTX[k] <- sd(dailyData$lnSTX[k:(k+lagTilde-1)])
  }
  if(!originalLag){
    nlnSTX <- get_min_NA(dailyData$lnSTX)-1
    dailyData$auxSdSTX[(nlnSTX-lagTilde):nlnSTX] <- sd(dailyData$lnSTX[(nlnSTX-lagTilde):nlnSTX])
  }
  dailyData$lnSTX_tilde <- dailyData$lnSTX/dailyData$auxSdSTX
  
  dailyData$auxSumSTX <- NA
  dailyData$VSTX <- NA
  for(k in 1:(n-lagPeriod+1)){
    dailyData$auxSumSTX[k] <- sum(abs(dailyData$lnSTX_tilde[k:(k+lagPeriod-1)]))
  }
  dailyData$VSTX <- dailyData$auxSumSTX/lagPeriod
  
  dailyData$auxMaxSTX <- NA
  dailyData$CMAX <- NA
  for(k in 1:(n-lagWindow+1)){
    dailyData$auxMaxSTX[k] <- max(dailyData$rSTX[k:(k+lagWindow-1)])
  }
  if(!originalLag){
    nrSTX <- get_min_NA(dailyData$rSTX)-1
    dailyData$auxMaxSTX[(nrSTX-lagWindow+1):nrSTX] <- max(dailyData$rSTX[(nrSTX-lagWindow+1):nrSTX])
  }
  dailyData$CMAX <- 1-(dailyData$rSTX/dailyData$auxMaxSTX)
  
  return(dailyData)
}

get_Rbs_daily <- function(dailyData, lagYear=261){
  
  n = nrow(dailyData)
  
  dailyData$rRbs <- NA
  dailyData$rRbs[1:(n-lagYear)] <- dailyData$Rbs[1:(n-lagYear)] - (dailyData$CPIbs[1:(n-lagYear)]/dailyData$CPIbs[(lagYear+1):n]-1)*100
  
  return(dailyData)
}

get_Ryr_daily <- function(dailyData, lagTilde = 2609, lagPeriod = 20,
                    lagWindow = 520, lagYear=261, originalLag = FALSE){
  n = length(dailyData$DATES)
  
  dailyData$rRyr <- NA
  dailyData$rRyr[1:(n-lagYear)] <- dailyData$Ryr[1:(n-lagYear)] - (dailyData$CPI[1:(n-lagYear)]/dailyData$CPI[(lagYear+1):n]-1)*100
  
  dailyData$chRyr <- NA
  dailyData$chRyr[1:(n-1)] <- dailyData$rRyr[1:(n-1)] - dailyData$rRyr[(1+1):(n)]
  
  dailyData$auxSdRyr <- NA
  dailyData$chRyr_tilde <- NA
  for(k in 1:(n-lagTilde)){
    dailyData$auxSdRyr[k] <- sd(dailyData$chRyr[k:(k+lagTilde-1)])
  }
  if(!originalLag){
    nchRyr <- get_min_NA(dailyData$chRyr)-1
    dailyData$auxSdRyr[(nchRyr-lagTilde):nchRyr] <- sd(dailyData$chRyr[(nchRyr-lagTilde):nchRyr])
  }
  dailyData$chRyr_tilde <- dailyData$chRyr/dailyData$auxSdRyr
  
  dailyData$auxSumRyr <- NA
  dailyData$VRyr <- NA
  for(k in 1:(n-lagPeriod+1)){
    dailyData$auxSumRyr[k] <- sum(abs(dailyData$chRyr_tilde[k:(k+lagPeriod-1)]))
  }
  dailyData$VRyr <- dailyData$auxSumRyr/lagPeriod
  
  dailyData$chSpread <- dailyData$rRyr - dailyData$rRbs
  dailyData$auxMinSpread <- NA
  dailyData$CDIFF <- NA
  for(k in 1:(n-lagWindow+1)){
    dailyData$auxMinSpread[k] <- min(dailyData$chSpread[k:(k+lagWindow-1)])
  }
  if(!originalLag){
    nchSpread <- get_min_NA(dailyData$chSpread)-1
    dailyData$auxMinSpread[(nchSpread-lagWindow+1):nchSpread] <- min(dailyData$chSpread[(nchSpread-lagWindow+1):nchSpread])
  }
  dailyData$CDIFF <- dailyData$chSpread - dailyData$auxMinSpread
  
  return(dailyData)
}

get_Ryr_daily_nominal <- function(dailyData, lagTilde = 2609, lagPeriod = 20,
                                  lagWindow = 520, lagYear=0, originalLag = FALSE){
  n = length(dailyData$DATES)
  
  dailyData$rRyr <- NA
  dailyData$rRyr[1:(n-lagYear)] <- dailyData$Ryr[1:(n-lagYear)]                 #Change: removed inflation term
  
  dailyData$chRyr <- NA
  dailyData$chRyr[1:(n-1)] <- dailyData$rRyr[1:(n-1)] - dailyData$rRyr[(1+1):(n)]
  
  dailyData$auxSdRyr <- NA
  dailyData$chRyr_tilde <- NA
  for(k in 1:(n-lagTilde)){
    dailyData$auxSdRyr[k] <- sd(dailyData$chRyr[k:(k+lagTilde-1)])
  }
  if(!originalLag){
    nchRyr <- get_min_NA(dailyData$chRyr)-1
    dailyData$auxSdRyr[(nchRyr-lagTilde):nchRyr] <- sd(dailyData$chRyr[(nchRyr-lagTilde):nchRyr])
  }
  dailyData$chRyr_tilde <- dailyData$chRyr/dailyData$auxSdRyr
  
  dailyData$auxSumRyr <- NA
  dailyData$VRyr <- NA
  for(k in 1:(n-lagPeriod+1)){
    dailyData$auxSumRyr[k] <- sum(abs(dailyData$chRyr_tilde[k:(k+lagPeriod-1)]))
  }
  dailyData$VRyr <- dailyData$auxSumRyr/lagPeriod
  
  dailyData$chSpread <- dailyData$rRyr - dailyData$Rbs                          #Change: change to nominal reference rate
  dailyData$auxMinSpread <- NA
  dailyData$CDIFF <- NA
  for(k in 1:(n-lagWindow+1)){
    dailyData$auxMinSpread[k] <- min(dailyData$chSpread[k:(k+lagWindow-1)])
  }
  if(!originalLag){
    nchSpread <- get_min_NA(dailyData$chSpread)-1
    dailyData$auxMinSpread[(nchSpread-lagWindow+1):nchSpread] <- min(dailyData$chSpread[(nchSpread-lagWindow+1):nchSpread])
  }
  dailyData$CDIFF <- dailyData$chSpread - dailyData$auxMinSpread
  
  return(dailyData)
}

get_FIX <- function(dailyData, lagTilde = 2609, lagPeriod = 20,
                    lagWindow = 521, originalLag = FALSE){
  n = length(dailyData$DATES)
  dailyData$rFIX  <- dailyData$FIX
  
  dailyData$lnFIX <- NA
  dailyData$lnFIX[1:(n-1)] <- log(dailyData$rFIX[1:(n-1)]/dailyData$rFIX[2:n])
  
  dailyData$auxSdFIX <- NA
  dailyData$lnFIX_tilde <- NA
  for(k in 1:(n-lagTilde)){
    dailyData$auxSdFIX[k] <- sd(dailyData$lnFIX[k:(k+lagTilde-1)])
  }
  if(!originalLag){
    nlnFIX <- get_min_NA(dailyData$lnFIX)-1
    dailyData$auxSdFIX[(nlnFIX-lagTilde):nlnFIX] <- sd(dailyData$lnFIX[(nlnFIX-lagTilde):nlnFIX])
  }
  dailyData$lnFIX_tilde <- dailyData$lnFIX/dailyData$auxSdFIX
  
  dailyData$auxSumFIX <- NA
  dailyData$VFIX <- NA
  for(k in 1:(n-lagPeriod+1)){
    dailyData$auxSumFIX[k] <- sum(abs(dailyData$lnFIX_tilde[k:(k+lagPeriod-1)]))
  }
  dailyData$VFIX <- dailyData$auxSumFIX/lagPeriod
  
  dailyData$auxMaxFIX <- NA
  dailyData$CFIX <- NA
  for(k in 1:(n-lagWindow+1)){
    dailyData$auxMaxFIX[k] <- max(dailyData$rFIX[k:(k+lagWindow-1)])
  }
  if(!originalLag){
    nrFIX <- get_min_NA(dailyData$rFIX)-1
    dailyData$auxMaxFIX[(nrFIX-lagWindow+1):nrFIX] <- max(dailyData$rFIX[(nrFIX-lagWindow+1):nrFIX])
  }
  dailyData$CFIX <- (dailyData$rFIX/dailyData$auxMaxFIX)
  
  return(dailyData)
}

get_d2m_dates <- function(dailyData){
  dailyData$YM <- year(dailyData$DATES)*100 + month(dailyData$DATES)
  
  dailyData$EOM <- FALSE
  dailyData$EOM[2:nrow(dailyData)] <- dailyData$YM[1:(nrow(dailyData)-1)] !=
    dailyData$YM[(1+1):nrow(dailyData)]
  dailyData$EOM[1] <- TRUE
  return(dailyData)
}


join_rEER <- function(mData, rEER){
  output <- mData
  output <- merge.data.frame(x = output, y = rEER,by="DATES",
                             all.x = TRUE, sort = FALSE)
  output <- output[order(output$DATES, decreasing = TRUE ),]
  output$rEER <- na_interpolation(output$rEER)
  output$rEER[output$DATES < min(rEER$DATES)] <- NA
  return(output)
}

join_daily <- function(mData, dData){
  output <- mData
  output <- merge.data.frame(x = output, y = dData,by="DATES",
                             all.x = TRUE, sort = FALSE, )
  output <- output[order(output$DATES, decreasing = TRUE ),]
  return(output)
}


get_rEER <- function(monthlyData, lagTildeMonth = 119, lagWindowMonth = 6,
                     lagMonth = 1, originalLag = FALSE){
  n <- length(monthlyData$DATES)
  
  monthlyData$lnEER <- NA
  monthlyData$lnEER[1:(n-lagMonth)] <- log(monthlyData$rEER[1:(n-lagMonth)]) - log(monthlyData$rEER[(1+lagMonth):n])
  
  monthlyData$auxSDrEER <- NA
  monthlyData$lnEER_tilde <- NA
  for(k in 1:(n-lagTildeMonth)){
    monthlyData$auxSDrEER[k] <- sd(monthlyData$lnEER [k: (k+lagTildeMonth) ])
  }
  if(!originalLag){
    nlnEER <- get_min_NA(monthlyData$lnEER)-1
    monthlyData$auxSDrEER[(nlnEER-lagTildeMonth+1):nlnEER] <- sd(monthlyData$lnEER [(nlnEER-lagTildeMonth+1):nlnEER])
  }
  monthlyData$lnEER_tilde <- monthlyData$lnEER/monthlyData$auxSDrEER
  
  monthlyData$VEER <- abs(monthlyData$lnEER_tilde)
  
  monthlyData$CUMUL <- NA
  monthlyData$CUMUL[1:(n-lagWindowMonth)] <- abs(monthlyData$rEER[1:(n-lagWindowMonth)] - monthlyData$rEER[(1+lagWindowMonth):n])
  return(monthlyData)
}

get_min_index <- function(iData){
  aux <- which(is.na(iData[,-1])) %% (nrow(iData))
  aux <- aux[aux!=0]
  aux <- min(aux)-1
  return(aux)
}


corr_EWMA <- function(I1, I2, lambda = 0.85, timeFrame = 120){
  sigma_12 <- cov(I1[1:timeFrame],I2[1:timeFrame])
  sigma_11 <- cov(I1[1:timeFrame],I1[1:timeFrame])
  sigma_22 <- cov(I2[1:timeFrame],I2[1:timeFrame])
  corr_12 <- sigma_12/(sigma_11*sigma_22)^.5
  corr <- c(corr_12)
  for(k in 2:length(I1)){
    sigma_12 <- lambda * sigma_12 + (1-lambda) * (I1[k] - .5) * (I2[k] - .5)
    sigma_11 <- lambda * sigma_11 + (1-lambda) * (I1[k] - .5) * (I1[k] - .5)
    sigma_22 <- lambda * sigma_22 + (1-lambda) * (I2[k] - .5) * (I2[k] - .5)
    corr_12 <- sigma_12/(sigma_11*sigma_22)^.5
    corr <- append(corr, corr_12)
  }
  return(corr)
}


corr_EWMA <- function(I1, I2, lambda = 0.85, timeFrame = 120){
  sigma_12 <- cov(I1[1:timeFrame],I2[1:timeFrame])
  sigma_11 <- cov(I1[1:timeFrame],I1[1:timeFrame])
  sigma_22 <- cov(I2[1:timeFrame],I2[1:timeFrame])
  corr_12 <- sigma_12/(sigma_11*sigma_22)^.5
  corr <- c(corr_12)
  for(k in 2:length(I1)){
    sigma_12 <- lambda * sigma_12 + (1-lambda) * (I1[k] - .5) * (I2[k] - .5)
    sigma_11 <- lambda * sigma_11 + (1-lambda) * (I1[k] - .5) * (I1[k] - .5)
    sigma_22 <- lambda * sigma_22 + (1-lambda) * (I2[k] - .5) * (I2[k] - .5)
    corr_12 <- sigma_12/(sigma_11*sigma_22)^.5
    corr <- append(corr, corr_12)
  }
  return(corr)
}

corr_EWMA_pdf <- function(I1, I2, lambda = 0.85, timeFrame = 120){
  sigma_12 <- cov(I1[1:timeFrame],I2[1:timeFrame])
  sigma_11 <- cov(I1[1:timeFrame],I1[1:timeFrame])
  sigma_22 <- cov(I2[1:timeFrame],I2[1:timeFrame])
  corr_12 <- sigma_12/(sigma_11*sigma_22)^.5
  corr <- c(corr_12)
  m1 <- mean(I1)
  m2 <- mean(I2)
  for(k in 2:length(I1)){
    sigma_12 <- lambda * sigma_12 + (1-lambda) * (I1[k] - m1) * (I2[k] - m2)
    sigma_11 <- lambda * sigma_11 + (1-lambda) * (I1[k] - m1) * (I1[k] - m1)
    sigma_22 <- lambda * sigma_22 + (1-lambda) * (I2[k] - m2) * (I2[k] - m2)
    corr_12 <- sigma_12/(sigma_11*sigma_22)^.5
    corr <- append(corr, corr_12)
  }
  return(corr)
}

FCIS <- function(iData){
  cSTX_Ryr <- iData$cSTX_Ryr
  cSTX_EER <- iData$cSTX_EER
  cRyr_EER <- iData$cRyr_EER
  iSTX <- iData$iSTX
  iRyr <- iData$iRyr
  iEER <- iData$iEER
  
  fci <- c()
  I <- as.matrix(numeric(3))
  n <- nrow(iData)
  for(k in 1:n){
    C <- diag(3)
    C[1,2] <- cSTX_Ryr[k]
    C[2,1] <- cSTX_Ryr[k]
    C[1,3] <- cSTX_EER[k]
    C[3,1] <- cSTX_EER[k]
    C[2,3] <- cRyr_EER[k]
    C[3,2] <- cRyr_EER[k]
    I[1] <- iSTX[k]
    I[2] <- iRyr[k]
    I[3] <- iEER[k]
    aux <- t(I) %*% C %*% I
    fci <- append(fci, aux)
    # auxEigen <- eigen(C)
    # maxEig <- auxEigen$vectors[,1]
    # dim(maxEig) <- c(3,1)
    # troll <<- append(troll,sum(maxEig * I)/(norm(maxEig)*norm(I)))
  }
  return(fci)
}


FCIS_2 <- function(iData){
  rho_1 <- iData[,1]
  rho_2 <- iData[,2]
  rho_3 <- iData[,3]
  idx1 <- iData[,3+1]
  idx2 <- iData[,3+2]
  idx3 <- iData[,3+3]
  
  fci <- c()
  I <- as.matrix(numeric(3))
  n <- nrow(iData)
  for(k in 1:n){
    C <- diag(3)
    C[1,2] <- rho_1[k]
    C[2,1] <- rho_1[k]
    C[1,3] <- rho_2[k]
    C[3,1] <- rho_2[k]
    C[2,3] <- rho_3[k]
    C[3,2] <- rho_3[k]
    I[1] <- idx1[k]
    I[2] <- idx2[k]
    I[3] <- idx3[k]
    aux <- t(I) %*% C %*% I
    fci <- append(fci, aux)
    # auxEigen <- eigen(C)
    # maxEig <- auxEigen$vectors[,1]
    # dim(maxEig) <- c(3,1)
    # troll <<- append(troll,sum(maxEig * I)/(norm(maxEig)*norm(I)))
  }
  return(fci)
}



FCIS_alt <- function(dataFrame,
                      lambda = 0.85,
                      timeFrame = 120){
  
  indices <- colnames(dataFrame)
  indices <- indices[indices!="DATES"]
  m <- length(indices)
  
  I <- as.matrix(numeric(m))
  n <- nrow(dataFrame)
  
  C <- data.frame(DATES = dataFrame$DATES)
  
  acc <- 1
  
  
  for(k1 in 1:m){
    for(k2 in k1:(m-1)){
      colTag <- paste0("C_",toString(k1),"_",toString(k2))
      C[[colTag]] <- NA
      C
    }
  }
  
  
  for(k in 2:n){
    for(k1 in 1:m){
      for(k2 in k1:(m-1)){
        
      }
    }
  }
}




#Java setup for r console

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}


#Visualization code

basic_range_plot <- function(dates, minFCIS, maxFCIS, FCIS){
  plot(dates,maxFCIS, type="l",bty="L", xlab = "", ylab="FCIS" ,col=rgb(1,1,1))
  polygon(c(dates,rev(dates)),c(minFCIS,rev(maxFCIS)),col=rgb(.8,.8,.8), border=rgb(1,1,1))
  points(dates,FCIS,type="l",col=rgb(0,0,1))
}


source("fci_standardizations.R")


###################Auxiliary functions

non_NA_max <- function(x)   ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
non_NA_min <- function(x)   ifelse( !all(is.na(x)), min(x, na.rm=T), NA)
non_NA_wav <- function(x,w) ifelse( !all(is.na(x)), sum(x[!is.na(x)]*w[!is.na(x)])/sum(w[!is.na(x)]), NA)

get_min_1value <- function(xData){
  for(k in 1:nrow(xData)){
    flag = ifelse( !all(is.na(xData[k,])), 1, NA)
    if(is.na(flag)){
      return(k-1)
    }
  }
}

my_save <- function(variable, path, stamp){
  fileName <- paste(path,"/",variable,"_",stamp,".RData",sep="")
  save(list = c(variable),file = fileName)
}

winsorize <- function(winsor_values, sigmas = 3){
  s = sd  (winsor_values, na.rm = TRUE)
  m = mean(winsor_values, na.rm = TRUE)
  threshold = sigmas * s
  idx <- abs(winsor_values-m)>threshold
  idx[is.na(idx)] = FALSE
  winsor_values[idx] = m + sign(winsor_values[idx]-m)*threshold
  return(winsor_values)
}