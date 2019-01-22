#'Initial quality control
#'
#'This function initialise the quality control of daily temperature. Calls the others functions (consis.check, rem.mean0, rem.min.vals, rem.sd).
#'@param tmax matrix of maximum temperatures. Columns are stations and rows are days.
#'@param tmin matrix of minimum temperatures. Columns are stations and rows are days.
#'@param txmax numeric threshold. Maximum daily temperature for tmax.
#'@param txmin numeric threshold. Minimum daily temperature for tmax.
#'@param tnmax numeric threshold. Maximum daily temperature for tmin.
#'@param tnmin numeric threshold. Minimum daily temperature for tmin.
#'@param minvals threshold of minimum number of data needed to keep the whole month.
#'@param datess vector of daily dates


iniqc <- function(tmin, tmax, txmax = 500, txmin = -300, tnmax = 400, tnmin = -350,
         minvals = 3, datess){
  
  ################################
  #prepare the data
  ################################
  nams <- colnames(tmax)
  #we order the columns of both matrices to be similar
  tmin <- tmin[, match(nams, colnames(tmin))]
  
  #we set a data.frame to register the metadata
  meta <- matrix(ncol = 6, nrow = 0)
  nams.meta <- c('date', 'id', 'tmax', 'tmin', 'diff', 'cause')
  
  ################################
  #1.1 Consistency check
  ################################
  print('Consistency check')
  diff.temp <- tmax - tmin
  diff.temp[diff.temp >= 0] <- NA
  diff.temp[diff.temp < 0] <- 1
  
  #tmax without inconsistencies
  tmax <- sapply(1:ncol(tmax),FUN = .consis.check, diff.temp, tmax, 
                 tmin, op = 1, meta)
  #tmin without inconsistencies
  tmin <- sapply(1:ncol(tmin),FUN = .consis.check, diff.temp, tmax, 
                 tmin, op = 2, meta)
  rm(diff.temp)
  colnames(tmax) <- colnames(tmin) <- nams
  
  ################################
  #1.2 Out of range values
  ################################
  print('Out of range values')
  #This removes values based on initial settings
  #first, metadata are filled
  m1 <- sapply(1:ncol(tmax),
               function(x){
                 w <- which(tmax[, x] >= txmax)
                 if(length(w) > 0) 
                   return(cbind(datess[w], colnames(tmax)[x], tmax[, x][w], 
                                NA, NA, 'over maximum'))}
               )
  m2 <- sapply(1:ncol(tmax),
               function(x){
                 w <- which(tmax[, x] <= txmin)
                 if(length(w) > 0) 
                   return(cbind(datess[w], colnames(tmax)[x], tmax[, x][w],
                                NA, NA, 'under minimum'))}
               )
  m3 <- sapply(1:ncol(tmin),
               function(x){
                 w <- which(tmin[, x] >= tnmax)
                 if(length(w) > 0) 
                   return(cbind(datess[w], colnames(tmin)[x],
                                NA, tmin[, x][w], NA, 'over maximum'))}
               )
  m4 <- sapply(1:ncol(tmin),
               function(x){
                 w <- which(tmin[, x] <= tnmin)
                 if(length(w) > 0) 
                   return(cbind(datess[w], colnames(tmin)[x],
                                NA, tmin[, x][w], NA, 'under minimum'))}
               )
  meta <- rbind(meta, 
                if(length(m1) > 0) do.call('rbind',m1),
                if(length(m2)>0) do.call('rbind',m2),
                if(length(m3)>0) do.call('rbind',m3),
                if(length(m4)>0) do.call('rbind',m4)
                )
  #then, data is removed
  tmax[tmax >= txmax] <- NA
  tmax[tmax <= txmin] <- NA
  tmin[tmin >= tnmax] <- NA
  tmin[tmin <= tnmin] <- NA
  
  ################################
  #1.3 Removal of complete incorrect months
  ################################
  print('Incomplete and incorrect months')
  #removes complete cases in a month with less than minimum values set at the beggining
  #first, number of data is summarized
  fwna <- function(x){length(which(!is.na(x)))}
  txcount <- aggregate(tmax, by = list(substr(datess, 1, 7)),
                       FUN = fwna)
  tncount <- aggregate(tmin, by = list(substr(datess, 1, 7)),
                       FUN = fwna)
  
  
  #tmax without low number of values
  tmax <- sapply(1:ncol(tmax),
                 FUN = .rem.min.vals, txcount, tncount, 
                 tmax, tmin, datess, op = 1, meta, minvals)
  #tmin without low number of values
  tmin <- sapply(1:ncol(tmin),
                 FUN = .rem.min.vals, txcount, tncount, 
                 tmax, tmin, datess, op = 2, meta, minvals)
  
  #removes complete cases in a month with standard deviation of 0
  txsd <- aggregate(tmax, by = list(substr(datess, 1, 7)),
                    FUN = sd, na.rm = T)
  tnsd <- aggregate(tmin, by = list(substr(datess, 1, 7)),
                    FUN = sd, na.rm = T)
  diff.temp <- tmax - tmin
  dtsd <- aggregate(diff.temp, by = list(substr(datess, 1, 7)),
                    FUN = sd, na.rm = T)
  
  #tmax without low number of values
  tmax <- sapply(1:ncol(tmax), FUN = .rem.sd0, txsd, tnsd, dtsd, tmax, tmin, 
                 diff.temp, datess, op = 1, meta)
  #tmin without low number of values
  tmin <- sapply(1:ncol(tmin), FUN = .rem.sd0, txsd, tnsd, dtsd, tmax, tmin, 
                 diff.temp, datess, op = 2, meta)
  #Also to the differences of temperatures
  diff.temp <- sapply(1:ncol(diff.temp), FUN = .rem.sd0, txsd, tnsd, dtsd, 
                      tmax, tmin, diff.temp, datess, op = 3, meta)

  #In addition to the standard deviations, removes complete cases in a month 
  #with an average of 0, only in diff.temp
  dtmean <- aggregate(diff.temp, by = list(substr(datess, 1, 7)),
                      FUN = mean, na.rm = T)
  
  diff.temp <- sapply(1:ncol(diff.temp), FUN = .rem.mean0, 
                      dtmean, diff.temp, datess, meta)
  colnames(tmax) <- colnames(tmin) <- colnames(diff.temp) <-nams
   
  if(nrow(meta)>0){
    colnames(meta) <- nams.meta
    write.table(meta, 'metaQC.txt', quote = F, 
                sep = '\t', row.names = F, na = '')
    }
  
  save(tmax, tmin, sts, file = 'data_iniqc.RData')
  
#End of initial QC
}
