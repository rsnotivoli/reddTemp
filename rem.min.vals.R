#'Rem. minimum values
#'
#'This function removes complete months with less than minimum number of data
#'@param i index of the column
#'@param txcount matrix with number of days with maximum temperature data per station
#'@param tncount matrix with number of days with minimum temperature data per station
#'@param tmax matrix of maximum temperatures. Columns are stations and rows are days.
#'@param tmin matrix of minimum temperatures. Columns are stations and rows are days.
#'@param datess vector of daily dates
#'@param op numeric. 1 is maximum and 2 is minimum temperature
#'@param meta logical. indicates wether metadata is written to file or not.
#'@param minvals threshold of minimum number of data needed to keep the whole month.


.rem.min.vals <- function(i, txcount, tncount, tmax, tmin, datess, op, meta, minvals){
  if(op == 1){ #tmax
    x <- tmax[,i]
    w <- which(txcount[, i + 1] < minvals & txcount[, i + 1] > 0)
    if (length(w) > 0){
      x[substr(datess, 1, 7)%in%txcount[w, 1]] <- NA
      k <- cbind(datess[substr(datess, 1, 7)%in%txcount[w, 1]],
                 colnames(tmax)[i],
                 x[substr(datess, 1, 7)%in%txcount[w, 1]], NA, NA,
                 'less values than required')
      ww <- which(is.na(k[, 2]) & is.na(k[, 3]))
      if(length(ww) > 0) k <- k[-c(ww), ]
      meta <- rbind(meta, k)}
  } else if(op == 2){ #tmin
    x <- tmin[,i]
    w <- which(tncount[, i + 1] < minvals & tncount[, i + 1] > 0)
    if (length(w) > 0){
      x[substr(datess, 1, 7)%in%tncount[w, 1]] <- NA
      k <- cbind(datess[substr(datess, 1, 7)%in%tncount[w, 1]],
                 colnames(tmin)[i],
                 NA, x[substr(datess, 1, 7)%in%tncount[w, 1]], NA,
                 'less values than required')
      ww <- which(is.na(k[, 2]) & is.na(k[, 3]))
      if(length(ww) > 0) k <- k[-c(ww), ]
      meta <- rbind(meta, k)}
  }
  return(x)
}