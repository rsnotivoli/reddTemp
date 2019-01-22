#'Rem. sd
#'
#'This function removes complete months with standard deviation == 0
#'@param i index of the column
#'@param txsd matrix of standard deviations of maximum temperatures
#'@param tnsd matrix of standard deviations of minimum temperatures
#'@param dtsd matrix of standard deviations of temperature differences
#'@param tmax matrix of maximum temperatures. Columns are stations and rows are days.
#'@param tmin matrix of minimum temperatures. Columns are stations and rows are days.
#'@param diff.temp matrix with TMAX-TMIN
#'@param datess vector of daily dates
#'@param op numeric. 1 is maximum, 2 is minimum temperature and 3 is temperature differences
#'@param meta logical. indicates wether metadata is written to file or not.

.rem.sd0 <- function(i, txsd, tnsd, dtsd, tmax, tmin, diff.temp, datess, op, meta){
  if(op == 1){ #tmax
    x <- tmax[, i]
    w <- which(txsd[, i + 1] == 0)
    if (length(w) > 0){
      x[substr(datess, 1, 7)%in%txsd[w, 1]] <- NA
      k <- cbind(datess[substr(datess, 1, 7)%in%txsd[w, 1]],
                 colnames(tmax)[i],
                 x[substr(datess, 1, 7)%in%txsd[w, 1]], NA, NA,
                 'sd == 0')
      ww <- which(is.na(k[, 2]) & is.na(k[, 3]) & is.na(k[, 4]))
      if(length(ww) > 0) k <- k[-c(ww),]
      meta <- rbind(meta, k)}
  } else if(op == 2){ #tmin
    x <- tmin[, i]
    w <- which(tnsd[, i + 1] == 0)
    if (length(w) > 0){
      x[substr(datess, 1, 7)%in%tnsd[w, 1]] <- NA
      k <- cbind(datess[substr(datess, 1, 7)%in%tnsd[w, 1]],
                 colnames(tmin)[i],
                 NA, x[substr(datess, 1, 7)%in%tnsd[w, 1]], NA,
                 'sd == 0')
      ww <- which(is.na(k[, 2]) & is.na(k[, 3]) & is.na(k[, 4]))
      if(length(ww) > 0) k <- k[-c(ww), ]
      meta <- rbind(meta, k)}
  } else if(op == 3){ #diff.temp
    x <- diff.temp[, i]
    w <- which(dtsd[, i + 1] == 0)
    if (length(w) > 0){
      x[substr(datess, 1, 7)%in%dtsd[w, 1]] <- NA
      k <- cbind(datess[substr(datess, 1, 7)%in%dtsd[w, 1]],
                 colnames(diff.temp)[i],
                 NA, NA, x[substr(datess, 1, 7)%in%dtsd[w, 1]],
                 'sd == 0')
      ww <- which(is.na(k[, 2]) & is.na(k[, 3]) & is.na(k[, 4]))
      if(length(ww) > 0) k <- k[-c(ww), ]
      meta <- rbind(meta, k)}
  }
  return(x)
}