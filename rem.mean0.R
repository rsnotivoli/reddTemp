#'Rem. mean
#'
#'This function removes complete months with standard mean in temperature difference == 0
#'@param i index of the column
#'@param dtmean mean of temperature differences
#'@param diff.temp matrix with TMAX-TMIN
#'@param datess vector of daily dates
#'@param meta logical. indicates wether metadata is written to file or not.

.rem.mean0 <- function(i, dtmean, diff.temp, datess, meta){
  x <- diff.temp[, i]
  w <- which(dtmean[, i + 1] == 0)
  if (length(w) > 0){
    x[substr(datess, 1, 7)%in%dtmean[w, 1]] <- NA
    k <- cbind(datess[substr(datess, 1, 7)%in%dtmean[w, 1]],
               colnames(diff.temp)[i],
               NA, NA, x[substr(datess, 1, 7)%in%dtmean[w, 1]],
               'diff.mean == 0')
    ww <- which(is.na(k[, 2]) & is.na(k[, 3]) & is.na(k[, 4]))
    if(length(ww) > 0) k <- k[-c(ww), ]
    meta <- rbind(meta, k)
  }
  return(x)
}