#'Consisteny check
#'
#'This function checks the consistency of maximum and minimum temperature in a dataset. Maximum must be higher than minimum, otherwise, value is removed and metadata is written.
#'@param i index of the column
#'@param diff.temp matrix with TMAX-TMIN
#'@param tmax matrix of maximum temperatures. Columns are stations and rows are days.
#'@param tmin matrix of minimum temperatures. Columns are stations and rows are days.
#'@param op numeric. 1 is maximum and 2 is minimum temperature
#'@param meta logical. indicates wether metadata is written to file or not.

.consis.check <- function(i, diff.temp, tmax, tmin, op, meta){
  w <- which(diff.temp[, i] == 1)
  if(length(w) > 0) {
    if(op == 1){
      x <- tmax[, i]
      x[w] <- tmin[w, i]
      meta <- rbind(meta, cbind(datess[w], colnames(tmax)[i], x[w], 
                                NA, NA, 'inconsistent'))
    } else if(op == 2){
      x <- tmin[, i]
      x[w] <- tmax[w, i]
      meta <- rbind(meta, cbind(datess[w], colnames(tmin)[i], 
                                NA, x[w], NA, 'inconsistent'))
    }
    return(x)
  } else {if(op == 1) return(tmax[, i]) else return(tmin[, i])}
}