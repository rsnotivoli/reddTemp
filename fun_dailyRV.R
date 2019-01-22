#'Daily estimates
#'
#'This function initialises the daily prediction of temperatures based on Rough and Fine Monthly RV
#'@param x matrix of maximum or minimum temperatures
#'@param fine FineMonthlyRV dataset
#'@param datess vector of daily dates
#'@param neibs matrix of neighbouring stations

dailyRV <- function(x, fine, datess, neibs){
  #stations' info
  geo <- sts[,c('ID', 'X', 'Y', 'ALT', 'DCOAST')]
  nams <- colnames(x)
  #run predictions by day
  x <- cbind(1:nrow(x), x) #adds an index for dates
  #non-parallelised
  # k <- t(apply(xx[1:100, ], 1, .daypreds, fine, datess, nams, geo, neibs, pb))
  #parallelisation using snowfall (requires initialisation)
  # sfExport('x', '.pred_daily', '.pred_stand', '.daypreds')
  # est <- t(sfApply(xx[1:100, ], 1, .daypreds, fine, datess, nams, geo, neibs))
  #parallelised with multiApply
  est <- t(Apply(data = x, margins = 1, fun = .daypreds, fine = fine, 
                 datess = datess, nams = nams, geo = geo, neibs = neibs, ncores = availableCores())[[1]])
  colnames(est) <- nams
  return(est)
}