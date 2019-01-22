#'Daily predictions for individual stations
#'
#'This function estimates new daily values of temperature for individual stations
#'@param n index
#'@param dat matrix of temperature data for a specific month
#'@param nams vector of names
#'@param geo data.frame with the geographic information of neighbouring stations
#'@param neibs matrix of neighbouring stations
#'@param finedata FineMonthlyRV of a specific month

.pred_daily <- function(n, dat, nams, geo, neibs, finedata){
  #select only stations with data
  dat <- setNames(dat, nams)
  dat <- dat[-c(n)]
  w <- which(!is.na(dat))
  if(length(w) == 0) res <- NA else{
    dat <- dat[w]
    d <- neibs[, which(geo[n, 1] == colnames(neibs))]
    m <- match(d, as.character(nams))
    if(length(which(is.na(m))) > 0) m <- m[-c(which(is.na(m)))]
    nn <- dat[m] #nearest neib
    w <- which(is.na(nn))
    if(length(w) > 0) nn <- nn[-c(w)]
    if(length(nn) > 15) nn <- nn[1:15]
    geoneib <- geo[match(names(nn), geo$ID), ]
    #corresponding fineMonthlyRV mean and sd
    finemean <- finedata$Tmean_pre2[which(geo[n, 1] == finedata$ID)]
    nn <- c(nn, finemean)
    df <- data.frame(rbind(geoneib, geo[n, ]), nn)
    mod <- suppressWarnings(glm(nn ~ ALT + X + Y + DCOAST, data = df,
                                family = gaussian))
    ndf <- data.frame(geo[n, ], nn = NA)
    res <- round(predict(mod, newdata = ndf, type = 'response'), 1)
  }
  return(res)
}