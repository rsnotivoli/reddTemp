#'Fine mean monthly RV
#'
#'This function predict fine mean monthly RV
#'@param n index
#'@param dat matrix of maximum or minimum temperature of a specific month
#'@param neibs matrix of neighbouring stations

.pred_fine_mean <- function(n, dat, neibs){
  s <- dat[n, ]
  dat <- dat[-c(n), ]
  dat <- dat[which(!is.na(dat$Tmean)),] #select only stations with data
  
  d <- neibs[, which(s[1] == colnames(neibs))]
  m <- match(d, as.character(dat[,1]))
  if(length(which(is.na(m))) > 0) m <- m[-c(which(is.na(m)))]
  nn <- dat[m[1:15], ] #nearest neib
  w <- which(is.na(nn$ID))
  if(length(w) > 0) nn <- nn[-c(w), ]
  nn <- rbind(nn, s)
  nn[nrow(nn), 7] <- nn[nrow(nn), 9] #includes mon. mean estimate of candidate
  mod <- suppressWarnings(glm(Tmean ~ ALT + X + Y + DCOAST, 
                              data = nn, family = gaussian))
  res <- round(predict(mod, newdata = s, type = 'response'), 1)
  return(res)
}