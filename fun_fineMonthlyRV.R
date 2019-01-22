#'Fine monthly RV
#'
#'This function intialises the FineMonthlyRV creation
#'@param rough Rough Monthly RV created in a previous step
#'@param datess vector of daily dates
#'@param neibs matrix of neighbouring stations

fineMonthlyRV <- function(rough, datess, neibs){
  print('Running Fine Monthly RV')
  fine <- rough
  fine$Tmean_pre2 <- NA
  fine$Tsd_pre2 <- NA
  
  #run predictions by month
  mon <- unique(substr(datess, 1, 7))
  pb <- txtProgressBar(min = 0, max = length(mon), initial = 0, 
                       char = "=", style = 3,
                       width = getOption('width') * 0.5)
  for(i in 1:length(mon)){
    setTxtProgressBar(pb, i)
    w <- which(mon[i] == fine$month)
    sub <- fine[w, ]
    fine$Tmean_pre2[w] <- Apply(data = as.matrix(1:nrow(sub)), margins = 1, fun = .pred_fine_mean, dat = sub, neibs = neibs, ncores = availableCores()) [[1]]
    fine$Tsd_pre2[w] <- Apply(data = as.matrix(1:nrow(sub)), margins = 1, fun = .pred_fine_sd, dat = sub, neibs = neibs, ncores = availableCores()) [[1]]
  }
  return(fine)
}  