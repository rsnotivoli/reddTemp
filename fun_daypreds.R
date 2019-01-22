#'Daily predictions for all stations and days in one month
#'
#'This function estimates new daily values of temperature for individual stations in one month
#'@param sub data of an individual station for a specific day and month
#'@param fine FineMonthlyRV of a specific month
#'@param datess vector of daily dates
#'@param nams vector of names
#'@param geo data.frame with the geographic information of neighbouring stations
#'@param neibs matrix of neighbouring stations

.daypreds <- function(sub, fine, datess, nams, geo, neibs){
  finedata <- fine[which(substr(datess[sub[1]], 1, 7) == fine$month), ]
  sub <- sub[2:length(sub)]
  #compute predictions
  preds <- sapply(1:length(sub), .pred_daily, dat = sub, nams, geo, neibs, finedata)
  #standardized predictions
  preds <- sapply(1:length(preds), .pred_stand, preds = preds, finedata = finedata, 
                  mpre = mean(preds), sdpre = sd(preds))
  round(preds)
}