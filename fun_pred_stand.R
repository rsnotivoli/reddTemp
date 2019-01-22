#'Standardization of daily estimates
#'
#'This function applies a stadardization to daily estimates
#'@param p index
#'@param preds daily estimates
#'@param finedata FineMonthlyRV of a specific month
#'@param mpre mean of preds
#'@param sdpre standard deviation of preds

.pred_stand <- function(p, preds, finedata, mpre, sdpre){
  (((preds[p] - mpre) / sdpre) * finedata$Tsd_pre2[p]) + finedata$Tmean_pre2[p]
}