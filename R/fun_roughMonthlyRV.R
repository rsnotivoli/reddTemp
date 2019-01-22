#'Rough monthly RV
#'
#'This function computes the Rough monthly Reference Values (RV)
#'@param x matrix of maximum or minimum temperatures. Columns are stations and rows are days.
#'@param datess vector of daily dates

roughMonthlyRV <- function(x, datess){
  
  #monthly mean
  mon_mean <- aggregate(x, list(month = substr(datess, 1, 7)), FUN = 'mean')
  mon_mean <- melt(mon_mean)
  mon_mean$year <- substr(mon_mean$month, 1, 4)
  mon_mean$month <- as.numeric(substr(mon_mean$month, 6, 7))
  names(mon_mean)[2] <- 'ID'
  geo <- sts[,c('ID', 'X', 'Y', 'ALT', 'DCOAST')]
  mon_mean <- merge(mon_mean, geo, by = 'ID')
  mon_mean$Tmean_pre <- NA
  
  #monthly sd
  mon_sd <- aggregate(x, list(month = substr(datess, 1, 7)), FUN = 'sd')
  mon_sd <- melt(mon_sd)
  mon_sd$year <- as.numeric(substr(mon_sd$month, 1, 4))
  mon_sd$month <- as.numeric(substr(mon_sd$month, 6, 7))
  names(mon_sd)[2] <- 'ID'
  geo <- sts[,c('ID', 'X', 'Y', 'ALT', 'DCOAST')]
  mon_sd <- merge(mon_sd, geo, by = 'ID')
  mon_sd$Tsd_pre <- NA
  
  #monthly models
  #
  mon_mean_c <- mon_mean[-c(which(is.na(mon_mean$value))),]
  mon_sd_c <- mon_sd[-c(which(is.na(mon_sd$value))),]
  
  for(i in 1:12){ ##!! Paralelizar esto
    #model mean
    sub <- mon_mean_c[which(mon_mean_c$month == i),]
    mod <- suppressWarnings(glmer(value ~ ALT + X + Y + DCOAST + (1 | year), 
                                  data = sub, family = 'gaussian'))
    w <- which(mon_mean$month == i)
    mon_mean$Tmean_pre[w] <- round(predict(mod, newdata = mon_mean[w,], type = 'response'), 1)
    
    #model sd
    sub <- mon_sd_c[which(mon_sd_c$month == i),]
    #we use integer to make it faster
    sub$value <- as.integer(round(sub$value * 1000, 0))
    mod <- suppressWarnings(glmer(value ~ ALT + X + Y + DCOAST + (1 | year), 
                                  data = sub, family = poisson(link = "log"), 
                                  nAGQ = 0,
                                  control = glmerControl(
                                    optimizer = "nloptwrap")))
    w <- which(mon_sd$month == i)
    mon_sd$Tsd_pre[w] <- round(predict(mod, newdata = mon_sd[w,], 
                                       type = 'response') / 1000, 1)
  }
  
  return(data.frame(ID = mon_mean$ID, 
                    month = paste(mon_mean$year, 
                                  formatC(mon_mean$month, 
                                          width = 2, flag = '0'), sep ='-'), 
                    X = mon_mean$X,
                    Y = mon_mean$Y,
                    ALT = mon_mean$ALT,
                    DCOAST = mon_mean$DCOAST,
                    Tmean = round(mon_mean$value, 1),
                    Tsd = round(mon_sd$value, 1),
                    Tmean_pre = mon_mean$Tmean_pre, 
                    Tsd_pre = mon_sd$Tsd_pre,
                    stringsAsFactors = F))
}
