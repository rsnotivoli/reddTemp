
#load raw dataset
load('data_test.RData')
#vector of dates coincident with the number of days in dataset
datess <- seq.Date(as.Date('1970-01-01'), as.Date('2014-12-31'), by = 'day')

#initial QC
iniqc(tmin, tmax, txmax = 500, txmin = -300, tnmax = 400, tnmin = -350,
      minvals = 3, datess)

#load of quality-controlled data from previous step
load('data_iniqc.RData')

#ALL of these packages are mandatory
library(lme4) #performs GLMM
library(fields) #required for distances computing
library(reshape2) #re-formatting of data
library(multiApply) #multicore work
library(future) #checks the available cores

#computes distances
distanc <- .distances(sts)

#computes RoughMonthlyRV
rough <- roughMonthlyRV(tmax, datess)

#computes neighbouring stations
neibs <- apply(distanc, 1, sel_neib, nams = colnames(distanc)) #select 15 nearest stations

#computes RoughMonthlyRV
fine <- fineMonthlyRV(rough, datess, neibs)

#computes daily estimates
daily_estimates <- dailyRV(tmax, fine, datess, neibs)



