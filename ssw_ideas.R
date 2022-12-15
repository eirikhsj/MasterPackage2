
#Import data

library(data.table)
library(mgcv)
ssw_mean = fread('R/dt_zonal_era.csv')
ssw_mean[,hour:= hour +1] #Changing to hours 1-24, to be compatible with ERA-temperature

mon =c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")


ssw_mean[,date:= as.Date(paste0(year,'-',mon[month], '-',day))] #If I need it

load('R/PC_ERA_79_92.Rda')
ERA = PC_ERA_79_92$dt_test

#Merge data
ERA_SSW = merge(ERA, ssw_mean, by_x = c('date','hour'), by_y = c('date','hour'))

y = 2000
plot(ERA_SSW[year==y & hour == 6,(u_wind-mean(u_wind))/(sd(u_wind))], type = 'l', col = 'blue', ylim = c(-2.5, 2.5))
lines(ERA_SSW[year==y &hour == 6,(PC1-mean(PC1))/(sd(PC1))], col = 'red')

#model
ssw_mod0 = gam(PC1~ s(u_wind), data = ERA_SSW)
ssw_mod1 = gam(PC1~ s(u_wind) + month, data = ERA_SSW)
ssw_mod2 = gam(PC1~ s(u_wind) + month + hour, data = ERA_SSW)

plot(predict(ssw_mod0, newdata=ERA_SSW[year == 2013]), type = 'l', ylim = c(-300, 300))
lines(ERA_SSW[year == 2013,PC1], col = 'red')

ssw_mean[,day_mean:= mean(u_wind), keyby = date]
ssw_mean[,mean_diff_24:= day_mean - shift(day_mean, 24), keyby = date]
ssw_mean[, mean_shift:=shift(day_mean, n = 24),by = day]
ssw_mean[, mean_shift:=day_mean - shift(day_mean, n = 1:10),by = day]

plot(ssw_mean$u_wind, col = ifelse(ssw_mean$mean_shift < (-5), 'blue', 'red'), cex = 0.2)

ssw_mean[,diff6:= u_wind - shift(u_wind, 6)]

ssw_mean[,diff12:= u_wind - shift(u_wind, 12)]
ssw_mean[,diff24:= u_wind - shift(u_wind, 24)]
ssw_mean[,diff48:= u_wind - shift(u_wind, 48)]
ssw_mean[,diff72:= u_wind - shift(u_wind, 72)]
ssw_mean[,diff96:= u_wind - shift(u_wind, 96)]
ssw_mean[,diff120:= u_wind - shift(u_wind, 120)]
plot(ssw_mean[1:3000,u_wind], cex = 0.1, type = 'l', ylim = c(-40, 70))
lines(ssw_mean[1:3000,diff120], cex = 0.1, col = 'green')
lines(ssw_mean[1:3000,diff96], cex = 0.1, col = 'blue')
lines(ssw_mean[1:3000,diff72], cex = 0.1, col = 'red')
