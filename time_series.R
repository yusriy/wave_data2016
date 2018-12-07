path_fig <- file.path('/Users/tansihwa/Desktop/FYP/figs/WT_time_series.jpg')
jpeg(file=path_fig,width=25,height=17,res=400, units = 'cm')
par(family='Times', mfrow = c(3,1))
###For CO2_FLUX
par(mar = c(2,4,2,4))
plot(by_monthly_grp_data$month,by_monthly_grp_data$co2_flux,lwd=2,type='l',ylab='',col='black',
     xlab='', xaxt = 'n')
mtext(expression(paste('F'['CO']['2'])),side = 2,line=1.8)
###For SWH
par(mar = c(2,4,2,4))
plot(by_monthly_grp_data$month,by_monthly_grp_data$swh,lwd=2,type='l',ylab='',col='black',
     xlab='', xaxt = 'n')

mtext(expression(paste('H'['s'])),side = 2,line=1.8)
###For u*
par(mar = c(2,4,2,4))
plot(by_monthly_grp_data$month,by_monthly_grp_data$u.,lwd=2,type='l',ylab='',col='black',
     xlab='', xaxt = 'n')

mtext(expression(paste('u*')),side = 2,line=1.8)

mtext(side = 1, 'Month', line = 2.5, cex = 1.1)

axis(side = 1, at = c(as.POSIXct('2015 12 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 02 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 04 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 06 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 08 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 10 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 12 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 02 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 04 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 06 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 08 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 10 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 12 15 00:00:00', format = '%Y %m %d %H:%M:%S')),
     labels = c('Dec15', 'Feb16', 'Apr16', 'Jun16','Aug16', 'Oct16','Dec16',
                'Feb17', 'Apr17', 'Jun17', 'Aug17', 'Oct17', 'Dec17'), cex.axis = 1)

dev.off()

#####K1 & K2
##Precipitation
path_fig <- file.path('/Users/tansihwa/Desktop/FYP/figs/K.png/')
jpeg(file=path_fig,width=25,height=17,res=1000, units = 'cm')
par(family='Times', mfrow = c(2,1))
par(mar = c(2,4,2,4))
plot(by_monthly_grp_data$month,by_monthly_grp_data$k1,lwd=2,type='l',ylab='',col='black',
     xlab='', xaxt = 'n')
mtext(expression(paste('k'['1'])),side = 2,line=1.8)
par(mar = c(2,4,2,4))

plot(by_monthly_grp_data$month,by_monthly_grp_data$k2,lwd=2,type='l',ylab='',col='black',
     xlab='', xaxt = 'n')
mtext(expression(paste('k'['2'])),side = 2,line=1.8)
mtext(side = 1, 'Month', line = 2.5, cex = 1.1)
axis(side = 1, at = c(as.POSIXct('2015 12 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 02 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 04 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 06 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 08 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 10 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2016 12 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 02 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 04 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 06 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 08 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 10 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                      as.POSIXct('2017 12 15 00:00:00', format = '%Y %m %d %H:%M:%S')),
     labels = c('Dec15', 'Feb16', 'Apr16', 'Jun16','Aug16', 'Oct16','Dec16',
                'Feb17', 'Apr17', 'Jun17', 'Aug17', 'Oct17', 'Dec17'), cex.axis = 1)
dev.off()

  