#fig for k vs swh
path_fig <- file.path('figs/k3_swh.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif', mfrow = c(3,2),
    mar = c(3.9, 3.9, 1, 1))

plot(c$swh, c$k1*3600*100, pch=16, col='darkblue',xlab= '', ylab='')
lm6 <- lm(c$k1*3600*100 ~ c$swh)
abline(lm6,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['in-situ'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(c$swh, c$kt*3600*100, pch=16, col='darkblue',xlab= '', ylab='')
lm1 <- lm(c$kt*3600*100 ~ c$swh)
abline(lm1,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['theory'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(c$swh, c$k3, pch=16, col='darkblue',xlab= '', ylab='')
lm2 <- lm(c$k3 ~ c$swh)
abline(lm2,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['N2000'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(c$swh, c$k4, pch=16, col='darkblue',xlab= '', ylab='')
lm3 <- lm(c$k4 ~ c$swh)
abline(lm3,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['W2014'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(c$swh, c$k5, pch=16, col='darkblue',xlab= '', ylab='')
lm4 <- lm(c$k5 ~ c$swh)
abline(lm4,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['MG2001'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)


plot(c$swh, c$k6, pch=16, col='darkblue',xlab= '', ylab='')
lm5 <- lm(c$k6 ~ c$swh)
abline(lm5,col='red', lwd=3)
mtext(expression(paste('k'['WMG1999'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)
dev.off()


#fig for k vs u
path_fig <- file.path('figs/k1_u.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif', mfrow = c(3,2),
    mar = c(3.9, 3.9, 1, 1))

plot(c$WS_site, c$k1*100*3600, pch=16, col='darkblue',xlab= '', ylab='')
lm6 <- lm(c$k1*3600*100 ~ c$WS_site)
abline(lm6,col='red', lwd=3)


plot(c$WS_site, c$kt*100*3600, pch=16, col='darkblue',xlab= '', ylab='')
lm1 <- lm(c$kt*3600*100 ~ c$WS_site)
abline(lm1,col='red', lwd=3)


plot(c$WS_site, c$k3, pch=16, col='darkblue',xlab= '', ylab='')
lm2 <- lm(c$k3 ~ c$WS_site)
abline(lm2,col='red', lwd=3)

plot(c$WS_site, c$k4, pch=16, col='darkblue',xlab= '', ylab='')
lm3 <- lm(c$k4 ~ c$WS_site)
abline(lm3,col='red', lwd=3)


plot(c$WS_site, c$k5, pch=16, col='darkblue',xlab= '', ylab='')
lm4 <- lm(c$k5 ~ c$WS_site)
abline(lm4,col='red', lwd=3)

plot(c$WS_site, c$k6, pch=16, col='darkblue',xlab= '', ylab='')
lm5 <- lm(c$k6 ~ c$WS_site)
abline(lm5,col='red', lwd=3)
minor.tick()


#fig for k vs u*
path_fig <- file.path('figs/k_u..jpg')
jpeg(file=path_fig,width=20,height=10,res=400, units = 'cm')
par(family='serif', mfrow = c(3,2),
    mar = c(3.9, 3.9, 1, 1))

plot(c$u., c$k1, pch=16, col='darkblue',xlab= '', ylab='')
lm6 <- lm(c$k1 ~ c$u.)
abline(lm6,col='red', lwd=3)



plot(c$u., c$kt, pch=16, col='darkblue',xlab= 'u*', ylab='k2')
lm1 <- lm(c$kt*3600*100 ~ c$u.)
abline(lm1,col='red', lwd=3)

plot(c$u., c$k3, pch=16, col='darkblue',xlab= 'u*', ylab='k3')
lm2 <- lm(c$k3 ~ c$u.)
abline(lm2,col='red', lwd=3)

plot(c$u., c$k4, pch=16, col='darkblue',xlab= 'u*', ylab='k4')
lm3 <- lm(c$k4 ~ c$u.)
abline(lm3,col='red', lwd=3)

plot(c$u., c$k5, pch=16, col='darkblue',xlab= 'u*', ylab='k5')
lm4 <- lm(c$k5 ~ c$u.)
abline(lm4,col='red', lwd=3)

plot(c$u., c$k6, pch=16, col='darkblue',xlab= 'u*', ylab='k6')
lm5 <- lm(c$k6 ~ c$u.)
abline(lm5,col='red', lwd=3)
dev.off()

plot(c$u., c$k1, pch=16, col='darkblue',xlab= 'u*', ylab='k1')
lm6 <- lm(c$k1*3600*100 ~ c$u.)
abline(lm6,col='red', lwd=3)
plot(c$WS_site, c$k1, pch=16, col='darkblue',xlab= 'u', ylab='k1')
lm6 <- lm(c$k1*3600*100 ~ c$WS_site)
abline(lm6,col='red', lwd=3)
plot(c$swh, c$k1, pch=16, col='darkblue',xlab= 'swh', ylab='k1')
lm6 <- lm(c$k1*3600*100 ~ c$swh)
abline(lm6,col='red', lwd=3)

summary(lm6)
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)

path_fig <- file.path('/Users/tansihwa/Desktop/FYP/figs/cor.jpg')
jpeg(file=path_fig,width=20,height=15,res=400, units = 'cm')
par(family='Times', mfrow = c(3,1))
par(mar = c(4,4,1,1))
plot(by_monthly_grp_data$SST,
     by_monthly_grp_data$co2_flux, pch = 10,
     xlab = '', ylab = '',
     xlim= c(28,33), ylim = c(-0.11,0.11), col = alpha('red',0.2), yaxt = 'n')

points(by_monthly_grp_data$SST,
       by_monthly_grp_data$co2_flux, pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-0.2,-0.1,0,0.1,0.2))
minor.tick()
mtext(expression(paste('F'['CO']['2'])),side = 2,line=2)
mtext(expression('T'['sat']),side = 1,line=2)
lmT <- lm(by_monthly_grp_data$co2_flux ~ 
            by_monthly_grp_data$SST)
abline(lmT, col = 'black', lwd = 3, lty = 2)
summary(lmT)



# CHL and CO2_flux
par(mar = c(4,4,1,1))
plot(c$swh,
     c$k1*100*3600, pch = 10,
     xlab = '', ylab = '',
     col = alpha('red',0.2), yaxt = 'n')
points(c$swh,
       c$k1*3600*100, pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-0.,-0.1,0,0.1,0.2))
minor.tick()
mtext(expression(paste('k'['in-situ'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2)
lmT <- lm(c$k1 ~ 
            c$swh)
abline(lmT, col = 'black', lwd = 3, lty = 2)
summary(lmT)


####test
library(Hmisc)
path_fig <- file.path('figs/testk_swh.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif', mfrow = c(3,2),
    mar = c(3.9, 3.9, 1, 1))

plot(c$swh, c$k1*3600*100, pch=16, col='darkblue',xlab= '', ylab='', ylim=c(0,40))
lm6 <- lm(c$k1*3600*100 ~ c$swh)
abline(lm6,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['in-situ'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(c$swh, c$kt*3600*100, pch=16, col='darkblue',xlab= '', ylab='')
lm1 <- lm(c$kt*3600*100 ~ c$swh)
abline(lm1,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['theory'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(c$swh, c$k3n, pch=16, col='darkblue',xlab= '', ylab='')
lm2 <- lm(c$k3n ~ c$swh)
abline(lm2,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['N2000'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(c$swh, c$k4n, pch=16, col='darkblue',xlab= '', ylab='')
lm3 <- lm(c$k4n ~ c$swh)
abline(lm3,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['W2014'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(c$swh, c$k5n, pch=16, col='darkblue',xlab= '', ylab='')
lm4 <- lm(c$k5n ~ c$swh)
abline(lm4,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['MG2001'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)


plot(c$swh, c$k6n, pch=16, col='darkblue',xlab= '', ylab='')
lm5 <- lm(c$k6n ~ c$swh)
abline(lm5,col='red', lwd=3)
mtext(expression(paste('k'['WMG1999'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)
dev.off()


#######
path_fig <- file.path('figs/kwee_swh.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif', mfrow = c(3,2),
    mar = c(3.9, 3.9, 1, 1))

plot(c$k1*3600*100,c$co2_flux, pch=16, col='darkblue',xlab= '', ylab='')
lm6 <- lm(c$co2_flux ~ c$k1*3600*100 )
abline(lm6,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['in-situ'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot( c$kt*3600*100, c$co2_flux, pch=16, col='darkblue',xlab= '', ylab='')
lm1 <- lm(c$co2_flux~ c$kt*3600*100 )
abline(lm1,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['theory'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot (c$k3, c$co2_flux, pch=16, col='darkblue',xlab= '', ylab='')
lm2 <- lm(c$co2_flux~ c$k3)
abline(lm2,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['N2000'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(c$k4, c$co2_flux, pch=16, col='darkblue',xlab= '', ylab='')
lm3 <- lm(c$co2_flux~ c$k4 )
abline(lm3,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['W2014'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot( c$k5, c$co2_flux, pch=16, col='darkblue',xlab= '', ylab='')
lm4 <- lm(c$co2_flux~ c$k5 )
abline(lm4,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['MG2001'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)


plot(c$k6,c$co2_flux, pch=16, col='darkblue',xlab= '', ylab='')
lm5 <- lm(c$co2_flux, c$k6 )
abline(lm5,col='red', lwd=3)
mtext(expression(paste('k'['WMG1999'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)
dev.off()


path_fig <- file.path('figs/swh_r1.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif',
    mar = c(3.9, 3.9, 1, 1))
plot(c$swh, c$co2_flux, pch=16, col='darkblue',xlab= '', ylab='')
lm6 <- lm(c$co2_flux ~ c$swh)
abline(lm6,col='red', lwd=3)
minor.tick()
mtext(expression(paste('F'['CO']['2'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)


dev.off()



path_fig <- file.path('figs/swh_r.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif', mfrow = c(2,1),
    mar = c(3.9, 3.9, 1, 1))
plot(c$WS_site, c$co2_flux, pch=16, col='darkblue',xlab= '', ylab='')
lm6 <- lm(c$co2_flux ~ c$WS_site)
abline(lm6,col='red', lwd=3)
minor.tick()
mtext(expression(paste('F'['CO']['2'])),side = 2,line=2)
mtext(expression('U'['in-situ']),side = 1,line=2.3)
plot(c$u., c$co2_flux, pch=16, col='darkblue',xlab= '', ylab='')
lm6 <- lm(c$co2_flux ~ c$u.)
abline(lm6,col='red', lwd=3)
minor.tick()
mtext(expression(paste('F'['CO']['2'])),side = 2,line=2)
mtext(expression('u'['*']),side = 1,line=2.3)
dev.off()

