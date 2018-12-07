#Conctruct graph for LE,H, CO2 flux
b <- df_group_month
wdeltaT_SST <- NA
wdeltaT_SST = (b$udeltaT_SST/b$WS_site) * b$swh
b<-cbind(b,wdeltaT_SST)

#H
path_fig <- file.path('figs/H_W_U_v1.jpg')
jpeg(file=path_fig,width=20,height=16,res=400, units = 'cm')
par(family='serif', mfrow = c(2,2),
    mar = c(3.9, 3.9, 1, 1))

plot(b$upt, b$H, pch=16, col='darkblue',xlab= 'upt', ylab='H')
lm4 <- lm(b$H ~ b$upt)
abline(lm4,col='red', lwd=3)

plot(b$udeltaT_SST, b$H, pch=16, col='darkblue',xlab= 'udeltaT_SST', ylab='H')
lm3 <- lm(b$H ~ b$udeltaT_SST)
abline(lm3,col='red', lwd=3)

plot(b$wdelta_T, b$H, pch=16, col='darkblue',xlab= 'wdelta_T', ylab='H')
lm1 <- lm(b$H ~ b$wdelta_T)
abline(lm1,col='red', lwd=3)


plot(b$wdeltaT_SST, b$H, pch=16, col='darkblue',xlab= 'wdeltaT_SST', ylab='H')
lm2 <- lm(b$H ~ b$wdeltaT_SST)
abline(lm2,col='red', lwd=3)
dev.off()

#LE
path_fig <- file.path('figs/LE_W_U_v1.jpg')
jpeg(file=path_fig,width=20,height=16,res=400, units = 'cm')
par(family='serif', mfrow = c(2,2),
    mar = c(3.9, 3.9, 1, 1))
plot(b$ude, b$LE, pch=16, col='darkblue',xlab= 'ude', ylab='LE')
lm8 <- lm(b$LE ~ b$ude)
abline(lm8,col='red', lwd=3)

plot(b$ude_SST, b$LE, pch=16, col='darkblue',xlab= 'ude_SST', ylab='LE')
lm5 <- lm(b$LE ~ b$ude_SST)
abline(lm5,col='red', lwd=3)

plot(b$wde, b$LE, pch=16, col='darkblue',xlab= 'wde', ylab='LE')
lm7 <- lm(b$LE ~ b$wde)
abline(lm7,col='red', lwd=3)

plot(b$wde_SST, b$LE, pch=16, col='darkblue',xlab= 'wde_SST', ylab='LE')
lm6 <- lm(b$LE ~ b$wde_SST)
abline(lm6,col='red', lwd=3)
dev.off()

summary(lm6)
summary(lm7)
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm8)

#CO2 flux
path_fig <- file.path('figs/CO2_Partial_Pressure_v1.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='serif', mfrow = c(1,2),
    mar = c(3.9, 3.9, 1, 1))
plot(b$delta_p, b$co2_flux, pch=16, col='darkblue',xlab= 'delta_p', ylab='CO2_flux')
ln1 <- lm(b$co2_flux ~ b$delta_p)
abline(ln1,col='red', lwd=3)

plot(b$delta_P_SST, b$co2_flux, pch=16, col='darkblue',xlab= 'delta_P_SST', ylab='CO2_flux')
ln2 <- lm(b$co2_flux ~ b$delta_P_SST)
abline(ln2,col='red', lwd=3)
dev.off()

summary(ln1)
summary(ln2)

summary(b$ts)
summary(b$WS_site)

par(mar=c(4,4,1,1))
plot(c$delta_p_p, c$co2_negative, pch=16, col='darkblue',xlab= 'delta_p', ylab='CO2_flux')
ln4 <- lm(c$co2_negative ~ c$delta_p_p)
abline(ln4,col='red', lwd=3)
summary(ln4)

plot(c$delta_p_p, c$co2_positive, pch=16, col='darkblue',xlab= 'delta_p', ylab='CO2_flux')
ln5 <- lm(c$co2_positive ~ c$delta_p_p)
abline(ln5,col='red', lwd=3)
summary(ln5)


plot(c$delta_p_p, c$co2_flux, pch=16, col='darkblue',xlab= 'delta_p', ylab='CO2_flux')
ln6 <- lm(c$co2_flux ~ c$delta_p_p)
abline(ln6,col='red', lwd=3)
summary(ln6)
