#input u*
u <- read.csv('csv_file/u_data.csv')
date <- as.POSIXct(u$daytime, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
u <- cbind(date,u)
u <- u[,-2]
u$u.[u$wind_dir > 90] <- NA

#group by month
library(dplyr)
s <- u
df_grp_mean1 <-s %>%
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(year=format(as.POSIXct(cut(time_stamp,breaks='year')),'%Y'),
           month=format(as.POSIXct(cut(time_stamp,breaks='month')),'%m')) %>%
  summarise(u.=mean(u., na.rm=TRUE))

df_grp_sd1 <-s %>%
  mutate(time_stamp= as.POSIXct(date)) %>%
  group_by(year=format(as.POSIXct(cut(time_stamp,breaks='year')),'%Y'),
           month=format(as.POSIXct(cut(time_stamp,breaks='month')),'%m')) %>%
  summarise(u._sd =sd(u., na.rm=TRUE))

#Merge the two dataframe
df_group_month2 <-merge(df_grp_mean1,df_grp_sd1,by=c('year','month'))
rm(df_grp_sd1,df_grp_mean1)
write.csv(df_group_month2, file="csv_file/u._monthly.csv")

#duplicate a data frame
t <- df_group_month2
#b is the monthly dataset
b$u. <- NA
b$u. <- t$u.
b$u._sd <- NA
b$u._sd <- t$u._sd

#k value calculate using u.
# k = 1/β * 1/(Sc^n) * u.
#β =16 -11  #16 is high turbulence, 11 is low turbulence
# n 0.67 -0.4   # 0.67 for smooth surface
# Sc = μ /(ρD)
# μ = dyanmic viscosity of fluid
#ρ = density of fluid (kg/m^3)
#D= mass diffusivity (m^2/s)
μ = 1.08 *10^-3
p = 1025 
D = 1.92*10^-5
Sc = (μ /(p*D))*100^2  # convert cm^2 to m^2
n = 0.67
β = 11
u. = b$u.
k = 1/β * 1/(Sc^n) * u.
k2 <-NA
k2 <- k
b<- cbind(b,k2)

# Monthly data
d16 <- read.csv('csv_file/monthly_data_2016_latest.csv')
d17 <- read.csv('csv_file/Monthly_data_2017_latest.csv')

#partial pressure of carbon dioxide in water
path_fig <- file.path('figs/Test_co2_1.jpg')
jpeg(file=path_fig,width=20,height=18,res=400, units = 'cm')
par(family='serif', mfrow = c(1,2),
    mar = c(3.9, 3.9, 1, 1))
plot(d16$month,d16$PCO2,pch=16, col='red')
points(d17$month, d17$PCO2, pch=16, col='blue')
lc3 <- lm(d16$PCO2 ~ d16$month)
abline(lc3,col='red',lwd = 3)
lc4 <- lm(d17$PCO2 ~ d17$month)
abline(lc4,col='blue',lwd = 3)

plot(d16$month,d16$co2_flux,pch=16, col='red')
points(d17$month, d17$co2_flux, pch=16, col='blue')
lc1 <- lm(d16$co2_flux ~ d16$month)
abline(lc1,col='red',lwd = 3)
lc2 <- lm(d17$co2_flux ~ d17$month)
abline(lc2,col='blue',lwd = 3)
dev.off()

#by monthly
path_fig <- file.path('figs/Test_k_monthly.jpg')
jpeg(file=path_fig,width=24,height=18,res=400, units = 'cm')
par(family='serif',
    mar = c(3.9, 3.9, 1, 1))
plot(d16$month, (d16$k2 * 100 * 3600), pch=16, col='red',xlab= 'month', ylab='k2',ylim=c(-10.8,28.8))
le1 <- lm(d16$k2 * 100 * 3600 ~ d16$month)
abline(le1,col='red',lwd = 3)
points(d16$month,(d16$k1* 100 * 3600), pch=16, col='darkblue',xlab= '', ylab='k1')
le2 <- lm(d16$k1 * 100 * 3600 ~ d16$month)
abline(le2,col='darkblue',lwd = 3)
points(d17$month,(d17$k1* 100 * 3600), pch=16, col='green',xlab= '', ylab='k1')
le3 <- lm(d17$k1 * 100 * 3600 ~ d17$month)
abline(le3,col='green',lwd = 3)
points(d17$month, (d17$k2 * 100 * 3600), pch=16, col='yellow',xlab= 'month', ylab='k2',ylim=c(-10.8,28.8))
le4 <- lm(d17$k2 * 100 * 3600 ~ d17$month)
abline(le4,col='yellow',lwd = 3)
dev.off()

path_fig <- file.path('figs/Test_k_3_in1.jpg')
jpeg(file=path_fig,width=24,height=14,res=400, units = 'cm')
par(family='serif',mfrow = c(2,2),
    mar = c(3.9, 3.9, 1, 1))
plot(b$u., (b$k2 * 100 * 3600), pch=16, col='red',xlab= 'u.', ylab='k2',ylim=c(-10.8,28.8))
points(d16$u.,(d16$k1* 100 * 3600), pch=16, col='darkblue',xlab= 'u.', ylab='k1')
points(d17$u.,(d17$k1* 100 * 3600), pch=16, col='green',xlab= 'u.', ylab='k1')

plot(b$swh,(b$k2* 100 * 3600), pch=16, col='red',xlab= 'swh', ylab='k2',ylim=c(-10.8,28.8))
points(d16$swh, (d16$k1* 100 * 3600),pch=16, col='darkblue',xlab= 'swh', ylab='k1')
points(d17$swh, (d17$k1* 100 * 3600),pch=16, col='green',xlab= 'swh', ylab='k1')

plot(d16$month, (d16$k2 * 100 * 3600), pch=16, col='red',xlab= 'month', ylab='k2',ylim=c(-10.8,28.8))
points(d16$month,(d16$k1* 100 * 3600), pch=16, col='darkblue',xlab= '', ylab='k1')
points(d17$month,(d17$k1* 100 * 3600), pch=16, col='green',xlab= '', ylab='k1')
points(d17$month, (d17$k2 * 100 * 3600), pch=16, col='yellow',xlab= 'month', ylab='k2',ylim=c(-10.8,28.8))
dev.off()

