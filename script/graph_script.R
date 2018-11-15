#Construct column for difference of temperature, delta E, U*difference of temperature, rh in fraction

#cal difference of temperature
pt <- NA
pt = data$ts - data$ta
data<- cbind(data,pt)

#cal in-situ wind speed * difference of T
upt <- NA
upt = data$pt * data$WS_site
data<- cbind(data,upt)

#Make the rh in fraction
rh_fraction <-NA
rh_fraction= data$rh /100
data<-cbind(data,rh_fraction)

source('tools/tool_vapor_cal.R')
e_s1 <- vap_pres_Buck(data$ts, 1.00)  # RH = 1.00 because saturated
e_a <- vap_pres_Buck(data$ta, data$rh_fraction) 
# deltaE must be in kPa for bulk aerodynamic transfer equation below
deltaE <- (e_s1 - e_a) * 0.1 # because to convert to kPa
data<-cbind(data,deltaE)

#Make udeltaE
ude <- NA
ude = data$deltaE * data$WS_site
data<-cbind(data,ude)


#Regression lines
lm1 <- lm(data$LE ~ data$deltaE)
plot(data$deltaE,data$LE,pch = 17, col='red')
abline(lm1,col='blue', lwd=3)

lm2 <- lm(data$LE ~ data$ude)
plot(data$ude,data$LE,pch = 17,col='red')
abline(lm2,col='blue', lwd=3)

lm3 <- lm(data$H ~ data$upt)
plot(data$upt, data$H, pch= 17,col='red')
abline(lm3, col='blue', lwd =3)



#plot the monthly scale
d16 <- read.csv('csv_file/monthly_2016.csv')
d17 <- read.csv('csv_file/monthly_2017.csv')

x1 <- d16$WS_satellite
x2 <- d17$WS_satellite

y1 <- d16$WS_site
y2 <- d17$WS_site

path_fig <- file.path('figs/mCO2_flux_V4.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='serif', mar = c(4.1, 4.1, 0.1, 0.1))
plot(x1,y1,type='l', col='red', xlab= 'Month', ylab='CO'['2'],' flux')
lines(x2,y2, col='blue')



# mtext(side = 1, 'Month', line = 2.5, cex = 1.5)
# mtext(side = 2, expression(paste('CO'['2'],' flux')),
#       line = 2.1, cex = 1.5)
dev.off()

path_fig <- file.path('figs/ws_satellite_ws_insitu_V2.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='serif', mar = c(4.1, 4.1, 0.1, 0.1))
plot(x1,y1,pch=16, col='red', xlab= 'ws_satellite', ylab='WS_insitu')
points(x2,y2, col='blue')
dev.off()







