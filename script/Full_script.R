#### open Muka Head, Wave and Precipitation Data ####

muka<-read.csv('csv_file/Finalise_Muka_Head_Data.csv')
wave <-read.csv('csv_file/DWave.csv')
rain <-read.csv('csv_file/IPENANGP2_2017-10-12_143327.csv')

#### change the time using POSIXct ####
#Muka Head Data (muka)
date <- as.POSIXct(muka$daytime, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
muka <- cbind(date,muka)
muka <- muka[,-2]

#Wave Data(wave)
date <- as.POSIXct(wave$time1, format = "%d/%m/%Y %H:%M", tz = "GMT")
date <- as.POSIXct(wave$time1, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
wave <- cbind(date,wave)
wave <- wave[,-5]

#Rain Data(rain)
date <- as.POSIXct(rain$Time, format = "%Y-%m-%d %H:%M:%S", tz= "Asia/Kuala_Lumpur")
rain <-cbind(date,rain)
rain <-rain[,-2]


#### Filter Data ####
#Remove unwanted column
#Wave Data
wave <- wave[,-2]

#Precipitation Data
rain <- rain[,-c(2,3,4,5,6,7,8,9,10,12,13,15,16,17)]

#Muka Head Data
#Remove problematic value in wind speed
plot(muka$wind_speed)
muka$wind_speed[muka$wind_speed >5]<-NA
plot(muka$wind_speed)

#Remove problematic value in sea surface temperature (TS)
plot(muka$TS)
muka$TS[muka$TS >33]<-NA
muka$TS[muka$TS <29]<-NA
plot(muka$TS)
 

#### Convert angle to radian ####
#Add column to datasheet
muka$dir_90 = muka$wind_dir
muka$dir_90[muka$dir_90 > 90 ]<- NA ## only want the wind dir from 0 to 90 degree
muka$radian_wd = muka$dir_90 
muka$radian_wd <- (muka$dir_90*pi)/180

#### Merge Muka Head, Wave, Precipitation Data ####
library(openair)
temp <- timeAverage(wave, avg.time = "30 min", start.date = "2015-11-03 11:09:00", interval = "30 min")
temp2 <- timeAverage(rain, avg.time = "30 min", start.date = "2015-11-12 00:13:00", interval = "30 min")
merged_df <- merge(muka, temp, by = c("date","date"))
merged_df <-merge(merged_df,temp2, by =c("date", "date"))
rm(muka,wave,rain,temp,temp2)

#### Quality Control ####
#Remove qc=2, wind direction > 90, precipitation >0
#Create temp data frame
m2<- merged_df

#Remove qc=2, wind direction > 90, rain >0
m2$co2_flux[m2$wind_dir > 90 ]<- NA
m2$co2_flux[m2$qc_co2_flux == 2] <- NA
m2$co2_flux[m2$HourlyPrecipMM >0] <- NA
m2$co2_flux[m2$co2_flux >0.5] <- NA
m2$co2_flux[m2$co2_flux < -0.5] <- NA
m2$u.[m2$wind_dir > 90] <- NA
m2$co2_mole_fraction[m2$wind_dir>90] <- NA
m2$co2_mixing_ratio[m2$wind_dir> 90] <- NA
m2$air_pressure[m2$wind_dir> 90] <- NA
m2$x_90.[m2$wind_dir> 90] <- NA

#Manually remove outlier for CO2 flux
plot(m2$co2_flux)
which.max(m2$co2_flux)
which.min(m2$co2_flux)


#### Grouping data monthly ####
library(dplyr)
by_monthly_grp_data_mean <- m2 %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            co2_mixing_ratio=mean(co2_mixing_ratio,na.rm=TRUE),
            co2_mole_fraction=mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure=mean(air_pressure,na.rm = TRUE),
            U_insitu =mean(wind_speed,na.rm=TRUE),
            u. =mean(u.,na.rm = TRUE),
            x_90. = mean(x_90.,na.rm = TRUE),
            Site_temp=mean(TS,na.rm=TRUE),
            radian_wd =mean(radian_wd, na.rm = TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE),
            Lat_sat = mean(lat1, na.rm = TRUE),
            Lon_sat = mean(lon1, na.rm = TRUE),
            swh = mean(swh1, na.rm = TRUE),
            U_sat = mean(wind_speed_alt1, na.rm = TRUE))
          
by_monthly_grp_data_sd <- m2 %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            co2_mixing_ratio_sd=sd(co2_mixing_ratio,na.rm=TRUE),
            co2_mole_fraction_sd=sd(co2_mole_fraction,na.rm=TRUE),
            air_pressure_sd=sd(air_pressure,na.rm = TRUE),
            U_insitu_sd =sd(wind_speed,na.rm=TRUE),
            radian_wd_sd =sd(radian_wd, na.rm = TRUE),
            u._sd =sd(u.,na.rm = TRUE),
            x_90._sd = sd(x_90.,na.rm = TRUE),
            Site_temp_sd=sd(TS,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM,na.rm=TRUE),
            Lat_sat_sd = sd(lat1, na.rm = TRUE),
            Lon_sat_sd = sd(lon1, na.rm = TRUE),
            swh_sd = sd(swh1, na.rm = TRUE),
            U_sat_sd =sd(wind_speed_alt1, na.rm = TRUE))

#Merge the two dataframe
by_monthly_grp_data <- merge(by_monthly_grp_data_mean, by_monthly_grp_data_sd,by='month')
rm(by_monthly_grp_data_mean,by_monthly_grp_data_sd)
month <- paste(by_monthly_grp_data$month,"15"," 00:00:00")
by_monthly_grp_data <- cbind(month,by_monthly_grp_data)
month <- as.POSIXct(month, format = "%Y %m %d %H:%M:%S", tz = 'Asia/Kuala_Lumpur')
by_monthly_grp_data <- cbind(month,by_monthly_grp_data)
by_monthly_grp_data <- by_monthly_grp_data[,-2]

#Create temp data frame
monthly_data<- by_monthly_grp_data

### Input lat lon of Muka Head Station ####
lat_site <-NA
lat_site = 5.468333
monthly_data<-cbind(monthly_data,lat_site)

lon_site <-NA
lon_site = 100.2002778
monthly_data<-cbind(monthly_data,lon_site)

#### Calculation of coordinate of carbon footprint #### 
# x = xo + r cos delta
# y = yo + r sin delta
# r = distance, delta = angle of wind direction in radian
# x and y in meter
#install package (rdgal) to convert the LatLon to UTM
require(rgdal)

#convert lon_site and lat_site into UTC
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
x <- monthly_data$lon_site
y<- monthly_data$lat_site
LongLatToUTM(x,y,47)  ## Malaysia in zone 47
monthly_data <- cbind(monthly_data,LongLatToUTM(x,y,47))
names(monthly_data)[34] <- "Easting_Site"
names(monthly_data)[35] <- "Northing_Site"
monthly_data <- monthly_data[,-33]

#Calculate the coordinate
x_cfp <- NA
x_cfp = monthly_data$Easting_Site + (monthly_data$x_90.*cos(monthly_data$radian_wd))
monthly_data <- cbind(monthly_data, x_cfp)

y_cfp <- NA
y_cfp = monthly_data$Northing_Site + (monthly_data$x_90.*sin(monthly_data$radian_wd))
monthly_data <- cbind(monthly_data, y_cfp)

#convert lon_sat and lat_sat into UTC
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
x <- monthly_data$Lon_sat -180
y<- monthly_data$Lat_sat
LongLatToUTM(x,y,47)  ## Malaysia in zone 47
monthly_data <- cbind(monthly_data,LongLatToUTM(x,y,47))
names(monthly_data)[38] <- "Easting_Sat"
names(monthly_data)[39] <- "Northing_Sat"
monthly_data <- monthly_data[,-37]

#Calculation of the distance between site carbon footprint and satellite
distance <- NA
x1<- (monthly_data$Easting_Sat - monthly_data$x_cfp)^2
y1 <- (monthly_data$Northing_Sat - monthly_data$y_cfp)^2
distance = (x + y)^1/2
monthly_data <- cbind(monthly_data, distance)

#### Calculation for Delta Partial Pressure of CO2 ####
#Calculation of partial pressure of co2 in air (Pa)
# mole fraction(mol/mol) = partial pressure(Pa)/ total pressure(Pa)
# convert Pa to kPa = 1Pa*0.001
#convert mole fraction in ppm to mol/mol (1ppm * 10^-6)
PP_air <- NA
co2_mF <- monthly_data$co2_mole_fraction*10^-6  #convert into mol/mol
PP_air= co2_mF*monthly_data$air_pressure
PP_air <- PP_air * 0.001 #convert to kPa
#convert PP_air into micro atm
PP_air = (PP_air/101.325)*10^6
monthly_data <- cbind(monthly_data, PP_air)

#partial pressure of carbon dioxide in seawater(PCO2)(microatam) 
#Use Zhu's Algorithm to calculate by using T and chlorophyll from Aqua Modis 
# PCO2_sw = 6.31 SST^2 + 61.9 chlor_a^2 - 365.85 SST - 94.41 chlor_a + 5715.94
#SST in degree celcius
#chlor_a in mg/m3
#combine the dataset 
chl <- read.csv('csv_file/by_monthly_group_data_hy.csv')
chl<- chl[-25,]
chlor_a <- NA
chlor_a <- chl$Chlorophyll
monthly_data <- cbind(monthly_data, chlor_a)

T_sat <- NA
T_sat <- chl$Sea_Surface_Temp
monthly_data <- cbind(monthly_data, T_sat)

SST <- monthly_data$T_sat
chlor_a <- monthly_data$chlor_a
PCO2_sw <- NA
PCO2_sw = 6.31*(SST^2) + 61.9*(chlor_a^2) - 365.85*SST -94.41*chlor_a + 5715.94 
monthly_data <- cbind(monthly_data, PCO2_sw)

#deltaP in microatm
delta_p_p <- NA
delta_p_p = monthly_data$PCO2_sw - monthly_data$PP_air
monthly_data <- cbind(monthly_data, delta_p_p)


#### Calculate k value (gas transfer velocity) with the bulk formula ####
#Fco2 = kSdelta_p_p
#Fco2 in μmol per m^2 per s
#S = solubility of Carbon dioxide in water
#S = 0.10615 mol per litre per atm
#delta_p_p in μatm
#k in m per second

Fco2 <- monthly_data$co2_flux
S = 0.10615
delta_p_p <- monthly_data$delta_p_p
k_insitu<-NA
k_insitu = (Fco2 / (S*delta_p_p))/1000  #convert L to m^3
k_insitu <- k_insitu*100 * 3600  ## unit in cm/hr
monthly_data<- cbind(monthly_data,k_insitu)


#### k value calculate using theory ####
#k = 1/β * 1/(Sc^n) * u.
#β =16 -11  #16 is high turbulence, 11 is low turbulence
#n 0.67 -0.4   # 0.67 for smooth surface
#Sc = 2116.8 + (-136.25*t)+4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
#t = sea surface temperature

t = monthly_data$T_sat
Sc = 2116.8 + (-136.25*t)+4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
n = 0.67
β = 11
u. = monthly_data$u.
k_theory <-NA
k_theory = 1/β * 1/(Sc^n) * u.
k_theory <- k_theory*100 * 3600  ## unit in cm/hr
monthly_data<- cbind(monthly_data,k_theory)


#### k value calculate from literature's equations ####
#k_N2000 based on Nightingale
#k_N2000 = ((660/Sc)^0.5)*(0.212*(U^2) + 0.318U)
U = monthly_data$U_insitu
k_N2000 <- NA
k_N2000 = ((660/Sc)^0.5)*(0.212*(U^2) + 0.318*U)  ## unit in cm/hr
monthly_data <- cbind(monthly_data,k_N2000)

#k_W2014 based on Wanniknof 2014
# k = ((660/Sc)^0.5)*(0.251*(U^2))
k_W2014<- NA
k_W2014= ((660/Sc)^0.5)*(0.251*(U^2))   ## unit in cm/hr
monthly_data <- cbind(monthly_data, k_W2014)

#k_MG2001 based on McGillis 2001
k_MG2001 <- NA
k_MG2001 = ((660/Sc)^0.5)*(0.026*(U^3) + 3.3)  ## unit in cm/hr
monthly_data <- cbind(monthly_data,k_MG2001)

#k_WMG1999 based on Wanniknof n McGillis 1999
k_WMG1999 <- NA
k_WMG1999 = ((660/Sc)^0.5)*(0.0283*(U^3))  ## unit in cm/hr
monthly_data<- cbind(monthly_data,k_WMG1999)


#### Use the k value to calculate the co2_flux ####
k_insitu_co2 <- NA
k_insitu_co2 = (k_insitu/3600/100) * S *1000* monthly_data$delta_p_p
monthly_data <- cbind(monthly_data, k_insitu_co2)

k_theory_co2 <- NA
k_theory_co2 = (k_theory/3600/1000) * S * 1000*monthly_data$delta_p_p
monthly_data <- cbind(monthly_data, k_theory_co2)

k_N2000_co2 <- NA
k_N2000_co2 = (k_N2000/3600/100) * S *1000* monthly_data$delta_p_p
monthly_data <- cbind(monthly_data, k_N2000_co2)

k_W2014_co2 <- NA
k_W2014_co2 = (k_W2014/3600/100) * S*1000 * monthly_data$delta_p_p
monthly_data<- cbind(monthly_data, k_W2014_co2)

k_MG2001_co2 <- NA
k_MG2001_co2 = (k_MG2001/3600/100) * S*1000 * monthly_data$delta_p_p
monthly_data <- cbind(monthly_data, k_MG2001_co2)

k_WMG1999_co2 <- NA
k_WMG1999_co2 = (k_WMG1999/3600/100) * S*1000 * monthly_data$delta_p_p
monthly_data<- cbind(monthly_data, k_WMG1999_co2)


#### Descriptive Analysis ####
summary(monthly_data)


#### Correlation Analysis ####
#Hs = significant waves height (swh)
cor.test(monthly_data$swh,monthly_data$co2_flux, method="pearson")
cor.test(monthly_data$swh,monthly_data$U_insitu, method="pearson")
cor.test(monthly_data$swh,monthly_data$U_sat, method="pearson")
cor.test(monthly_data$swh,monthly_data$u., method="pearson")

#co2_flux
cor.test(monthly_data$co2_flux,monthly_data$U_insitu, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$U_sat, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$u., method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$PP_air, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$PCO2_sw, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$delta_p_p, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$T_sat, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$chlor_a, method="pearson")

#U_insitu
cor.test(monthly_data$U_insitu,monthly_data$U_sat, method="pearson")
cor.test(monthly_data$U_insitu,monthly_data$u., method="pearson")

#U_sat
cor.test(monthly_data$U_sat,monthly_data$u., method="pearson")

#PP_air
cor.test(monthly_data$PP_air,monthly_data$PCO2_sw, method="pearson")
cor.test(monthly_data$PP_air,monthly_data$delta_p_p, method="pearson")
cor.test(monthly_data$PP_air,monthly_data$T_sat, method="pearson")
cor.test(monthly_data$PP_air,monthly_data$chlor_a, method="pearson")

#PCO2_sw
cor.test(monthly_data$PCO2_sw,monthly_data$delta_p_p, method="pearson")
cor.test(monthly_data$PCO2_sw,monthly_data$T_sat, method="pearson")
cor.test(monthly_data$PCO2_sw,monthly_data$chlor_a, method="pearson")

#delta_p_p
cor.test(monthly_data$delta_p_p,monthly_data$T_sat, method="pearson")
cor.test(monthly_data$delta_p_p,monthly_data$chlor_a, method="pearson")

#T_sat
cor.test(monthly_data$T_sat,monthly_data$chlor_a, method="pearson")


#### Plot k versus Hs ####
library(Hmisc)
path_fig <- file.path('figs/k_versus_Hs.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif', mfrow = c(3,2),
    mar = c(3.9, 3.9, 1, 1))

plot(monthly_data$swh, monthly_data$k_insitu, pch=16, col='darkblue',xlab= '', ylab='')
lm1 <- lm(monthly_data$k_insitu ~ monthly_data$swh)
abline(lm1,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['in-situ'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_theory, pch=16, col='darkblue',xlab= '', ylab='')
lm2 <- lm(monthly_data$k_theory ~ monthly_data$swh)
abline(lm2,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['theory'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_N2000, pch=16, col='darkblue',xlab= '', ylab='')
lm3 <- lm(monthly_data$k_N2000 ~ monthly_data$swh)
abline(lm3,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['N2000'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_W2014, pch=16, col='darkblue',xlab= '', ylab='')
lm4 <- lm(monthly_data$k_W2014 ~ monthly_data$swh)
abline(lm4,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['W2014'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_MG2001, pch=16, col='darkblue',xlab= '', ylab='')
lm5 <- lm(monthly_data$k_MG2001 ~ monthly_data$swh)
abline(lm5,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['MG2001'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_WMG1999, pch=16, col='darkblue',xlab= '', ylab='')
lm6 <- lm(monthly_data$k_WMG1999 ~ monthly_data$swh)
abline(lm6,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['WMG1999'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)
dev.off()

#to know the value for slope for k_versus_Hs
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)



