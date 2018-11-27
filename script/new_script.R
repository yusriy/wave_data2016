#input insitu lat, initu lon, lat_sat, lon_sat, mixing ratio, mole fraction of CO2, x_90(distance of carbon footprint)
Completed_data <- read.csv('csv_file/complete_data.csv')
Wave_data <- read.csv('csv_file/DWave.csv')
N_muka <- read.csv('csv_file/muka_head_new_mixing_ratio.csv')

#Change the time POXIST
#Completed data
date <- as.POSIXct(Completed_data$date, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kuala_Lumpur")
Completed_data <- cbind(date,Completed_data)
Completed_data <- Completed_data[,-c(2,3)]

#New muka head data
date <- as.POSIXct(N_muka$daytime, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
N_muka <- cbind(date,N_muka)
N_muka <- N_muka[,-2]

#Wave Data
date <- as.POSIXct(Wave_data$time1, format = "%d/%m/%Y %H:%M", tz = "GMT")
date <- as.POSIXct(Wave_data$time1, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
Wave_data <- cbind(date,Wave_data)
Wave_data <- Wave_data[,-5]

#Remove duplicate data (swh1, wind_speed_alt1)
Wave_data <- Wave_data[,-c(2,5,6)]


#Merge data
library(openair)
temp <- timeAverage(Wave_data, avg.time = "30 min", start.date = "2015-11-03 11:09:00", interval = "30 min")
merged_df3 <- merge(Completed_data, temp, by = c("date","date"))
temp2 <- timeAverage(N_muka, avg.time = "30 min", start.date = "2015-11-12 00:30:00", interval = "30 min")
merged_df4 <- merge(merged_df3, temp2, by = c("date","date"))
write.csv(merged_df4,file="csv_file/final_complete_data_with_latlon.csv")


#Filter data
#Create temp data frame
merged_df <- merged_df4
#based on wind direction
merged_df$co2_mole_fraction[merged_df$wind_dir] <- NA
merged_df$co2_mixing_ratio[merged_df$wind_dir> 90] <- NA
merged_df$air_pressure[merged_df$wind_dir> 90] <- NA
merged_df$x_90.[merged_df$wind_dir> 90] <- NA

#Input lat lon of Cemacs
lat_site <-NA
lat_site = 5.468333
merged_df<-cbind(merged_df,lat_site)

lon_site <-NA
lon_site = 100.2002778
merged_df<-cbind(merged_df,lon_site)

#write merged_df into csv
write.csv(merged_df, file="final_completed_filtered_data.csv")

#Calculation of partial pressure of co2 in air
#Case 1 (the unit of air pressure in Pa)
# mole fraction(mol/mol) = partial pressure(Pa)/ total pressure(Pa)
# convert Pa to kPa = 1Pa*0.001
#convert mole fraction in ppm to mol/mol (1ppm * 10^-6)
PP_air <- NA
co2_mF <- merged_df$co2_mole_fraction*10^-6  #convert into mol/mol
PP_air= co2_mF*merged_df$air_pressure
PP_air <- PP_air * 0.001 #convert to kPa
merged_df <- cbind(merged_df, PP_air)

#Calculation of partial pressure of co2 in seawater
# Use Antoine eq to find the partial pressure of co2 in water
# logP = A-[B/(T+C)]  ## P in mmHg, T in degree Celcius
#1 mmHg = 0.133322 kPa
# A= 7.5322 	 B=835.06 	 C=268.223
PP_seawater <- NA
A = 7.5322
B = 835.06
C = 268.223
PP_seawater = 10^(A-(B/(merged_df$TS + C)))
PP_seawater <- PP_seawater*0.133322  ##convert to kPa
merged_df <- cbind(merged_df, PP_seawater)

#Create a column for delta P
delta_p <- NA
delta_p = merged_df$PP_seawater - merged_df$PP_air
merged_df <- cbind( merged_df, delta_p)

#Case2 (the unit of air pressure in hPa)
#New PP_air
NPP_air <- NA
co2_mF <- merged_df$co2_mole_fraction*10^-6  #convert into mol/mol
NPP_air= co2_mF*merged_df$air_pressure
NPP_air <- NPP_air * 0.1 #convert to kPa
merged_df <- cbind(merged_df, NPP_air)
Ndelta_p <- NA
Ndelta_p = merged_df$PP_seawater - merged_df$NPP_air
merged_df <- cbind( merged_df, Ndelta_p)

#Calculation of coordinate of carbon footprint 
# x = xo + r cos delta
# y = yo + r sin delta
# r = distance, delta = angle of wind direction in radian
# x and y in meter
#install package (rdgal) to convert the LatLon to UTM
require(rgdal)

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
x <- merged_df$lon_site
y<- merged_df$lat_site
LongLatToUTM(x,y,47)  ## Malaysia in zone 47
merged_df <- cbind(merged_df,LongLatToUTM(x,y,47))
names(merged_df)[39] <- "Easting_Site"
names(merged_df)[40] <- "Northing_Site"

#Convert angle to radian
#Add column to datasheet
merged_df$dir_90 = merged_df$wind_dir
merged_df$dir_90[merged_df$dir_90 > 90 ]<- NA ## only want the wind dir from 0 to 90 degree
merged_df$radian_wd = merged_df$dir_90 
merged_df$radian_wd <- (merged_df$dir_90*pi)/180

#Calculate the coordinate
x_cfp <- NA
x_cfp = merged_df$Easting_Site + (merged_df$x_90.*cos(merged_df$radian_wd))
merged_df <- cbind(merged_df, x_cfp)

y_cfp <- NA
y_cfp = merged_df$Northing_Site + (merged_df$x_90.*sin(merged_df$radian_wd))
merged_df <- cbind(merged_df, y_cfp)

# addition of SST data
SST <- read.csv('csv_file/df_hy.csv')
#SST
date <- as.POSIXct(SST$time_stamp, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kuala_Lumpur")
SST <- cbind(date,SST)
SST <- SST[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18)]
temp3 <- timeAverage(SST, avg.time = "30 min", start.date = "2015-11-12 00:30:00", interval = "30 min")
merged_df_f <- merge(merged_df, temp3, by = c("date","date"))


#Construct column for difference of temperature(SST-TA), delta E_SST, U*difference of temperature(SST)
#cal difference of temperature
delta_T_SST <- NA
delta_T_SST = merged_df_f$SST - merged_df_f$TA
merged_df_f <- cbind(merged_df_f,delta_T_SST)

#cal in-situ wind speed * difference of T
udeltaT_SST <- NA
udeltaT_SST = merged_df_f$delta_T_SST * merged_df_f$wind_speed
merged_df_f <- cbind(merged_df_f,udeltaT_SST)

source('tools/tool_vapor_cal.R')
e_s1 <- vap_pres_Buck(merged_df_f$SST, 1.00)  # RH = 1.00 because saturated
e_a <- vap_pres_Buck(merged_df_f$TA, merged_df_f$rh_fraction) 
# deltaE must be in kPa for bulk aerodynamic transfer equation below
deltaE_SST <- (e_s1 - e_a) * 0.1 # because to convert to kPa
merged_df_f<-cbind(merged_df_f,deltaE_SST)

#Make udeltaE_SST
ude_SST <- NA
ude_SST = merged_df_f$deltaE_SST * merged_df_f$wind_speed
merged_df_f<-cbind(merged_df_f,ude_SST)


#Calculation of partial pressure of co2 in seawater based on SST
# Use Antoine eq to find the partial pressure of co2 in water
# logP = A-[B/(T+C)]  ## P in mmHg, T in degree Celcius
#1 mmHg = 0.133322 kPa
# A= 7.5322 	 B=835.06 	 C=268.223
PP_seawater_SST <- NA
A = 7.5322
B = 835.06
C = 268.223
PP_seawater_SST = 10^(A-(B/(merged_df_f$SST + C)))
PP_seawater_SST <- PP_seawater_SST*0.133322  ##convert to kPa
merged_df_f <- cbind(merged_df_f, PP_seawater_SST)

#Create a column for delta P_SST
delta_p_SST <- NA
delta_p_SST = merged_df_f$PP_seawater_SST - merged_df_f$PP_air
merged_df_f <- cbind( merged_df_f, delta_p_SST)

#Write csv
write.csv(merged_df_f, file="csv_file/final_dataset_with_SST.csv")

#Group the data by month
library(dplyr)
a <- merged_df_f
df_grp_mean1 <-a %>%
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(year=format(as.POSIXct(cut(time_stamp,breaks='year')),'%Y'),
           month=format(as.POSIXct(cut(time_stamp,breaks='month')),'%m')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm = TRUE),
            LE=mean(LE,na.rm = TRUE),
            H=mean(H,na.rm = TRUE),
            WS_site=mean(wind_speed,na.rm = TRUE),
            WS_satellite=mean(wind_speed_alt1, na.rm = TRUE),
            swh=mean(swh1, na.rm=TRUE),
            Precipitation=mean(HourlyPrecipMM, na.rm = TRUE),
            e=mean(e,na.rm=TRUE),
            ta=mean(TA, na.rm=TRUE),
            ts=mean(TS, na.rm=TRUE),
            sst = mean(SST, na.rm = TRUE),
            rh_fraction=mean(rh_fraction, na.rm=TRUE),
            deltaE = mean(deltaE, na.rm = TRUE),
            deltaE_SST = mean (deltaE_SST, na.rm=TRUE),
            upt = mean(upt, na.rm=TRUE),
            udeltaT_SST = mean(udeltaT_SST, na.rm=TRUE),
            ude= mean(ude,na.rm = TRUE),
            ude_SST = mean(ude_SST, na.rm=TRUE),
            lat_sat = mean(lat1, na.rm=TRUE),
            lon_sat = mean (lon1, na.rm = TRUE),
            co2_mole_fraction = mean(co2_mole_fraction, na.rm = TRUE),
            carbon_footprint = mean(x_90.,na.rm=TRUE),
            air_pressure = mean(air_pressure, na.rm = TRUE),
            lat_site = mean(lat_site, na.rm = TRUE),
            lon_site = mean(lon_site, na.rm=TRUE),
            PP_air = mean(PP_air, na.rm=TRUE),
            PP_seawater = mean (PP_seawater, na.rm=TRUE),
            PP_seawater_SST = mean (PP_seawater_SST, na.rm=TRUE),
            delta_p =mean(delta_p, na.rm=TRUE),
            NPP_air = mean (NPP_air, na.rm = TRUE),
            Ndelta_p = mean(Ndelta_p, na.rm = TRUE),
            Easting_site = mean(Easting_Site, na.rm=TRUE),
            Northing_site = mean(Northing_Site, na.rm=TRUE),
            radian_wd = mean(radian_wd, na.rm = TRUE),
            x_cfp = mean(x_cfp, na.rm=TRUE),
            y_cfp = mean (y_cfp, na.rm=TRUE))

df_grp_sd <-a %>%
  mutate(time_stamp= as.POSIXct(date)) %>%
  group_by(year=format(as.POSIXct(cut(time_stamp,breaks='year')),'%Y'),
           month=format(as.POSIXct(cut(time_stamp,breaks='month')),'%m')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm = TRUE),
            LE_sd=sd(LE,na.rm = TRUE),
            H_sd=sd(H,na.rm = TRUE),
            WS_site_sd=sd(wind_speed,na.rm = TRUE),
            WS_satellite_sd=sd(wind_speed_alt1, na.rm = TRUE),
            swh_sd=sd(swh1, na.rm=TRUE),
            Precipitation_sd=sd(HourlyPrecipMM, na.rm = TRUE),
            e_sd=sd(e,na.rm=TRUE),
            ta_sd=sd(TA, na.rm=TRUE),
            ts_sd=sd(TS, na.rm=TRUE),
            sst_sd = sd(SST, na.rm = TRUE),
            rh_fraction_sd=sd(rh_fraction, na.rm=TRUE),
            deltaE_sd = sd(deltaE, na.rm = TRUE),
            deltaE_SST_sd = sd (deltaE_SST, na.rm=TRUE),
            upt_sd = sd(upt, na.rm=TRUE),
            udeltaT_SST_sd = sd(udeltaT_SST, na.rm=TRUE),
            ude_sd= sd(ude,na.rm = TRUE),
            ude_SST_sd = sd(ude_SST, na.rm=TRUE),
            lat_sat_sd = sd(lat1, na.rm=TRUE),
            lon_sat_sd = sd (lon1, na.rm = TRUE),
            co2_mole_fraction_sd = sd(co2_mole_fraction, na.rm = TRUE),
            carbon_footprint_sd = sd(x_90.,na.rm=TRUE),
            air_pressure_sd = sd(air_pressure, na.rm = TRUE),
            lat_site_sd = sd(lat_site, na.rm = TRUE),
            lon_site_sd = sd(lon_site, na.rm=TRUE),
            PP_air_sd = sd(PP_air, na.rm=TRUE),
            PP_seawater_sd = sd (PP_seawater, na.rm=TRUE),
            PP_seawater_SST_sd = sd (PP_seawater_SST, na.rm=TRUE),
            delta_p_sd =sd(delta_p, na.rm=TRUE),
            NPP_air_sd = sd (NPP_air, na.rm=TRUE),
            Ndelta_p_sd = sd(Ndelta_p, na.rm=TRUE),
            Easting_site_sd = sd(Easting_Site, na.rm=TRUE),
            Northing_site_sd = sd(Northing_Site, na.rm=TRUE),
            radian_wd_sd = sd(radian_wd, na.rm=TRUE),
            x_cfp_sd = sd(x_cfp, na.rm=TRUE),
            y_cfp_sd = sd (y_cfp, na.rm=TRUE))
            
#Merge the two dataframe
df_group_month <-merge(df_grp_mean,df_grp_sd,by=c('year','month'))
rm(df_grp_sd,df_grp_mean)

#Calculate delta_P_SST
delta_P_SST <-NA
delta_P_SST <- df_group_month$PP_seawater_SST - df_group_month$PP_air
df_group_month <-cbind(df_group_month,delta_P_SST)

#Calculate delta_P_SST_sd
delta_P_SST_sd <-NA
delta_P_SST_sd <- df_group_month$PP_seawater_SST_sd - df_group_month$PP_air_sd
df_group_month <-cbind(df_group_month,delta_P_SST_sd)



#UTM for latlon_sat
require(rgdal)

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
x <- df_group_month$lon_sat -180
y<- df_group_month$lat_sat
LongLatToUTM(x,y,47)  ## Malaysia in zone 47
df_group_month <- cbind(df_group_month,LongLatToUTM(x,y,47))
names(df_group_month)[78] <- "Easting_Sat"
names(df_group_month)[79] <- "Northing_Sat"


#Calculation of the distance between site carbon footprint and satellite
distance <- NA
x1<- (df_group_month$Easting_Sat - df_group_month$x_cfp)^2
y1 <- (df_group_month$Northing_Sat - df_group_month$y_cfp)^2
distance = (x + y)^1/2
df_group_month <- cbind(df_group_month, distance)

write.csv(df_group_month, file="csv_file/group_month_V1.csv")
