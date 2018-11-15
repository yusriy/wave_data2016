#open MH and Wave data

muka<-read.csv('csv_file/CMukaHead.csv')
wave <-read.csv('csv_file/DWave.csv')
rain <-read.csv('csv_file/IPENANGP2_2017-10-12_143327.csv')

#change the time to be same
#Muka Head Data (muka)
date <- as.POSIXct(muka$date, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
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

#Remove problematic value in wind speed
plot(muka$wind_speed)
muka$wind_speed[muka$wind_speed >5]<-NA
plot(muka$wind_speed)

#Remove lat1, lon1, index1 in wave data
wave <- wave[,-c(2,3,4)]

#Remove unwanted column in rain data
rain <- rain[,-c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17)]

#Merge Muka Head, Wave, Rain Data
library(openair)
temp <- timeAverage(wave, avg.time = "30 min", start.date = "2015-11-03 11:09:00", interval = "30 min")
temp2 <- timeAverage(rain, avg.time = "30 min", start.date = "2015-11-12 00:13:00", interval = "30 min")
merged_df <- merge(muka, temp, by = c("date","date"))
merged_df <-merge(merged_df,temp2, by =c("date", "date"))

#Filter Data
#Remove qc=2, wind direction > 90, wind speed >5, rain >0
#Write csv
write.csv(merged_df, file="ori_v2.csv")
#Create temp data frame
m2<- merged_df


#Remove qc=2, wind direction > 90, rain >0
m2$co2_flux[m2$wind_dir > 90 ]<- NA
m2$co2_flux[m2$qc_co2_flux == 2] <- NA
m2$co2_flux[m2$HourlyPrecipMM >0] <- NA
m2$co2_flux[m2$co2_flux >0.5] <- NA
m2$co2_flux[m2$co2_flux < -0.5] <- NA
m2$LE[m2$qc_LE == 2 ] <- NA
m2$LE[m2$HourlyPrecipMM >0]<- NA
m2$LE[m2$wind_dir > 90]<- NA
m2$H[m2$qc_H == 2 ] <- NA
m2$H[m2$HourlyPrecipMM >0]<- NA
m2$H[m2$wind_dir > 90]<- NA


#Manually remove outlier for CO2 flux
plot(m2$co2_flux)
which.max(m2$co2_flux)
which.min(m2$co2_flux)



#Manually remove outlier in LE
plot(m2$LE)
which.max(m2$LE)
which.min(m2$LE)
m2$LE[m2$LE< -100] <- NA
m2$LE[m2$LE> 100] <- NA
m2$LE[m2$LE< -20] <- NA
m2$LE[m2$LE> 60] <- NA
m2$LE[m2$LE< -10] <- NA
m2$LE[m2$LE> 50] <- NA

plot(m2$LE[20000:30000])
plot(m2$LE[28000:30000])
plot(m2$LE[28950:28965])
plot(m2$LE[19330:19400])
plot(m2$LE[19380:19381])
plot(m2$LE[16340:16348])
m2$LE[28950:28965] <- NA

#Manually remove outlier in H
plot(m2$H)
which.max(m2$H)
which.min(m2$H)
m2$H[m2$H< -200] <- NA
m2$H[m2$H< -20] <- NA
m2$H[m2$H< -5] <- NA
m2$H[m2$H> 15] <- NA

plot(m2$H[32420:32421])
plot(m2$H[27000:30000])
plot(m2$H[29000:34000])
plot(m2$H[29547:29555])
plot(m2$H[27910:27920])
m2$H[32420:32421] <- NA


write.csv(m2, file="m2.csv")




# #a set of merged_df with swh <3m
# merged_df5 <- merged_df4
# merged_df5$swh1[merged_df5$swh1>3] <- NA
# plot(merged_df5$swh1)
# plot(merged_df5$swh1,merged_df5$LE)
# ggscatter(merged_df5,x="swh1",y="LE",
#           add="reg.line",conf.int = TRUE,
#           cor.coef = TRUE,cor.method = "pearson",
#           xlab="swh(m)",ylab="LE")
          