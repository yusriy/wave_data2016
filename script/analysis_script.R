#open MH and Wave data
library(readxl)
m<-read.csv(file.choose())
w<-read.csv(file.choose())

#change the time to be same
#Muka Head Data (m)
date <- as.POSIXct(m$date, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
m <- cbind(date,m)
m <- m[,-2]

#Wave Data(w)
date <- as.POSIXct(w$time1, format = "%d/%m/%Y %H:%M", tz = "GMT")
date <- as.POSIXct(w$time1, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
w <- cbind(date,w)
w <- w[,-5]

#Merge 2 data
library(openair)
names(w)[1] <- "date"
temp <- timeAverage(w, avg.time = "30 min", start.date = "2015-11-03 11:09:00", interval = "30 min")
merged_df <- merge(m, temp, by = c("date","date"))

#Write in csv
write.csv(merged_df,file="merged_data_wt.csv")

#Filter data
#Create temp data frame
merged_df2 <- merged_df
#based on wind direction
merged_df2$co2_flux[merged_df2$dir > 90 & merged_df2$qc_co2_flux == 2] <- NA
merged_df2$LE[merged_df2$dir > 90 & merged_df2$qc_LE == 2] <- NA

#write merged_df2 into csv
write.csv(merged_df2, file="done_merged_df2.csv")

#Data analysis
#Time Series
library(ggplot2)
ggplot(merged_df2, aes(x=date,y=swh1))+ geom_smooth()
ggplot(merged_df2, aes(x=date,y=LE))+ geom_smooth()
ggplot(merged_df2, aes(x=date,y=co2_flux))+ geom_smooth()
ggplot(merged_df2, aes(x=date,y=wind_speed))+ geom_smooth()
ggplot(merged_df2, aes(x=date,y=wind_speed_alt1))+ geom_smooth()

plot(merged_df2$LE~merged_df2$date, type = 'l',ylim=c(-200,200))
plot(merged_df2$co2_flux~merged_df2$date, type = 'l',ylim=c(-20,20))
plot(merged_df2$wind_speed~merged_df2$date, pch =19)
plot(merged_df2$wind_speed_alt1~merged_df2$date, pch=19)
plot(merged_df2$swh1~merged_df2$date, pch=19)
plot(merged_df2$swh1~date,type='l',col='red',main="Time Series Plot of Significant Wave Height",ylab='swh',xlab='Time(Month)')

#Relationship between the variables
plot(merged_df2$swh1,merged_df2$LE, pch=19, col = "green", xlim=c(0,2),ylim=c(-5,25))
abline(lm(merged_df2$LE~merged_df2$swh1),col="red")
plot(merged_df2$swh1,merged_df2$co2_flux, pch=19, col = "blue", xlim=c(0,2), ylim=c(-1,1))
abline(lm(merged_df2$co2_flux~merged_df2$swh1),col="red")

##Correlation
library(ggpubr)
library(dplyr)
library(nortest)

shapiro.test(merged_df2$LE[0:5000])
shapiro.test(merged_df2$co2_flux[0:5000])
shapiro.test(merged_df2$wind_speed[0:5000])
shapiro.test(merged_df2$swh1[0:5000])
shapiro.test(merged_df2$wind_speed_alt1[0:5000])


# ad.test(merged_df2$wind_speed_alt1)$p.value
# ad.test(merged_df2$wind_speed)$p.value
# ad.test(merged_df2$swh1)$p.value
# ad.test(merged_df2$LE)$p.value
# ad.test(merged_df2$co2_flux)$p.value



ggqqplot(merged_df2$wind_speed_alt1,ylab="Wind_speed_alt1")
ggqqplot(merged_df2$wind_speed,ylab="Wind_speed")
ggqqplot(merged_df2$swh1,ylab="swh")
ggqqplot(merged_df2$LE,ylab="LE")
ggqqplot(merged_df2$co2_flux,ylab="CO2 Flux")



ggscatter(merged_df2,x="swh1",y="co2_flux",
          add="reg.line",conf.int = TRUE,
          cor.coef = TRUE,cor.method = "spearman",
          xlab="swh(m)",ylab="CO2 flux")

ggscatter(merged_df2,x="swh1",y="LE",
          add="reg.line",conf.int = TRUE,
          cor.coef = TRUE,cor.method = "spearman",
          xlab="swh(m)",ylab="LE")
          
ggscatter(merged_df2,x="wind_speed",y="wind_speed_alt1",
          add="reg.line",conf.int = TRUE,
          cor.coef = TRUE,cor.method = "spearman",
          xlab="In-situ wind_speed",ylab="Satellite wind speed", xlim=c(0,5) )  

ggscatter(merged_df2,x="wind_speed",y="swh1",
          add="reg.line",conf.int = TRUE,
          cor.coef = TRUE,cor.method = "spearman",
          xlab="In-situ wind_speed",ylab="SWH", xlim=c(0,5))  

ggscatter(merged_df2,x="wind_speed_alt1",y="swh1",
          add="reg.line",conf.int = TRUE,
          cor.coef = TRUE,cor.method = "pearson",
          xlab="Satellite wind_speed",ylab="SWH", xlim=c(0,5)) 

#Pearson correlation test
res<-cor.test(merged_df2$swh1,merged_df2$co2_flux,
              method="pearson")
res$estimate

res<-cor.test(merged_df2$swh1,merged_df2$co2_flux,
              method="spearman")
res$estimate

res<-cor.test(merged_df2$wind_speed,merged_df2$co2_flux,
              method="pearson")
res$estimate

res<-cor.test(merged_df2$wind_speed,merged_df2$co2_flux,
              method="spearman")
res$estimate

res<-cor.test(merged_df2$wind_speed_alt1,merged_df2$co2_flux,
              method="pearson")
res$estimate

res<-cor.test(merged_df2$wind_speed_alt1,merged_df2$co2_flux,
              method="spearman")
res$estimate

res<-cor.test(merged_df2$co2_flux,merged_df2$LE,
              method="pearson")
res
res<-cor.test(merged_df2$co2_flux,merged_df2$LE,
              method="spearman")
res

res<-cor.test(merged_df2$wind_speed,merged_df2$LE,
              method="pearson")
res
res<-cor.test(merged_df2$wind_speed,merged_df2$LE,
              method="spearman")
res

res<-cor.test(merged_df2$wind_speed_alt1,merged_df2$LE,
              method="pearson")
res
res<-cor.test(merged_df2$wind_speed_alt1,merged_df2$LE,
              method="spearman")
res
res<-cor.test(merged_df2$swh1,merged_df2$LE,
              method="pearson")
res 
res<-cor.test(merged_df2$swh1,merged_df2$LE,
              method="spearman")
res 

res<-cor.test(merged_df2$wind_speed_alt1,merged_df2$wind_speed,
              method="pearson")
res 
res<-cor.test(merged_df2$wind_speed_alt1,merged_df2$wind_speed,
              method="spearman")
res 

res<-cor.test(merged_df2$wind_speed_alt1,merged_df2$swh1,
              method="pearson")
res 
res<-cor.test(merged_df2$wind_speed_alt1,merged_df2$swh1,
              method="spearman")
res

res<-cor.test(merged_df2$wind_speed,merged_df2$swh1,
              method="pearson")
res 
res<-cor.test(merged_df2$wind_speed,merged_df2$swh1,
              method="spearman")
res

#extract p-value n cerrelation coefficient
res$p.value
res$estimate



# co2_flux <- merged_df2$co2_flux[merged_df2$dir..0.90. <= 90 & 
#                                   merged_df2$qc_co2_flux != 2]
# swh <-merged_df2$swh1[merged_df2$dir..0.90. <= 90 & 
#                         merged_df2$qc_co2_flux != 2]



