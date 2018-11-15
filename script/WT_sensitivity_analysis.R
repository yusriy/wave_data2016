#Group the data according to the time scale
#Library used
library(lubridate)
library(dplyr)
library(openair)


#Import dataset
l<- read.csv('csv_file/correct.csv')
l <- l[,-1]
date <- as.POSIXct(l$date, format= "%Y-%m-%d %H:%M:%S")
l <- cbind(date, l)
l <- l[,-2]

#Monthly
lb <- l
df_grp_mean <-lb %>%
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
            rh=mean(RH, na.rm=TRUE))

df_grp_sd <-lb %>%
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
            rh_sd=sd(RH, na.rm=TRUE))

#Merge the two dataframe
df_group_month <-merge(df_grp_mean,df_grp_sd,by=c('year','month'))
rm(df_grp_sd,df_grp_mean)

write.csv(df_group_month, file="group_month.csv")

#Seasonally


#Yearly
df_grp_mean <-fd %>%
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(year=format(as.POSIXct(cut(time_stamp,breaks='year')),'%Y')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm = TRUE),
            LE=mean(LE,na.rm = TRUE),
            H=mean(H,na.rm = TRUE),
            WS_site=mean(wind_speed,na.rm = TRUE),
            WS_satellite=mean(wind_speed_alt1, na.rm = TRUE),
            swh=mean(swh1, na.rm=TRUE),
            Precipitation=mean(HourlyPrecipMM, na.rm = TRUE))

df_grp_sd <-fd %>%
  mutate(time_stamp= as.POSIXct(date)) %>%
  group_by(year=format(as.POSIXct(cut(time_stamp,breaks='year')),'%Y')) %>%
           
  summarise(co2_flux_sd=sd(co2_flux,na.rm = TRUE),
            LE_sd=sd(LE,na.rm = TRUE),
            H_sd=sd(H,na.rm = TRUE),
            WS_site_sd=sd(wind_speed,na.rm = TRUE),
            WS_satellite_sd=sd(wind_speed_alt1, na.rm = TRUE),
            swh_sd=sd(swh1, na.rm=TRUE),
            Precipitation_sd=sd(HourlyPrecipMM, na.rm = TRUE))

#Merge the two dataframe
df_group_year <-merge(df_grp_mean,df_grp_sd,by=c('year'))
rm(df_grp_sd,df_grp_mean)

write.csv(df_group_year, file="group_year.csv")

#Yearly
cor.test(df_group_year$swh,df_group_year$co2_flux, method="pearson")
cor.test(df_group_year$swh,df_group_year$LE, method="pearson")
cor.test(df_group_year$swh,df_group_year$WS_satellite, method="pearson")
cor.test(df_group_year$swh,df_group_year$WS_site, method="pearson")

cor.test(df_group_year$co2_flux,df_group_year$WS_site, method="pearson")
cor.test(df_group_year$co2_flux,df_group_year$WS_satellite, method="pearson")
cor.test(df_group_year$co2_flux,df_group_year$LE, method="pearson")

cor.test(df_group_year$LE,df_group_year$WS_site, method="pearson")
cor.test(df_group_year$LE,df_group_year$WS_satellite, method="pearson")

cor.test(df_group_year$WS_site,df_group_year$WS_satellite, method="pearson")

#Monthly
cor.test(df_group_month$swh,df_group_month$co2_flux, method="pearson")
cor.test(df_group_month$swh,df_group_month$LE, method="pearson")
cor.test(df_group_month$swh,df_group_month$WS_satellite, method="pearson")
cor.test(df_group_month$swh,df_group_month$WS_site, method="pearson")
cor.test(df_group_month$swh,df_group_month$H, method="pearson")

cor.test(df_group_month$co2_flux,df_group_month$WS_site, method="pearson")
cor.test(df_group_month$co2_flux,df_group_month$WS_satellite, method="pearson")
cor.test(df_group_month$co2_flux,df_group_month$LE, method="pearson")
cor.test(df_group_month$co2_flux,df_group_month$H, method="pearson")

cor.test(df_group_month$LE,df_group_month$WS_site, method="pearson")
cor.test(df_group_month$LE,df_group_month$WS_satellite, method="pearson")
cor.test(df_group_month$LE,df_group_month$H, method="pearson")

cor.test(df_group_month$WS_site,df_group_month$H, method="pearson")
cor.test(df_group_month$WS_satellite,df_group_month$H, method="pearson")
