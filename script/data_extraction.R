##Significant Waves Height Data Extraction
#Load library
library(ncdf4)

#Listing all the fies in the folder
filename <- list.files('waveoct-dec2016/')
#To remove the '.nc' at the end
path <- strsplit(filename,split='.nc')
#and change it to character
path1 <- unlist(as.character(path))

#Init the variables
time1 <- 0
lat1 <- 0
lon1 <- 0
swh1 <- 0
index1 <- 0
wind_speed_alt1 <- 0

for (i in 1:length(path)) {
  #Load the data
  name_file <- paste0("waveoct-dec2016/", path[i], ".nc")
  nc <- nc_open(name_file)
  
  #Extract the data
  #View on column names
  
  #attributes(nc$var)$names #to list out the varibles
  #NOTE:
  #[1] time
  #[3] lat
  #[4] lon 
  #[45] swh (in meters)
  time <- ncvar_get(nc, attributes(nc$var)$names[1])
  swh <- ncvar_get(nc, attributes(nc$var)$names[45])
  lat <- ncvar_get(nc, attributes(nc$var)$names[2])
  lon <- ncvar_get(nc, attributes(nc$var)$names[3])
  wind_speed_alt <-ncvar_get(nc,attributes(nc$var)$names[81])
  
  #plot(lon, lat)
  
  #Extract the data for a coordinate
  index <- which (lon>277.5 & lon<282.5 & lat>2.5 & lat<7.5)
  
  if (length(index) == 0){
    index1[i] <- 1000
  } else {
    index1[i] <- index
  }
  
  lon1[i] <- lon[index1[i]]
  lat1[i] <- lat[index1[i]]
  swh1[i] <- swh[index1[i]]
  wind_speed_alt1[i] <- wind_speed_alt[index1[i]]
  
  #Change time setting: set at 20, coz is 40Hz.
  time0 <- time[20,index1[i]]
  time1[i] <- time0
  
}



#Convert to POSIX
time1 <- as.POSIXct(time1,origin="2000-01-01",tz="GMT")

#Clear workspace
rm(time,nc)

df <- data.frame(index1,lat1,lon1,time1,swh1,wind_speed_alt1)
df1 <- df[which(df$index1 !=1000),]

#Write in csv
write.csv(df1,file="swhOD2016.csv")


