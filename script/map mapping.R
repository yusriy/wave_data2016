#Map Mapping
# install.packages('marmap')
library(marmap)

Pulau_Pinang <- getNOAA.bathy(lon1=97,lon2=103,
                              lat1=2,lat2=8,
                              resolution=1)
summary(Pulau_Pinang)

#Plot
jpeg(filename="figs/Study_Area.jpg",height=8,width = 8,
     bg="white",units='cm', res=360)
par(family='serif', mar=c(3.1,3.1,0.6,0.6))
plot(Pulau_Pinang,image=TRUE,
     xlim=c(100.1,100.5), ylim=c(5.35,5.65),
     lwd = c(1,0.8,0.6),lty = c(1,1,1),
     col = c("black", "darkgrey", "darkblue"),
     drawlabel= c(TRUE,TRUE,TRUE),
     cex.lab=1, cex.axis=0.6,
     xlab='',ylab='')
mtext(side = 1,'Longitude', line = 2)
mtext(side = 2, 'Latitude', line = 2)
points(100.2025,5.4750, pch = 19, col = 'green', cex =1)
scaleBathy(Pulau_Pinang, deg = 0.045, x = "bottomright", inset = 5,
           cex =0.8)
dev.off()


