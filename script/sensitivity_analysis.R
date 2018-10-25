#Filter data
#Create temp data frame
merged_df3 <- merged_df2
#based on co2 flux (-ve value)
merged_df3$co2_flux[merged_df3$co2_flux >= 0] <- NA
merged_df3$wind_speed[merged_df3$co2_flux >= 0] <- NA
merged_df3$swh1[merged_df3$co2_flux >= 0] <- NA

#Plot graph
plot(merged_df3$co2_flux~merged_df3$date, pch=19)
plot(merged_df3$swh1,merged_df3$co2_flux, pch=19, xlim=c(0,4),ylim=c(0,-3))
ggscatter(merged_df3,x="swh1",y="co2_flux",
          add="reg.line",conf.int = TRUE,
          xlab="swh(m)",ylab="CO2 flux",
          xlim=c(0,5), ylim=c(0,-3))
        
