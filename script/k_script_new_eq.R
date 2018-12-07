c <- read.csv('csv_file/complete_with_k.csv')
cor.test(c$chlor_a,c$co2_flux,
                  method="pearson")
m16 <- read.csv('csv_file/monthly_data_2016_latest.csv')
m17 <- read.csv('csv_file/Monthly_data_2017_latest.csv')
plot(m16$month, m16$PCO2, type='l')
points(m17$month, m17$PCO2,type='l', col='red')
la1 <- lm(c$sst ~c$PCO2)
abline(la1,col='red')
summary(la1)
plot(c$sst,c$PCO2)

#k3 based on Nightingale
#k = ((660/Sc)^0.5)*(0.212*(U^2) + 0.318U)
t = c$sst
Sc = 2116.8 + (-136.25*t)+4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
U = c$WS_site
k3 <- NA
k3 = ((660/Sc)^0.5)*(0.212*(U^2) + 0.318*U)
c <- cbind(c,k3)

#k4 based on Wanniknof 2014
# k = ((660/Sc)^0.5)*(0.251*(U^2))
k4 <- NA
k4 = ((660/Sc)^0.5)*(0.251*(U^2))
c <- cbind(c, k4)

#test
μ = 1.08 *10^-3
p = 1025 
D = 1.92*10^-5
Sc = (μ /(p*D))*100^2  # convert cm^2 to m^2

n = 0.67
β = 11
u. = c$u.
kt <-NA
kt = 1/β * 1/(Sc^n) * u.
c<- cbind(c,kt)

a <- abs(c$k1)
c <-cbind(c, a)
names(c)[96] <- 'abs_k1'
plot(c$month, c$k1)
write.csv(c,file='csv_file/alot_k.csv')

S = 0.10615
k1_co2 <- NA
k1_co2 = c$abs_k1 * S *1000* c$delta_p_p
c <- cbind(c, k1_co2)

k2_co2 <- NA
k2_co2 = kt * S * 1000*c$delta_p_p
c <- cbind(c, k2_co2)

k3_co2 <- NA
k3_co2 = (k3/3600/100) * S *1000* c$delta_p_p
c <- cbind(c, k3_co2)

k4_co2 <- NA
k4_co2 = (k4/3600/100) * S*1000 * c$delta_p_p
c <- cbind(c, k4_co2)

#k based on Mcgillis
k5 <- NA
k5 = ((660/Sc)^0.5)*(0.026*(U^3) + 3.3)
c <- cbind(c,k5)
k5_co2 <- NA
k5_co2 = (k5/3600/100) * S*1000 * c$delta_p_p
c <- cbind(c, k5_co2)

#k based on W n M
k6 <- NA
k6 = ((660/Sc)^0.5)*(0.0283*(U^3)) 
c <- cbind(c,k6)
k6_co2 <- NA
k6_co2 = (k6/3600/100) * S*1000 * c$delta_p_p
c <- cbind(c, k6_co2)
