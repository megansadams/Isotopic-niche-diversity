## Figure 6 - Provincial Threshold Comparisons Between Ecoprovince and GBR

# Adams, M. S., C. N. Service, A. Bateman, M. Bourbonnais, K. A. Artelle, T. Nelson, P. C. Paquet, T. Levi, andC. T. Darimont. 2017. Intrapopulation diversity in isotopic niche over landscapes: Spatial patterns inform conservation ofbear–salmon systems. Ecosphere 8(6):e01843. 10.1002/ecs2.1843

rm(list=ls())
.rs.restartR()

library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(png)

setwd("C:/Users/megan/Dropbox/hotspot ms/Figures/R")

coast_ecopro <- readOGR("shapefiles", "coastandmtns")
coast_ecopro <- spTransform(coast_ecopro,CRS("+init=EPSG:3005")) #transform into albers
GBR1 <- readOGR("shapefiles", "gbr_polygon" )
GBR1 <- spTransform(GBR,CRS("+init=EPSG:3005")) #transform into albers

male <- raster("rasters/NEW_prov_Male_kde_95UDmask.tiff", sep="")
projection(male) <- CRS("+init=epsg:3005") #transform into albers
female <- raster("rasters/NEW_prov_Female_kde_95UDmask.tiff", sep="")
projection(female) <- CRS("+init=epsg:3005") #transform into albers

maleimg <- readPNG("graphics/male.png")
femimg <- readPNG("graphics/female.png")
maleimg<- as.raster(maleimg)
femimg<- as.raster(femimg)

options(scipen=99)

# MA L E S

m_thresh_high <- 0.5
m_thresh_low <- 0.25

m_prov <- as.data.frame(male, na.rm = TRUE)
names(m_prov)[1] <- "salmon"
total_m_prov <- length(m_prov$salmon)
m_prov_high <- sum(m_prov$salmon >= m_thresh_high)
m_prov_low <- sum(m_prov$salmon >= m_thresh_low)

male1 <- crop(male, extent(GBR1)) #looks good
male1 <- mask(male1, GBR1)
m_GBR1 <- as.data.frame(male1, na.rm = TRUE)
names(m_GBR1)[1] <- "salmon"
GBR1_NA <- colSums(is.na(m_GBR1))
total_m_GBR1 <- length(m_GBR1$salmon)
m_GBR1_high <- sum(m_GBR1$salmon >= m_thresh_high)
prop_m_GBR1_high <- m_GBR1_high / m_prov_high
prop_m_GBR1_high
m_GBR1_low <- sum(m_GBR1$salmon >= m_thresh_low)
prop_m_GBR1_low <- m_GBR1_high / m_prov_low
prop_m_GBR1_low


male2 <- crop(male, extent(coast_ecopro))
male2 <- mask(male2, coast_ecopro)
m_coast_ecopro <- as.data.frame(male2, na.rm = TRUE)
names(m_coast_ecopro)[1] <- "salmon"
coast_ecopro_NA <- colSums(is.na(m_coast_ecopro))
total_m_coast_ecopro <- length(m_coast_ecopro$salmon)
m_coast_ecopro_high <- sum(m_coast_ecopro$salmon >= m_thresh_high)
prop_m_coast_ecopro_high <- m_coast_ecopro_high / m_prov_high
prop_m_coast_ecopro_high
m_coast_ecopro_low <- sum(m_coast_ecopro$salmon >= m_thresh_low)
prop_m_coast_ecopro_low <- m_coast_ecopro_high / m_prov_low
prop_m_coast_ecopro_low


mcols <- c(paste(m_thresh_high," in GBR"), paste(m_thresh_high," in ecoprovince"), paste(m_thresh_low," in GBR"), paste(m_thresh_low," in ecoprovince"))
mdata <- c(prop_m_GBR1_high, prop_m_coast_ecopro_high, prop_m_GBR1_low, prop_m_coast_ecopro_low )
m_metrics <- rbind (mcols, mdata)
as.data.frame(m_metrics)

mQ <- quantile(m_prov$salmon, c(0.25, 0.5, 0.75))



# F E M A L E S

f_thresh_high <- 0.25
f_thresh_low <- 0.1

f_prov <- as.data.frame(female, na.rm = TRUE)
names(f_prov)[1] <- "salmon"
total_f_prov <- length(f_prov$salmon)
f_prov_high <- sum(f_prov$salmon >= f_thresh_high)
f_prov_low <- sum(f_prov$salmon >= f_thresh_low)

female1 <- crop(female, extent(GBR1))
female1 <- mask(female1, GBR1)
f_GBR1 <- as.data.frame(female1, na.rm = TRUE)
names(f_GBR1)[1] <- "salmon"
GBR1_NA <- colSums(is.na(f_GBR1))
total_f_GBR1 <- length(f_GBR1$salmon)
f_GBR1_high <- sum(f_GBR1$salmon >= f_thresh_high)
prop_f_GBR1_high <- f_GBR1_high / f_prov_high
prop_f_GBR1_high
f_GBR1_low <- sum(f_GBR1$salmon >= f_thresh_low)
prop_f_GBR1_low <- f_GBR1_high / f_prov_low
prop_f_GBR1_low


female2 <- crop(female, extent(coast_ecopro))
female2 <- mask(female2, coast_ecopro)
f_coast_ecopro <- as.data.frame(female2, na.rm = TRUE)
names(f_coast_ecopro)[1] <- "salmon"
coast_ecopro_NA <- colSums(is.na(f_coast_ecopro))
total_f_coast_ecopro <- length(f_coast_ecopro$salmon)
f_coast_ecopro_high <- sum(f_coast_ecopro$salmon >= f_thresh_high)
prop_f_coast_ecopro_high <- f_coast_ecopro_high / f_prov_high
prop_f_coast_ecopro_high
f_coast_ecopro_low <- sum(f_coast_ecopro$salmon >= f_thresh_low)
prop_f_coast_ecopro_low <- f_coast_ecopro_high / f_prov_low
prop_f_coast_ecopro_low


fcols <- c(paste(f_thresh_high," in GBR"), paste(f_thresh_high," in ecoprovince"), paste(f_thresh_low," in GBR"), paste(f_thresh_low," in ecoprovince"))
fdata <- c(prop_f_GBR1_high, prop_f_coast_ecopro_high, prop_f_GBR1_low, prop_f_coast_ecopro_low )
f_metrics <- rbind (fcols, fdata)
as.data.frame(f_metrics)

fQ <- quantile(f_prov$salmon, c(0.25, 0.5, 0.75))


boxplot(f_prov$salmon)
boxplot(m_prov$salmon)



# Q U A N T I L E   P L O T

#plot proportion of bears consuming more than a certain amount of salmon within the GBR and coastal ecoregion

quantiles = seq(0, 1, 0.001)

Fprop.above.quantiles.GBR1 = quantiles*0
Fprop.above.quantiles.coast = quantiles*0
Mprop.above.quantiles.GBR1 = quantiles*0
Mprop.above.quantiles.coast = quantiles*0

for(i in 1:length(quantiles)) {
  T = quantile(f_prov$salmon, quantiles[i])
  prov.f = sum(f_prov$salmon > T)
  GBR1.f = sum(f_GBR1$salmon > T)
  coast.f = sum(f_coast_ecopro$salmon > T)
  
  Fprop.above.quantiles.GBR1[i] = GBR1.f/prov.f
  Fprop.above.quantiles.coast[i] = coast.f/prov.f
  
}
for(i in 1:length(quantiles)) {
  T = quantile(m_prov$salmon, quantiles[i])
  prov.m = sum(m_prov$salmon > T)
  GBR1.m = sum(m_GBR1$salmon > T)
  coast.m = sum(m_coast_ecopro$salmon > T)
  
  Mprop.above.quantiles.GBR1[i] = GBR1.m/prov.m
  Mprop.above.quantiles.coast[i] = coast.m/prov.m
}


# Plot

grizzimg <- readPNG("graphics/grizzly_grph.png")
grizzlyimg<- as.raster(grizzimg)
femaleimg <- readPNG("graphics/female.png")
femaleimg <- as.raster(femaleimg)
maleimg <- readPNG("graphics/male.png")
maleimg <- as.raster(maleimg)


pdf("Resub Figs/Final Figs/Fig6_final.pdf", height=5, width=6, family = "Helvetica")
close.screen(all.screens = TRUE)
par(mfrow=c(2,1))
par(mar=c(4,5,2,1), oma = c(0.5, 2, 0.5, 0.5)) #c(bottom, left, top, right)


par(font = 1)
options(scipen=-1)
plot(-1000,-10000, axes=FALSE,
     xlab='Minimum Salmon Consumption', ylab='Proportion of \nArea within Region',
     ylim = c(0, 1.05), xlim=c(0,1))

lines(quantiles,Fprop.above.quantiles.coast,'l',col='grey20', lwd=5)
#abline(v=mQ, col = "grey40")
axis(2, las =1, cex.axis=0.8)
#abline(v=c(0.25,0.5,0.75), lwd=2, col='grey', lty=3)

lines(quantiles,Fprop.above.quantiles.GBR1, col='grey70',lwd=4)
box()
rasterImage(grizzimg, 0.04, 0.72, 0.17, 0.995, angle = 0, interpolate = TRUE)
rasterImage(femaleimg, 0, 0.81, 0.03, 0.97, angle = 0, interpolate = TRUE)
text(-0.02, 0.61, "a) Female Grizzly", cex = 1, font = 1, adj = c(0,NA))

par(font = 2)
legend(0.59, 0.33, c("Coastal EcoProvince", "GBR"), cex = 0.75, lty = c(1,1), col = c("grey20", "grey70"), lwd = c(3, 3), adj = c(0, 0.6), bty = "n")

par(new=TRUE)
step=0.25
axis(side=1, at=seq(0,1,step),
     labels=signif(c(0,quantile(f_prov$salmon, c(0.25,0.5,0.75, 1))),2), #generates this string at each quartile: 0, 1.3e-08, 0.014, 0.079, 0.7
     cex.axis=0.8)



par(font = 1)
options(scipen=-1)
plot(-1000,-10000, axes=FALSE,
     xlab='Minimum Salmon Consumption', ylab='Proportion of \nArea within Region',
     ylim = c(0, 1.05), xlim=c(0,1))

lines(quantiles,Mprop.above.quantiles.coast,'l',col='grey20', lwd=5)
axis(2, las =1, cex.axis=0.8)

lines(quantiles,Mprop.above.quantiles.GBR1, col='grey70',lwd=4)
box()

rasterImage(grizzimg, 0.04, 0.72, 0.17, 0.995, angle = 0, interpolate = TRUE)
rasterImage(maleimg, -0.01, 0.80, 0.04, 0.97, angle = 0, interpolate = TRUE)
text(-0.02, 0.61, "b) Male Grizzly", cex = 1, font = 1, adj = c(0,NA))

par(font = 2)

par(new=TRUE)
axis(side=1, at=seq(0,1,step),
     #labels=signif(c(0,quantile(m_prov$salmon, seq(step,1,step))),2),
     labels=signif(c(0,quantile(m_prov$salmon, c(0.25,0.5,0.75, 1))),2),
     cex.axis=0.8)

dev.off()