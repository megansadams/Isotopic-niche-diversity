## Figure 4 - Coastal protected areas for female blacks and grizzlies

# Adams, M. S., C. N. Service, A. Bateman, M. Bourbonnais, K. A. Artelle, T. Nelson, P. C. Paquet, T. Levi, andC. T. Darimont. 2017. Intrapopulation diversity in isotopic niche over landscapes: Spatial patterns inform conservation ofbear–salmon systems. Ecosphere 8(6):e01843. 10.1002/ecs2.1843

#total area of combined spp spatial extents: 43044km2
#total area of conservancies within spatial extents: 11890
#park area % = 0.276
#Done on all years of combined data.

rm(list=ls())
.rs.restartR()

library(raster)
library(rgdal)
library(sp)
library(png)
library(rgeos)


setwd("C:/Users/megan/Dropbox/hotspot ms/Figures/R")

#Read in data for calculating quartiles
full_a<-read.csv("iso_bears2014_forM_ALBERS.csv", header=TRUE, stringsAsFactors = TRUE, strip.white = TRUE, na.strings = c("NA", ""))
names(full_a)[names(full_a) == "year"] <- "Year"
names(full_a)[names(full_a) == "species"] <- "Species"
names(full_a)[names(full_a) == "sex"] <- "Sex"
names(full_a)[names(full_a) == "utmn"] <- "UTMN"
names(full_a)[names(full_a) == "utme"] <- "UTME"

table(full_a$Species, full_a$Sex)
sex <- "female"

#call in raster layers
gb <- raster(paste("rasters/grizzly",sex,"kde_95UDRmask_a.tiff", sep=""))
projection(gb) <- CRS("+init=epsg:3005") #transform into albers
proj4string(gb) <- CRS("+init=EPSG:3005")
bb <- raster(paste("rasters/black",sex,"kde_95UDRmask_a.tiff", sep =""))
projection(bb) <- CRS("+init=epsg:3005")


#call in shapefiles of the spatial extents
gbUD <- shapefile(paste("shapefiles/grizzly",sex,"band_a_95UD.shp", sep=""))
gbUD <- spTransform(gbUD,CRS("+init=EPSG:3005"))
bbUD <- shapefile(paste("shapefiles/black",sex,"band_a_95UD.shp", sep=""))
bbUD <- spTransform(bbUD,CRS("+init=EPSG:3005"))
proj4string(bbUD) <- CRS("+init=EPSG:3005")


# Parks within the relevant species' spatial extent
parks <- readOGR("shapefiles", "TA_CA_SVW_polygon")
parks <- spTransform(parks,CRS("+init=EPSG:3005")) #transform into albers

bbparks <- intersect (bbUD, parks)
proj4string(bbparks) <- CRS("+init=EPSG:3005")

gbparks <- intersect (gbUD, parks)
proj4string(gbparks) <- CRS("+init=EPSG:3005")

#Clipped rasters just within parks
bbparks_est <- crop (bb, extent(bbparks))
bbparks_est <- mask (bbparks_est, bbparks)

gbparks_est <- crop (gb, extent(gbparks))
gbparks_est <- mask (gbparks_est, gbparks)

#Outside parks with the relevant species' spatial extent
bbparkhole <- gDifference(bbUD, bbparks, byid = TRUE) #equivalent of Arc Erase
gbparkhole <- gDifference(gbUD, gbparks, byid = TRUE)

#Clipped rasters just ouside parks
bboutside_est <- crop (bb, extent(bbparkhole))
bboutside_est <- mask (bboutside_est, bbparkhole)

gboutside_est <- crop (gb, extent(gbparkhole))
gboutside_est <- mask (gboutside_est, gbparkhole)

#Calculate quartile values for thresholds. Do we end up needing to do this? See below?

grizz <- subset(full_a, Species == "grizzly")
grizzf <- subset(grizz, Sex == "female")

quantile(grizzf$X50., c(0.5, 0.75))

gbQ50 <- 0.632
gbQ75 <- 0.678 #the upper quartile of our observations is too high - no estimate is above this:) So, run this with median values

black <- subset(full_a, Species == "black")
blackf <- subset(black, Sex == "female")

quantile(blackf$X50., c(0.5, 0.75))


bbQ50<-0.061
bbQ75 <- 0.114


# B L A C K S
#Total number in spatial extent

bb.df <- as.data.frame (bb, na.rm = TRUE)
names(bb.df)[1] <- "salmon"
bb_total <- length(bb.df$salmon)
bb_total_Q50 <- sum(bb.df$salmon >= bbQ50)


bbparks.df <- as.data.frame (bbparks_est, na.rm = TRUE)
names(bbparks.df)[1] <- "salmon"
bb_parks_total <- length(bbparks.df$salmon)
bb_parks_Q50 <- sum(bbparks.df$salmon >= bbQ50)
bb_parks_Q50

prop_bb_parks_Q50 <- bb_parks_Q50 / bb_total_Q50
prop_bb_parks_Q50

median(bbparks.df$salmon)

bboutside.df <- as.data.frame (bboutside_est, na.rm = TRUE)
names(bboutside.df)[1] <- "salmon"
median(bboutside.df$salmon)


# G R I Z Z L I E S

gb.df <- as.data.frame (gb, na.rm = TRUE)
names(gb.df)[1] <- "salmon"
gb_total <- length(gb.df$salmon)
gb_total_Q50 <- sum(gb.df$salmon >= gbQ50)


gbparks.df <- as.data.frame (gbparks_est, na.rm = TRUE)
names(gbparks.df)[1] <- "salmon"
gb_parks_total <- length(gbparks.df$salmon)
gb_parks_Q50 <- sum(gbparks.df$salmon >= gbQ50)
gb_parks_Q50

prop_gb_parks_Q50 <- gb_parks_Q50 / gb_total_Q50
prop_gb_parks_Q50

median(gbparks.df$salmon)

gboutside.df <- as.data.frame (gboutside_est, na.rm = TRUE)
names(gboutside.df)[1] <- "salmon"
median(gboutside.df$salmon)

#  P L O T

#plot proportion of female bears consuming more than a certain amount of salmon within the conservancies

bQ <- quantile(bb.df$salmon, c(0.25, 0.5, 0.75))
gQ <- quantile(gb.df$salmon, c(0.25, 0.5, 0.75))


bb.thresholds = seq(0,max(bb.df$salmon),0.001)
gb.thresholds = seq(0,max(gb.df$salmon),0.001)

quantiles = seq(0, 1, 0.001)

bbprop.above.threshold.parks = bb.thresholds*0
gbprop.above.threshold.parks = gb.thresholds*0

bbprop.above.quantile.parks = quantiles*0
gbprop.above.quantile.parks = quantiles*0

#Quantile loop
for(i in 1:length(quantiles)) {
  T = quantile(bb.df$salmon, quantiles[i])
  total.bb = sum(bb.df$salmon > T)
  parks.bb = sum(bbparks.df$salmon > T)


  bbprop.above.quantile.parks[i] = parks.bb/total.bb
}

for(i in 1:length(quantiles)) {
  T = quantile(gb.df$salmon, quantiles[i])
  total.gb = sum(gb.df$salmon > T)
  parks.gb = sum(gbparks.df$salmon > T)


  gbprop.above.quantile.parks[i] = parks.gb/total.gb
}

#Quantiles plot

blackimg <- readPNG("graphics/black_grph.png")
grizzimg <- readPNG("graphics/grizzly_grph.png")
blackimg<- as.raster(blackimg)
grizzlyimg<- as.raster(grizzimg)

femaleimg <- readPNG("graphics/female.png")
femaleimg <- as.raster(femaleimg)


pdf("Resub Figs/Final Figs/Fig4_final.pdf", height=5, width=6, family = "Helvetica")
close.screen(all.screens = TRUE)
par(mfrow=c(2,1))
par(mar=c(4,5,2,1), oma = c(0.5, 0.5, 0.5, 0.5)) #c(bottom, left, top, right)

par(font = 1)
options(scipen=-1)
plot(-1000,-10000, axes=FALSE,
     xlab='Minimum Salmon Consumption', ylab='Proportion of Area \nwithin Conservancies',
     ylim = c(0, 1), xlim=c(0,1))
lines(quantiles,bbprop.above.quantile.parks,'l',col='grey70', lwd=5)
axis(2, las=1, cex.axis=0.8)
box()
par(new=TRUE)
step=0.25
axis(side=1,
     at=seq(0,1,step),
     labels=signif(c(0,quantile(bb.df$salmon, c(0.25,0.5,0.75, 1))),2),
     cex.axis=0.8)
rasterImage(blackimg, 0.04, 0.74, 0.14, 0.975, angle = 0, interpolate = TRUE)
rasterImage(femaleimg, 0, 0.81, 0.03, 0.97, angle = 0, interpolate = TRUE)
text(0, 0.61, "a) Female Black", cex = 1, font = 1, adj = c(0,NA))

options(scipen=-1)
plot(-1000,-10000, axes=FALSE,
     xlab='Minimum Salmon Consumption', ylab='Proportion of Area \nwithin Conservancies',
     ylim = c(0, 1), xlim=c(0,1))
lines(quantiles,gbprop.above.quantile.parks,'l',col='grey70', lwd=5)
axis(2, las=1, cex.axis=0.8)
box()
#text(0, 0.9, "b)", cex = 1, font = 1, adj = c(0,NA))
par(new=TRUE)
step=0.25
axis(side=1, at=seq(0,1,step),
     #labels=signif(c(0,quantile(gb.df$salmon, seq(step,1,step))),2),
     labels=signif(c(0,quantile(gb.df$salmon, c(0.25,0.5,0.75, 1))),2),
     cex.axis=0.8)
rasterImage(grizzimg, 0.04, 0.72, 0.17, 0.995, angle = 0, interpolate = TRUE)
rasterImage(femaleimg, 0, 0.81, 0.03, 0.97, angle = 0, interpolate = TRUE)
text(0, 0.59, "b) Female Grizzly", cex = 1, font = 1, adj = c(0,NA))

dev.off()