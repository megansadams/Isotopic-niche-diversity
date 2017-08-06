
## Figure 1 - Provincial diet models

# Adams, M. S., C. N. Service, A. Bateman, M. Bourbonnais, K. A. Artelle, T. Nelson, P. C. Paquet, T. Levi, andC. T. Darimont. 2017. Intrapopulation diversity in isotopic niche over landscapes: Spatial patterns inform conservation ofbear–salmon systems. Ecosphere 8(6):e01843. 10.1002/ecs2.1843

require(sp)
require(maptools)
require(rgeos)
library(rgdal)
library(raster)
library(RColorBrewer)
library(png)
library(GISTools)
library(extrafont)

## Read in shapefiles
prov <- readOGR ("shapefiles", "lwsgbc_Dissolve_Project")
prov <- spTransform(prov,CRS("+init=EPSG:3005"))
WNAm <- readOGR("shapefiles", "provfig_extent2")
WNAm <- spTransform(WNAm,CRS("+init=EPSG:3005"))
north_america_poly<- readOGR(dsn="shapefiles", layer="ne_50m_admin_1_states_provinces_lakes_-_Canada_and_US_only") #for inset
gbpu_dietmodels<- readOGR(dsn="shapefiles", layer="gbpu_newmethods_CORRECT")
coaststudy <- readOGR ("shapefiles", "combined_study_areas_2015_-_no_gt")
coaststudy <- spTransform(coaststudy,CRS("+init=EPSG:3005"))
north_america_poly<- readOGR(dsn="shapefiles", layer="ne_50m_admin_1_states_provinces_lakes_-_Canada_and_US_only") #for inset
van <- readOGR(dsn="shapefiles", layer = "vancouver")


## Generate plot based on diet category per GBPU
model_names_no.d <- c("Eastern: temperate \nmeat + plants + salmon", "Central: boreal meat \n+ plants + salmon", "Interior: temperate \nmeat + plants + salmon", "Coastal: black tailed deer \n+ plants + salmon + intertidal",  "No models (bears extirpated)")

# Megan Calculation codes
#
# a - Coastal - plants, salmon, intertidal (analogous to Mowat and Heard 1)
# b - Eastern - temperate meat + plants + salmon(analogous to Mowat and Heard 2)
# c - Central - boreal meat + plants + salmon (analogous to Mowat and Heard 4)
# e - Fraseretc - temperate meat + plants + salmon

models <- c("b", "c", "e", "a", "na")

plotclr <- brewer.pal(5, 'RdYlBu')
dietcol = data.frame(models, plotclr)
diets = merge(x = gbpu_dietmodels@data, y = dietcol, by.x = "Meg_Calc", by.y = "models", all.x = TRUE)
inset<-readPNG("Resub Figs/inset.png")

pdf("Resub Figs/Final Figs/Fig1_final.pdf", height=5.5, width=6, family = "Helvetica")
windows = (height=5.5, width=6)
close.screen(all.screens = TRUE)
par(mar=c(.2,.2,.2,0.2))
par(xpd=TRUE)

## Create main plot
e <- c(358306, 1796169, 371796, 1729246) #extent from region models

plot(WNAm, border = "grey40", col = "grey80", lwd = 0.5, xlim = e[1:2], ylim = e[3:4])
plot(gbpu_dietmodels[order(gbpu_dietmodels@data$Meg_Calc),], col = as.character(diets$plotclr), border = TRUE, lwd = 0.3, add = TRUE)
points(1202519, 463925.9, pch = 21, col = "grey", bg = "black", cex = 1.5)
text(1355519, 420926, "Vancouver", cex = 1)
legend(310306, 795096, legend = model_names_no.d, fill = plotclr, bty = "n", cex = 0.8, y.intersp=1.4, bg = "white")
north.arrow(xb=360306, yb=905796, len=20000, lab="N")
rect(770306, 715796, 1000306, 915796, density = NULL, angle = 45,
     col = NA, border = "black", lwd = 3)
box("inner")

## Add inset
par(mar=c(0,0,0,0), oma = c(0,0,0,0), fig = c(0.71, 0.998,0.71, 0.998), new=TRUE)
plot(-1000, -1000, xlim = c(0,1), ylim=c(0,1), axes=FALSE, xaxs = "i", yaxs = "i")
rasterImage(inset, 0, 0, 1, 1)
box("plot", lwd=1)

dev.off()