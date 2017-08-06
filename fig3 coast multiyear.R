## Figure 3 - Data combined among years for male black and grizzly kernel regressions

# Adams, M. S., C. N. Service, A. Bateman, M. Bourbonnais, K. A. Artelle, T. Nelson, P. C. Paquet, T. Levi, andC. T. Darimont. 2017. Intrapopulation diversity in isotopic niche over landscapes: Spatial patterns inform conservation ofbear–salmon systems. Ecosphere 8(6):e01843. 10.1002/ecs2.1843

rm(list=ls())
.rs.restartR()

library(png)
library(RColorBrewer)
library(rgdal)
library(raster)
library(sp)
library(maptools)
library(extrafont)
library(GISTools)

setwd("C:/Users/megan/Dropbox/hotspot ms/Figures/R")

## Read in data for histograms
full_a<-read.csv("iso_bears2014_forM_ALBERS.csv", header=TRUE, stringsAsFactors = TRUE, strip.white = TRUE, na.strings = c("NA", ""))

## Adjust column names to match older code
names(full_a)[names(full_a) == "year"] <- "Year"
names(full_a)[names(full_a) == "species"] <- "Species"
names(full_a)[names(full_a) == "sex"] <- "Sex"
names(full_a)[names(full_a) == "utmn"] <- "UTMN"
names(full_a)[names(full_a) == "utme"] <- "UTME"

table(full_a$Species, full_a$Sex)

coastcombo<-data.frame(sex = rep(c("male", "female"), each=2), species = rep(c("black", "grizzly"), times=2), count = c(467, 246, 90, 52), letters = c("a", "b", "c", "d"), stringsAsFactors = FALSE)

## Graphics files
blackimg <- readPNG("graphics/black_grph.png")
grizzimg <- readPNG("graphics/grizzly_grph.png")
blackimg<- as.raster(blackimg)
grizzlyimg<- as.raster(grizzimg)

maleimg <- readPNG("graphics/male.png")
femaleimg <- readPNG("graphics/female.png")
maleimg<- as.raster(maleimg)
femaleimg<- as.raster(femaleimg)

## Shapefiles
prov <- readOGR ("shapefiles", "lwsgbc_Dissolve_Project")
prov <- spTransform(prov,CRS("+init=EPSG:3005"))
salmonrivers <- readOGR("shapefiles", "all_spawn_reaches")
salmonrivers <- spTransform(salmonrivers,CRS("+init=EPSG:3005"))



# Build the kernel colour ramp
brk <- seq(0,1, 0.01)
nb <- length(brk) - 1
palette_cols<-brewer.pal(7, 'OrRd') #was 8 in the original submission
palette<-colorRampPalette(palette_cols)(nb)


## Figure loop
for (i in 1:nrow(coastcombo)) {
  windows(width = 4.8, height = 4.5)  
  sex=coastcombo$sex[i]
  species=coastcombo$species[i]
  df <- subset (full_a, (Sex == sex) & (Species == species))
  raster <- raster(paste("rasters/", species,sex,"kde_95UDRmask_a.tiff", sep=""))

  par(mar=c(.2,.2,.2,1.5))

  paddingx = 50000
  paddingy = 6000

  ex = (c((792544.5 - paddingx), (1069610- paddingx), (682565.3 + paddingy), (970603.8 - paddingy))) #(xmin, xmax, ymin, ymax)

  plot(prov, border = "white", lwd = 0.3, xlim = ex[1:2], ylim = ex[3:4])

  plot(raster, breaks = brk, col = palette, lab.breaks = brk, zlim = c(0, 0.8), box = FALSE, legend = FALSE, axes = FALSE, add = TRUE) #break codes allow colour ramps to be standardized across plots, not relative to plots


  plot(prov, border = "grey20", lwd = 0.3, xlim = ex[1:2], ylim = ex[3:4], add = TRUE)

  plot(salmonrivers, add = TRUE, lwd = 0.8, col = "grey30", xlim = ex[1:2], ylim = ex[3:4])

  if (i%in% c(1))
    scalebar(50000, xy=c(965000, 695000), type = "bar", divs = 2, below = "km", lonlat = NULL, label = c(0, 25, 50), lwd = 2) #from {raster}, distance is in meters
  if (i%in% c(1))
    north.arrow(xb=990000, yb=735000, len=4000, lab="N")


  par(mar=c(2.5,2.5,15,15), oma = c(0.1, 0.1, 0, 0), new=TRUE)
  par(tcl = -0.15) #length of ticks marks as a faction of the height of a line of text
  par(mgp = c(1, 0.1, 0)) #location of location of labels, tick mark labels, tick marks
  palette.g <- brewer.pal("Greys", n=9)

  hist(df$X50.,
       freq = FALSE,
       axes = FALSE,
       xlab = "Salmon Consumption",
       ylab = paste("Density (n =", coastcombo$count[i], ")", sep=""),
       breaks = seq(0,1, by = 0.05),
       ylim = c(0, 10),
       col = palette.g[7],
       border = palette.g[6],
       las = 1,
       main=""

  )
  #


  axis(1, col = "grey40", col.axis = "grey20", at = seq(0,1, 0.5), cex.axis=0.8, mgp=c(3,0.05,0))#, tck = -0.01) #need to reduce tick size
  axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 10, 5), las = 1, cex.axis=0.8, mgp=c(3,0.25,0))#, tck = -0.01) #need to reduce tick size

savePlot(paste("Resub Figs/plots/",species,sex,"combined_plot", sep=""), type = "png", res=300)

dev.off()

}

#colour bar legend
source("col.keyFig3.R")
#function to add legend
add_legend<-function(){
  col.key(at=seq(0, 100, 20),
          labels=seq(0, 1, 0.2),
          cols=palette,
          orientation="horizontal",
          key.loc=c(0.3, 0.7, 0.043, 0.06),
          user_mgp=c(2, 0.2,0),
          axis_label_size=2,
          legend_title_size=1.1)

}


#load pre-made panels
for(i in 1:nrow(coastcombo)){
sex=coastcombo[i,]$sex
species=coastcombo[i,]$species
assign(paste(species,sex, sep=""), readPNG(paste("Resub Figs/plots/", species, sex, "combined_plot.png", sep="")))
}


#recombine as multipanel (from bottom to top this time)
#total height of fig (assuming 4.5 width and height of each panel), 2*4.5+0.75+0.5
#total width of fig 2*4.5+0.5

colour_bar_height<-0.8
label_width<-0.1
total_width<-9.1
total_height<-10.3
m<-rbind(c(label_width/total_width, 1, 0, colour_bar_height/total_height), #1 colour bar
         c(label_width/total_width, label_width/total_width+(4.5/total_width), colour_bar_height/total_height, (colour_bar_height/total_height)+(4.5/total_height)), #2 black female x1, x2, y1, y2
         c(label_width/total_width+(4.5/total_width),1,colour_bar_height/total_height,(colour_bar_height/total_height)+(4.5/total_height)), #3 black male
         c(label_width/total_width, label_width/total_width+(4.5/total_width),(colour_bar_height/total_height)+(4.5/total_height),(colour_bar_height/total_height)+2*(4.5/total_height)), # 4 grizzly female
         c(label_width/total_width+(4.5/total_width),1,(colour_bar_height/total_height)+(4.5/total_height),(colour_bar_height/total_height)+2*(4.5/total_height))
         )
)

pdf("Resub Figs/Final Figs/Fig3_final.pdf", width=total_width, height=total_height, family = "Helvetica")
close.screen(all.screens = TRUE)
par(mar=c(0,0,0,0), oma=c(1,1,1,1),cex.axis=0.5, tck=-0.02)

split.screen(m)
screen(1)
add_legend()

screen(2)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(blackfemale,0, 0, 100, 100)
rasterImage(blackimg, 75, 80, 94, 93)
rasterImage(femaleimg, 68, 85, 74, 95) #x1, y1, x2, y2
#rasterImage(blackimg, 68, 80, 87, 93)
#rasterImage(femaleimg, 90, 85, 96, 95) #x1, y1, x2, y2
text(79, 77, "c) Female Black", cex = 1.5, col = "grey20")
box()

screen(3)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(blackmale,0, 0, 100, 100)
rasterImage(blackimg, 77, 80, 96, 93)
rasterImage(maleimg, 66, 84, 75, 95)
#rasterImage(blackimg, 66, 80, 85, 93)
#rasterImage(maleimg, 87, 84, 96, 95)
text(81, 77, "d) Male Black", cex = 1.5, col = "grey20")
box()

screen(4)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(grizzlyfemale,0, 0, 100, 100)
rasterImage(grizzimg, 70, 82, 95, 97)
rasterImage(femaleimg, 63, 87, 69, 97)
#rasterImage(grizzimg, 63, 82, 88, 97)
#rasterImage(femaleimg, 90, 87, 96, 97)
text(77.5, 79, "a) Female Grizzly", cex = 1.5, col = "grey20")
box()

screen(5)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(grizzlymale,0, 0, 100, 100)
rasterImage(maleimg, 60, 86, 69, 97)
rasterImage(grizzimg, 69, 82, 94, 97)
#rasterImage(grizzimg, 60, 82, 85, 97)
#rasterImage(maleimg, 87, 86, 96, 97)
text(79, 77, "b) Male Grizzly", cex = 1.5, col = "grey20")
box()

dev.off()
close.screen(all.screens = TRUE)

