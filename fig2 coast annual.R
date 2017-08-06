## Figure 2 - Annual plots for male black and grizzly kernel regressions

# Adams, M. S., C. N. Service, A. Bateman, M. Bourbonnais, K. A. Artelle, T. Nelson, P. C. Paquet, T. Levi, andC. T. Darimont. 2017. Intrapopulation diversity in isotopic niche over landscapes: Spatial patterns inform conservation ofbear–salmon systems. Ecosphere 8(6):e01843. 10.1002/ecs2.1843

rm(list=ls())
.rs.restartR()

library(png)
library(RColorBrewer)
library(rgdal)
library(raster)
library(sp)
library(maptools)
library(GISTools)


## Read in data for histograms
full_a<-read.csv("iso_bears2014_forM_ALBERS.csv", header=TRUE, stringsAsFactors = TRUE, strip.white = TRUE, na.strings = c("NA", ""))
names(full_a)

## Adjust column names to match older code
names(full_a)[names(full_a) == "year"] <- "Year"
names(full_a)[names(full_a) == "species"] <- "Species"
names(full_a)[names(full_a) == "sex"] <- "Sex"
names(full_a)[names(full_a) == "utmn"] <- "UTMN"
names(full_a)[names(full_a) == "utme"] <- "UTME"
full_a_male <- subset(full_a, Sex == "male")
full_a_female <- subset(full_a, Sex == "female")
black <- subset(full_a_male, Species == "black")
grizz <- subset(full_a_male, Species == "grizzly")
blackf <- subset(full_a_female, Species == "black")
grizzf <- subset(full_a_female, Species == "grizzly")


## Build counter for loop
coast<-data.frame(year = rep(2010:2014, each=2), species = rep(c("black", "grizzly"), times=5), count = c(34, 21, 41, 28, 55, 40, 102, 53, 189, 97), letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), stringsAsFactors = FALSE)


## Call in files for figures
blackimg <- readPNG("graphics/black_grph.png")
grizzimg <- readPNG("graphics/grizzly_grph.png")
blackimg<- as.raster(blackimg)
grizzlyimg<- as.raster(grizzimg)
maleimg <- readPNG("graphics/male.png")
maleimg <- as.raster(maleimg)
coastinset <- readPNG("coast inset fig_thingborder.png")

prov <- readOGR ("shapefiles", "lwsgbc_Dissolve_Project")
prov <- spTransform(prov,CRS("+init=EPSG:3005"))
salmonrivers <- readOGR("shapefiles", "all_spawn_reaches")
salmonrivers <- spTransform(salmonrivers,CRS("+init=EPSG:3005"))


## Build the kernel colour ramp
brk <- seq(0,1, 0.01)
nb <- length(brk) - 1
palette_cols<-brewer.pal(7, 'OrRd')
palette<-colorRampPalette(palette_cols)(nb)


## Figure loop
for (i in 1:nrow(coast)) {
  year=coast$year[i]
  species=coast$species[i]
  df <- subset (full_a_male, (Year == year) & (Species == species))
  raster <- raster(paste("rasters/", species,year,"kde_95UDRmask_a.tiff", sep=""))

  windows(width = 4.8, height = 4.5)
  
  par(mar=c(.2,.2,.2,1.5))

  paddingx = 50000
  paddingy = 6000

  ex = (c((792544.5 - paddingx), (1069610- paddingx), (682565.3 + paddingy), (970603.8 - paddingy))) #(xmin, xmax, ymin, ymax)


  plot(prov, border = "white", lwd = 0.3, xlim = ex[1:2], ylim = ex[3:4])

  plot(raster, breaks = brk, col = palette, lab.breaks = brk, zlim = c(0, 0.8), box = FALSE, legend = FALSE, axes = FALSE, add = TRUE) #break codes allow colour ramps to be standardized across plots, not relative to plots


  plot(prov, border = "grey20", lwd = 0.3, xlim = ex[1:2], ylim = ex[3:4], add = TRUE)

  plot(salmonrivers, add = TRUE, lwd = 0.8, col = "grey30", xlim = ex[1:2], ylim = ex[3:4])

  cex = 1.5

  if (i%in% c(1))
    scalebar(50000, xy=c(965000, 695000), type = "bar", divs = 2, below = "km", lonlat = NULL, label = c(0, 25, 50), lwd = 2) #from {raster}, distance is in meters
  if (i%in% c(1))
    north.arrow(xb=990000, yb=735000, len=4000, lab="N")

  par(mar=c(2.5,2.5,15,15), oma = c(0.1, 0.1, 0, 0), new=TRUE)
  par(tcl = -0.15) #length of ticks marks as a faction of the height of a line of text
  par(mgp = c(1, 0.2, 0)) #location of location of labels, tick mark labels, tick marks
  palette.g <- brewer.pal("Greys", n=9)

  hist(df$X50.,
       freq = FALSE,
       axes = FALSE,
       xlab = "Salmon Consumption",
       #ylab = paste("Frequency (n = ", coast$count[i], ")", sep=""),
       ylab = paste("Density (n = ", coast$count[i], ")", sep=""),
       breaks = seq(0,1, by = 0.05),
       ylim = c(0, 10),
       #ylim = c(0, 15),
       col = palette.g[7],
       border = palette.g[6],
       las = 1,
       main="",

       cex.axis = 0.5, #you'll notice the axes are built slightly differently than prov plot multipanel (ie with xlab than with mtext). Tweak however is easiest for you.
       cex.lab = 0.8
  )
  axis(1, col = "grey40", col.axis = "grey20", at = seq(0,1, 0.5), cex.axis=0.8)#, tck = -0.01) #need to reduce tick size
  axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 10, 5), las = 1, cex.axis=0.8)#, tck = -0.01) #need to reduce tick size


  savePlot(paste("Resub Figs/plots/",species,year,"male_plot", sep=""), type = "pdf")
  savePlot(paste("Resub Figs/plots/",species,year,"male_plot", sep=""), type = "png")

  dev.off()

}

## Build colour ramp and legend
source("col.keyFig2.R")

## Function to add legend
add_legend<-function(){
  col.key(at=seq(0, 100, 20),
          labels=seq(0, 1, 0.2),
          cols=palette,
          orientation="horizontal",
          key.loc=c(0.3, 0.7, 0.043, 0.06),
          user_mgp=c(2, 0.8,0),
          axis_label_size=2.5,
          legend_title_size=1.3)
  
}

## Load pre-made panels
for(i in 1:nrow(coast)){
  year=coast$year[i]
  species=coast$species[i]
  assign(paste(species,year, sep=""), readPNG(paste("Resub Figs/plots/", species, year, "male_plot.png", sep="")))
} 

## Recombine panels as a multipanel. Build from the bottom to the top.
## Total height of the fig (assuming 4.5 width and height of each panel) = 5*4.5+0.75+0.5 = 23.75
## Total width of fig 2*4.5+0.5 = 10

#resize = 0.652

colour_bar_height<-1.8 #* resize
total_width<-9.2 #* resize#this is for the whole figure
total_height<-24.8 #* resize#this is for the whole figure


## m <- builds the layout for the plot. Build from the bottom to the top.
## x1, x2, y1, y2. This is the order these all build. The corners of each windows.
## Unit is inches.
## Each row represents a window. The first window is the colour bar.  Splitscreens like ratios, so this is all built on ratios.
## Where the first window ends in y space is where the next window begins. y1 is lower than y2. x2 is to the right of x1. y2(t-1) is y1(t). x2(t-1) is x1(t)


m<-rbind(c(label_width/total_width, 1, 0, colour_bar_height/total_height), #1 color bar

         c(label_width/total_width, label_width/total_width+(4.5/total_width), colour_bar_height/total_height, (colour_bar_height/total_height)+(4.5/total_height)), #2 black 2014

         c(label_width/total_width+(4.5/total_width),1,colour_bar_height/total_height,(colour_bar_height/total_height)+(4.5/total_height)),#3 grizzly 2014

         c(label_width/total_width, label_width/total_width+(4.5/total_width),(colour_bar_height/total_height)+(4.5/total_height),(colour_bar_height/total_height)+2*(4.5/total_height)), # 4 black 2013

         c(label_width/total_width+(4.5/total_width),1,(colour_bar_height/total_height)+(4.5/total_height),(colour_bar_height/total_height)+2*(4.5/total_height)), # 5 grizzly 2013

         c(label_width/total_width, label_width/total_width+(4.5/total_width), (colour_bar_height/total_height)+2*(4.5/total_height), (colour_bar_height/total_height)+3*(4.5/total_height)), #6 black 2012

         c(label_width/total_width+(4.5/total_width),1, (colour_bar_height/total_height)+2*(4.5/total_height), (colour_bar_height/total_height)+3*(4.5/total_height)), #7 grizzly 2012

         c(label_width/total_width, label_width/total_width+(4.5/total_width), (colour_bar_height/total_height)+3*(4.5/total_height), (colour_bar_height/total_height)+4*(4.5/total_height)), #8 black 2011

         c(label_width/total_width+(4.5/total_width),1, (colour_bar_height/total_height)+3*(4.5/total_height), (colour_bar_height/total_height)+4*(4.5/total_height)), #9 grizzly 2011

         c(label_width/total_width, label_width/total_width+(4.5/total_width), (colour_bar_height/total_height)+4*(4.5/total_height), (colour_bar_height/total_height)+5*(4.5/total_height)), #10 black 2010

         c(label_width/total_width+(4.5/total_width),1, (colour_bar_height/total_height)+4*(4.5/total_height), (colour_bar_height/total_height)+5*(4.5/total_height)) #11 grizzly 2010
)

pdf("Resub Figs/Final Figs/Fig2_final.pdf", width=total_width, height=total_height, family = "Helvetica")
close.screen(all.screens = TRUE)
par(mar=c(0,0,0,0), oma=c(1,1,1,1),cex.axis=0.5, tck=-0.02)

split.screen(m) 
screen(1)
add_legend()

screen(2)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="") #coordinate system within an individual plot is from 1 to 100. This has no axes or labels, its type n, its just a place for the raster image to land.
rasterImage(black2014,0, 0, 100, 100)
text(95, 93, "i)", cex = 1.5, col = "grey20")
box()

screen(3)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(grizzly2014,0, 0, 100, 100)
text(95, 93, "j)", cex = 1.5, col = "grey20")
box()

screen(4)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(black2013,0, 0, 100, 100)
text(95, 93, "g)", cex = 1.5, col = "grey20")
box()

screen(5)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(grizzly2013,0, 0, 100, 100)
text(95, 93, "h)", cex = 1.5, col = "grey20")
box()

screen(6)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(black2012,0, 0, 100, 100)
text(95, 93, "e)", cex = 1.5, col = "grey20")
box()

screen(7)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(grizzly2012,0, 0, 100, 100)
text(95, 93, "f)", cex = 1.5, col = "grey20")
box()

screen(8)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(black2011,0, 0, 100, 100)
text(95, 93, "c)", cex = 1.5, col = "grey20")
box()

screen(9)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(grizzly2011,0, 0, 100, 100)
text(95, 93, "d)", cex = 1.5, col = "grey20")
box()

screen(10)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(black2010,0, 0, 100, 100)
rasterImage(blackimg, 74, 82, 93, 95)
rasterImage(maleimg, 62, 86, 71, 97)
text(80, 78, "a) Male Black", cex = 1.5, col = "grey20")
rasterImage(coastinset, 0, 60, 50, 100) #x1, y1, x2, y2
box()

screen(11)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(grizzly2010,0, 0, 100, 100)
rasterImage(grizzimg, 68, 82, 93, 97)
rasterImage(maleimg, 58, 86, 67, 97)
text(78, 78, "b) Male Grizzly", cex = 1.5, col = "grey20")
box()

dev.off()
close.screen(all.screens = TRUE)
