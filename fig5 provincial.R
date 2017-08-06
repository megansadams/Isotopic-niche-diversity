## Figure 5 - female and male grizzly salmon consumption at a provincial scale

# Adams, M. S., C. N. Service, A. Bateman, M. Bourbonnais, K. A. Artelle, T. Nelson, P. C. Paquet, T. Levi, andC. T. Darimont. 2017. Intrapopulation diversity in isotopic niche over landscapes: Spatial patterns inform conservation ofbear–salmon systems. Ecosphere 8(6):e01843. 10.1002/ecs2.1843

rm(list=ls())
.rs.restartR()

require(sp)
require(maptools)
require(rgeos)
library(rgdal)
library(raster)
library(RColorBrewer)
library(png)

setwd("C:/Users/megan/Dropbox/hotspot ms/Figures/R")
source("col.key.R")

motherLL <- read.csv("mother_Jul1.csv", header = TRUE, stringsAsFactors = TRUE, strip.white = TRUE, na.strings = c("NA", ""))
coordinates(motherLL)=~X+Y
proj4string(motherLL)=CRS("+init=EPSG:4326") #project as lat long
mother_alb <- spTransform(motherLL,CRS("+init=EPSG:3005")) #transform into albers
mother_alb <- as.data.frame(mother_alb)
#mother_alb <- mother_alb #X and Y are the correct, albers coordinates
provdata <- mother_alb[, c("sex", "YEAR", "X50_salmon", "X", "Y")]

full_a<-read.csv("iso_bears2014_forM_ALBERS.csv", header=TRUE, stringsAsFactors = TRUE, strip.white = TRUE, na.strings = c("NA", "")) #Christina's data, median consumption per individual detection, duplicates included. #see 'utm clean.r' for converting utm from z9 and z10 into albers
plot(full_a$utme, full_a$utmn)
names(full_a)[names(full_a) == "X"] <- "unknown"
names(full_a)[names(full_a) == "year"] <- "YEAR"
names(full_a)[names(full_a) == "species"] <- "Species"
names(full_a)[names(full_a) == "sex"] <- "sex"
names(full_a)[names(full_a) == "utmn"] <- "Y"
names(full_a)[names(full_a) == "utme"] <- "X"
names(full_a)[names(full_a) == "X50."] <- "X50_salmon"
plot(full_a$X, full_a$Y)
full_a$sex <-ifelse (full_a$sex=="male", "M", "F") #if male, replace to M, if not male, replace to F.
grizzly <- subset(full_a, Species == "grizzly")
coastdata <- grizzly[, c("sex", "YEAR", "X50_salmon", "X", "Y")]
mother <- rbind(provdata, coastdata)
Males <- subset(mother, sex == "M")
Females <- subset(mother, sex == "F")

#### Generate Figures ####
coast_ecopro <- readOGR("shapefiles", "coastandmtns")
coast_ecopro <- spTransform(coast_ecopro,CRS("+init=EPSG:3005")) #transform into albers
GBR <- readOGR("shapefiles", "gbr_polygon" )
GBR <- spTransform(GBR,CRS("+init=EPSG:3005")) #transform into albers
#bcmask <- spTransform(bcmaskLL,CRS("+init=EPSG:3005")) #transform into albers
band_95border_m <- readOGR("shapefiles", "band_95border_m")
band_95border_f <- readOGR("shapefiles", "band_95border_F")

prov <- readOGR ("shapefiles", "lwsgbc_Dissolve_Project")
prov <- spTransform(prov,CRS("+init=EPSG:3005"))
salmonrivers <- readOGR("shapefiles", "all_spawn_reaches")
salmonrivers <- spTransform(salmonrivers,CRS("+init=EPSG:3005"))
WNAm <- readOGR("shapefiles", "provfig_extent2_nobc")
WNAm <- spTransform(WNAm,CRS("+init=EPSG:3005"))
male <- raster("rasters/NEW_prov_Male_kde_95UDmask.tiff", sep="")
projection(male) <- CRS("+init=epsg:3005") #transform into albers
female <- raster("rasters/NEW_prov_Female_kde_95UDmask.tiff", sep="")
projection(female) <- CRS("+init=epsg:3005") #transform into albers
north_america_poly<- readOGR(dsn="shapefiles", layer="ne_50m_admin_1_states_provinces_lakes_-_Canada_and_US_only") #for inset

maleimg <- readPNG("graphics/male.png")
femaleimg <- readPNG("graphics/female.png")
maleimg<- as.raster(maleimg)
femaleimg<- as.raster(femaleimg)

grizzimg <- readPNG("graphics/grizzly_grph.png")
grizzlyimg<- as.raster(grizzimg)

e <- c(297306, 1856169, 341796, 1729246) #(xmin, xmax, ymin, ymax) #extent


# Build the kernel colour ramp
brk <- seq(0,1, 0.01)
nb <- length(brk) - 1

palette_cols<-brewer.pal(7, 'OrRd')
palette<-colorRampPalette(palette_cols)(nb)

#grey <- sample("grey20", 100, TRUE)


make_main_plot<-function(kernel_data, point_data, label_text){
  plot(WNAm, border = "grey40", col = "grey80", lwd = 0.3, xlim = e[1:2], ylim = e[3:4]) #xlim and ylim values
  plot(kernel_data, ext = e, breaks = brk, col=palette, lab.breaks = brk, zlim = c(0, 1), box = TRUE, legend = FALSE, axes = FALSE, add = TRUE) #255 means fully transparent
    plot(prov, border = "grey20", add = TRUE, lwd=0.6) #angle=45, density=12,
    plot(salmonrivers, add = TRUE, lwd = 0.7, col = "grey30")
    }


#prep for hist
hist_palette <- brewer.pal("Greys", n=9)
m_hist <- subset(mother, sex == "M")
m_count <- length(m_hist$sex)
f_hist <- subset(mother, sex == "F")
f_count <- length(f_hist$sex)
hist_palette <- brewer.pal("Greys", n=9)


#function to make histogram
make_hist<-function(sub_hist, n_disp){
  par(mar=c(0,0,0,0), oma=c(0,0.4,0,0),cex.axis=0.5, tck=-0.02, xpd=NA) 
  hist(sub_hist$X50_salmon,
       freq = TRUE,
       axes = FALSE,
       bty="l", #makes axes touch
       xlab = "",
       ylab = "",
       breaks = seq(0,1, by = 0.05),
       ylim = c(0, 200),
       col = hist_palette[7],
       border = hist_palette[6],
       las = 1,
       main=""
       )

  axis(1, col = "grey40", col.axis = "grey20", at = seq(0,1, 0.5), cex.axis=0.65, mgp=c(3,0.05,0))
  axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 200, 50), las = 1, cex.axis=0.65, mgp=c(2,0.25,0))

  mtext("Salmon Consumption", side = 1,  cex = 0.8, line = 0.8, col = "grey20") 
  mtext(paste("Density (n = ", n_disp, ")",sep=""), side = 2, cex = 0.8, line = 1.2, col = "grey20") 
}

#function for NA map
make_map_inset<-function(){
  par(bg="white", mar=c(2,0,0,0), cex=2)
  plot_xlim = c(-170, -55)
  plot_ylim = c(40, 85.7)
  north_america_poly@data$plot_col=ifelse(north_america_poly@data$name=="British Columbia", "grey90", "grey60") #set BC to white
  plot(north_america_poly, border = "grey40", col = north_america_poly@data$plot_col, lwd = 0.3, xlim = plot_xlim, ylim = plot_ylim) #plot upper part of NA
  bc_extent<-extent(subset(north_america_poly, name=="British Columbia"))
  rect(bc_extent[1], bc_extent[3], bc_extent[2], bc_extent[4], lwd=4) #x1, y1, x2, y2
  arrows(mean(plot_xlim), plot_ylim[1]-3, mean(bc_extent[1:2]), bc_extent[3], length=0.4, lwd=6, xpd=TRUE, col="white") #x1, y1, x2, y2
  arrows(mean(plot_xlim), plot_ylim[1]-3, mean(bc_extent[1:2]), bc_extent[3], length=0.4, lwd=5, xpd=TRUE) #x1, y1, x2, y2
  mtext(side=1, "Area Enlarged", cex=3.8,line=0.7)
}

#function to add legend
add_legend<-function(){
  col.key(at=seq(0, 100, 20),
          labels=seq(0, 1, 0.2),
          cols=palette,
          orientation="horizontal",
          key.loc=c(0.25, 0.75, 0.045, 0.057),
          user_mgp=c(2, 0.2,0),
          axis_label_size=1.3,
          legend_title_size=0.7)
}



#Create and export main plots.  An ugly work-around due to a bug in
#raster package which resets par settings, breaking attempts
#to multipanel
par(mar=c(0,0,0,0), oma=c(0,0,0,0),cex.axis=0.5, tck=-0.02) #this line might be redundant - could try running without it and see if different
png("main-panel-female.png", height=825, width=960, res=300)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
make_main_plot(female, Females, "A) Female Grizzly Bears")
dev.off()

png("main-panel-male.png", height=825, width=960, res=300)
# par(mar=c(0,0,1,0), oma=c(0,0,1,0)) this was your squished male problem here
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
make_main_plot(male, Males, "B) Male Grizzly Bears")
dev.off()

png("inset.png", height=960, width=960, res=300)
make_map_inset()
dev.off()
# For now, manually crop

#load pre-made panels
female_main_fig<-readPNG("main-panel-female.png")
male_main_fig<-readPNG("main-panel-male.png")
inset<-readPNG("Resub Figs/inset_cropped.png")



pdf("Resub Figs/Final Figs/Fig5_final_highres.pdf", width=3.5, height=7, family = "Helvetica")
close.screen(all.screens = TRUE)
par(mar=c(0,0,0,0), oma=c(0,0,0,0),cex.axis=0.5, tck=-0.02)
hist_adj<-0.04 
y_height<-2.078333
  
  m<-rbind(c(0, 1, (y_height-(0.86/10))/y_height, 1), #1 female title bar
           c(0, 1, (y_height-(0.86/10)-0.86)/y_height,(y_height-(0.86/10))/y_height), #2 female main plot window x1, x2, y1, y2
           c(0.1, 0.43, hist_adj+(y_height-(0.86/10)-0.86)/y_height, hist_adj+(y_height-(0.86/10)-0.86+0.3)/y_height), #3 female hist window
           c(0, 1, (y_height-(0.86/10)-0.86-(0.86/10))/y_height,( y_height-(0.86/10)-0.86)/y_height), # 4 male title
           c(0, 1, (y_height-(0.86/10)-0.86-(0.86/10)-0.86)/y_height,(y_height-(0.86/10)-0.86-(0.86/10))/y_height), # 5male main plot window
           c(0.1, 0.43, hist_adj+(y_height-(0.86/10)-0.86-(0.86/10)-0.86)/y_height,hist_adj+(y_height-(0.86/10)-0.86-(0.86/10)-0.86+0.3)/y_height), #6 male hist window
           c(0.6625, 0.998, 0.84, 0.958), #7 NA inset
           c(0, 1, 0, (y_height-(0.86/8)-0.86-(0.86/8)-0.86)/y_height) #8colour key
  )



split.screen(m)
screen(1)
plot(c(0, 1), c(0, 1),  bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

screen(2)
par(mar=c(0,0,0,0), oma=c(0,0,0,0),cex.axis=0.5, tck=-0.02, xpd=NA)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
rasterImage(female_main_fig,0, 0, 100, 100)
rasterImage(grizzimg, 80, 55, 95, 65)
rasterImage(femaleimg, 74, 57, 78, 65)
text(68.5, 48, "a) Female Grizzly", adj=c(0,0), cex=0.8)
screen(3)
make_hist(f_hist, f_count)
screen(4) 
plot(c(0, 1), c(0, 1),  bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
screen(5)
par(mar=c(0,0,0,0), oma=c(0,0,0,0),cex.axis=0.5, tck=-0.02, xpd=NA)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i",axes=FALSE, xlab="", ylab="")
rasterImage(male_main_fig,0, 0, 100, 100)
rasterImage(grizzimg, 80, 80, 95, 90)
rasterImage(maleimg, 73, 83, 79, 91)
text(69, 75, "b) Male Grizzly", adj=c(0,0), cex=0.8)
screen(6)
make_hist(m_hist, m_count)
screen(7)
plot(1, type="n", xlim=c(1, 100), ylim=c(1, 100), xaxs="i", yaxs="i",axes=FALSE, xlab="", ylab="")
rasterImage(inset,0, 0, 100, 100)
box("figure", lwd=1)
screen(8)
add_legend()
dev.off()
close.screen(all.screens = TRUE)
