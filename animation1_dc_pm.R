# Part 1 - RasterVis and output the maximum concentration for each raster file
# Need to do RasterVis and ggplot separately as RasterVis will interfere with ggplot package
install.packages("animation")

library(raster)
library(rgdal)
library(rasterVis)
library(animation)
library(classInt)

setwd("/Users/mdcc/Box/dc_hia/conc_vdk_tif/dc_pm25_vdk")
  dc_2014 <- raster("/Users/mdcc/Box/dc_hia/conc_vdk_tif/dc_pm25_vdk/vdk_2014_dc.tif")
  plot(dc_2014)

# Stack files for 19 years of pm2.5 data for DC
s <- list.files(pattern = "\\.tif*", full.names=TRUE)
s <- stack(s)
  print(s)
  range(s)
  maxValue(s)

# Shapefile outline of DC (outer boundary and neighborhoods)
setwd("/Users/mdcc/Box/dc_hia/HIA_calculations_mc/run/clip_dc")
  getwd()
dc <- readOGR(dsn=getwd(), layer='dc_boundary')
  plot(dc)
dc_neigh <- readOGR(dsn=getwd(), layer='png_dc')
  plot(dc_neigh)

# Color scale
col.2 <- colorRampPalette(c("#556270", "#4ECDC4", "#C7F464", "#FF6B6B", "#C44D58"))

# Create the dates to coordinate with the raster files
itemizeDates <- function(startDate="2000", endDate="2015",
                         format="%Y") {
  out <- seq(as.Date(startDate, format=format),
             as.Date(endDate, format=format), by="years") 
  format(out, format)
}

# Create a dataframe of titles for each date to scroll across the top
titles <- itemizeDates(startDate="2000", endDate="2015")
title <- as.data.frame(titles)

# This is a loop that will go through each raster file in the folder and add it to the animation
setwd("/Users/mdcc/Box/dc_hia/conc_vdk_tif/dc_pm25_vdk")

dc_gif <- saveGIF({
  for(i in c(1:nlayers(s))){
    l <- levelplot(s[[i]],
                   margin = FALSE,
                   main = list(paste0('Annual Mean PM2.5: ',title$titles[i],sep=' '), 
                               fontfamily = "spectral", cex=1, font=2, size=10),
                   col.regions = col.2,
                   # sub = list('ug/m^3'),
                   at = seq(8, 17.5, len = 101),
                   scales = list(draw = FALSE),
                   par.settings = list(panel.background=list(col='black'),
                                     alpha=0.4,
                                     axis.text=list(fontfamily="Arial"),
                                     par.xlab.text=list(fontfamily="Arial"),
                                     par.ylab.text=list(fontfamily="Arial"),
                                     par.main.text=list(fontfamily="Arial"),
                                     par.sub.text=list(fontfamily="Arial")),
                   xlab=NULL,
                   ylab=NULL,
                   xlim=c(-77.1198, -76.90915 ), # from extent of list (s)
                   ylim=c(38.79164, 38.99597),
                   colorkey=list(
                     space='bottom',
                     axis.line=list(col='white'),
                     # width=1))+ layer(sp.polygons(dc, fill='white', alpha=0.2)) # dc outer boundary
                     width=1))+ layer(sp.polygons(dc_neigh, fill='white', alpha=0.1)) # dc 51 neighborhoods
    plot(l)
  }
}, interval=0.8, movie.name="vdk_dc_00-15.gif") # save as one animation


# Output a file that will give the date and the max and minimum value for each year

d <- as.matrix(titles)
mat <- matrix(nrow=nlayers(s),ncol=2)

for(i in c(1:nlayers(s))){
  mat[,1] <- minValue(s)
  mat[,2] <- maxValue(s)
  }

mat <- cbind(mat, d)
head(mat)

min.max <- as.data.frame(mat)
head(min.max)

names(min.max) <- c('Min','Max','Date')
min.max$Min <- as.numeric(as.character(min.max$Min))
min.max$Max <- as.numeric(as.character(min.max$Max))
min.max$Date <- as.character(min.max$Date)

setwd("/Users/mdcc/Box/dc_hia/HIA_calculations_mc/results/animations/")
getwd()

write.csv(min.max, 'min.max.2000.2015.pm.dc.csv')

