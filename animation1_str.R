# attempt 1 of doing an animation for the results from Stroke HIA with DOH data (2000-2015) 
# 
# Part 1 - RasterVis and output the maximum concentration for each raster file
# Need to do RasterVis and ggplot separately as RasterVis will interfere with ggplot package
install.packages("animation")
library(raster)
library(rgdal)
library(rasterVis)
library(animation)
library(classInt)

# setwd("/Users/mdcc/Box/dc_hia/conc_vdk_tif/dc_pm25_vdk")
# dc_2014 <- raster("/Users/mdcc/Box/dc_hia/conc_vdk_tif/dc_pm25_vdk/vdk_2014_dc.tif")
# plot(dc_2014)


setwd("/Users/mdcc/Box/dc_hia/HIA_calculations_mc/results/lc_doh/paf")

# Stack files for 19 years of pm2.5 data for DC
# s <- list.files(pattern = "\\.tif*", full.names=TRUE)
# s <- stack(s)
list <- list.files(pattern = "\\.tif*", full.names=TRUE)
  print(list)
list <- stack(list) # if it doesnt work, go to folder and remove .tif.aux.xml

# library(sp)

# Shapefile outline of DC (outer boundary and neighborhoods)
setwd("/Users/mdcc/Box/dc_hia/HIA_calculations_mc/run/clip_dc")
  getwd()
  dc <- readOGR(dsn=getwd(), layer='dc_boundary')
  plot(dc)
  
  dc_neigh <- readOGR(dsn=getwd(), layer='png_dc')
  plot(dc_neigh)
  print(dc_neigh)

  dc_ward <- readOGR(dsn=getwd(), layer='ward_dc')
  plot(dc_ward)
  print(dc_ward)
  
  
# Color scale
col.2 <- colorRampPalette(c("#556270", "#4ECDC4", "#C7F464", "#FF6B6B", "#C44D58"))


# Create the dates to coordinate with the raster files
itemizeDates <- function(startDate="2000", endDate="2005",
                         format="%Y") {
  out <- seq(as.Date(startDate, format=format),
             as.Date(endDate, format=format), by="years") 
  format(out, format)
}


# Create a dataframe of titles for each date to scroll across the top
titles <- itemizeDates(startDate="2000", endDate="2015")
title <- as.data.frame(titles)
  print(title)

# This is a loop that will go through each raster file in the folder and add it to the animation
# setwd("/Users/mdcc/Box/dc_hia/conc_vdk_tif/dc_pm25_vdk")
setwd("/Users/mdcc/Box/dc_hia/HIA_calculations_mc/results/lc_doh/paf")

library(ggplot2)

lc_gif <- saveGIF({
  for(i in c(1:nlayers(list))){    # change letter of list if needed
    l <- levelplot(list[[i]],      # ^ here too
                   margin = FALSE,
                   main = list(paste0('Percent Attributable ',title$titles[i],sep=' '), fontfamily = "serif",cex=1,font=6),
                   col.regions = col.2,
                   # sub = list('ug/m^3'),
                   at = seq(0, 5, len = 101),
                   scales = list(draw = FALSE),
                   par.settings = list(panel.background=list(col='black'),
                                       alpha=0.3,
                                       axis.text=list(fontfamily="Arial"),
                                       par.xlab.text=list(fontfamily="Arial"),
                                       par.ylab.text=list(fontfamily="Arial"),
                                       par.main.text=list(fontfamily="Arial"),
                                       par.sub.text=list(fontfamily="Arial")),
                   xlab=NULL,
                   ylab=NULL,
                   xlim=c(-77.1198, -76.90915 ), # from extent(s)
                   ylim=c(38.79164, 38.99597),
                   colorkey=list(space='bottom',
                                axis.line=list(col='white'), 
                                width=1)) + layer(sp.polygons(dc_ward, fill='white', alpha=0.1)) # dc 51 neighborhoods
    plot(l)
  }
}, interval=0.8, movie.name="lc_dc_00-15.gif") # save as one animation


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
write.csv(min.max, 'min.max.2010.2016.pm.dc.csv')

