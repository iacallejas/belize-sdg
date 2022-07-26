####################################################
########## September Avg SST Maps by Ileana Callejas
####################################################

# import libraries
library(raster)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(maps)
library(maptools)

setwd("C:/Users/calle/OneDrive/JPL/SDG 14/Sept_SST_data") #cHANGE TO WHERE TIFF FILES ARE

# Create list of file paths
sept_tifs <- list.files(pattern = ".tif")

# Create a time series raster stack
sept_stack <- stack(sept_tifs)               


## Create a SpatialLines object
countries <- map("world", plot=FALSE) 
countries <- map2SpatialLines(countries, proj4string = CRS("+proj=longlat"))


levelplot(sept_stack,
          main="September SST Averages",
          col.regions=colorRampPalette((brewer.pal(9,"YlOrRd"))),
          par.settings=list(panel.background=list(col="black"))) + 
  latticeExtra::layer(sp.lines(countries, col='darkgray'))
