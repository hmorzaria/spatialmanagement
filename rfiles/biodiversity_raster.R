#'Hem Nalini Morzaria Luna
#'hmorzarialuna@gmail.com
#' rscript to create polygons for red clam 
#'clean up the space
rm(list=ls())

#' Automatically install required libraries  

if(!require(dismo)){install.packages("dismo"); library(dismo)}
if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(XML)){install.packages("XML"); library(XML)}
if(!require(jsonlite)){install.packages("jsonlite"); library(jsonlite)}
if(!require(graphics)){install.packages("graphics"); library(graphics)}
if(!require(maps)){install.packages("maps"); library(maps)}
if(!require(maptools)){install.packages("maptools"); library(maptools)}
if(!require(rgeos)){install.packages("rgeos"); library(rgeos)}
if(!require(rgdal)){install.packages("rgdal"); library(rgdal)}
if(!require(magrittr)){install.packages("magrittr"); library(magrittr)}
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(Hmisc)){install.packages("Hmisc"); library(Hmisc)}
if(!require(rgdal)){install.packages("rgdal"); library(rgdal)}
if(!require(readxl)){install.packages("readxl"); library(readxl)}

workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis"
savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files"

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
crs.lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Frame")
poly.r = readOGR(".", "corridor_wetland_polygon")
north.r = raster("Frame_corridor_final.tif")
buffer.r = raster("frame_buffer")
#define projection of data
proj.lcc = proj4string(north.r)
crs.geo <- CRS(proj.lcc)

#' make empty mask raster
#' set background values to 0
e.shape <- extent(north.r)
mask.raster = raster(e.shape,resolution=c(1000,1000),crs=crs.geo)
mask.raster[is.na(mask.raster)] <- 0

setwd(workpath)
#' add empty frame
buffer.r[buffer.r==1] <- 0
frame.raster = raster("species_richness_corridor.tif") %>% 
  projectRaster(., crs=crs.geo) %>% raster::mask(poly.r) %>% 
  raster::resample(.,mask.raster,method='bilinear') %>% 
  merge(.,buffer.r)
#' normalize from 0 - 1
#' norm.data = (frame.raster - cellStats(frame.raster,"min")) / (cellStats(frame.raster,"max")-cellStats(frame.raster,"min"))
#' frame.raster =   merge(norm.data,buffer.r)

setwd(savepath)

frame.raster[frame.raster==255] <- -9999
raster::mask(frame.raster,buffer.r) %>% writeRaster(., "biodiversity_hotspots", format="GTiff", overwrite=TRUE)  

