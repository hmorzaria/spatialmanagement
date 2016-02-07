#' February 2016
#' resample rasters of species distribution models
#' SDMs were repeated using additonal ocurrence points
#' hmorzarialuna@gmail.com
#' install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))

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


rm(list=ls())

graphics.off()

datapath = "E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Analysis/Maxent_models/SDMs"
savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/SDMs"
framepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Frame"

#' get corridor layer and empty frame
setwd(framepath)
poly.r = readOGR(".", "corridor_wetland_polygon")
north.r = raster("Frame_corridor_final.tif")
buffer.r = raster("frame_buffer")

#define projection of data
proj.lcc = proj4string(north.r)
crs.geo <- CRS(proj.lcc)
#' make empty mask raster
e.shape <- extent(north.r)
mask.raster = raster(e.shape,resolution=c(1000,1000),crs=crs.geo)
#' set background values to 0
mask.raster[is.na(mask.raster)] <- 0

#' make summary of Paralychtidae
setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Climate_change_species_distribution/Analysis/Maxent_models/SDMs/Lenguados")
loc.sp=list.files(getwd(), pattern="*.asc$")
all.sp = stack(loc.sp)
all.sp = projectRaster(all.sp, crs=proj.lcc)
FineResampRaster = raster::resample(all.sp,mask.raster,method='bilinear')
FineResampRaster = mean(FineResampRaster)
r.layers = names(FineResampRaster)
#' normalize raster 0-1 and save
FineResampRaster =  raster::mask(FineResampRaster,poly.r)
norm.data = (FineResampRaster - cellStats(FineResampRaster,"min")) / (cellStats(FineResampRaster,"max")-cellStats(FineResampRaster,"min"))

setwd(savepath)
writeRaster(norm.data, filename="Paralychtidae_RES", format="GTiff", overwrite=TRUE)  

#'run all species separately
setwd(datapath)
#' list rasters of SDMs generated with MaxEnt
loc.sp=list.files(getwd(), pattern="*.asc$")

all.sp = stack(loc.sp)
all.sp = projectRaster(all.sp, crs=proj.lcc)
FineResampRaster = raster::resample(all.sp,mask.raster,method='bilinear')
r.layers = names(FineResampRaster)
print("Done resampling")
#' normalize raster 0-1 and save
FineResampRaster =  raster::mask(FineResampRaster,poly.r)
norm.data = (FineResampRaster - cellStats(FineResampRaster,"min")) / (cellStats(FineResampRaster,"max")-cellStats(FineResampRaster,"min"))

setwd(savepath)
for (eachlayer in 1:length(r.layers)){
#' save each layer as raster  
  name.sp.folder = r.layers[eachlayer]
  r <- raster(FineResampRaster, layer=eachlayer)
  
  writeRaster(r, filename=paste(name.sp.folder,"RES",sep="_"), format="GTiff", overwrite=TRUE)  
}

print("Done saving individual rasters")

file.list=list.files(getwd(),  pattern="*RES.tif$", full.names=FALSE)
#' add frame for Zonation
for(eachlayer in 1:length(file.list)) {
  all.sp.loc = raster(file.list[eachlayer])
  #' add empry frame
  name.sp.loc = names(all.sp.loc)
  print(names(all.sp.loc))
  buffer.r[buffer.r==1] <- 0
  frame.raster = merge(all.sp.loc,buffer.r)
  frame.raster[frame.raster==255] <- -9999
  all.masked = raster::mask(frame.raster,buffer.r)
  
  writeRaster(all.masked, filename=paste(name.sp.loc,"COR",sep="_"), format="GTiff", overwrite=TRUE)  
} # close layers

#'Move SDMs to 
setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/SDMs")
from.dir <- "/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/SDMs"
to.dir   <- "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files"

files    <- list.files(path = from.dir, pattern="RES_COR",full.names = TRUE, recursive = TRUE)
for (f in files) file.copy(from = f, to = to.dir, overwrite = TRUE)
