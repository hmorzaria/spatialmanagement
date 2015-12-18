#' November 2015
#' resample rasters of species distribution models
#' hmorzarialuna@gmail.com
#' install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","magrittr","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

datapath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/SDMs"
framepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/Frame"

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

setwd(datapath)

#' list rasters of SDMs generated with MaxEnt
loc.sp=list.files(getwd(), pattern="*.tif$")

all.sp = stack(loc.sp)

FineResampRaster = resample(all.sp,mask.raster,method='bilinear')
r.layers = names(FineResampRaster)
print("Done resampling")
#' normalize raster 0-1 and save
FineResampRaster =  mask(FineResampRaster,poly.r)
norm.data = (FineResampRaster - cellStats(FineResampRaster,"min")) / (cellStats(FineResampRaster,"max")-cellStats(FineResampRaster,"min"))

for (eachlayer in 1:length(r.layers)){
#' save each layer as raster  
  name.sp.folder = r.layers[eachlayer]
  r <- raster(FineResampRaster, layer=eachlayer)
  
  writeRaster(r, filename=paste(name.sp.folder,"RES",sep="_"), format="GTiff", overwrite=TRUE)  
}

print("Done saving individual rasters")
setwd(datapath)
file.list=list.files(getwd(),  pattern="*RES.tif$", full.names=FALSE)
#' add frame for Zonation
for(eachlayer in 1:length(file.list)) {
  setwd(datapath)
  all.sp.loc = raster(file.list[eachlayer])
  #' add empry frame
  name.sp.loc = names(all.sp.loc)
  print(names(all.sp.loc))
  buffer.r[buffer.r==1] <- 0
  frame.raster = merge(all.sp.loc,buffer.r)
  frame.raster[frame.raster==255] <- -9999
  all.masked = mask(frame.raster,buffer.r)
  
  writeRaster(all.masked, filename=paste(name.sp.loc,"COR",sep="_"), format="GTiff", overwrite=TRUE)  
} # close layers

