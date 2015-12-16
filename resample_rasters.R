#' November 2015
#' organize shapefiles from CEDO surveys
#' for fishery intensity project
#' hmorzarialuna@gmail.com
#' install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

datapath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/SDMs"
framepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Frame"

setwd(framepath)
poly.r = readOGR(".", "corridor_wetland_polygon")
north.r = raster("Frame_corridor_final.tif")
buffer.r = raster("frame_buffer")

#define projection of data
proj.lcc = proj4string(north.r)
crs.geo <- CRS(proj.lcc)

e.shape <- extent(north.r)
#ras.rows = nrow(north.r)
#ras.cols = ncol(north.r)

mask.raster = raster(e.shape,resolution=c(1000,1000),crs=crs.geo)

#set background values to 0
mask.raster[is.na(mask.raster)] <- 0

setwd(datapath)

loc.sp=list.files(getwd(), pattern="*.tif$")

all.sp = stack(loc.sp)
#test.raster = raster(all.sp, layer = 1)

FineResampRaster = resample(all.sp,mask.raster,method='bilinear')
r.layers = names(FineResampRaster)
print("Done resampling")
#normalize raster 0-1 and save
FineResampRaster =  mask(FineResampRaster,poly.r)
norm.data = (FineResampRaster - cellStats(FineResampRaster,"min")) / (cellStats(FineResampRaster,"max")-cellStats(FineResampRaster,"min"))

for (eachlayer in 1:length(r.layers)){
  
  name.sp.folder = r.layers[eachlayer]
  r <- raster(FineResampRaster, layer=eachlayer)
  
  writeRaster(r, filename=paste(name.sp.folder,"RES",sep="_"), format="GTiff", overwrite=TRUE)  
}

print("Done saving individual rasters")
setwd(datapath)
file.list=list.files(getwd(),  pattern="*RES.tif$", full.names=FALSE)

for(eachlayer in 1:length(file.list)) {
  setwd(datapath)
  all.sp.loc = raster(file.list[eachlayer])
  
  name.sp.loc = names(all.sp.loc)
  print(names(all.sp.loc))
  #test.layer = raster(all.sp.loc,layer = 1)
  buffer.r[buffer.r==1] <- 0
  #frame.raster = mosaic(test.layer,buffer.r,fun=max)
  #frame.raster = mosaic(all.sp.loc,buffer.r,fun=max)
  frame.raster = merge(all.sp.loc,buffer.r)
  frame.raster[frame.raster==255] <- -9999
  all.masked = mask(frame.raster,buffer.r)
  #writeRaster(test.masked, filename="test_layer", format="GTiff", overwrite=TRUE)  
  
  writeRaster(all.masked, filename=paste(name.sp.loc,"COR",sep="_"), format="GTiff", overwrite=TRUE)  
} # close layers

