#' Nov 2015
#' organize shapefiles from conflict surveys
#' for spatial management project
#' hmorzarialuna@gmail.com
#' install.packages()
#' 
x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","magrittr","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

pathToSaveShapes = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Original_data"
datapath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Datos/Conflictos_revisados/"
mainpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/"

setwd(mainpath)

#Species names and prices
species_data = fread("species_names.csv", header=TRUE)

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

setwd(datapath)
#' list files for conflict
shape.files = list.files(getwd(),  pattern="MC.shp$", full.names=FALSE)
#' revalue species codes
new.sp = c("JAI"="CALBEL","ANG"="SQUCAL","CAM"= "LITSTY", "CCR" = "PHYERY", "GUI" = "RHIPRO", "LEN" = "PARPLE", "CCN"  = "HEXNIG")

for(eachfile in 1:length(shape.files))
{
  setwd(datapath)
  print(eachfile)
  this.folder = shape.files[eachfile]
  shape.file.name = unlist(strsplit(this.folder,"[.]"))[1]
  
  name.loc = unlist(strsplit(this.folder,"_"))[1]
  
  if(name.loc =="BSJ"){
    name.loc ="SJO"
  } 
  print(name.loc)
  name.sp.folder = unlist(strsplit(this.folder,"_"))[2]
  new.name = new.sp[name.sp.folder] %>% as.character
  
  poly.data = readOGR(".", shape.file.name) 
  proj4string(poly.data)
  
  poly.data = spTransform(poly.data, crs.geo)
  #' assign Weight
  poly.data$W = 1
  X11()
  plot(poly.data)
  #' rasterize using mask raster
  out.r <- rasterize(poly.data, mask.raster,field=poly.data@data$W)
  out.r[is.na(out.r)] <- 0
  out.r = mask(out.r,poly.r)
  setwd(pathToSaveShapes)
  writeRaster(out.r, filename=paste(name.loc,new.name,"rev_CF",sep="_"), format="GTiff", overwrite=TRUE)  
  X11()
  plot(out.r)
}

setwd(pathToSaveShapes)
file.list=list.files(getwd(),  pattern="*rev_CF.tif$", full.names=FALSE)

for(eachlayer in 1:length(file.list)) {
  all.sp.loc = raster(file.list[eachlayer])
  
  name.sp.loc = names(all.sp.loc)
  print(names(all.sp.loc))
  buffer.r[buffer.r==1] <- 0
  #' add empty frame
  frame.raster = merge(all.sp.loc,buffer.r)
  frame.raster[frame.raster==255] <- -9999
  all.masked = mask(frame.raster,buffer.r)
  writeRaster(all.masked, filename=paste(name.sp.loc,"COR",sep="_"), format="GTiff", overwrite=TRUE)  
} # close layers
