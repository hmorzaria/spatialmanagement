#' Nov 2015
#' organize shapefiles from conflict surveys
#' for spatial management project
#' hmorzarialuna@gmail.com
#' install.packages()
#' 
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
#' revalue species codes
new.sp = c("CALBEL"="CALBEL","HEXNIG"="HEXNIG","Escarlopa2"="SPOCAL","Escarlopa1"="SPOCAL","Pulpo"="OCTSPP","PULPO"="OCTSPP", "JAI"="CALBEL","ANG"="SQUCAL","CAM"= "LITSTY", "CROSA2" = "PHYERY","CROSA1" = "PHYERY", "CROSA" = "PHYERY", "CCR" = "PHYERY", "GUI" = "RHIPRO", "LEN" = "PARPLE", "CCN"  = "HEXNIG")

folders = list.dirs(full.names = FALSE)
indx.folders = grep('Conflictos|conflictos', folders)
conflict.folder = folders[indx.folders]
for (eachfolder in 1:length(conflict.folder)){
  
  this.folder = conflict.folder[eachfolder] %>% 
 paste(datapath,.,sep="")
  
  setwd(this.folder)
  
  shape.files = list.files(getwd(),  pattern="*.shp$", full.names=FALSE) 
  
for(eachfile in 1:length(shape.files))
{
  setwd(this.folder)
  print(eachfile)
  shape.file.name = shape.files[eachfile] %>% strsplit("[.]") %>% 
    unlist %>% .[1]
  
  name.loc = shape.files[eachfile] %>% strsplit("_") %>% 
    unlist %>% strsplit("[.]") %>% unlist %>% .[1]
  
  if(name.loc =="BSJ"){
    name.loc ="SJO"
  } 
  print(name.loc)
  name.sp.folder = shape.files[eachfile] %>% strsplit("_") %>% 
    unlist %>% strsplit("[.]") %>% unlist %>% .[2]
  
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
  out.r = raster::mask(out.r,poly.r)
  setwd(pathToSaveShapes)
  writeRaster(out.r, filename=paste(name.loc,new.name,"rev_CF",sep="_"), format="GTiff", overwrite=TRUE)  
  X11()
  plot(out.r)
}
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
  all.masked = raster::mask(frame.raster,buffer.r)
  writeRaster(all.masked, filename=paste(name.sp.loc,"COR",sep="_"), format="GTiff", overwrite=TRUE)  
} # close layers

setwd(pathToSaveShapes)

from.dir <- "/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Original_data"
to.dir   <- "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files"

files    <- list.files(path = from.dir, pattern="*rev_CF_COR.tif$",full.names = TRUE, recursive = TRUE)
for (f in files) file.copy(from = f, to = to.dir)

