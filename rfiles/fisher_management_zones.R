#' Dec 2015
#' Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
#' install.packages()
#' organize shapefiles from fisher workshops
#' selecting management areas
#' for spatial management project
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

#' clean space
rm(list=ls())
graphics.off()
#' set working directories
pathToSaveShapes = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Original_data/Nov2015_workshop_polygons"
datapath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/Mapas_areas_prioritarias_bentonicos/Poligonos_Mapas_Taller_Nov_2015"
mainpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/"

setwd(mainpath)

#' read species names and prices
species_data = fread("species_names.csv", header=TRUE)
#' read corridor shapefile and empty frame
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

#' list folders
location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

#folders are by community

for (eachfolder in 1:length(location.folders)) {
  setwd(datapath)
  name.sp.folder = location.folders[eachfolder] %>% 
    strsplit(.,"/")%>% unlist %>% .[2]
  print(name.sp.folder)
  paththisLocation = paste(datapath,name.sp.folder,sep="/")
  setwd(paththisLocation)
#' list files 
shape.files = list.files(getwd(),  pattern="*.shp$", full.names=FALSE)

for(eachfile in 1:length(shape.files))
{
  setwd(paththisLocation)
  print(eachfile)
  #' break apart name elements
  shape.file.name = shape.files[eachfile] %>% strsplit(.,"[.]") %>% 
    unlist %>% .[1]
  name.sp = shape.files[eachfile] %>% strsplit(.,"_") %>% unlist %>% .[2] %>% 
  strsplit(.,"[.]") %>% unlist %>% .[1]
  name.loc = shape.files[eachfile] %>% strsplit(.,"_") %>% unlist %>% .[1] 
  
  if(name.loc =="BSJ"){
    name.loc ="SJO"
  } 
  print(name.loc)
  poly.data = readOGR(".", shape.file.name) %>% spTransform(crs.geo)
  #' make new name with updated location name
  shape.file.name = paste(name.loc,name.sp, sep="_")
  #' add new field for category fishing area or conflict
  poly.data$W = 1
  #X11()
  plot(poly.data)
  setwd(pathToSaveShapes)
  writeOGR(poly.data, ".", shape.file.name,driver="ESRI Shapefile",overwrite_layer=TRUE)
  
} # end shapefile
} #end folder

#' write the polygon with the new field
#' the shapefiles have to be modified by hand to add the weight depending on
#' whether they are conflicted areas (2)
#' or preferred fishing zone (1)
 
setwd(pathToSaveShapes)

shape.files = list.files(getwd(),  pattern="*.shp$", full.names=FALSE)

for(eachfile in 1:length(shape.files))
{
  setwd(pathToSaveShapes)
  print(eachfile)
  #' break apart name elements
  shape.file.name = shape.files[eachfile] %>% strsplit(.,"[.]") %>% 
    unlist %>% .[1]
  print(shape.file.name)
 #' read polygon
  poly.data = readOGR(".", shape.file.name) 

  #' make empty rasters  
  conflict.r = mask.raster
  fishing.r = mask.raster
  
  if(length(poly.data)>1){
  #' disaggregate multipart polygon in single part
  #' result is a list
  each.poly = poly.data %>%  disaggregate
#' for each polygon rasterize depending on whether is preferred
#' area (1) or conflict (2)
  for(eachpolygon in 1:length(each.poly)){
  #' exctract polygon
    this.polygon = each.poly@polygons[eachpolygon] %>% SpatialPolygons
    proj4string(this.polygon) <- crs.geo
    this.polygon$W = each.poly$W[eachpolygon]
    
      if(this.polygon$W == 1){
      #' rasterize using mask raster
      fish.r <- rasterize(this.polygon, mask.raster,field=this.polygon@data$W)
      fish.r[is.na(fish.r)] <- 0
      fishing.r = mask(fish.r,poly.r) %>% sum(fishing.r,.)
      fishing.r[is.na(fishing.r)] <- 0
        } else if(this.polygon$W == 2){
      #' rasterize using mask raster
      conf.r <- rasterize(this.polygon, mask.raster,field=this.polygon@data$W)
      conf.r[is.na(conf.r)] <- 0
      conflict.r = mask(conf.r,poly.r) %>% sum(conflict.r,.)
      conflict.r[is.na(conflict.r)] <- 0
    }
    }#end each polygon multipart
  } else if (length(poly.data)==1){
    this.polygon = poly.data
    if(this.polygon$W == 1){
      #' rasterize using mask raster
      fish.r <- rasterize(this.polygon, mask.raster,field=this.polygon@data$W)
      fish.r[is.na(fish.r)] <- 0
      fishing.r = mask(fish.r,poly.r) %>% sum(fishing.r,.)
      fishing.r[is.na(fishing.r)] <- 0
    } else if(this.polygon$W == 2){
      #' rasterize using mask raster
      conf.r <- rasterize(this.polygon, mask.raster,field=this.polygon@data$W)
      conf.r[is.na(conf.r)] <- 0
      conflict.r = mask(conf.r,poly.r) %>% sum(conflict.r,.)
      conflict.r[is.na(conflict.r)] <- 0
    }
  }# end single polygon
  
  if(max(getValues(conflict.r))>0){
  writeRaster(conflict.r, filename=paste(shape.file.name,"CON",sep="_"), format="GTiff", overwrite=TRUE)  
  X11()
  plot(conflict.r)
    #add empty frame and save again
  buffer.r[buffer.r==1] <- 0
  #' add empty frame
  frame.raster = merge(conflict.r,buffer.r)
  frame.raster[frame.raster==255] <- -9999
 mask(frame.raster,buffer.r) %>% writeRaster(., filename=paste(shape.file.name,"CON_COR",sep="_"), format="GTiff", overwrite=TRUE)  
  } 
  if(max(getValues(fishing.r))>0){
  writeRaster(fishing.r, filename=paste(shape.file.name,"FSA",sep="_"), format="GTiff", overwrite=TRUE)  
    X11()
    plot(fishing.r)
  buffer.r[buffer.r==1] <- 0
  #' add empty frame
  frame.raster = merge(fishing.r,buffer.r)
  frame.raster[frame.raster==255] <- -9999
  mask(frame.raster,buffer.r) %>% writeRaster(., filename=paste(shape.file.name,"FSA_COR",sep="_"), format="GTiff", overwrite=TRUE)  
  }
} #endshapefile

# move final rasters to Zonation files directory
setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Original_data")
from.dir <- "/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Original_data"
to.dir   <- "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files"

files    <- list.files(path = from.dir, pattern="_COR",full.names = TRUE, recursive = TRUE)
for (f in files) file.copy(from = f, to = to.dir)

