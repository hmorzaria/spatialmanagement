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

workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management"
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
#' polygons received from Francisco Fernandez COBI on 7/30/2015
#' Zona de manejo integral Puerto Libertad
# Vértice Latitud Longitud
# A       29.877200       -112.659350
# B       29.875167       -112.662867
# C       29.870618       -112.656243
# D       29.868504       -112.659589
# 
# Zona de manejo integral Bahia de Kino
# Vértice Latitud Longitud
# A       28.895793       -112.048435
# B       28.894307       -112.053376
# C       28.900324       -112.055267
# D       28.901763       -112.052142
# 
# Reserva de almeja en Puerto Libertad
# 
# A       29.836510       -112.662689
# B       29.869839       -112.669733
# C       29.875464       -112.664417
# D       29.835000       -112.654000

#' Zona de manejo integral Puerto Libertad
#' Zona de manejo integral Bahia Kino
#' Reserva de almeja en Puerto Libertad
p <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(-112.659350, -112.662867, -112.659589,-112.656243),c(29.877200,29.875167,29.868504,29.870618)))), "ZMI_PL"),
                          Polygons(list(Polygon(cbind(c(-112.048435, -112.053376, -112.055267,-112.052142),c(28.895793,28.894307,28.900324,28.901763)))), "ZMI_BK"),
                          Polygons(list(Polygon(cbind(c(-112.662689, -112.669733, -112.664417,-112.654000),c(29.836510,29.869839, 29.875464, 29.835000)))), "RA_PL")))
class(p)    

# Create a dataframe and display default rownames
( p.df <- data.frame( ID=1:length(p)) ) 
rownames(p.df)

# Extract polygon ID's
( pid <- sapply(slot(p, "polygons"), function(x) slot(x, "ID")) )

# Create dataframe with correct rownames
( p.df <- data.frame( ID=1:length(p), row.names = pid) )    

# Try coersion again and check class
p <- SpatialPolygonsDataFrame(p, p.df)
class(p) 
p$W = 1
setwd(savepath)
proj4string(p) <- crs.geo.wgs
poly.data = spTransform(p, crs.geo)

writeOGR(poly.data, ".", "Poligonos_almeja", driver="ESRI Shapefile", overwrite_layer = TRUE)


#' rasterize using mask raster
out.r <- rasterize(poly.data, mask.raster,field=poly.data@data$W)
out.r[is.na(out.r)] <- 0
out.r = raster::mask(out.r,poly.r)
setwd(savepath)

buffer.r[buffer.r==1] <- 0
#' add empty frame
frame.raster = merge(out.r,buffer.r)
frame.raster[frame.raster==255] <- -9999
raster::mask(frame.raster,buffer.r) %>% writeRaster(., "clam_areas", format="GTiff", overwrite=TRUE)  

