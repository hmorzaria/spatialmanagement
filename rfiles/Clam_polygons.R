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
if(!require(spocc)){install.packages("spocc"); library(spocc)}
if(!require(rgdal)){install.packages("rgdal"); library(rgdal)}
if(!require(ridigbio)){install.packages("ridigbio"); library(ridigbio)}
if(!require(rvertnet)){install.packages("rvertnet"); library(rvertnet)}
if(!require(ecoengine)){install.packages("ecoengine"); library(ecoengine)}
if(!require(rbison)){install.packages("rbison"); library(rbison)}
if(!require(rgbif)){install.packages("rgbif"); library(rgbif)}
if(!require(rebird)){install.packages("rebird"); library(rebird)}
if(!require(readxl)){install.packages("readxl"); library(readxl)}

workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management"
savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation"

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
crs.lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

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
p <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(29.877200,29.875167,29.868504,29.870618),c(-112.659350, -112.662867, -112.659589,-112.656243)))), "ZMI_PL"),
                          Polygons(list(Polygon(cbind(c(28.895793,28.894307,28.900324,28.901763),c(-112.048435, -112.053376, -112.055267,-112.052142)))), "ZMI_BK"),
                          Polygons(list(Polygon(cbind(c(29.836510,29.869839, 29.875464, 29.835000),c(-112.662689, -112.669733, -112.664417,-112.654000)))), "RA_PL")))
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

setwd(savepath)

writeOGR(p, ".", "Poligonos_almeja", driver="ESRI Shapefile")
