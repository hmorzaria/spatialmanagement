#' Hem Nalini Morzaria
#' February 2016
#' Convert important fishing areas from Zonation models into polygons
#' Converts raster values <0.95 into 0 and>0.95 
#' then transforms to polygon

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
if(!require(readxl)){install.packages("readxl"); library(readxl)}


rm(list=ls())

graphics.off()

datapath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Results"
polygonpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Results/original_polygons"
newpolygonpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Results/polygons"
finalpolygonpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Results/final_polygons"

#'initially take Zonation models and make polygons
setwd(datapath)
files    <- list.files(pattern="rank.compressed")
zonation.models = stack(files)
#'set values lower than 0.95 to 0
zonation.models[zonation.models<0.95] <- 0
#'set values > 0.95 to 1
zonation.models[zonation.models>0.95] <- 1

r.layers = names(zonation.models)

setwd(polygonpath)

for (eachlayer in 1:length(r.layers)){
  #' save each layer as raster  
  name.sp.folder = r.layers[eachlayer] %>% strsplit(.,"[.]") %>% 
    unlist %>% .[1]
  this.raster <- raster(zonation.models, layer=eachlayer)
  
raster.pol = rasterToPolygons(this.raster, fun = function(x) {x==1})
writeOGR(raster.pol, ".", name.sp.folder,driver="ESRI Shapefile",overwrite_layer=TRUE)
}

#'polygons were edited directly in ArcGIS to eliminate spurious areas
#'kept polygons near selected areas by fishers
#'dissolve into single polygon
setwd(newpolygonpath)
shape.files    <- list.files(pattern="*.shp$")

for (eachfile in 1:length(shape.files)){
  setwd(newpolygonpath)
    this.shape.file = shape.files[eachfile] %>% strsplit(.,"[.]") %>% 
    unlist %>% .[1]
  poly.file = readOGR(".", this.shape.file)
  
  poly.file$region = 1

region <- gUnaryUnion(poly.file, id = poly.file@data$region)

# Make a data frame that meets the requirements above:

df<- data.frame(id = getSpPPolygonsIDSlots(region))
row.names(df) <- getSpPPolygonsIDSlots(region)

# Make spatial polygon data frame
spdf <- SpatialPolygonsDataFrame(region, data =df)

setwd(finalpolygonpath)
writeOGR(spdf, ".", paste(this.shape.file,"final",sep="_"),driver="ESRI Shapefile",overwrite_layer=TRUE)

}
