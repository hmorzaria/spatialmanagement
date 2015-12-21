#' Dec 2015
#' run zonation models
#' for spatial management project
#' hmorzarialuna@gmail.com
#' install.packages()
#' 
x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","magrittr","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

rm(list=ls())

zonationfiles = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/Zonation_files"
location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)
