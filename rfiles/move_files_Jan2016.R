#' Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
#' November 2015
#' Moves Zonation results files from multiple project directories 
#' to one results folder

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files")
from.dir <- "/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files"
to.dir   <- "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Results"

files    <- list.files(path = from.dir, pattern="rank.compressed",full.names = TRUE, recursive = TRUE)
for (f in files) file.copy(from = f, to = to.dir)
