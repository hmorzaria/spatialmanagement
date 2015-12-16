x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Zonation_files")
from.dir <- "/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Zonation_files"
to.dir   <- "/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Results"

files    <- list.files(path = from.dir, pattern="rank.compressed",full.names = TRUE, recursive = TRUE)
for (f in files) file.copy(from = f, to = to.dir)
