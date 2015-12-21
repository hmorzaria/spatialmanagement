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

zonationfiles = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files"

setwd(zonationfiles)
location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

for (eachfolder in 1:length(location.folders)){
  print(paste("Analyzing this folder",location.folders[eachfolder],sep=" "))
location.folders[eachfolder] %>% strsplit(.,"[.]")%>% unlist %>% 
    .[2] %>% paste(zonationfiles,.,sep="") %>% setwd(.)

list.files(getwd(),  pattern="*.bat$", full.names=FALSE) %>% shell
  shell
  
}