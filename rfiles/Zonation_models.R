#' Hem Nalini Morzaria Luna
#' hmorzarialuna@gmail.com
#' Dec 2015
#' run zonation models
#' for spatial management project
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
if(!require(readxl)){install.packages("readxl"); library(readxl)}


rm(list=ls())

zonationfiles = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files"

setwd(zonationfiles)
location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

for (eachfolder in 1:length(location.folders)){
  print(paste("Analyzing this folder",location.folders[eachfolder],sep=" "))
location.folders[eachfolder] %>% strsplit(.,"[.]")%>% unlist %>% 
    .[2] %>% paste(zonationfiles,.,sep="") %>% setwd(.)

list.files(getwd(),  pattern="*.bat$", full.names=FALSE) %>% shell

}

#'move files from Zonation directories to results folder
#'
setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files")
from.dir <- "/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files"
to.dir   <- "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Results"

files    <- list.files(path = from.dir, pattern="rank.compressed",full.names = TRUE, recursive = TRUE)
for (f in files) file.copy(from = f, to = to.dir, overwrite = TRUE)

#' use this code to open all input files for Zonation models
# setwd(zonationfiles)
# location.folders <- list.dirs(full.names = TRUE,recursive=TRUE)
# indx.folders = grep('inputs', location.folders)
# input.folder = location.folders[indx.folders]
# 
# for (eachfolder in 1:length(input.folder)){
# #change path
#   input.folder[eachfolder] %>% strsplit(.,"[.]")%>% unlist %>% 
#     .[2] %>% paste(zonationfiles,.,sep="") %>% setwd(.)
#  #list input files
#   input.files = list.files(getwd(),  pattern="*.spp$", full.names=TRUE) 
#   #call notepad
#   shell(paste("start", "notepad++",input.files))
#  }