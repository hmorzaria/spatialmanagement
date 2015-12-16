#' Nov 2015
#' organize shapefiles from conflict surveys
#' for spatial management project
#' hmorzarialuna@gmail.com
#' install.packages()
#' 
x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/"
datapath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Conflictos_codificados/"
mainpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/"

setwd(mainpath)

#Species names and prices
species_data = fread("species_names.csv", header=TRUE)


#' Make a raster stack to calculate fishing events per pixel all species
#' for each community

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Frame")
poly.r = readOGR(".", "corridor_wetland_polygon")
north.r = raster("Frame_corridor_final.tif")
buffer.r = raster("frame_buffer")
#define projection of data
proj.lcc = proj4string(north.r)
crs.geo <- CRS(proj.lcc)

e.shape <- extent(north.r)
#ras.rows = nrow(north.r)
#ras.cols = ncol(north.r)

mask.raster = raster(e.shape,resolution=c(1000,1000),crs=crs.geo)

#set background values to 0
mask.raster[is.na(mask.raster)] <- 0

setwd(datapath)
shape.files = list.files(getwd(),  pattern="MC.shp$", full.names=FALSE)

new.sp = c("JAI"="CALBEL","ANG"="SQUCAL","CAM"= "LITSTY", "CCR" = "PHYERY", "GUI" = "RHIPRO", "LEN" = "PARPLE", "CCN"  = "HEXNIG")
           
for(eachfile in 1:length(shape.files))
{
  print(eachfile)
  this.folder = shape.files[eachfile]
  shape.file.name = unlist(strsplit(this.folder,"[.]"))[1]
  
  name.loc = unlist(strsplit(this.folder,"_"))[1]
  
  if(name.loc =="BSJ"){
    name.loc ="SJO"
  } 
  print(name.loc)
  name.sp.folder = unlist(strsplit(this.folder,"_"))[2]
  new.name = new.sp[name.sp.folder] %>% as.character
  
  poly.data = readOGR(".", shape.file.name) 
  proj4string(poly.data)
  
  poly.data = spTransform(poly.data, crs.geo)
  
  poly.data$W = 1
  
  #rasterize using mask raster
  out.r <- rasterize(poly.data, mask.raster,field=poly.data@data$W)
  out.r[is.na(out.r)] <- 0
  out.r = mask(out.r,poly.r)
  writeRaster(out.r, filename=paste(name.loc,new.name,"CF",sep="_"), format="GTiff", overwrite=TRUE)  

}

setwd(datapath)
file.list=list.files(getwd(),  pattern="*CF.tif$", full.names=FALSE)

for(eachlayer in 1:length(file.list)) {
  setwd(datapath)
  all.sp.loc = raster(file.list[eachlayer])
  
  name.sp.loc = names(all.sp.loc)
  print(names(all.sp.loc))
  #test.layer = raster(all.sp.loc,layer = 1)
  buffer.r[buffer.r==1] <- 0
  
  #frame.raster = mosaic(test.layer,buffer.r,fun=max)
  #frame.raster = mosaic(all.sp.loc,buffer.r,fun=max)
  frame.raster = merge(all.sp.loc,buffer.r)
  frame.raster[frame.raster==255] <- -9999
  all.masked = mask(frame.raster,buffer.r)
  #writeRaster(test.masked, filename="test_layer", format="GTiff", overwrite=TRUE)  
  
  setwd(pathToSaveShapes)
  writeRaster(all.masked, filename=paste(name.sp.loc,"COR",sep="_"), format="GTiff", overwrite=TRUE)  
} # close layers
