#' November 2015
#' Las updated December 2015
#' organize rasters from fishing zones and conflicts
#' for fishery intensity project
#' hmorzarialuna@gmail.com

#' install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))
x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","maptools","raster","data.table","wesanderson", "plyr")
lapply(x, require, character.only = TRUE)

#' remove objects and figures
rm(list=ls())
graphics.off()
#' paths
datapath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/Original_data/"
savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Original_data"
framepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Frame"
species_list = c("PARPLE","HEXNIG","PHYERY","SQUCAL","RHIPRO","CALBEL","ZAPEXA")
#' get corridor shapefile and empty frame
setwd(framepath)
poly.r = readOGR(".", "corridor_wetland_polygon")
north.r = raster("Frame_corridor_final.tif")
buffer.r = raster("frame_buffer")
#' define projection of data
proj.lcc = proj4string(north.r)
crs.geo <- CRS(proj.lcc)

#' create empty mask raster
#' set background values to 0
e.shape <- extent(north.r)
mask.raster = raster(e.shape,resolution=c(1000,1000),crs=crs.geo)
mask.raster[is.na(mask.raster)] <- 0
#' list communities in the corridor
community.list = c("PPE","SJO","PJA","STO","DDC","PLO")

#' summarize files across data types for each community and species
setwd(datapath)
#list all intensity files across communities and data types
for(eachcommunity in 1:length(community.list)){
  this.com = community.list[eachcommunity]
  print(this.com)
 loc.sp=list.files(getwd(), pattern=glob2rx(paste(this.com,"_*_In.tif",sep="")), full.names=FALSE)
#   print(loc.sp)
for(eachspecies in 1:length(species_list))
{
  this.species = species_list[eachspecies] 
list.sp = grep(this.species,loc.sp, value=TRUE)
print(list.sp)

if(length(list.sp) > 1)  {
 
  fish.areas = stack(list.sp)
  fish.areas[is.na(fish.areas)] <- 0
# normalize data between min -1  
  norm.data = fish.areas / cellStats(fish.areas,"max")
    r = sum(norm.data)
  writeRaster(r, filename=paste(this.com,this.species,"FI",sep="_"), format="GTiff", overwrite=TRUE)  
  } else if (length(list.sp) == 1){
    fish.areas2 = raster(list.sp)
    fish.areas2[is.na(fish.areas2)] <- 0
    #norm.data2 = (fish.areas2 - cellStats(fish.areas2,"min")) / (cellStats(fish.areas2,"max")-cellStats(fish.areas2,"min"))
    norm.data2 = fish.areas2 / cellStats(fish.areas2,"max")
    writeRaster(norm.data2, filename=paste(this.com,this.species,"FI",sep="_"), format="GTiff", overwrite=TRUE)  
  }
} 
}

print("Done with rasters")

setwd(datapath)
#Add frame to all layers

file.list=list.files(getwd(),  pattern="*FI.tif$", full.names=FALSE)

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

setwd(savepath)
writeRaster(all.masked, filename=paste(name.sp.loc,"COR",sep="_"), format="GTiff", overwrite=TRUE)  
} # close layers

#' make raster for rock outcrops reported by shrimp trawlers
#' make raster for vaquita area and Bahia Adair wetland
#' management areas prohibit gillnets
#' 
setwd(savepath)
poly.vaq = readOGR(".", "zonif_humedales_badair_Proje")
poly.vaq$W = 1
#rasterize using mask raster
out.r <- rasterize(poly.vaq, mask.raster,field=poly.vaq@data$W) 
# set the cells associated with the shapefile to the specified value
out.r[is.na(out.r)] <- 0
sp.r =  mask(out.r,poly.r)

frame.raster = merge(sp.r,buffer.r)
frame.raster[frame.raster==255] <- -9999
all.masked = mask(frame.raster,buffer.r)
#writeRaster(test.masked, filename="test_layer", format="GTiff", overwrite=TRUE)  

setwd(savepath)

writeRaster(all.masked, filename="badair_humedales", format="GTiff", overwrite=TRUE)  

#' rocky outcrops
poly.vaq = readOGR(".", "Rock_outcrops_shrimptrawls")
poly.vaq$W = 1
#rasterize using mask raster
out.r <- rasterize(poly.vaq, mask.raster,field=poly.vaq@data$W) 
# set the cells associated with the shapefile to the specified value
out.r[is.na(out.r)] <- 0
sp.r =  mask(out.r,poly.r)

frame.raster = merge(sp.r,buffer.r)
frame.raster[frame.raster==255] <- -9999
all.masked = mask(frame.raster,buffer.r)
setwd(savepath)
writeRaster(all.masked, filename="rocky_areas", format="GTiff", overwrite=TRUE)  

#' vaquita net and longline exclusion area
poly.vaq = readOGR(".", "Zona_exclusion_Vaquita_lm")
poly.vaq$W = 1
#rasterize using mask raster
out.r <- rasterize(poly.vaq, mask.raster,field=poly.vaq@data$W) 
# set the cells associated with the shapefile to the specified value
out.r[is.na(out.r)] <- 0
sp.r =  mask(out.r,poly.r)

frame.raster = merge(sp.r,buffer.r)
frame.raster[frame.raster==255] <- -9999
all.masked = mask(frame.raster,buffer.r)
setwd(savepath)
writeRaster(all.masked, filename="vaquita_exclusion", format="GTiff", overwrite=TRUE)  

from.dir <- "/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Original_data"
to.dir   <- "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Jan2016/Zonation_files"

files    <- list.files(path = from.dir, pattern="FI_COR",full.names = TRUE, recursive = TRUE)
for (f in files) file.copy(from = f, to = to.dir)
