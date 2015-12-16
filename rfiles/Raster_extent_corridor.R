#' November 2015
#' organize shapefiles from CEDO surveys
#' for fishery intensity project
#' hmorzarialuna@gmail.com
#' install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

datapath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Original_data"
savepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/"
mainpath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/"
framepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Frame"
species_list = c("PARPLE","HEXNIG","PHYERY","SQUCAL","RHIPRO","CALBEL")

setwd(framepath)
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

#file.types = c("bitac","pangas","cedo")
file.types = c("bitac","pangas")

community.list = c("PPE","SJO","PJA","STO","DDC","PLO")

#summarize files across data types for each community and species
setwd(datapath)

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
  
 # norm.data = (fish.areas - cellStats(fish.areas,"min")) / (cellStats(fish.areas,"max")-cellStats(fish.areas,"min"))
  norm.data = fish.areas / cellStats(fish.areas,"max")
    r = sum(norm.data)
    #normalize raster min-1 and save
       
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

#make raster for vaquita area
#only management area
setwd(mainpath)
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
#writeRaster(test.masked, filename="test_layer", format="GTiff", overwrite=TRUE)  

setwd(savepath)

writeRaster(all.masked, filename="vaquita_exclusion", format="GTiff", overwrite=TRUE)  
