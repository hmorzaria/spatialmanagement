#' July 2015
#' last edited Dec 2015
#' organize shapefiles from bitacoras
#' for spatial management project
#' hmorzarialuna@gmail.com
#' install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))
#' uses revised shape files from Fishing intensity project
#' Based on code by Zia Ahmed zua3 at cornell.edu 


#' require libraries
x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","magrittr","maptools","raster","data.table","wesanderson", "plyr")
lapply(x, require, character.only = TRUE)
#' remove objects and graphics
rm(list=ls())
graphics.off()

#' set folder paths
datapath = "E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity/Analysis/Fishing_intensity/Bitacoras_revisado"
mainpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/"
pathToSaveShapes = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/Original_data/"

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity/")

#List of locations and acronyms
#subset communities in the corridor
names_locations = c("PPE","SJO","PLO")#"PJA","STO","DDC" - Not included

#Bitacora_database
bitacoras_data = fread("Datos_bitacoras.txt", header=TRUE)

#Species names
species_data = fread("species_names.csv", header=TRUE)

#' this are bitacoras files revised in Organization_botacoras_shape_file_V4.r
#' make a raster stack to calculate fishing events per pixel all species
#' for each community

#' get corridor shapefile and empty frame
setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/Frame")
poly.r = readOGR(".", "corridor_wetland_polygon")
north.r = raster("Frame_corridor_final.tif")
#define projection of data
proj.lcc = proj4string(north.r)
crs.geo <- CRS(proj.lcc)

#' make empty mask raster
#' set background values to 0
e.shape <- extent(north.r)
mask.raster = raster(e.shape,resolution=c(1000,1000),crs=crs.geo)
mask.raster[is.na(mask.raster)] <- 0

setwd(datapath)
location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

#folders are by species

for (eachfolder in 1:length(location.folders)) 
{
  setwd(datapath)
  this.folder = location.folders[eachfolder]
  name.sp.folder = unlist(strsplit(this.folder,"/"))[2]
  print(name.sp.folder)
  
  paththisLocation = paste(datapath,name.sp.folder,sep="/")
  setwd(paththisLocation)

  for(eachloc in 1:length(names_locations))
  {
   this.com = names_locations[eachloc]
  #' get shapefiles for that species 
    shape.sp=list.files(getwd(),  pattern=glob2rx(paste(this.com,"*.shp",sep="")), full.names=FALSE)
 
     if(length(shape.sp)!=0){
      {
    setwd(paththisLocation)
    this.shape.species = shape.sp[1]
    shape.file.name = unlist(strsplit(this.shape.species,"[.]"))[1]
    #' read shapefile
    poly.data = readOGR(".", shape.file.name) 
    #' create weight column
    poly.data$W = 1
    #' rasterize using mask raster
    out.r <- rasterize(poly.data, mask.raster,field=poly.data@data$W) 
    #' set the cells associated with the shapefile to the specified value
    out.r[is.na(out.r)] <- 0
    
    #' for species with more than one shapefile rasterize other shapefiles

    if(length(shape.sp)>1){
      for (eachshapefile in 2:length(shape.sp)) {
        this.shape.species = shape.sp[eachshapefile]
        shape.file.name = unlist(strsplit(this.shape.species,"[.]"))[1]
        temp.data = readOGR(".", shape.file.name) 
        temp.data$W = 1
        temp.r <- rasterize(temp.data, mask.raster,field= temp.data@data$W) 
        # set the cells associated with the shapefile to the specified value
        temp.r[is.na(temp.r)] <- 0
        out.r = stack(out.r, temp.r)
        } #end other polygons
#' sum all rasters
    sp.r= sum(out.r)
      
    } else{
      sp.r = out.r
    }
    
  # export to the working directory as a tif file
  
 setwd(pathToSaveShapes)
 sp.r =  mask(sp.r,poly.r)
    
 #raster per community species
  writeRaster(sp.r, filename=paste(this.com,name.sp.folder,"bitac_sp",sep="_"), format="GTiff", overwrite=TRUE)  
  
      }
     }
  }
  } # end all species

#obtain raster sum all species per community

print("sum rasters per species per community")

setwd(pathToSaveShapes)

list.locations = names_locations


for(eachlocation in 1:length(list.locations))
{
  this.location = list.locations[eachlocation]
  
  shape.sp=list.files(getwd(),  pattern=glob2rx(paste(this.location,"*bitac_sp.tif",sep="")), full.names=FALSE)
  
  all.r = raster(shape.sp[1])
  
  for(eachfile in 1:length(shape.sp))
  {
    this.file = raster(shape.sp[eachfile])
    all.r = sum(all.r, this.file)
    }
  for(eachfile in 1:length(shape.sp))
  {
   this.file = raster(shape.sp[eachfile])
  species.indx = this.file / all.r 
  
  name.sp.folder = unlist(strsplit(names(this.file),"_"))[2]
    
  writeRaster(species.indx , filename=paste(this.location,name.sp.folder,"bitac_In",sep="_"), format="GTiff", overwrite=TRUE)  

    #get average price
    
    species.price = species_data %>% 
      filter(NewCode==name.sp.folder) %>% 
      .$Price %>% 
      as.numeric %>% .[1]
    r.economic = species.indx  * species.price
    
    writeRaster(r.economic, filename=paste(this.location,name.sp.folder,"bitac_IE",sep="_"), format="GTiff", overwrite=TRUE)  
    
    } # close locations
}

