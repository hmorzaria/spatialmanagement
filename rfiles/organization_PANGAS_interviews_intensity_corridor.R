#' November 2015
#' organize shapefiles from PANGAS fishery participatory maps
#' for spatial management project
#' hmorzarialuna@gmail.com
#' Last revised December 2015                                                                                                            # Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
# July 30, 2014
# Organize datafiles and calculate fishing intensity

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","magrittr","maptools","raster","data.table","wesanderson", "plyr")
lapply(x, require, character.only = TRUE)

#' remove files and graphics
rm(list=ls())
graphics.off()

#' set working directories
pathToSaveShapes = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/Original_data/"
mainpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/"

#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity/Analysis/Fishing_intensity")
#read data
all.data <- readOGR(".", "all_interviews_v032710")

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity")

#' read table of species names
species_data = fread("species_names.csv", header=TRUE)

#' read shapefile of corridor and empty frame
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

#' define communities in the corridor to subset
community.list = c("PPE","SJO","PJA","STO","DDC","PLO")
all.rg =subset(all.data, sitio_code %in% community.list) %>% drop.levels
sp.levels = unique(all.rg$spp_code) %>% drop.levels

for (eachspecies in 1:length(sp.levels)){
  this.species = sp.levels[eachspecies] %>% as.character
  
  species.price = species_data %>% 
    filter(SppCode==this.species) %>% 
    .$Price %>% 
    as.numeric
  
  print(paste("Analyzing",this.species))
    sp.corridor.com <- all.rg[all.rg$spp_code == this.species,]
    setwd(pathToSaveShapes)
    
    unique.id <- unique(sp.corridor.com@data$Id) %>% as.character
    
    uid = 1
    
  #' get first polygon
      poly.data <- sp.corridor.com[sp.corridor.com$Id == unique.id[1], ] 
      n <- length(slot(poly.data, "polygons"))
      poly.data <- spChFIDs(poly.data, as.character(uid:(uid+n-1)))
      uid <- uid + n
  #' calculates polygon area
            pol.area= gArea(poly.data)
      poly.data$area = pol.area
  #' revalues codes assigned to polygons by fishers of area importance 
      revalue(poly.data$CODE, c("-" = 0, "A" = 1,"ABC"= 1, "B"  = 1, "C"= 1,   "Y"= 0.5,   "Y1"= 0.5,  "Y2"= 0.5,  "Y3"= 0.5,  "YA" = 0.5, "YB"= 0.5,  "YC" = 0.5, "YD"= 0.5))
  #' normalizes polygons by dividing polygon importance by area   
      poly.data$W= as.numeric(poly.data$CODE) / pol.area
  #' revalues other polygons and combines with first polygoan
    
    if (length(unique.id)>1){
      
          for (i in 2:length(unique.id)) {
      
      temp.data <- sp.corridor.com[sp.corridor.com$Id == unique.id[i], ] 
      n <- length(slot(temp.data, "polygons"))
      temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1)))
      uid <- uid + n
      pol.area= gArea(temp.data)
      #' obtains polygon area
      temp.data$area = pol.area
      #' revalues codes and divides by area
      revalue(temp.data$CODE, c("-" = 0, "A" = 1,"ABC"= 1, "B"  = 1, "C"= 1,   "Y"= 0.5,   "Y1"= 0.5,  "Y2"= 0.5,  "Y3"= 0.5,  "YA" = 0.5, "YB"= 0.5,  "YC" = 0.5, "YD"= 0.5))
      temp.data$W= as.numeric(temp.data$CODE) / pol.area
      poly.data <- spRbind(poly.data,temp.data)
              } # end other polygons
      
      #' Normalize between min and 1
      #' did not normalize between 0 and 1 because it effectively eliminates the 
      #' minimum value, when being 0 it cannot be added to rasters from other
      #' sources
      #' This is the same thing Marcia did for her paper
      poly.data$Indx = (poly.data$W) / (max(poly.data$W))
      
    } else if (length(unique.id)==1) {
      poly.data$Indx = 1
      }

      print(paste("Saving raster ",this.species,sep =" "))
      
  names(poly.data)
  proj4string(poly.data)
  
 poly.data$Eco = species.price[1] * poly.data$Indx
    
  #writeOGR(poly.data, ".", paste(this.species,"pangas_Ix",sep="_"),driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  out.r <- rasterize(poly.data, field="Indx", mask.raster)
  out.r2 <- rasterize(poly.data, field="Eco", mask.raster)
  
  # set the cells associated with the shapfile to the specified value
  out.r[is.na(out.r)] <- 0
  out.r2[is.na(out.r2)] <- 0
  
  out.r =  mask(out.r,poly.r)
  out.r2 =  mask(out.r2,poly.r)
  
   # export to the working directory as a tif file
    setwd(pathToSaveShapes)
    writeRaster(out.r, filename=paste(this.species,"pangas_Ix",sep="_"), format="GTiff", overwrite=TRUE)  
    writeRaster(out.r2, filename=paste(this.species,"pangas_Eco",sep="_"), format="GTiff", overwrite=TRUE)  
    
    loc.levels = unique(poly.data$sitio_code) %>% as.character
    
    for (eachloc in 1:length(loc.levels))
    {
     
      this.loc = loc.levels[eachloc]
      print(paste("Saving locality files",this.loc,this.species,sep =" "))
            
          loc.corridor.com <- poly.data[poly.data$sitio_code == this.loc,]
          
      out.r3 <- rasterize(loc.corridor.com, field="Indx", mask.raster)
      out.r4 <- rasterize(loc.corridor.com, field="Eco", mask.raster)
      out.r3[is.na(out.r3)] <- 0
      out.r4[is.na(out.r4)] <- 0
      
      out.r3 =  mask(out.r3,poly.r)
      out.r4 =  mask(out.r4,poly.r)
        # set the cells associated with the shapfile to the specified value
      
      # export to the working directory as a tif file
      setwd(pathToSaveShapes)
      writeRaster(out.r3, filename=paste(this.loc,this.species,"pangas_In",sep="_"), format="GTiff", overwrite=TRUE)  
      writeRaster(out.r4, filename=paste(this.loc,this.species,"pangas_IE",sep="_"), format="GTiff", overwrite=TRUE)  
    }
    
  
    print(paste("Done with ",this.species,sep =" "))
} # end species
  


  

