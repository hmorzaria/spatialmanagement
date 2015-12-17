#' November 2015
#' organize shapefiles from CEDO surveys
#' for fishery intensity project
#' hmorzarialuna@gmail.com
#' Last revised December 2015
#' install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))
#' Based on code by Zia Ahmed zua3 at cornell.edu 

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","magrittr","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

#' remove all files
rm(list=ls())
#' graphics off
graphics.off()

pathToSaveShapes = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/Original_data/"
mainpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/"
framepath ="E:/Archivos/1Archivos/Articulos/En preparacion/Spatial_management/Analisis/Zonation/Scenarios_Nov2015/Frame"

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity")

cedo_data = fread("cedo_fishing_records.csv")

#' List of locations and acronyms
names_locations = unique(cedo_data$CODE)

#' Species names and prices
species_data = fread("species_names.csv", header=TRUE)

#' Species names
species_names = unique(cedo_data$SP_CODE)

#' Get corridor polygon and raster frame
setwd(framepath)
poly.r = readOGR(".", "corridor_wetland_polygon")
north.r = raster("Frame_corridor_final.tif")
#' Define projection of data
proj.lcc = proj4string(north.r)
crs.geo <- CRS(proj.lcc)

#' Create blank raster
e.shape <- extent(north.r)
mask.raster = raster(e.shape,resolution=c(1000,1000),crs=crs.geo)
#' set background values to 0
mask.raster[is.na(mask.raster)] <- 0

setwd(pathToSaveShapes)

#' make rasters per species per community

for (eachfolder in 1:length(names_locations)) 
{
  this.folder = names_locations[eachfolder]
  print(this.folder)
  folder.data = cedo_data %>% 
    tbl_df %>% 
    filter(CODE==this.folder)
  unique.sp = unique(folder.data$SP_CODE)

  for(eachsp in 1:length(unique.sp))
      {
    sp.r = mask.raster
    this.sp = unique.sp[eachsp]
print(this.sp)
    sp.data = folder.data %>% 
      tbl_df %>% 
      filter(SP_CODE==this.sp) %>% 
      data.frame
    
    sp.data$W = 1
    
 #' if data for species has > 1000 row divide in chunks
 #' of 1000 for quicker analysis  
  
    if(nrow(sp.data)>1000)
    {
      last.row = 1001
      first.row = 1
      iterations = sp.data %>% nrow %>% divide_by(1000)%>% round(0)
      
      for (each.iteration in 1:iterations)
      {
        new.last.row = ((last.row-1)*each.iteration)
   #' divide data in section of 1000 rows (or less)
    section.biodiv = sp.data[first.row:new.last.row,] %>% 
    mutate_each(funs(as.numeric),LAT,LON) %>% 
    as.data.frame
   #' eliminate NAs  
   section.biodiv = section.biodiv[!is.na(section.biodiv$LAT),]
   #' make spatial     
   coordinates(section.biodiv) = c("LON","LAT")
   #' define and assign projection
   crs.wgs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
   proj4string(section.biodiv) <- crs.wgs
   section.biodiv = spTransform(section.biodiv, crs.geo)
   #' eliminate points outside of corridor 
    stations_subset <- section.biodiv[poly.r, ]
         }}
    #' for data sets of < 1000 rows
    if(nrow(sp.data)<1000){
      #' convert into data frame
      section.biodiv = sp.data %>% 
       mutate_each(funs(as.numeric),LAT,LON) %>% 
        as.data.frame
      #' eliminate NAs
      section.biodiv = section.biodiv[!is.na(section.biodiv$LAT),]
      #' make spatial
      coordinates(section.biodiv) = c("LON","LAT")
      #' assign geographical, datum WGS84 and transform to lcc
      crs.wgs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  
      proj4string(section.biodiv) <- crs.wgs
      section.biodiv = spTransform(section.biodiv, crs.geo)
      #' eliminate points outside of corridor
      stations_subset <- section.biodiv[poly.r, ]
          }
    data_frame_species <- as(stations_subset, "data.frame")
    
    sp.r = mask.raster
    #' make each point a raster
    for(eachline in 1:nrow(data_frame_species))
      {
      sp.data.geo = data_frame_species[eachline,]
      coordinates(sp.data.geo) = c("LON","LAT")
      proj4string(sp.data.geo) <- crs.geo
      #' rasterize using mask raster
    out.r <- rasterize(sp.data.geo, mask.raster,field=sp.data.geo@data$W) 
      #' set the cells associated with the shapefile to the specified value
    out.r[is.na(out.r)] <- 0
     sp.r = sum(sp.r, out.r)
    }#' end lines
  
   #' raster per community species
  writeRaster(sp.r, filename=paste(this.folder,this.sp,"cedo_sp",sep="_"), format="GTiff", overwrite=TRUE)  
  
}#' end species
  print(paste("Done with species",this.sp,sep=""))
} #' end folder

#' obtain raster sum all species per community
print("Calculating rasters per community")

setwd(pathToSaveShapes)

for(eachlocation in 1:length(names_locations))
{
  this.location = names_locations[eachlocation] 
  print(paste("Analyzing",this.location,sep=" "))
  #' list all rasters
  shape.sp=list.files(getwd(),  pattern=glob2rx(paste(this.location,"*sp.tif",sep="")), full.names=FALSE)
  #' stack rasters and divide by sum of all rasters in stack, output is brick layer
  species.indx = shape.sp %>%  stack %>% divide_by(shape.sp %>%  stack %>% sum)
  #' get layer names in stack
  #' brick layer loses names
  r.layers = shape.sp %>%  stack %>% names
  #' save each layer in the raster brick separately
  for (eachlayer in 1:length(r.layers)){
    name.sp.folder = unlist(strsplit(r.layers[eachlayer],"_"))[2]
    r <- raster(species.indx, layer=eachlayer)
    writeRaster(r, filename=paste(this.location,name.sp.folder,"cedo_In",sep="_"), format="GTiff", overwrite=TRUE)  
} # close locations
}

#' Read in all species across communities and average

print("Averaging species across communities")

for(eachspecies in 1:length(species_names))
{
  this.species = species_names[eachspecies] 
  print(paste("Analyzing",this.species,sep=" "))
  #' lists all rasters for that species
  raster.files=list.files(getwd(),  pattern=glob2rx(paste("*",this.species,"*_In.tif",sep="")), full.names=FALSE)
  
  #' get average price
  species.price = species_data %>% 
  filter(NewCode==this.species) %>% 
  .$Price %>% 
  as.numeric %>% .[1]
  
  if(length(raster.files)==1)
  {
    raster.species=stack(raster.files)
    #' sum to get fishing intensity
    average.intensity = raster.species
    writeRaster(average.intensity, filename=paste(this.species,"cedo_Indx",sep=""), format="GTiff", overwrite=TRUE)  
    #' calculate economic index
    #' the economic index per species is equal to fishing intensity
    #' only useful when combining species
    average.economic = average.intensity * species.price
    writeRaster(average.economic, filename=paste(this.species,"cedo_IE",sep=""), format="GTiff", overwrite=TRUE)  
  }
  
  if(length(raster.files)>1)
  {
    raster.species=stack(raster.files)
   #' sum to get fishing intensity
    average.intensity= mean(raster.species)
    writeRaster(average.intensity, filename=paste(this.species,"cedo_Indx",sep=""), format="GTiff", overwrite=TRUE)  
  #' calculate economic index
    average.economic = average.intensity * species.price
    writeRaster(average.economic, filename=paste(this.species,"cedo_IE",sep=""), format="GTiff", overwrite=TRUE)  
      }
    }
  
#' Now sum all rasters
  
print("Summing all rasters")
setwd(pathToSaveShapes)
  
raster.files = list.files(getwd(),  pattern="cedo_Indx.tif$", full.names=FALSE)
raster.species.comm = stack(raster.files)
raster.species.comm[is.na(raster.species.comm)] <- 0
    
index.intensity = sum(raster.species.comm)
    
#' write raster data
writeRaster(index.intensity, filename="cedo_intensity_index", format="GTiff", overwrite=TRUE)  
writeRaster(index.intensity, "cedo_intensity_index.asc", format="ascii",overwrite=TRUE)
    
print("Saved intensity rasters") 
norm.data = raster("cedo_intensity_index.tif") %>% 
reclassify(cbind(0, NA)) 
   
#' normalize raster 0-1 and save
norm.data = (index.intensity - cellStats(index.intensity,"min")) / (cellStats(index.intensity,"max")-cellStats(index.intensity,"min"))
    
norm.data2 = norm.data %>% 
rasterToPoints %>% 
data.frame %>% 
setnames(c('Longitude', 'Latitude', 'Intensity'))
    
norm.data2[norm.data2 == 0] <- NA
norm.data2 =  norm.data2[complete.cases(norm.data2),]#eliminate rows with NA
    
#' create color palette
    
pal <- wes_palette(10, name = "FantasticFox", type = "continuous")
    
#' plot richness model and robustness
non.plot = ggplot(data=norm.data2, aes(y=Latitude, x=Longitude)) +
geom_raster(aes(fill=Intensity)) +
      theme_bw() +
      coord_equal() + 
      scale_fill_gradientn(colours = pal)+
      theme(axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12, angle=90),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size=10),
            legend.title = element_text(size=12))
    
setwd(mainpath)
#' export as png
png("cedo_intensity.png")
non.plot
dev.off()
    
    #' Sum all economic rasters
    
    setwd(pathToSaveShapes)
    
    
    raster.files = list.files(getwd(),  pattern="cedo_IE.tif$", full.names=FALSE)
    raster.species.comm = stack(raster.files)
    raster.species.comm[is.na(raster.species.comm)] <- 0
    
    index.intensity = sum(raster.species.comm)
    
    #' write raster data
    writeRaster(index.intensity, filename="cedo_economic_index", format="GTiff", overwrite=TRUE)  
    writeRaster(index.intensity, "cedo_economic_index.asc", format="ascii",overwrite=TRUE)
    
    print("Saved economic importance rasters") 
        norm.data = raster("cedo_economic_index.tif") %>% 
      reclassify(cbind(0, NA)) 
    
        #' normalize raster 0-1 and save
        norm.data = (index.intensity - cellStats(index.intensity,"min")) / (cellStats(index.intensity,"max")-cellStats(index.intensity,"min"))
        
    norm.data2 = norm.data %>% 
      rasterToPoints %>% 
      data.frame %>% 
      setnames(c('Longitude', 'Latitude', 'Importance'))
    
    norm.data2[norm.data2 == 0] <- NA
    norm.data2 =  norm.data2[complete.cases(norm.data2),]#eliminate rows with NA
    
    #' create color palette
    
    pal <- wes_palette(10, name = "FantasticFox", type = "continuous")
    
    #' plot richness model and robustness
    non.plot = ggplot(data=norm.data2, aes(y=Latitude, x=Longitude)) +
      geom_raster(aes(fill=Intensity)) +
      theme_bw() +
      coord_equal() + 
      scale_fill_gradientn(colours = pal)+
      theme(axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12, angle=90),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size=10),
            legend.title = element_text(size=12))
    
    setwd(mainpath)
    #' export as png
    png("cedo_economic.png")
    non.plot
    dev.off()