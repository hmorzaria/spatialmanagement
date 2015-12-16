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

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Original_data/"
mainpath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/"

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity")

cedo_data = fread("cedo_fishing_records.csv")

#List of locations and acronyms
names_locations = unique(cedo_data$CODE)

#Species names and prices
species_data = fread("species_names.csv", header=TRUE)

#Species names
species_names = unique(cedo_data$SP_CODE)

## make rasters per species per community
##Based on code by Zia Ahmed zua3 at cornell.edu 
###############################################################################

# Get list of all shape files from directory
#------------------------------------------------------------

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Frame")
poly.r = readOGR(".", "corridor_wetland_polygon")
north.r = raster("Frame_corridor_final.tif")
#define projection of data
proj.lcc = proj4string(north.r)
crs.geo <- CRS(proj.lcc)

e.shape <- extent(north.r)
#ras.rows = nrow(north.r)
#ras.cols = ncol(north.r)

mask.raster = raster(e.shape,resolution=c(1000,1000),crs=crs.geo)

#set background values to 0
mask.raster[is.na(mask.raster)] <- 0

setwd(pathToSaveShapes)

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
    
    biodiv.rows = nrow(sp.data)
    
    iterations = round(biodiv.rows/1000,0)
    
    if(nrow(sp.data)>1000)
    {
      last.row = 1001
      first.row = 1
      
      for (each.iteration in 1:iterations)
        
      {
        new.last.row = ((last.row-1)*each.iteration)
        
        section.biodiv = sp.data[first.row:new.last.row,] %>% 
         mutate_each(funs(as.numeric),LAT,LON) %>% 
          as.data.frame
        
        section.biodiv = section.biodiv[!is.na(section.biodiv$LAT),]
        
   coordinates(section.biodiv) = c("LON","LAT")
    
   crs.wgs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
   proj4string(section.biodiv) <- crs.wgs
   section.biodiv = spTransform(section.biodiv, crs.geo)
   stations_subset <- section.biodiv[poly.r, ]
   
      }}
    
    if(nrow(sp.data)<1000){
      
      section.biodiv = sp.data %>% 
       mutate_each(funs(as.numeric),LAT,LON) %>% 
        as.data.frame
      
      section.biodiv = section.biodiv[!is.na(section.biodiv$LAT),]
      
      coordinates(section.biodiv) = c("LON","LAT")
      
      crs.wgs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
      proj4string(section.biodiv) <- crs.wgs
      section.biodiv = spTransform(section.biodiv, crs.geo)
      stations_subset <- section.biodiv[poly.r, ]
          }
      
    data_frame_species <- as(stations_subset, "data.frame")
    
    for(eachline in 1:nrow(data_frame_species))
      
    {
      sp.data.geo = data_frame_species[eachline,]
      coordinates(sp.data.geo) = c("LON","LAT")
      proj4string(sp.data.geo) <- crs.geo
      #rasterize using mask raster
    out.r <- rasterize(sp.data.geo, mask.raster,field=sp.data.geo@data$W) 
   
    # set the cells associated with the shapefile to the specified value
    out.r[is.na(out.r)] <- 0
    
    sp.r = sum(sp.r, out.r)
    
  }#end lines
  
   #raster per community species
  writeRaster(sp.r, filename=paste(this.folder,this.sp,"cedo_sp",sep="_"), format="GTiff", overwrite=TRUE)  
  
}# end species
  print(paste("Done with species",this.sp,sep=""))
} # end folder
#obtain raster sum all species per community

print("Calculating rasters per community")

setwd(pathToSaveShapes)

for(eachlocation in 1:length(names_locations))
{
  this.location = names_locations[eachlocation] 
  
  print(paste("Analyzing",this.location,sep=" "))
  
  shape.sp=list.files(getwd(),  pattern=glob2rx(paste(this.location,"*sp.tif",sep="")), full.names=FALSE)
  
  all.sp.loc = stack(shape.sp)

  all.r = sum(all.sp.loc)    
  
  species.indx = all.sp.loc / all.r #will this work
  
  r.layers = names(all.sp.loc)
  
   for (eachlayer in 1:length(r.layers)){
    
     name.sp.folder = unlist(strsplit(r.layers[eachlayer],"_"))[2]
    
     r <- raster(species.indx, layer=eachlayer)
     
    writeRaster(r, filename=paste(this.location,name.sp.folder,"cedo_In",sep="_"), format="GTiff", overwrite=TRUE)  

    species.price = species_data %>% 
      filter(NewCode==name.sp.folder) %>% 
      .$Price %>% 
      as.numeric %>% .[1]
    r.economic = r * species.price
    
    writeRaster(r.economic, filename=paste(this.location,name.sp.folder,"cedo_IE",sep="_"), format="GTiff", overwrite=TRUE)  
    
} # close locations
}


#Read in all species across communities and average
#
print("Averaging species across communities")

for(eachspecies in 1:length(species_names))
{
  this.species = species_names[eachspecies] 

  print(paste("Analyzing",this.species,sep=" "))
  
  raster.files=list.files(getwd(),  pattern=glob2rx(paste("*",this.species,"*_In.tif",sep="")), full.names=FALSE)
  
  #get average price
  
  species.price = species_data %>% 
    filter(NewCode==this.species) %>% 
    .$Price %>% 
    as.numeric %>% .[1]
  
  if(length(raster.files)==1)
  {
    
    raster.species=stack(raster.files)
  
    #sum to get fishing intensity
    
    average.intensity = raster.species
    writeRaster(average.intensity, filename=paste(this.species,"cedo_Indx",sep=""), format="GTiff", overwrite=TRUE)  
    #calculate economic index
    average.economic = average.intensity * species.price
    writeRaster(average.economic, filename=paste(this.species,"cedo_IE",sep=""), format="GTiff", overwrite=TRUE)  
  }
  
  if(length(raster.files)>1)
  {
    
    raster.species=stack(raster.files)
    
   #sum to get fishing intensity
    
    average.intensity= mean(raster.species)
    
    writeRaster(average.intensity, filename=paste(this.species,"cedo_Indx",sep=""), format="GTiff", overwrite=TRUE)  
  #calculate economic index
    average.economic = average.intensity * species.price
    writeRaster(average.economic, filename=paste(this.species,"cedo_IE",sep=""), format="GTiff", overwrite=TRUE)  
    
    }
  
  
  }
  
# Now sum all rasters
  
print("Summing all rasters")

  setwd(pathToSaveShapes)
  
   raster.files = list.files(getwd(),  pattern="cedo_Indx.tif$", full.names=FALSE)
    raster.species.comm = stack(raster.files)
    raster.species.comm[is.na(raster.species.comm)] <- 0
    
    index.intensity = sum(raster.species.comm)
    
    #write raster data
    writeRaster(index.intensity, filename="cedo_intensity_index", format="GTiff", overwrite=TRUE)  
    writeRaster(index.intensity, "cedo_intensity_index.asc", format="ascii",overwrite=TRUE)
    
    print("Saved intensity rasters") 
    norm.data = raster("cedo_intensity_index.tif") %>% 
      reclassify(cbind(0, NA)) 
   
    #normalize raster 0-1 and save
    norm.data = (index.intensity - cellStats(index.intensity,"min")) / (cellStats(index.intensity,"max")-cellStats(index.intensity,"min"))
    
    norm.data2 = norm.data %>% 
      rasterToPoints %>% 
      data.frame %>% 
      setnames(c('Longitude', 'Latitude', 'Intensity'))
    
    norm.data2[norm.data2 == 0] <- NA
    norm.data2 =  norm.data2[complete.cases(norm.data2),]#eliminate rows with NA
    
    #create color palette
    
    pal <- wes_palette(10, name = "FantasticFox", type = "continuous")
    
    #plot richness model and robustness
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
    #export as png
    png("cedo_intensity.png")
    non.plot
    dev.off()
    
    # Sum all economic rasters
    
    setwd(pathToSaveShapes)
    
    
    raster.files = list.files(getwd(),  pattern="cedo_IE.tif$", full.names=FALSE)
    raster.species.comm = stack(raster.files)
    raster.species.comm[is.na(raster.species.comm)] <- 0
    
    index.intensity = sum(raster.species.comm)
    
    #write raster data
    writeRaster(index.intensity, filename="cedo_economic_index", format="GTiff", overwrite=TRUE)  
    writeRaster(index.intensity, "cedo_economic_index.asc", format="ascii",overwrite=TRUE)
    
    print("Saved economic importance rasters") 
        norm.data = raster("cedo_economic_index.tif") %>% 
      reclassify(cbind(0, NA)) 
    
        #normalize raster 0-1 and save
        norm.data = (index.intensity - cellStats(index.intensity,"min")) / (cellStats(index.intensity,"max")-cellStats(index.intensity,"min"))
        
    norm.data2 = norm.data %>% 
      rasterToPoints %>% 
      data.frame %>% 
      setnames(c('Longitude', 'Latitude', 'Importance'))
    
    norm.data2[norm.data2 == 0] <- NA
    norm.data2 =  norm.data2[complete.cases(norm.data2),]#eliminate rows with NA
    
    #create color palette
    
    pal <- wes_palette(10, name = "FantasticFox", type = "continuous")
    
    #plot richness model and robustness
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
    #export as png
    png("cedo_economic.png")
    non.plot
    dev.off()