#' July 2015
#' last edited Sep 2015
#' organize shapefiles from bitacoras
#' for fishery intensity project
#' hmorzarialuna@gmail.com
#' install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
"rgdal","rgeos","maptools","raster","pipeR","data.table","wesanderson")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

datapath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/Bitacoras_revisado"
mainpath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/"
pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Scenarios_Nov2015/Original_data/"

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity")


#List of locations and acronyms
#names_locations = c("BKI","DDS","LDV","PCH","PLI","PLO","PPE","SJO")
names_locations = c("PPE","SJO","PLO")#"PJA","STO","DDC" - Not included

#Bitacora_database
bitacoras_data = fread("Datos_bitacoras.txt", header=TRUE)

#Species names
species_data = fread("species_names.csv", header=TRUE)



#this are bitacoras files revised in Organization_botacoras_shape_file_V4.r


## make rasters per species per community
##Based on code by Zia Ahmed zua3 at cornell.edu 
###############################################################################

# Get list of all shape files from directory
#------------------------------------------------------------

#' Make a raster stack to calculate fishing events per pixel all species
#' for each community

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
     shape.sp=list.files(getwd(),  pattern=glob2rx(paste(this.com,"*.shp",sep="")), full.names=FALSE)
 
  
     if(length(shape.sp)!=0){
      {
    setwd(paththisLocation)
    
   this.shape.species = shape.sp[1]
    
   shape.file.name = unlist(strsplit(this.shape.species,"[.]"))[1]
    
    poly.data = readOGR(".", shape.file.name) 
    
    poly.data$W = 1
    
    #rasterize using mask raster
    out.r <- rasterize(poly.data, mask.raster,field=poly.data@data$W) 
    
    # set the cells associated with the shapefile to the specified value
    out.r[is.na(out.r)] <- 0
    
    if(length(shape.sp)>1){
      for (eachshapefile in 2:length(shape.sp)) {
        
        this.shape.species = shape.sp[eachshapefile]
        shape.file.name = unlist(strsplit(this.shape.species,"[.]"))[1]
        
        temp.data = readOGR(".", shape.file.name) 
        
        temp.data$W = 1
        
        temp.r <- rasterize(temp.data, mask.raster,field= temp.data@data$W) 
        
        # set the cells associated with the shapfile to the specified value
        temp.r[is.na(temp.r)] <- 0
        
        out.r = stack(out.r, temp.r)
        
      } #end other polygons

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
  species.indx = this.file / all.r #will this work
  
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

#Read in all species across communities and average
#

list.species = species_data %>% .$NewCode %>% unique


for(eachspecies in 1:length(list.species))
{
  this.species = list.species[eachspecies] 

  raster.files=list.files(getwd(),  pattern=glob2rx(paste("*",this.species,"*bitac_In.tif",sep="")), full.names=FALSE)
  
  #get average price
  
  species.price = species_data %>% 
    filter(NewCode==this.species) %>% 
    .$Price %>% 
    as.numeric %>% .[1]
 
   if(length(raster.files)!=0){
     
  raster.species=raster(raster.files[1])
  
  if(length(raster.files)==1)
  {
      #sum to get fishing intensity
    
    average.intensity = raster.species
    writeRaster(average.intensity, filename=paste(this.species,"bitac_Indx",sep=""), format="GTiff", overwrite=TRUE)  
    #calculate economic index
    average.economic = average.intensity * species.price
    writeRaster(average.economic, filename=paste(this.species,"bitac_IE",sep=""), format="GTiff", overwrite=TRUE)  
  }
  
  if(length(raster.files)>1)
  {
    
    raster.species=stack(raster.files)
    
   #sum to get fishing intensity
    
    raster.species[is.na(raster.species)] <- 0
    average.intensity= mean(raster.species)
    
    writeRaster(average.intensity, filename=paste(this.species,"bitac_Indx",sep=""), format="GTiff", overwrite=TRUE)  
  #calculate economic index
    average.economic = average.intensity * species.price
    writeRaster(average.economic, filename=paste(this.species,"bitac_IE",sep=""), format="GTiff", overwrite=TRUE)  
    
    }
  
  
  }}
  
# Now sum all rasters
  
  setwd(pathToSaveShapes)
  
   raster.files = list.files(getwd(),  pattern="bitac_Indx.tif$", full.names=FALSE)
   
    raster.species.comm = stack(raster.files)
    raster.species.comm[is.na(raster.species.comm)] <- 0
    
    index.intensity = sum(raster.species.comm)
    
    #write raster data
    writeRaster(index.intensity, filename="bitac_intensity_index", format="GTiff", overwrite=TRUE)  
    writeRaster(index.intensity, "bitac_intensity_index.asc", format="ascii", overwrite=TRUE)
    
     
    norm.data = raster("bitac_intensity_index.tif") %>% 
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
    png("log_books_intensity.png")
    non.plot
    dev.off()
    
    # Sum all economic rasters
    
    setwd(pathToSaveShapes)
    
    
    raster.files = list.files(getwd(),  pattern="bitac_IE.tif$", full.names=FALSE)
    raster.species.comm = stack(raster.files)
    raster.species.comm[is.na(raster.species.comm)] <- 0
    
    index.intensity = sum(raster.species.comm)
    
    #write raster data
    writeRaster(index.intensity, filename="bitac_economic_index", format="GTiff", overwrite=TRUE)  
    writeRaster(index.intensity, "bitac_economic_index.asc", format="ascii")
    
    
    norm.data = raster("bitac_economic_index.tif") %>% 
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
    png("log_books_economic.png")
    non.plot
    dev.off()