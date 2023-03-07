### load libraries
library(ncdf4)
library(raster)
library(rgeos)
library(mapview)

DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep o? il y a le fichier de script
setwd(DIR)

Cities_US <- read.csv("../01_cities_selection/USA_cities.txt",header=T,sep=';')
Cities <- SpatialPointsDataFrame(Cities_US[,c("Longitude", "Latitude")], Cities_US)
crs(Cities) = CRS("+proj=longlat +datum=WGS84")
mapview(Cities)

crs_ST4        <- CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-105 +x_0=0 +y_0=0 +R=6371200 +units=m +no_defs") 
Start_Year = c(2002,2006,2010,2014,2018)
End_Year = c(2005,2009,2013,2017,2021)

for (i in 1:nrow(Cities_US)){
  for (y in 1:length(Start_Year)){
  meta.timer.start = Sys.time()
  cat(paste0("############################################################################################",'\n'))
  cat(paste0("Debut de l'extraction des donnees pour la ville ",Cities$Urban.Agglomeration[i],'\n'))
  DIR_Data      <- paste0(DIR,'/Extracted_Cities/', Cities$Urban.Agglomeration[i],'/')
    
    ## Chargement des donn?es de la ville
    cat(paste0("## Chargement des donnees ..."))
    timer.start = Sys.time()
    no_proj_P        <- brick(paste0(DIR_Data,'ST4_', Cities$Urban.Agglomeration[i],Start_Year[y],'_',End_Year[y],'.nc'))

    files = paste0(DIR_Data,'ST4_',Cities$Urban.Agglomeration[i],as.character(Start_Year),'_',as.character(End_Year),'.nc')
    files.out = paste0(DIR_Data,'ST4_',Cities$Urban.Agglomeration[i],as.character(Start_Year),'_',as.character(End_Year),'_proj.nc')
  
    for (i_files in 1:length(files)){
      Dates = seq.POSIXt(as.POSIXct(paste0(as.character(Start_Year[i_files]),"-01-01 00:00:00"),tz = 'UTC'), as.POSIXct(paste0(as.character(End_Year[i_files]),"-12-31 23:00:00"),tz = 'UTC'),by = "hour")
      b=nc_open(files[i_files])
      ST4_P = brick(files[i_files])
      indices_lacunes <- which(diff(as.numeric(gsub("X","",names(ST4_P)))) != 1) + 1
      if(i_files == 1) {Lacunes = Dates[indices_lacunes]} else {Lacunes = c(Lacunes, Dates[indices_lacunes])}
      Lacunes = c(Lacunes,Dates[indices_lacunes])
      longitudes <- ncvar_get(b,"lon")
      latitudes <- ncvar_get(b,"lat")
      splonlat = SpatialPoints(cbind(c(longitudes),c(latitudes)),proj4string =  CRS("+proj=longlat +datum=WGS84"))
      splonlat_proj = spTransform(splonlat,crs_ST4)
      #plot(splonlat_proj)
      e = extent(splonlat_proj)
      xmin = e@xmin - (e@xmax - e@xmin)/dim(longitudes)[1]
      xmax = e@xmax + (e@xmax - e@xmin)/dim(longitudes)[1]
      ymin = e@ymin - (e@ymax - e@ymin)/dim(longitudes)[2]
      ymax = e@ymax + (e@ymax - e@ymin)/dim(longitudes)[2]
      Limits_proj = data.frame(Positions = c("Leftbottom","Rightup"),
                              Longitude = c(xmin,xmax),
                              Latitude = c (ymin,ymax))
      sp_Limits_proj <- SpatialPointsDataFrame(Limits_proj[,c("Longitude", "Latitude")], Limits_proj,
                                           proj4string =  crs_ST4)
      crs(ST4_P) <- crs_ST4
  
      extent(ST4_P) <- extent(xmin,xmax,ymin,ymax)
      ST4_P_proj <- projectRaster(ST4_P,no_proj_P)
      writeRaster(ST4_P_proj, files.out[i_files], datatype='FLT4S', force_v4=TRUE, compression=7,overwrite=T)   
      gc()
    }
  write.table(Lacunes,paste0(DIR_Data,'ST4_',Cities$Urban.Agglomeration[i],"_Lacunes.txt"),row.names = F)
    meta.timer.end = Sys.time()
    
    cat(paste0("TEMPS TOTAL PASSE     =   ",round(difftime(meta.timer.end, meta.timer.start, units = "mins"),2),'mins \n'))
    cat(paste0("############################################################################################",'\n'))
    gc()
  }
}
