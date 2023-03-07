### load libraries
library(ncdf4)
library(raster)
library(rgeos)
library(mapview)
library(geoknife)
DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep o? il y a le fichier de script
setwd(DIR)

Cities_US <- read.csv("../01_cities_selection/USA_cities.txt",header=T,sep=';')
Cities <- SpatialPointsDataFrame(Cities_US[,c("Longitude", "Latitude")], Cities_US)
crs(Cities) = CRS("+proj=longlat +datum=WGS84")
mapview(Cities)

###################################################################################
# MERGE ALL DOWNLOADED STAGE IV DATA

Start_Year = c(2002,2006,2010,2014,2018)
End_Year = c(2005,2009,2013,2017,2021)

for (i in 1:nrow(Cities)){
  meta.timer.start = Sys.time()
  Target_city <- as.character(Cities_US$Urban.Agglomeration[i])   
  Target_city <- gsub(" ","_",Target_city)
  Target_Dir <- paste0(DIR,"/Extracted_Cities/",Target_city)
  
  files = paste0(Target_Dir,'/ST4_',Target_city,as.character(Start_Year),'_',as.character(End_Year),'.nc')
  system(paste("cdo -mergetime",paste(files,collapse = ' '), paste0(Target_Dir,'/ST4_',Target_city,'_2002_2021.nc')))

  files = paste0(Target_Dir,'/ERA5_U_',Target_city,as.character(Start_Year),'_',as.character(End_Year),'.nc')
  system(paste("cdo -mergetime",paste(files,collapse = ' '), paste0(Target_Dir,'/ERA5_U_',Target_city,'_2002_2021.nc')))
  
  files = paste0(Target_Dir,'/ERA5_V_',Target_city,as.character(Start_Year),'_',as.character(End_Year),'.nc')
  system(paste("cdo -mergetime",paste(files,collapse = ' '), paste0(Target_Dir,'/ERA5_V_',Target_city,'_2002_2021.nc')))
  
 }

