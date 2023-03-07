################################################################################
# LIBRARIES
library(ncdf4)
library(raster)
library(rgeos)
library(mapview)
library(stringr)

# SET DIRECTORY
DIR = dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(DIR)

# LOAD CITIES
Cities_US <- read.csv("../01_cities_selection/USA_cities.txt",header=T,sep=';')
Cities <- SpatialPointsDataFrame(Cities_US[,c("Longitude", "Latitude")], Cities_US)
crs(Cities) = CRS("+proj=longlat +datum=WGS84")
mapview(Cities)

# planar crs (for raster analysis)
crs_WRF   <- CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=39.7000122070312 +lon_0=-98 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
R         <-  70 # city zone radius 

# LOOP ON CITIES
for (i in 1:nrow(Cities_US)){
  meta.timer.start = Sys.time()
  
  #################################
  # LOAD CITY AND DIRECTORY
  #################################
  Target_city <- str_replace(Cities$Urban.Agglomeration[i]  , " ", "_")  
  cat(paste0("###############################################################################",'\n'))
  cat(paste0("Start of ERA5 cutting for the city: ",Target_city,'\n'))
  Target_Dir <- paste0(DIR,"/Extracted_Cities/",Target_city)
  
  #######################################
  # CREATE CORRECT EXTENT FOR EXTRACTION
  #######################################
  # select city point
  City_WGS  <- Cities[i,]
  # Project on WRF reference projection
  crs_WGS <- crs(City_WGS)
  City_LCC  <- spTransform(City_WGS,crs_WRF)
  # Create a circle around city center with a radius R
  Circle_LCC <- gBuffer(City_LCC,width = R*1000)
  # Project the circle on WGS (for stage 4)
  Circle_WGS <- spTransform(Circle_LCC,crs_WGS)
  e_city = extent(Circle_WGS)
  ROI <- paste(round(e_city@xmin,2),round(e_city@xmax,2),round(e_city@ymin,2),round(e_city@ymax,2),sep=',')
  
  #################################
  # Call CDO for extracting ERA5
  #################################
  cat(paste0("# Extraction of ERA5 data for the city:",Target_city,'\n'))
  timer.start = Sys.time()

  File_ERA5_U <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_U_700_2002_2005.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_U," ",paste0(Target_Dir,'/ERA5_U_',Target_city,'_2002_2005.nc'),sep=""),show.output.on.console = F)
  
  File_ERA5_U <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_U_700_2006_2009.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_U," ",paste0(Target_Dir,'/ERA5_U_',Target_city,'_2006_2009.nc'),sep=""),show.output.on.console = F)
  
  File_ERA5_U <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_U_700_2010_2013.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_U," ",paste0(Target_Dir,'/ERA5_U_',Target_city,'_2010_2013.nc'),sep=""),show.output.on.console = F)
  
  File_ERA5_U <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_U_700_2014_2017.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_U," ",paste0(Target_Dir,'/ERA5_U_',Target_city,'_2014_2017.nc'),sep=""),show.output.on.console = F)
  
  File_ERA5_U <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_U_700_2018_2021.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_U," ",paste0(Target_Dir,'/ERA5_U_',Target_city,'_2018_2021.nc'),sep=""),show.output.on.console = F)
  
  File_ERA5_V <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_V_700_2002_2005.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_V," ",paste0(Target_Dir,'/ERA5_V_',Target_city,'_2002_2005.nc'),sep=""),show.output.on.console = F)
  
  File_ERA5_V <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_V_700_2006_2009.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_V," ",paste0(Target_Dir,'/ERA5_V_',Target_city,'_2006_2009.nc'),sep=""),show.output.on.console = F)
  
  File_ERA5_V <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_V_700_2010_2013.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_V," ",paste0(Target_Dir,'/ERA5_V_',Target_city,'_2010_2013.nc'),sep=""),show.output.on.console = F)
  
  File_ERA5_V <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_V_700_2014_2017.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_V," ",paste0(Target_Dir,'/ERA5_V_',Target_city,'_2014_2017.nc'),sep=""),show.output.on.console = F)
  
  File_ERA5_V <- paste0(DIR,'/Downloaded_ERA5/ERA_5_US_V_700_2018_2021.nc')
  s = system(paste("cdo -sellonlatbox,",ROI," ",File_ERA5_V," ",paste0(Target_Dir,'/ERA5_V_',Target_city,'_2018_2021.nc'),sep=""),show.output.on.console = F)

  timer.end = Sys.time()
  cat("# City : ",Target_city ,", Time for processing ERA5 data (Clipping) = ",round((timer.end - timer.start),2),' min. \n')
  
  meta.timer.end = Sys.time()
  
  Target_city <- as.character(Cities$Urban.Agglomeration[i])    #REMPLIR NAME DE LA VILLE
  cat(paste0("End of ERA5 extraction for the city: ",Target_city,'\n'))
  cat(paste0("TOTAL TIME =  ",round((meta.timer.end - meta.timer.start),2),' min. \n'))
  cat(paste0("###############################################################################",'\n'))
  
}

