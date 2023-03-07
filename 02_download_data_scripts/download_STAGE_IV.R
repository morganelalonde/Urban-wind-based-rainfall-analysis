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

if(dir.exists(paste0(DIR,"/Extracted_Cities")) == F) dir.create(paste0(DIR,"/Extracted_Cities"))

for (i in 1:nrow(Cities)){
  ##########################################################
  # ON CHARGE LA VILLE
  meta.timer.start = Sys.time()
  
  Target_city <- as.character(Cities_US$Urban.Agglomeration[i])   
  Target_city <- gsub(" ","_",Target_city)
  Target_Dir <- paste0(DIR,"/Extracted_Cities/",Target_city)
  if(dir.exists(Target_Dir) == F) dir.create(Target_Dir)
  cat(paste0("############################################################################################",'\n'))
  cat(paste0("Debut de l'extraction des donnees pour la ville ",Target_city,'\n'))
  if (file.exists(paste0(Target_Dir,"/ST4_",Target_city,"2002_2005.nc")) & 
      file.exists(paste0(Target_Dir,"/ST4_",Target_city,"2006_2009.nc")) & 
      file.exists(paste0(Target_Dir,"/ST4_",Target_city,"2010_2013.nc")) & 
      file.exists(paste0(Target_Dir,"/ST4_",Target_city,"2014_2017.nc")) & 
      file.exists(paste0(Target_Dir,"/ST4_",Target_city,"2018_2021.nc")) ) {
    cat(paste0("Extraction des donnees pour la ville ",Target_city,' deja faite \n'))
    
  } else
  {
    
    #dir.create(Target_Dir,recursive = T)
    R         <-  75 # Rayon autour du point du centre ville
    
    ##########################################################
    # ON DEFINIT L'ETENDUE DE LA VILLE EN PROJECTION METRIQUE
    
    City_WGS  <- Cities[i,]
    mapview(City_WGS)
    # Project on WRF reference projection
    crs_WGS <- crs(City_WGS)
    crs_WRF   <- CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=39.7000122070312 +lon_0=-98 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
    City_LCC  <- spTransform(City_WGS,crs_WRF)
    mapview(City_LCC)
    # Create a circle around city center with a radius R
    Circle_LCC <- gBuffer(City_LCC,width = R*1000)
    mapview(Circle_LCC)
    # Project the circle on WGS (for stage 4)
    Circle_WGS <- spTransform(Circle_LCC,crs_WGS)
    mapview(Circle_WGS)
    e_city = extent(Circle_WGS)
    Pol_City_wgs <- as(e_city, 'SpatialPolygons') 
    crs(Pol_City_wgs) = CRS("+proj=longlat +datum=WGS84")
  
    start_date <- "2002-01-01 00:00:00"
    end_date <- "2005-12-31 23:00:00"
    
    stencil <- geoknife::simplegeom(Pol_City_wgs)
    fabric <- geoknife::webdata(url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                                variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                                times = c(start_date, end_date))
    knife <- geoknife::webprocess("subset")
    job <- geoknife::geoknife(stencil, fabric, knife, show.progress = TRUE,wait = TRUE, 
                              REQUIRE_FULL_COVERAGE=FALSE, 
                              OUTPUT_TYPE="netcdf")
    options(timeout = max(90000000, getOption("timeout")))
    download.file(url = geoknife::check(job)$URL, 
                  destfile = paste0(Target_Dir,"/ST4_",Target_city,"2002_2005.nc"), 
                  mode="wb")
    
    start_date <- "2006-01-01 00:00:00"
    end_date <- "2009-12-31 23:00:00"
    
    stencil <- geoknife::simplegeom(Pol_City_wgs)
    fabric <- geoknife::webdata(url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                                variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                                times = c(start_date, end_date))
    knife <- geoknife::webprocess("subset")
    job <- geoknife::geoknife(stencil, fabric, knife, show.progress = TRUE,wait = TRUE, 
                              REQUIRE_FULL_COVERAGE=FALSE, 
                              OUTPUT_TYPE="netcdf")
    options(timeout = max(90000000, getOption("timeout")))
    download.file(url = geoknife::check(job)$URL, 
                  destfile = paste0(Target_Dir,"/ST4_",Target_city,"2006_2009.nc"), 
                  mode="wb")
    
    start_date <- "2010-01-01 00:00:00"
    end_date <- "2013-12-31 23:00:00"
    
    stencil <- geoknife::simplegeom(Pol_City_wgs)
    fabric <- geoknife::webdata(url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                                variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                                times = c(start_date, end_date))
    knife <- geoknife::webprocess("subset")
    job <- geoknife::geoknife(stencil, fabric, knife, show.progress = TRUE,wait = TRUE, 
                              REQUIRE_FULL_COVERAGE=FALSE, 
                              OUTPUT_TYPE="netcdf")
    options(timeout = max(90000000, getOption("timeout")))
    download.file(url = geoknife::check(job)$URL, 
                  destfile = paste0(Target_Dir,"/ST4_",Target_city,"2010_2013.nc"), 
                  mode="wb")
    
    start_date <- "2014-01-01 00:00:00"
    end_date <- "2017-12-31 23:00:00"
    
    stencil <- geoknife::simplegeom(Pol_City_wgs)
    fabric <- geoknife::webdata(url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                                variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                                times = c(start_date, end_date))
    knife <- geoknife::webprocess("subset")
    job <- geoknife::geoknife(stencil, fabric, knife, show.progress = TRUE,wait = TRUE, 
                              REQUIRE_FULL_COVERAGE=FALSE, 
                              OUTPUT_TYPE="netcdf")
    options(timeout = max(90000000, getOption("timeout")))
    download.file(url = geoknife::check(job)$URL, 
                  destfile = paste0(Target_Dir,"/ST4_",Target_city,"2014_2017.nc"), 
                  mode="wb")
    
    start_date <- "2018-01-01 00:00:00"
    end_date <- "2021-12-31 23:00:00"
    
    stencil <- geoknife::simplegeom(Pol_City_wgs)
    fabric <- geoknife::webdata(url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                                variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                                times = c(start_date, end_date))
    knife <- geoknife::webprocess("subset")
    job <- geoknife::geoknife(stencil, fabric, knife, show.progress = TRUE,wait = TRUE, 
                              REQUIRE_FULL_COVERAGE=FALSE, 
                              OUTPUT_TYPE="netcdf")
    options(timeout = max(90000000, getOption("timeout")))
    download.file(url = geoknife::check(job)$URL, 
                  destfile = paste0(Target_Dir,"/ST4_",Target_city,"2018_2021.nc"), 
                  mode="wb")
    meta.timer.end = Sys.time()
    
    cat(paste0("Fin de l'extraction des donnees pour la ville ",Target_city,'\n'))
    cat(paste0(" TEMPS TOTAL PASSE pour la ville   ",Target_city,'                           =   ',round(difftime(meta.timer.end, meta.timer.start, units = "mins"),2),'mins \n'))
    cat(paste0("############################################################################################",'\n'))
    gc()
  }
}

