# morgane.lalonde-le-pajolec@ipsl.latmos.fr


# charge libraries
library(rstudioapi) #confort pour chemin des rep et fichiers
library(R.utils)    #confort pour chemin des rep et fichiers
library(raster)
library(rgdal)
library(rgeos)
library(ggpubr)
library(mapview)
library(ncdf4)
library(keyring)
library(foreach)
library(doParallel)
library(magrittr)
library(sf)
library(st)
devtools::install_github("ropensci/FedData")
library(FedData)
library(readr)

# set working directory
DIR = dirname(rstudioapi::getSourceEditorContext()$path) # script's folder
setwd(DIR)



urban_extent <- read_csv("urban_extent.csv")










# choose your parameters

P_level   <- as.character(700)
THETA        <-  90 # Angle pour d?terminer les zones (90 veut dire qu'on fait 4 quadrans)


# indicate the projections we will be using
crs_ST4   <- CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-105 +x_0=0 +y_0=0 +R=6371200 +units=m +no_defs") # download nc file of ST4 don't have crs so we need to asign it
crs_WGS <- CRS("+proj=longlat +datum=WGS84 +no_defs ")
crs_WRF   <- CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=39.7000122070312 +lon_0=-98 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

# import cities
Cities_US <- read.csv("../01_cities_selection/USA_cities.txt",header=T,sep=';')
Cities_US$Urban.Agglomeration <- gsub(" ","_",Cities_US$Urban.Agglomeration)
Cities <- SpatialPointsDataFrame(Cities_US[,c("Longitude", "Latitude")], Cities_US)
crs(Cities) = CRS("+proj=longlat +datum=WGS84")
mapview(Cities)
Cities_proj    <- spTransform(Cities,crs_WRF)
Circle_proj    <- gBuffer(Cities_proj,width = Rmax*1000,byid = T)
#On repasse les cercles en WGS
Circles        <- spTransform(Circle_proj,crs(Cities))
Names_Cities <- Cities_US$Urban.Agglomeration

# lets do dis !
for (i in 1:length(Names_Cities)) {
  
  Rurb         <- urban_extent$Rurb[i]
  Rmin = Rurb
  Rmax=sqrt((((pi*Rmin^2)+((pi*Rmin^2)/4))*4)/pi)

  Circle_WGS <- Circles[i,]
  e = extent(Circle_WGS)
  Target_city <- Cities$Urban.Agglomeration[i]
  city <- Cities[i,]
  city_proj    <- spTransform(city,crs_ST4)
  DIR_Data      <- paste0(DIR,'/../02_download_data/Extracted_Cities/',Target_city,'/')

  File_ERA5_U <- paste0(DIR_Data,'ERA5_U_',Target_city,'_',P_level,'_2002_2021.nc')
  File_ERA5_V <- paste0(DIR_Data,'ERA5_V_',Target_city,'_',P_level,'_2002_2021.nc')
  u <- brick(File_ERA5_U, varname="u")
  v <- brick(File_ERA5_V, varname="v")
  vecU = cellStats(u, stat='mean', na.rm=TRUE)
  vecV = cellStats(v, stat='mean', na.rm=TRUE)
  source("plot.windrose.R")
  vecW = sqrt(vecU^2 + vecV^2) # vitesse du vent
  
  #determiner direction du vent a partir des vecteurs u et v
  wind_dir_trig_from_degrees = mod(atan2(vecV,vecU)*180/pi + 180,360) #provenance du vent
  wind_dir_trig_to_degrees = mod(wind_dir_trig_from_degrees + 180,360) #provenance du vent
  
  #On charge le fichier avec les pas de temps (fait dans un autre script)
  Dates = seq.POSIXt(as.POSIXct("2002-01-01 00:00:00",tz = 'UTC'),
                     as.POSIXct("2021-12-31 23:00:00",tz = 'UTC'),by = "hour")
  
  #On rassemble nos donnees (vents, temps etc)
  df_wind = data.frame(Date = Dates,  
                       u = vecU,
                       v = vecV,
                       ws = vecW,
                       wdir_rose = mod(atan2(vecU,vecV)*180/pi + 180,360),
                       wdir_to = mod(atan2(vecV,vecU)*180/pi + 180,360),
                       wdir_from = mod(mod(atan2(vecV,vecU)*180/pi + 180,360) + 180,360))
  
  df_wind$upwind_min = mod(df_wind$wdir_from-THETA/2,360)
  df_wind$upwind_max = mod(df_wind$wdir_from+THETA/2,360)
  df_wind$downwind_min = mod(df_wind$wdir_to-THETA/2,360)
  df_wind$downwind_max = mod(df_wind$wdir_to+THETA/2,360)
  df_wind$leftwind_min = mod(df_wind$wdir_to + 90 -THETA/2,360)
  df_wind$leftwind_max = mod(df_wind$wdir_to + 90 + THETA/2,360)
  df_wind$rightwind_min = mod(df_wind$wdir_to + 270 -THETA/2,360)
  df_wind$rightwind_max = mod(df_wind$wdir_to+ 270 + THETA/2,360)
  plot.windrose(data = df_wind,
                spd = "ws",
                dir = "wdir_rose")
  
   df_Res_TOT = c()
  
  # Parallell Computing 
  list_file_Dates =list()
  list_ras = list()
  i = 1
  
  Start_Year= c(2002,2006,2010,2014,2018)
  End_Year=c(2005,2009,2013,2017,2021)
  
  for (i in 1:5){
  for (i_Y in 1:length(Start_Year)){
    list_ras[[i]] = paste0(DIR_Data,'ST4_',Target_city,as.character(Start_Year[i_Y]),'_',as.character(End_Year[i_Y]),'.nc')
  }
  }
  UseCores <- detectCores() -2
  #Register CoreCluster
  cl       <- makeCluster(UseCores)
  registerDoParallel(cl)
  
  system.time({
    r <- foreach (i=1:length(list_ras) , .inorder=T,.combine='rbind') %dopar% {
      
      library(raster)
      library(ncdf4)
  
  file = list_ras[[i]]
  b=nc_open(file)
  ST4_P = brick(file)
  
  
  indices_time <- as.numeric(gsub("X","",names(ST4_P)))
  indices_time_seconds <- indices_time*60*60
  TIME <- as.POSIXct(indices_time_seconds,tz = 'UTC', origin = paste0(as.character(Start_Year),"-01-01 00:00:00"))
  indices_lacunes <- which(diff(as.numeric(gsub("X","",names(ST4_P)))) != 1) + 1
  Lacunes = Dates[indices_lacunes]
  Dates = Dates[TIME]
  
  longitudes <- ncvar_get(b,"lon")
  latitudes <- ncvar_get(b,"lat")
  splonlat = SpatialPoints(cbind(c(longitudes),c(latitudes)),proj4string =  CRS("+proj=longlat +datum=WGS84"))
  splonlat_proj = spTransform(splonlat,crs_ST4)
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
  ras_Temp <-ST4_P[[1]]
  ras_R <- distanceFromPoints(ras_Temp, coordinates(city_proj))/1000 #in km
  x <- init(ras_Temp, 'x') -  coordinates(city_proj)[1]
  y <- init(ras_Temp, 'y') -  coordinates(city_proj)[2]
  t <- atan2(y,x)/pi*180 +180# Angles range from 0 to 360 (same as wdir_to of df_wind table)
  
  ST4_PRCP <- ST4_P
  df_wind_Dates = df_wind[indices_time,]    
  df_Restemp = data.frame(Dates = TIME,
                              P_up = NA,
                              P_dw = NA,
                              P_lt = NA,
                              P_rt = NA,
                              P_urb = NA)
      # empty netcdf
      b_upwind = ST4_PRCP
      values(b_upwind) = NA
      b_downwind = b_upwind
      b_leftwind = b_upwind
      b_rightwind = b_upwind
      b_urban = b_upwind
      
      #1. upwind
      tmin = b_upwind
      values(tmin) = rep(df_wind_Dates$upwind_min, each = dim(b_upwind)[1]*dim(b_upwind)[2])
      tmax = b_upwind
      values(tmax) = rep(df_wind_Dates$upwind_max,each = dim(b_upwind)[1]*dim(b_upwind)[2])
      
      b_upwind[ ((tmin < tmax) & (t > tmin) & (t < tmax)) | ((tmin > tmax) & (((t > tmin) & (t < 360)) | ((t > 0) & (t < tmax))))] = 1
      b_upwindR = b_upwind
      b_upwindR[ras_R <= Rmin | ras_R > Rmax] = NA
      df_Restemp[,2] = cellStats(b_upwindR*ST4_PRCP,mean)
      
      #2. downwind
      tmin = b_downwind
      values(tmin) = rep(df_wind_Dates$downwind_min, each = dim(b_downwind)[1]*dim(b_downwind)[2])
      tmax = b_downwind
      values(tmax) = rep(df_wind_Dates$downwind_max,each = dim(b_downwind)[1]*dim(b_downwind)[2])
      
      b_downwind[ ((tmin < tmax) & (t > tmin) & (t < tmax)) | ((tmin > tmax) & (((t > tmin) & (t < 360)) | ((t > 0) & (t < tmax))))] = 1 #partie ciblee en 1
      b_downwindR = b_downwind
      b_downwindR[ras_R <= Rmin | ras_R > Rmax] = NA
      df_Restemp[,3] = cellStats(b_downwindR*ST4_PRCP,mean) # on obtient une moyenne sur la zone, par heure
      
      #3. leftwind
      tmin = b_leftwind
      values(tmin) = rep(df_wind_Dates$leftwind_min, each = dim(b_leftwind)[1]*dim(b_leftwind)[2])
      tmax = b_leftwind
      values(tmax) = rep(df_wind_Dates$leftwind_max,each = dim(b_leftwind)[1]*dim(b_leftwind)[2])
      
      b_leftwind[ ((tmin < tmax) & (t > tmin) & (t < tmax)) | ((tmin > tmax) & (((t > tmin) & (t < 360)) | ((t > 0) & (t < tmax))))] = 1
      b_leftwindR = b_leftwind
      b_leftwindR[ras_R <= Rmin | ras_R > Rmax] = NA
      df_Restemp[,4] = cellStats(b_leftwindR*ST4_PRCP,mean)
      
      #4. rightwind
      tmin = b_rightwind
      values(tmin) = rep(df_wind_Dates$rightwind_min, each = dim(b_rightwind)[1]*dim(b_rightwind)[2])
      tmax = b_rightwind
      values(tmax) = rep(df_wind_Dates$rightwind_max,each = dim(b_rightwind)[1]*dim(b_rightwind)[2])
      
      b_rightwind[ ((tmin < tmax) & (t > tmin) & (t < tmax)) | ((tmin > tmax) & (((t > tmin) & (t < 360)) | ((t > 0) & (t < tmax))))] = 1
      b_rightwindR = b_rightwind
      b_rightwindR[ras_R <= Rmin | ras_R > Rmax] = NA
      df_Restemp[,5] = cellStats(b_rightwindR*ST4_PRCP,mean)
      
      #5. urban zone
      ras_R <- reclassify(ras_R, c(0, Rurb, 1))
      ras_R <- reclassify(ras_R, c(Rurb, 1000, NA))
      df_Restemp[,6]=cellStats(ras_R*ST4_PRCP,mean)
      
      df_Restemp
      
    }
    }
  )
  
  stopCluster(cl)
  
  df_Res_TOT = cbind(df_wind[1:nrow(r),],r)
  write.table(df_Res_TOT,paste0("Spatial_precip/df_Res_TOT_radius_dym1_",Target_city,'_',P_level,".TXT"),row.names = F,sep=';')
  
  }
  
