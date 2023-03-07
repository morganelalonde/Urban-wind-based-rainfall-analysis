################################################################################
#########################   CITIES SELECTION ###################################
################################################################################

# Define your parameters values
city.zone.radius = 65000 # (meters) the radius around the city that define your study zone
max.range = 600 # (meters) max range of altitude in the city s study zone
max.uncover = 15 # (%) max area not above continental surface (ocean or lake)
min.intersect = 55 # (%) if two cities zones intersect more than this value, they are merge
max.intersect = 30 # (%) if two cities zones intersect less than min.intersect but more than this value, they are both excluded

# Define working directory 
DIR = dirname(rstudioapi::getSourceEditorContext()$path) # script's directory
setwd(DIR) # set the directory

# Charge packages
library(readxl)
library(rgeos)
library(mapview)
library(rgdal)
library(raster)
library(terra)
library(sf)

# To select as many cities as we can, the selection will occur in two steps
# First the biggest cities, then the smallest cities

# Selection of big cities

# First download cities information here (file 12):
# https://population.un.org/wup/Download/

# read the file
Cities <- read_excel("WUP2018-F22-Cities_Over_300K_Annual.xls", 
                                                  range = "A17:CP1877")

# Select only USA cities (144 cities)
Cities_US <- Cities[which(Cities$`Country or area` == "United States of America"),]

# Only continental cities (honolulu not covered by the precipitation radar product)
# (143 cities)
Cities_US <- Cities_US[-which(Cities_US$`Urban Agglomeration` == "Honolulu"),]
Cities_US <- Cities_US[,c(1,4,5,7,8)] # we remove any unnecessary column
 
# Create a formal class SpatialPointsDataFrame with the 143 cities
Cities <- SpatialPointsDataFrame(Cities_US[,c("Longitude", "Latitude")], Cities_US)

# fill in the projection
crs(Cities) = CRS("+proj=longlat +datum=WGS84")
crs_not_planar = CRS("+proj=longlat +datum=WGS84")

# see your cities on map
mapview(Cities)

# define a planar projection (to buffer and intersect cities)
crs_planar <- CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=39.7000122070312 +lon_0=-98 +x_0=0 +y_0=0 +datum=WGS84 +a=6370000 +b=6370000 +units=m +no_defs")

# reproject cities
Cities.proj <- spTransform(Cities,crs_planar)

# transform points into circles of 65km radius (polygon)
Circle_City <- gBuffer(Cities.proj,byid = T, width = city.zone.radius)

# see your circle cities on map
mapview(Circle_City)

# Now we will only select cities without orographic precipitation

# Download elevation here (elec 30s):
# https://worldclim.org/data/worldclim21.html

# unzip elevation file
#unzip('wc2.1_30s_elev.zip')

# import raster
elevation <- raster('wc2.1_30s_elev.tif')

# reproject cities in raster crs
Circle_City.proj <- spTransform(Circle_City, elevation@crs@projargs)

# calcul elevation range for each city
Cities_US$min.elev = NA # create a column for minimum values
Cities_US$max.elev = NA # create a column for maximum values

for(i in 1:length(Cities_US$Index)){ 
elevation.city <- crop(elevation, Circle_City.proj[i,]) # raster with city extent
Cities_US$min.elev[i] <- cellStats(elevation.city, min) # minimum value of city raster
Cities_US$max.elev[i] <- cellStats(elevation.city, max) # maximum value of city raster
}

Cities_US$range.elev = Cities_US$max.elev - Cities_US$min.elev # calcul the range

# subset the cities (with the value given at the beginning of script)
Big_cities <- Cities_US[Cities_US$range.elev < max.range,]

# Create a formal class SpatialPointsDataFrame of big cities
Big_cities.sp <- SpatialPointsDataFrame(Big_cities[,c("Longitude", "Latitude")], Big_cities)

# fill in the projection
crs(Big_cities.sp) = CRS("+proj=longlat +datum=WGS84")

# reproject big cities
Big_cities.sp <- spTransform(Big_cities.sp,crs_planar)

# transform big cities into circles of 65km radius (polygon)
Circle_big_cities <- gBuffer(Big_cities.sp,byid = T, width = city.zone.radius)

# see your big cities on map
mapview(Circle_big_cities)

# Now we will only select cities with sufficient area correctly measured by radars
# download usa shapefile at 
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# (cb_2018_us_nation_20m.zip [<1.0 MB])

# import usa conteninental surface shapefile
usa <- readOGR(paste0(DIR,"/cb_2018_us_nation_20m"),"cb_2018_us_nation_20m") 

# reproject usa polygon ib planar coordinates
usa.sp <- spTransform(usa,crs_planar)

#  create a column for continental coverage of the city zone
Big_cities$not.cont = 0

# define area of 65km city zone
#city.area <- gArea(Circle_City.proj[1,])

# loop to extract area above continental surfaces 
for(y in 1:length(Big_cities$Index)){ # loop on selected big cities
  big.city <- terra::intersect(usa.sp,  Circle_big_cities[y,]) # new polygon obtain by intersection between usa continental surface and city area
  Big_cities$not.cont[y] = ((gArea(Circle_big_cities[y,]) - gArea(big.city)) / gArea(Circle_big_cities[y,])) * 100 # calcul % of the city area not above continent
}

# remove cities without sufficient area correctly measured by radars
Big_cities <- Big_cities[Big_cities$not.cont < max.uncover,]

# Create a formal class SpatialPointsDataFrame of big cities
Big_cities.sp <- SpatialPointsDataFrame(Big_cities[,c("Longitude", "Latitude")], Big_cities)

# fill in the projection
crs(Big_cities.sp) = CRS("+proj=longlat +datum=WGS84")

# reproject big cities
Big_cities.sp <- spTransform(Big_cities.sp,crs_planar)

# transform big cities into circles of 65km radius (polygon)
Circle_big_cities <- gBuffer(Big_cities.sp,byid = T, width = city.zone.radius)

# see your big cities on map
mapview(usa.sp)+mapview(Circle_big_cities)

# remove and fusion cities overlaping

# calcul intersection (of all initial cities)

# dataframe that will be filled with intersection values
df_Intersect = as.data.frame(matrix(NA,nrow=nrow(Circle_City), 
                                    ncol = nrow(Circle_City)))
colnames(df_Intersect) = Circle_City$Index # add colnames
row.names(df_Intersect) = Circle_City$Index # add rownames

# fill the dataframe
for (i_City1 in 1:(nrow(Circle_City)-1)){ # loop by line, expect the last one (the colomn will be filed by the others iterations)
  for (i_City2 in (i_City1+1):nrow(Circle_City)){ # loop by column, but not the symetry qnd not the same city
    pol_intersect <- gIntersection(Circle_City[i_City1,],Circle_City[i_City2,]) # intersect both cities
    if(is.null(pol_intersect)){df_Intersect[i_City1,i_City2] = 0} else { # if cities do not overlap, intersection equals 0
      df_Intersect[i_City1,i_City2] = (gArea(pol_intersect) / gArea(Circle_City[i_City1,])) * 100 # if they intersect fill the area of intersection
    }
  }
}

# select cities with an overlapping inferior to 25%

#create the lists, we will fill from all cities
cities_overlap_25 = c()

# loop to fill the lists, we select the max of row+column of the city
for (j_City in 1:(length(df_Intersect))){
  frac_Intersect <- max(max(df_Intersect[j_City,],na.rm=T), max(df_Intersect[,j_City],na.rm=T))
  if(frac_Intersect < 25){cities_overlap_25 = c(cities_overlap_25,j_City)}
}

# loop on the list of all cities with an overlapping inferior to 25%
for(j_City1 in 1:length(cities_overlap_25)) { # our list has numbers not index so :
  j_City2 <- cities_overlap_25[j_City1] # select a city
 index_city <- Cities_US$Index[j_City2] # select its index
 cities_overlap_25[j_City1] <- index_city # replace the number by its index
}

# now select cities that are in our subset of big cities and an overlap inferior to 25%

# create a list
cities_ok_25 = c()

# loop  on the list of all cities with an overlapping inferior to 25%
for(j_City3 in 1:length(cities_overlap_25)) {
  for(j_City4 in 1:length(Big_cities$Index)) {  # loop on the list of subset cities
  if(Big_cities$Index[j_City4] == cities_overlap_25[j_City3]){cities_ok_25 = c(cities_ok_25,j_City4)} # if city from all cities dataset exists in the subset, we keep it
  }
}

# subset dataframe of big cities by the list we created
Big_cities_25 <- Big_cities[cities_ok_25,]

# Create a formal class SpatialPointsDataFrame of big cities
Big_cities_25.sp <- SpatialPointsDataFrame(Big_cities_25[,c("Longitude", "Latitude")], Big_cities_25)

# fill in the projection
crs(Big_cities_25.sp) = CRS("+proj=longlat +datum=WGS84")

# reproject big cities
Big_cities_25.sp <- spTransform(Big_cities_25.sp,crs_planar)

# transform big cities into circles of 65km radius (polygon)
Circle_Big_cities_25 <- gBuffer(Big_cities_25.sp,byid = T, width = city.zone.radius)

# see your big cities on map
mapview(usa.sp)+mapview(Circle_Big_cities_25)


#create the lists, we will fill from all cities
cities_overlap_55 = c()

# loop to fill the lists, first verify that the city do not overlap with cities more than 25% and less than 55%
for (k_City in 1:(length(df_Intersect))){
  if(any((df_Intersect[k_City,] > 25 & df_Intersect[k_City,] < 55) | (df_Intersect[,k_City] > 25 & df_Intersect[,k_City] < 55), na.rm = T) == F){ 
    frac_Intersect <- max(max(df_Intersect[k_City,],na.rm=T),max(df_Intersect[,k_City],na.rm=T))
    if(frac_Intersect > 55){cities_overlap_55 = c(cities_overlap_55,k_City)} # now select the ones overlapping more than 55%
  }
}

# loop on the list of all cities with an overlapping superior to 55%
for(k_City1 in 1:length(cities_overlap_55)) { # our list has numbers not index so :
  k_City2 <- cities_overlap_55[k_City1] # select a city
  index_city <- Cities_US$Index[k_City2] # select its index
  cities_overlap_55[k_City1] <- index_city # replace the number by its index
}

# now select cities that are in our subset of big cities and an overlap superior to 55%

# create a list
cities_ok_55 = c()

# loop  on the list of all cities with an overlapping superior to 55%
for(k_City3 in 1:length(cities_overlap_55)) {
  for(k_City4 in 1:length(Big_cities$Index)) {  # loop on the list of subset cities
    if(Big_cities$Index[k_City4] == cities_overlap_55[k_City3]){cities_ok_55 = c(cities_ok_55,k_City4)} # if city from all cities dataset exists in the subset, we keep it
  }
}

# subset dataframe of big cities by the list we created
Big_cities_55 <- Big_cities[cities_ok_55,]

# Create a formal class SpatialPointsDataFrame of big cities
Big_cities_55.sp <- SpatialPointsDataFrame(Big_cities_55[,c("Longitude", "Latitude")], Big_cities_55)

# fill in the projection
crs(Big_cities_55.sp) = CRS("+proj=longlat +datum=WGS84")

# reproject big cities
Big_cities_55.sp <- spTransform(Big_cities_55.sp,crs_planar)

# transform big cities into circles of 65km radius (polygon)
Circle_Big_cities_55 <- gBuffer(Big_cities_55.sp,byid = T, width = city.zone.radius)

# see your big cities on map
mapview(usa.sp)+mapview(Circle_Big_cities_55)

# now fusion the cities overlapping

for (b in 1:(length(Circle_Big_cities_55)-1)) {
  if((b <= length(Circle_Big_cities_55)) == T){
  for (b2 in (b+1):length(Circle_Big_cities_55)) {
    if((b2 <= length(Circle_Big_cities_55)) == T){
  inter_55 <- intersect(Circle_Big_cities_55[b,], Circle_Big_cities_55[b2,])
  if(is.null(inter_55)==F){
      inter_552 <- st_as_sf(inter_55)
      centroid_55 <- st_centroid(inter_552)
      coord1 <- st_coordinates(centroid_55)
      centroid_55_2 <- as(centroid_55, "Spatial")
      centroid_55_3 <- spTransform(centroid_55_2,crs_not_planar)
      coord2 <- coordinates(centroid_55_3)
      Big_cities_55$Index[b] = 'citiesmerge'
      Big_cities_55$`Urban Agglomeration`[b] = paste0(Big_cities_55$`Urban Agglomeration`[b],' ',Big_cities_55$`Urban Agglomeration`[b+1])
      Big_cities_55$Latitude[b] = coord2[1,2]
      Big_cities_55$Longitude[b] = coord2[1,1]
    } 
   } 
  } 
 }
}

Big_cities_merged <- Big_cities_55[Big_cities_55$Index == 'citiesmerge',]

Big_cities_final <- rbind(Big_cities_25, Big_cities_merged)

Big_cities_final <- Big_cities_final[,c(3,4,5)]

# Create a formal class SpatialPointsDataFrame of big cities
Big_cities_final.sp <- SpatialPointsDataFrame(Big_cities_final[,c("Longitude", "Latitude")], Big_cities_final)

# fill in the projection
crs(Big_cities_final.sp) = CRS("+proj=longlat +datum=WGS84")

# reproject big cities
Big_cities_final.sp <- spTransform(Big_cities_final.sp,crs_planar)

# transform big cities into circles of 65km radius (polygon)
Circle_Big_cities_final <- gBuffer(Big_cities_final.sp,byid = T, width = city.zone.radius)

# see your big cities on map
mapview(usa.sp)+mapview(Circle_Big_cities_final)

# save table
write.table(Big_cities_final, file = 'USA_cities.txt', row.names = F, sep= ";")

################################################################################
##################                   END                      ##################
################################################################################

# Pour le moment je travaille que avec les grandes villes. En dessous il y a un 
# brouillon pour les petites villes mais inacheve (on obtient que 6 ou 7 villes)
























################ TEST ZONE #################




# Selection of small cities #---------------------------------------------------

# File with cities download at https://www.census.gov/en.html

usa.cities <- readOGR(paste0(DIR,"/USA_Major_Cities"),"USA_Major_Cities")
tab_cities <- usa.cities@data
tab_cities <- tab_cities[,c(1,2,9)]
tab_cities$Lat = coordinates(usa.cities)[,2]
tab_cities$Long = coordinates(usa.cities)[,1]
tab_cities <- tab_cities[tab_cities$POPULATION > 50000,]

# Create a formal class SpatialPointsDataFrame with the 143 cities
Cities_USA <- SpatialPointsDataFrame(tab_cities[,c("Long", "Lat")], tab_cities)

# fill in the projection
crs(Cities_USA) = CRS("+proj=longlat +datum=WGS84")

# see your cities on map
mapview(Cities_USA)

# define a planar projection (to buffer and intersect cities)
crs_planar <- CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=39.7000122070312 +lon_0=-98 +x_0=0 +y_0=0 +datum=WGS84 +a=6370000 +b=6370000 +units=m +no_defs")

# reproject cities
Cities_USA.proj <- spTransform(Cities_USA,crs_planar)

# transform points into circles of 65km radius (polygon)
Circle_Cities_USA <- gBuffer(Cities_USA.proj,byid = T, width = city.zone.radius)

# see your circle cities on map
mapview(Circle_Cities_USA)

# reproject cities in raster crs
Circle_Cities_USA.proj <- spTransform(Circle_Cities_USA, elevation@crs@projargs)

# calcul elevation range for each city
tab_cities$min.elev = NA # create a column for minimum values
tab_cities$max.elev = NA # create a column for maximum values

for(i in 1:length(tab_cities$FID)){ 
  elevation.city <- crop(elevation, Circle_Cities_USA.proj[i,]) # raster with city extent
  tab_cities$min.elev[i] <- cellStats(elevation.city, min) # minimum value of city raster
  tab_cities$max.elev[i] <- cellStats(elevation.city, max) # maximum value of city raster
}

tab_cities$range.elev = tab_cities$max.elev - tab_cities$min.elev # calcul the range

# subset the cities (with the value given at the beginning of script)
tab_cities <- tab_cities[tab_cities$range.elev < max.range,]

# Create a formal class SpatialPointsDataFrame with the 143 cities
Cities_USA <- SpatialPointsDataFrame(tab_cities[,c("Long", "Lat")], tab_cities)

# fill in the projection
crs(Cities_USA) = CRS("+proj=longlat +datum=WGS84")

# see your cities on map
mapview(Cities_USA)

# define a planar projection (to buffer and intersect cities)
crs_planar <- CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=39.7000122070312 +lon_0=-98 +x_0=0 +y_0=0 +datum=WGS84 +a=6370000 +b=6370000 +units=m +no_defs")

# reproject cities
Cities_USA.proj <- spTransform(Cities_USA,crs_planar)

# transform points into circles of 65km radius (polygon)
Circle_Cities_USA <- gBuffer(Cities_USA.proj,byid = T, width = city.zone.radius)

# see your circle cities on map
mapview(Circle_Cities_USA)

#  create a column for continental coverage of the city zone
tab_cities$not.cont = 0

# define area of 65km city zone
#city.area <- gArea(Circle_City.proj[1,])

# loop to extract area above continental surfaces 
for(y in 1:length(tab_cities$FID)){ # loop on selected big cities
  big.city <- terra::intersect(usa.sp,  Circle_Cities_USA[y,]) # new polygon obtain by intersection between usa continental surface and city area
  tab_cities$not.cont[y] = ((gArea(Circle_Cities_USA[y,]) - gArea(big.city)) / gArea(Circle_Cities_USA[y,])) * 100 # calcul % of the city area not above continent
}

# remove cities without sufficient area correctly measured by radars
tab_cities <- tab_cities[tab_cities$not.cont < max.uncover,]

# Create a formal class SpatialPointsDataFrame with the 143 cities
Cities_USA <- SpatialPointsDataFrame(tab_cities[,c("Long", "Lat")], tab_cities)

# fill in the projection
crs(Cities_USA) = CRS("+proj=longlat +datum=WGS84")

# see your cities on map
mapview(Cities_USA)

Circle_Cities_USA