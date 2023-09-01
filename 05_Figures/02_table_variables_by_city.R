#######################################################
### TABLE WITH ALL VARIABLES (PRECIP AND URBAN AND CLIMATE) ###
#######################################################

# On charge les librairies
library(readr)


# directory
DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep où il y a le fichier de script
setwd(DIR)

# cities

# usa
Cities_US <- read.csv("USA/USA_cities.txt",header=T,sep=';')
Cities_US$Urban.Agglomeration <- gsub(" ","_",Cities_US$Urban.Agglomeration)

# europe
Cities_EU <- read.csv("EUROPE/Europe_cities.txt",header=T,sep=';')

# all cities
#Cities <- rbind(Cities_EU, Cities_US)

#-------------------------------------------------------------------------------
# USA
#-------------------------------------------------------------------------------

# table results
tab_res_US <- matrix(ncol = 36, nrow = length(Cities_US$Urban.Agglomeration))
colnames(tab_res_US) <- c('City','mean precipitation', 
                          'beforeselec Up nb events','beforeselec Dw nb events','beforeselec Ur nb events',
                          'Up nb events','Dw nb events','Ur nb events',
                          'events lost up','events lost dw','events lost ur',
                          'extent', 'imperviousness','roughness', 
                          'sat_UHI_day', 'sat_UHI_night', 
                          'UHI_WC_day', 'UHI_WC_night',
                          'UHI_WC_day_winter', 'UHI_WC_night_winter',
                          'UHI_WC_day_spring', 'UHI_WC_night_spring',
                          'UHI_WC_day_summer', 'UHI_WC_night_summer',
                          'UHI_WC_day_fall', 'UHI_WC_night fall',
                          'Aerosol_PM10', 'Aerosol_PM25',
                          'Aerosol_PM10_winter', 'Aerosol_PM25_winter',
                          'Aerosol_PM10_spring', 'Aerosol_PM25_spring',
                          'Aerosol_PM10_summer', 'Aerosol_PM25_summer',
                          'Aerosol_PM10_fall', 'Aerosol_PM25_fall')

# cities names
tab_res_US[,1] <- Cities_US$Urban.Agglomeration

# mean precipitation

for(i in 1:length(Cities_US$Urban.Agglomeration)){ 
  df_city <- read_delim(paste0('00_Bulk/df_Res_TOT_',Cities_US$Urban.Agglomeration[i],'_850.TXT'), 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
  #(20 years)
  P_up <- sum(df_city$P_up, na.rm = T) / 20
  P_dw <- sum(df_city$P_dw, na.rm = T) / 20
  P_rt <- sum(df_city$P_rt, na.rm = T) / 20
  P_lt <- sum(df_city$P_lt, na.rm = T) / 20
  P_ur <- sum(df_city$P_urb, na.rm = T) / 20
  
  tab_res_US[i,2] <- mean(P_up, P_dw, P_rt, P_lt, P_ur)
}

# number of events before selection
for(i in 1:length(Cities_US$Urban.Agglomeration)){ 
  df_city <- read_delim(paste0('01_Series/Series_Pup_df_Res_TOT_2_mdp_',Cities_US$Urban.Agglomeration[i],'_850.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_US[i,3] <- length(df_city$X1)
  
  df_city <- read_delim(paste0('01_Series/Series_Pdw_df_Res_TOT_2_mdp_',Cities_US$Urban.Agglomeration[i],'_850.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_US[i,4] <- length(df_city$X1)
  
  df_city <- read_delim(paste0('01_Series/Series_Pur_df_Res_TOT_2_mdp_',Cities_US$Urban.Agglomeration[i],'_850.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_US[i,5] <- length(df_city$X1)
}

# number of events
for(i in 1:length(Cities_US$Urban.Agglomeration)){ 
  df_city <- read_delim(paste0('03_Events/Events_Pup_df_Res_TOT_2_mdp_',Cities_US$Urban.Agglomeration[i],'_850.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

  
  tab_res_US[i,6] <- length(df_city$lenght)
  
  df_city <- read_delim(paste0('03_Events/Events_Pdw_df_Res_TOT_2_mdp_',Cities_US$Urban.Agglomeration[i],'_850.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_US[i,7] <- length(df_city$lenght)
  
  df_city <- read_delim(paste0('03_Events/Events_Pur_df_Res_TOT_2_mdp_',Cities_US$Urban.Agglomeration[i],'_850.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_US[i,8] <- length(df_city$lenght)
}

# difference of events

tab_res_US[,9] =   as.numeric(tab_res_US[,3]) - as.numeric(tab_res_US[,6])
tab_res_US[,10] =   as.numeric(tab_res_US[,4]) - as.numeric(tab_res_US[,7])
tab_res_US[,11] =   as.numeric(tab_res_US[,5]) - as.numeric(tab_res_US[,8])


# urban extent

roughness_US <- read_delim("Variables/roughness/Roughness_USA.TXT", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
  tab_res_US[,12] <- roughness_US$extent

# imperviousness
  tab_res_US[,13] <- roughness_US$imperviousness
  
  # roughness
  tab_res_US[,14] <- roughness_US$roughness
  
# UHI day and night sattelite summer
ATTRIBUTION_VARIABLES <- read_delim("Variables/ATTRIBUTION_VARIABLES_US.TXT", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_US[,15] <- ATTRIBUTION_VARIABLES$D_T_DIFF
tab_res_US[,16] <- ATTRIBUTION_VARIABLES$N_T_DIFF       

# UHI day and night WORLDCLIM
UHI_USA <- read_delim("Variables/UHI/USA_UHI.TXT", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_US[,17] <- UHI_USA$uhi_day
tab_res_US[,18] <- UHI_USA$uhi_night

# UHI day and night WORLDCLIM winter
UHI_USA_winter <- read_delim("Variables/UHI/USA_UHI_winter.TXT", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_US[,19] <- UHI_USA_winter$uhi_day
tab_res_US[,20] <- UHI_USA_winter$uhi_night

# UHI day and night WORLDCLIM spring
UHI_USA_spring <- read_delim("Variables/UHI/USA_UHI_spring.TXT", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_US[,21] <- UHI_USA_spring$uhi_day
tab_res_US[,22] <- UHI_USA_spring$uhi_night

# UHI day and night WORLDCLIM summer
UHI_USA_summer <- read_delim("Variables/UHI/USA_UHI_summer.TXT", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_US[,23] <- UHI_USA_summer$uhi_day
tab_res_US[,24] <- UHI_USA_summer$uhi_night

# UHI day and night WORLDCLIM fall
UHI_USA_fall <- read_delim("Variables/UHI/USA_UHI_fall.TXT", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_US[,25] <- UHI_USA_fall$uhi_day
tab_res_US[,26] <- UHI_USA_fall$uhi_night

# aerosols
USA_aerosols <- read_delim("Variables/Aerosols/USA_aerosols.TXT", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
tab_res_US[,27] <- USA_aerosols$PM10     
tab_res_US[,28] <- USA_aerosols$PM2.5   
tab_res_US[,29] <- USA_aerosols$PM10_winter     
tab_res_US[,30] <- USA_aerosols$PM2.5_winter 
tab_res_US[,31] <- USA_aerosols$PM10_spring     
tab_res_US[,32] <- USA_aerosols$PM2.5_spring 
tab_res_US[,33] <- USA_aerosols$PM10_summer     
tab_res_US[,34] <- USA_aerosols$PM2.5_summer 
tab_res_US[,35] <- USA_aerosols$PM10_fall     
tab_res_US[,36] <- USA_aerosols$PM2.5_fall 

# total precipitation differences
total_P_dif_us <- read_csv("USA/Precipitation_USA.txt")
tab_res_US <- cbind(tab_res_US, total_P_dif_us[,-1])

# total precipitation differences
total_P_dif_us <- read_csv("USA/Precipitation_USA_winter.txt")
tab_res_US <- cbind(tab_res_US, total_P_dif_us[,-1])

# total precipitation differences
total_P_dif_us <- read_csv("USA/Precipitation_USA_spring.txt")
tab_res_US <- cbind(tab_res_US, total_P_dif_us[,-1])

# total precipitation differences
total_P_dif_us <- read_csv("USA/Precipitation_USA_summer.txt")
tab_res_US <- cbind(tab_res_US, total_P_dif_us[,-1])

# total precipitation differences
total_P_dif_us <- read_csv("USA/Precipitation_USA_fall.txt")
tab_res_US <- cbind(tab_res_US, total_P_dif_us[,-1])

#-------------------------------------------------------------------------------
# EUROPE
#-------------------------------------------------------------------------------


# table results
tab_res_EU <- matrix(ncol = 36, nrow = length(Cities_EU$Urban.Agglomeration))
colnames(tab_res_EU) <- c('City','mean precipitation', 
                          'beforeselec Up nb events','beforeselec Dw nb events','beforeselec Ur nb events',
                          'Up nb events','Dw nb events','Ur nb events',
                          'events lost up','events lost dw','events lost ur',
                          'extent', 'imperviousness','roughness', 
                          'sat_UHI_day', 'sat_UHI_night', 
                          'UHI_WC_day', 'UHI_WC_night',
                          'UHI_WC_day_winter', 'UHI_WC_night_winter',
                          'UHI_WC_day_spring', 'UHI_WC_night_spring',
                          'UHI_WC_day_summer', 'UHI_WC_night_summer',
                          'UHI_WC_day_fall', 'UHI_WC_night fall',
                          'Aerosol_PM10', 'Aerosol_PM25',
                          'Aerosol_PM10_winter', 'Aerosol_PM25_winter',
                          'Aerosol_PM10_spring', 'Aerosol_PM25_spring',
                          'Aerosol_PM10_summer', 'Aerosol_PM25_summer',
                          'Aerosol_PM10_fall', 'Aerosol_PM25_fall')

# cities names
tab_res_EU[,1] <- Cities_EU$Urban.Agglomeration

# mean precipitation

for(i in 1:length(Cities_EU$Urban.Agglomeration)){ 
  df_city <- read_delim(paste0('00_Bulk/',Cities_EU$Urban.Agglomeration[i],'.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  #(20 years)
  P_up <- sum(df_city$P_up, na.rm = T) / 8
  P_dw <- sum(df_city$P_dw, na.rm = T) / 8
  P_rt <- sum(df_city$P_rt, na.rm = T) / 8
  P_lt <- sum(df_city$P_lt, na.rm = T) / 8
  P_ur <- sum(df_city$P_urb, na.rm = T) / 8
  
  tab_res_EU[i,2] <- mean(P_up, P_dw, P_rt, P_lt, P_ur)
}

# before selection, number of events
for(i in 1:length(Cities_EU$Urban.Agglomeration)){ 
  df_city <- read_delim(paste0('01_Series/Series_Pup_2_mdp_',Cities_EU$Urban.Agglomeration[i],'.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_EU[i,3] <- length(df_city$X1)
  
  df_city <- read_delim(paste0('01_Series/Series_Pdw_2_mdp_',Cities_EU$Urban.Agglomeration[i],'.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_EU[i,4] <- length(df_city$X1)
  
  df_city <- read_delim(paste0('01_Series/Series_Pur_2_mdp_',Cities_EU$Urban.Agglomeration[i],'.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_EU[i,5] <- length(df_city$X1)
}

# number of events
for(i in 1:length(Cities_EU$Urban.Agglomeration)){ 
  df_city <- read_delim(paste0('03_Events/Events_Pup_2_mdp_',Cities_EU$Urban.Agglomeration[i],'.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_EU[i,6] <- length(df_city$lenght)
  
  df_city <- read_delim(paste0('03_Events/Events_Pdw_2_mdp_',Cities_EU$Urban.Agglomeration[i],'.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_EU[i,7] <- length(df_city$lenght)
  
  df_city <- read_delim(paste0('03_Events/Events_Pur_2_mdp_',Cities_EU$Urban.Agglomeration[i],'.TXT'), 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  tab_res_EU[i,8] <- length(df_city$lenght)
}

# difference of events

tab_res_EU[,9] =   as.numeric(tab_res_EU[,3]) - as.numeric(tab_res_EU[,6])
tab_res_EU[,10] =   as.numeric(tab_res_EU[,4]) - as.numeric(tab_res_EU[,7])
tab_res_EU[,11] =   as.numeric(tab_res_EU[,5]) - as.numeric(tab_res_EU[,8])

# urban extent

roughness_EU <- read_delim("Variables/Roughness/Roughness_EUROPE.TXT", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
tab_res_EU[,12] <- roughness_EU$extent

# imperviousness
tab_res_EU[,13] <- roughness_EU$imperviousness

# roughness
tab_res_EU[,14] <- roughness_EU$roughness

# UHI day and night sattelite summer
ATTRIBUTION_VARIABLES <- read_delim("Variables/ATTRIBUTION_VARIABLES_EU.TXT", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_EU[,15] <- ATTRIBUTION_VARIABLES$D_T_DIFF
tab_res_EU[,16] <- ATTRIBUTION_VARIABLES$N_T_DIFF       

# UHI day and night WORLDCLIM
UHI_EUROPE <- read_delim("Variables/UHI/EUROPE_UHI.TXT", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_EU[,17] <- UHI_EUROPE$uhi_day
tab_res_EU[,18] <- UHI_EUROPE$uhi_night

# UHI day and night WORLDCLIM winter
UHI_EUROPE_winter <- read_delim("Variables/UHI/EUROPE_UHI_winter.TXT", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_EU[,19] <- UHI_EUROPE_winter$uhi_day
tab_res_EU[,20] <- UHI_EUROPE_winter$uhi_night

# UHI day and night WORLDCLIM spring
UHI_EUROPE_spring <- read_delim("Variables/UHI/EUROPE_UHI_spring.TXT", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_EU[,21] <- UHI_EUROPE_spring$uhi_day
tab_res_EU[,22] <- UHI_EUROPE_spring$uhi_night

# UHI day and night WORLDCLIM summer
UHI_EUROPE_summer <- read_delim("Variables/UHI/EUROPE_UHI_summer.TXT", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_EU[,23] <- UHI_EUROPE_summer$uhi_day
tab_res_EU[,24] <- UHI_EUROPE_summer$uhi_night

# UHI day and night WORLDCLIM fall
UHI_EUROPE_fall <- read_delim("Variables/UHI/EUROPE_UHI_fall.TXT", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

tab_res_EU[,25] <- UHI_EUROPE_fall$uhi_day
tab_res_EU[,26] <- UHI_EUROPE_fall$uhi_night

# aerosols
EUROPE_aerosols <- read_delim("Variables/Aerosols/EU_aerosols.TXT", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
tab_res_EU[,27] <- EUROPE_aerosols$PM10     
tab_res_EU[,28] <- EUROPE_aerosols$PM2.5  
tab_res_EU[,29] <- EUROPE_aerosols$PM10_winter     
tab_res_EU[,30] <- EUROPE_aerosols$PM2.5_winter 
tab_res_EU[,31] <- EUROPE_aerosols$PM10_spring     
tab_res_EU[,32] <- EUROPE_aerosols$PM2.5_spring 
tab_res_EU[,33] <- EUROPE_aerosols$PM10_summer     
tab_res_EU[,34] <- EUROPE_aerosols$PM2.5_summer 
tab_res_EU[,35] <- EUROPE_aerosols$PM10_fall     
tab_res_EU[,36] <- EUROPE_aerosols$PM2.5_fall 

# total precipitation differences
total_P_dif_EU <- read_csv("EUROPE/Precipitation_EUROPE.txt")
tab_res_EU <- cbind(tab_res_EU, total_P_dif_EU[,-1])

# total precipitation differences
total_P_dif_EU <- read_csv("EUROPE/Precipitation_EUROPE_winter.txt")
tab_res_EU <- cbind(tab_res_EU, total_P_dif_EU[,-1])

# total precipitation differences
total_P_dif_EU <- read_csv("EUROPE/Precipitation_EUROPE_spring.txt")
tab_res_EU <- cbind(tab_res_EU, total_P_dif_EU[,-1])

# total precipitation differences
total_P_dif_EU <- read_csv("EUROPE/Precipitation_EUROPE_summer.txt")
tab_res_EU <- cbind(tab_res_EU, total_P_dif_EU[,-1])

# total precipitation differences
total_P_dif_EU <- read_csv("EUROPE/Precipitation_EUROPE_fall.txt")
tab_res_EU <- cbind(tab_res_EU, total_P_dif_EU[,-1])



################################################################################

# regroup both

tab_res <- rbind(tab_res_EU, tab_res_US)

write.table(tab_res, 'Results/table_variables_by_city.txt', sep= ';', row.names = F)


          