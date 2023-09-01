#######################################################################
# 
#######################################################################

# On charge les librairies
library(rstudioapi) #confort pour chemin des rep et fichiers
library(R.utils)    #confort pour chemin des rep et fichiers
library(ggplot2)
library(ggrepel) # Pour mieux gérer les label dans ggplot
library(ismev) # Pour les ajustements de Gumbel
library(readxl)
library(gplots)
library(rgdal)
library(rgeos)
library(mapview)
library(sp)
library(raster)
library(sf)
library(readr)

# On indique le repertoire de travail
DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep o? il y a le fichier de script
setwd(DIR)

# On charge les villes
Cities_US <- read.csv("USA/USA_cities.txt",header=T,sep=';')
Cities_US$Urban.Agglomeration <- gsub(" ","_",Cities_US$Urban.Agglomeration)

mat_reference <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference) <- c('Ur', 'Up', 'Dw', 'Lt', 'Rt', 
                             'Ur_Up', 'Dw_Up', 'Lt_Rt',
                             'Ur_Up_duration','Ur_Up_occurence','Ur_Up_accumulation','Ur_Up_intensity','Ur_Up_maxintensity',
                             'Dw_Up_duration','Dw_Up_occurence','Dw_Up_accumulation','Dw_Up_intensity','Dw_Up_maxintensity')

row.names(mat_reference) <- Cities_US$Urban.Agglomeration

for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/df_Res_TOT_',  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference[i,1] <- Pacc_ur
  mat_reference[i,2] <- Pacc_up
  mat_reference[i,3] <- Pacc_dw
  mat_reference[i,4] <- Pacc_lt
  mat_reference[i,5] <- Pacc_rt
  mat_reference[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
}

df_ref <- data.frame(mat_reference)
write.csv(df_ref, 'USA/Precipitation_USA.txt',col.names=T,sep=';')


# winter

mat_reference_winter <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference_winter) <- c('winter_Ur', 'winter_Up', 'winter_Dw', 'winter_Lt', 'winter_Rt', 
                                    'winter_Ur_Up', 'winter_Dw_Up', 'winter_Lt_Rt',
                                    'winter_Ur_Up_duration','winter_Ur_Up_occurence','winter_Ur_Up_accumulation','winter_Ur_Up_intensity','winter_Ur_Up_maxintensity',
                                    'winter_Dw_Up_duration','winter_Dw_Up_occurence','winter_Dw_Up_accumulation','winter_Dw_Up_intensity','winter_Dw_Up_maxintensity')

row.names(mat_reference_winter) <- Cities_US$Urban.Agglomeration


for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/df_Res_TOT_',  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
  df_P[is.na(df_P)] = 0
  df_P$month <- months(df_P$Dates)
  df_P <- df_P[df_P$month == 'décembre' | df_P$month == 'janvier' |df_P$month == 'février' ,]
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference_winter[i,1] <- Pacc_ur
  mat_reference_winter[i,2] <- Pacc_up
  mat_reference_winter[i,3] <- Pacc_dw
  mat_reference_winter[i,4] <- Pacc_lt
  mat_reference_winter[i,5] <- Pacc_rt
  mat_reference_winter[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference_winter[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference_winter[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_winter_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_winter_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_winter_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference_winter[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_winter[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_winter[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_winter[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_winter[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference_winter[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_winter[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_winter[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_winter[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_winter[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
}

df_ref <- data.frame(mat_reference_winter)
write.csv(df_ref, 'USA/Precipitation_USA_winter.txt',col.names=T,sep=';')



# spring

mat_reference_spring <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference_spring) <- c('spring_Ur', 'spring_Up', 'spring_Dw', 'spring_Lt', 'spring_Rt', 
                                    'spring_Ur_Up', 'spring_Dw_Up', 'spring_Lt_Rt',
                                    'spring_Ur_Up_duration','spring_Ur_Up_occurence','spring_Ur_Up_accumulation','spring_Ur_Up_intensity','spring_Ur_Up_maxintensity',
                                    'spring_Dw_Up_duration','spring_Dw_Up_occurence','spring_Dw_Up_accumulation','spring_Dw_Up_intensity','spring_Dw_Up_maxintensity')

row.names(mat_reference_spring) <- Cities_US$Urban.Agglomeration


for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/df_Res_TOT_',  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
  df_P[is.na(df_P)] = 0
  df_P$month <- months(df_P$Dates)
  df_P <- df_P[df_P$month == 'mars' | df_P$month == 'avril' |df_P$month == 'mai' ,]
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference_spring[i,1] <- Pacc_ur
  mat_reference_spring[i,2] <- Pacc_up
  mat_reference_spring[i,3] <- Pacc_dw
  mat_reference_spring[i,4] <- Pacc_lt
  mat_reference_spring[i,5] <- Pacc_rt
  mat_reference_spring[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference_spring[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference_spring[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_spring_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_spring_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_spring_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference_spring[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_spring[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_spring[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_spring[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_spring[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference_spring[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_spring[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_spring[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_spring[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_spring[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
}

df_ref <- data.frame(mat_reference_spring)
write.csv(df_ref, 'USA/Precipitation_USA_spring.txt',col.names=T,sep=';')


# summer

mat_reference_summer <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference_summer) <- c('summer_Ur', 'summer_Up', 'summer_Dw', 'summer_Lt', 'summer_Rt', 
                                    'summer_Ur_Up', 'summer_Dw_Up', 'summer_Lt_Rt',
                                    'summer_Ur_Up_duration','summer_Ur_Up_occurence','summer_Ur_Up_accumulation','summer_Ur_Up_intensity','summer_Ur_Up_maxintensity',
                                    'summer_Dw_Up_duration','summer_Dw_Up_occurence','summer_Dw_Up_accumulation','summer_Dw_Up_intensity','summer_Dw_Up_maxintensity')

row.names(mat_reference_summer) <- Cities_US$Urban.Agglomeration


for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/df_Res_TOT_',  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
  df_P[is.na(df_P)] = 0
  df_P$month <- months(df_P$Dates)
  df_P <- df_P[df_P$month == 'juin' | df_P$month == 'juillet' |df_P$month == 'août' ,]
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference_summer[i,1] <- Pacc_ur
  mat_reference_summer[i,2] <- Pacc_up
  mat_reference_summer[i,3] <- Pacc_dw
  mat_reference_summer[i,4] <- Pacc_lt
  mat_reference_summer[i,5] <- Pacc_rt
  mat_reference_summer[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference_summer[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference_summer[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_summer_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_summer_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_summer_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference_summer[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_summer[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_summer[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_summer[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_summer[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference_summer[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_summer[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_summer[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_summer[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_summer[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
}

df_ref <- data.frame(mat_reference_summer)
write.csv(df_ref, 'USA/Precipitation_USA_summer.txt',col.names=T,sep=';')


# fall

mat_reference_fall <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference_fall) <- c('fall_Ur', 'fall_Up', 'fall_Dw', 'fall_Lt', 'fall_Rt', 
                                  'fall_Ur_Up', 'fall_Dw_Up', 'fall_Lt_Rt',
                                  'fall_Ur_Up_duration','fall_Ur_Up_occurence','fall_Ur_Up_accumulation','fall_Ur_Up_intensity','fall_Ur_Up_maxintensity',
                                  'fall_Dw_Up_duration','fall_Dw_Up_occurence','fall_Dw_Up_accumulation','fall_Dw_Up_intensity','fall_Dw_Up_maxintensity')

row.names(mat_reference_fall) <- Cities_US$Urban.Agglomeration


for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/df_Res_TOT_',  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
  df_P[is.na(df_P)] = 0
  df_P$month <- months(df_P$Dates)
  df_P <- df_P[df_P$month == 'septembre' | df_P$month == 'octobre' |df_P$month == 'novembre' ,]
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference_fall[i,1] <- Pacc_ur
  mat_reference_fall[i,2] <- Pacc_up
  mat_reference_fall[i,3] <- Pacc_dw
  mat_reference_fall[i,4] <- Pacc_lt
  mat_reference_fall[i,5] <- Pacc_rt
  mat_reference_fall[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference_fall[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference_fall[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_fall_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_fall_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_fall_", "df_Res_TOT_", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'_850.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference_fall[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_fall[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_fall[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_fall[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_fall[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference_fall[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_fall[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_fall[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_fall[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_fall[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
 }

df_ref <- data.frame(mat_reference_fall)
write.csv(df_ref, 'USA/Precipitation_USA_fall.txt',col.names=T,sep=';')


# EUROPE ------------------------------------------------------------------------


# On charge les villes
Cities_US <- read.csv("EUROPE/Europe_cities.txt",header=T,sep=';')
Cities_US$Urban.Agglomeration <- gsub(" ","_",Cities_US$Urban.Agglomeration)

mat_reference <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference) <- c('Ur', 'Up', 'Dw', 'Lt', 'Rt', 
                             'Ur_Up', 'Dw_Up', 'Lt_Rt',
                             'Ur_Up_duration','Ur_Up_occurence','Ur_Up_accumulation','Ur_Up_intensity','Ur_Up_maxintensity',
                             'Dw_Up_duration','Dw_Up_occurence','Dw_Up_accumulation','Dw_Up_intensity','Dw_Up_maxintensity')

row.names(mat_reference) <- Cities_US$Urban.Agglomeration

for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/',  Cities_US$Urban.Agglomeration[i],'.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference[i,1] <- Pacc_ur
  mat_reference[i,2] <- Pacc_up
  mat_reference[i,3] <- Pacc_dw
  mat_reference[i,4] <- Pacc_lt
  mat_reference[i,5] <- Pacc_rt
  mat_reference[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
}

df_ref <- data.frame(mat_reference)
write.csv(df_ref, 'EUROPE/Precipitation_EUROPE.txt',col.names=T,sep=';')


# winter

mat_reference_winter <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference_winter) <- c('winter_Ur', 'winter_Up', 'winter_Dw', 'winter_Lt', 'winter_Rt', 
                                    'winter_Ur_Up', 'winter_Dw_Up', 'winter_Lt_Rt',
                                    'winter_Ur_Up_duration','winter_Ur_Up_occurence','winter_Ur_Up_accumulation','winter_Ur_Up_intensity','winter_Ur_Up_maxintensity',
                                    'winter_Dw_Up_duration','winter_Dw_Up_occurence','winter_Dw_Up_accumulation','winter_Dw_Up_intensity','winter_Dw_Up_maxintensity')

row.names(mat_reference_winter) <- Cities_US$Urban.Agglomeration


for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/',  Cities_US$Urban.Agglomeration[i],'.TXT')      # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
  df_P[is.na(df_P)] = 0
  df_P$month <- months(df_P$Dates)
  df_P <- df_P[df_P$month == 'décembre' | df_P$month == 'janvier' |df_P$month == 'février' ,]
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference_winter[i,1] <- Pacc_ur
  mat_reference_winter[i,2] <- Pacc_up
  mat_reference_winter[i,3] <- Pacc_dw
  mat_reference_winter[i,4] <- Pacc_lt
  mat_reference_winter[i,5] <- Pacc_rt
  mat_reference_winter[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference_winter[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference_winter[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_winter_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_winter_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_winter_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference_winter[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_winter[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_winter[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_winter[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_winter[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference_winter[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_winter[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_winter[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_winter[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_winter[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
}

df_ref <- data.frame(mat_reference_winter)
write.csv(df_ref, 'EUROPE/Precipitation_EUROPE_winter.txt',col.names=T,sep=';')



# spring

mat_reference_spring <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference_spring) <- c('spring_Ur', 'spring_Up', 'spring_Dw', 'spring_Lt', 'spring_Rt', 
                                    'spring_Ur_Up', 'spring_Dw_Up', 'spring_Lt_Rt',
                                    'spring_Ur_Up_duration','spring_Ur_Up_occurence','spring_Ur_Up_accumulation','spring_Ur_Up_intensity','spring_Ur_Up_maxintensity',
                                    'spring_Dw_Up_duration','spring_Dw_Up_occurence','spring_Dw_Up_accumulation','spring_Dw_Up_intensity','spring_Dw_Up_maxintensity')

row.names(mat_reference_spring) <- Cities_US$Urban.Agglomeration


for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/',  Cities_US$Urban.Agglomeration[i],'.TXT')      # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
  df_P[is.na(df_P)] = 0
  df_P$month <- months(df_P$Dates)
  df_P <- df_P[df_P$month == 'mars' | df_P$month == 'avril' |df_P$month == 'mai' ,]
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference_spring[i,1] <- Pacc_ur
  mat_reference_spring[i,2] <- Pacc_up
  mat_reference_spring[i,3] <- Pacc_dw
  mat_reference_spring[i,4] <- Pacc_lt
  mat_reference_spring[i,5] <- Pacc_rt
  mat_reference_spring[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference_spring[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference_spring[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_spring_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_spring_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_spring_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference_spring[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_spring[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_spring[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_spring[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_spring[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference_spring[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_spring[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_spring[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_spring[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_spring[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
}

df_ref <- data.frame(mat_reference_spring)
write.csv(df_ref, 'EUROPE/Precipitation_EUROPE_spring.txt',col.names=T,sep=';')


# summer

mat_reference_summer <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference_summer) <- c('summer_Ur', 'summer_Up', 'summer_Dw', 'summer_Lt', 'summer_Rt', 
                                    'summer_Ur_Up', 'summer_Dw_Up', 'summer_Lt_Rt',
                                    'summer_Ur_Up_duration','summer_Ur_Up_occurence','summer_Ur_Up_accumulation','summer_Ur_Up_intensity','summer_Ur_Up_maxintensity',
                                    'summer_Dw_Up_duration','summer_Dw_Up_occurence','summer_Dw_Up_accumulation','summer_Dw_Up_intensity','summer_Dw_Up_maxintensity')

row.names(mat_reference_summer) <- Cities_US$Urban.Agglomeration


for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/',  Cities_US$Urban.Agglomeration[i],'.TXT')    # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
  df_P[is.na(df_P)] = 0
  df_P$month <- months(df_P$Dates)
  df_P <- df_P[df_P$month == 'juin' | df_P$month == 'juillet' |df_P$month == 'août' ,]
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference_summer[i,1] <- Pacc_ur
  mat_reference_summer[i,2] <- Pacc_up
  mat_reference_summer[i,3] <- Pacc_dw
  mat_reference_summer[i,4] <- Pacc_lt
  mat_reference_summer[i,5] <- Pacc_rt
  mat_reference_summer[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference_summer[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference_summer[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_summer_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_summer_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_summer_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference_summer[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_summer[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_summer[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_summer[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_summer[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference_summer[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_summer[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_summer[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_summer[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_summer[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
}

df_ref <- data.frame(mat_reference_summer)
write.csv(df_ref, 'EUROPE/Precipitation_EUROPE_summer.txt',col.names=T,sep=';')


# fall

mat_reference_fall <- matrix(ncol=18, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference_fall) <- c('fall_Ur', 'fall_Up', 'fall_Dw', 'fall_Lt', 'fall_Rt', 
                                  'fall_Ur_Up', 'fall_Dw_Up', 'fall_Lt_Rt',
                                  'fall_Ur_Up_duration','fall_Ur_Up_occurence','fall_Ur_Up_accumulation','fall_Ur_Up_intensity','fall_Ur_Up_maxintensity',
                                  'fall_Dw_Up_duration','fall_Dw_Up_occurence','fall_Dw_Up_accumulation','fall_Dw_Up_intensity','fall_Dw_Up_maxintensity')

row.names(mat_reference_fall) <- Cities_US$Urban.Agglomeration


for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
  
  File_Name_ST4 <- paste0('00_Bulk/',  Cities_US$Urban.Agglomeration[i],'.TXT')      # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
  df_P[is.na(df_P)] = 0
  df_P$month <- months(df_P$Dates)
  df_P <- df_P[df_P$month == 'septembre' | df_P$month == 'octobre' |df_P$month == 'novembre' ,]
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  Pacc_lt <- sum(df_P$P_lt, na.rm=T)
  Pacc_rt <- sum(df_P$P_rt, na.rm=T)
  mat_reference_fall[i,1] <- Pacc_ur
  mat_reference_fall[i,2] <- Pacc_up
  mat_reference_fall[i,3] <- Pacc_dw
  mat_reference_fall[i,4] <- Pacc_lt
  mat_reference_fall[i,5] <- Pacc_rt
  mat_reference_fall[i,6] <- ((Pacc_ur - Pacc_up)/Pacc_up) * 100
  mat_reference_fall[i,7] <- ((Pacc_dw - Pacc_up)/Pacc_up) * 100
  mat_reference_fall[i,8] <- ((Pacc_lt - Pacc_rt)/Pacc_rt) * 100
  
  File_Name_events_up <- paste0("03_Events/Events_Pup_fall_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_up <- read.table(file = File_Name_events_up, sep= ";",header = T) 
  File_Name_events_dw <- paste0("03_Events/Events_Pdw_fall_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_dw <- read.table(file = File_Name_events_dw, sep= ";",header = T) 
  File_Name_events_ur <- paste0("03_Events/Events_Pur_fall_", "", "2", '_mdp_', Cities_US$Urban.Agglomeration[i],'.TXT')     
  df_events_ur <- read.table(file = File_Name_events_ur, sep= ";",header = T)  
  
  mat_reference_fall[i,9] = ((mean(df_events_ur$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_fall[i,10] = ((length(df_events_ur$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_fall[i,11] = ((mean(df_events_ur$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_fall[i,12] = ((mean(df_events_ur$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_fall[i,13] = ((mean(df_events_ur$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
  mat_reference_fall[i,14] = ((mean(df_events_dw$lenght) - mean(df_events_up$lenght)) / mean(df_events_up$lenght)) * 100
  mat_reference_fall[i,15] = ((length(df_events_dw$sum) - length(df_events_up$sum)) / length(df_events_up$sum)) * 100
  mat_reference_fall[i,16] = ((mean(df_events_dw$sum) - mean(df_events_up$sum)) / mean(df_events_up$sum)) * 100
  mat_reference_fall[i,17] = ((mean(df_events_dw$mean) - mean(df_events_up$mean)) / mean(df_events_up$mean)) * 100
  mat_reference_fall[i,18] = ((mean(df_events_dw$max) - mean(df_events_up$max)) / mean(df_events_up$max)) * 100
  
}

df_ref <- data.frame(mat_reference_fall)
write.csv(df_ref, 'EUROPE/Precipitation_EUROPE_fall.txt',col.names=T,sep=';')



