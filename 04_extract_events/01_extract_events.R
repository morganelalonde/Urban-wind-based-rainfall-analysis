#######################################################
### ALL PLOTS TO COMPARE IMPACT AT DIFFERENT SCALES ###
#######################################################

####################################################
# charge libraries
####################################################
library(IETD)
library(readr)
library(magrittr)

####################################################
# set directory path
####################################################
DIR = dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(DIR)

####################################################
# create the folders
####################################################
folder <- c("00_Bulk","01_Series","02_Angles","03_Events")

for (i in 1:length(folder)) {
  if (file.exists(folder[i])) {
    cat("The folder already exists")
  } else {
    dir.create(folder[i])
    cat(paste0(" folder ", folder[i], " created "))
  }
}
####################################################
# choose run options
####################################################

min_dry_period = 2
min_rainfall_depth_threshold = 0.5 # default value is 0.5
Var_angle_max = 30
list_File_start <- c("df_Res_TOT_")
File_start <- list_File_start[1]
P_level <- "_850"

#####################################################################
######################## 01 - EUROPE ################################
#####################################################################

# LOAD CITIES
Cities_EU <- read.csv("EUROPE/Europe_cities.TXT",header=T,sep=';')

# on extrait les evenements

system.time(
  
  for(i in 1:length(Cities_EU$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
    File_Name_ST4 <- paste0(DIR,'/00_Bulk/', Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_P <- read.table(file = File_Name_ST4, sep= ";",header = T) 
    df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
    df_P[is.na(df_P)] = 0
    
    series_Pup <- drawre(Time_series = df_P[,c(1,3)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_Pup <- matrix(ncol=2, nrow = length(series_Pup$Rainfall_Characteristics$Starting))
    mat_Pup[,1] = series_Pup$Rainfall_Characteristics$Starting
    mat_Pup[,2] = series_Pup$Rainfall_Characteristics$End
    mat_Pup = data.frame(mat_Pup)
    mat_Pup$X1 <- as.POSIXct(mat_Pup$X1, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    mat_Pup$X2 <- as.POSIXct(mat_Pup$X2, origin = "1970-01-01 00:00:00",  format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    write.table(mat_Pup, file = paste0("01_Series/series_Pup_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    
    series_Pdw <- drawre(Time_series = df_P[,c(1,4)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_Pdw <- matrix(ncol=2, nrow = length(series_Pdw$Rainfall_Characteristics$Starting))
    mat_Pdw[,1] = series_Pdw$Rainfall_Characteristics$Starting
    mat_Pdw[,2] = series_Pdw$Rainfall_Characteristics$End
    mat_Pdw = data.frame(mat_Pdw)
    mat_Pdw$X1 <- as.POSIXct(mat_Pdw$X1, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    mat_Pdw$X2 <- as.POSIXct(mat_Pdw$X2, origin = "1970-01-01 00:00:00",  format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    write.table(mat_Pdw, file = paste0("01_Series/series_Pdw_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    
    series_Pur <- drawre(Time_series = df_P[,c(1,7)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_Pur <- matrix(ncol=2, nrow = length(series_Pur$Rainfall_Characteristics$Starting))
    mat_Pur[,1] = series_Pur$Rainfall_Characteristics$Starting
    mat_Pur[,2] = series_Pur$Rainfall_Characteristics$End
    mat_Pur = data.frame(mat_Pur)
    mat_Pur$X1 <- as.POSIXct(mat_Pur$X1, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    mat_Pur$X2 <- as.POSIXct(mat_Pur$X2, origin = "1970-01-01 00:00:00",  format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    write.table(mat_Pur, file = paste0("01_Series/series_Pur_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    
  }
  
)


# on regarde si le vent varie trop  ( 3 x 8.5 min = 25 min)

# Pup
system.time(
for(i in 1:length(Cities_EU$Urban.Agglomeration)){ 
  File_Name_Series <- paste0("01_Series/series_Pup_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
  File_Name_W_ST4 <- paste0(DIR,"/00_Bulk/",  Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
  df_series <- read.table(file = File_Name_Series, sep= ";",header = T) 
  df_series$X1 = as.POSIXct(df_series$X1, tz = 'UTC')
  df_series$X2 = as.POSIXct(df_series$X2, tz = 'UTC')
  df_W <- read.table(file = File_Name_W_ST4, sep= ";",header = T) 
  df_W$Date = as.POSIXct(df_W$Date, tz = 'UTC')
  mat_var <- matrix(ncol=1, nrow = length(df_series$X1))
  
  df_W$wdir_to = mod(atan2(df_W$v,df_W$u)*180/pi + 180,360)
  
  for(n in 1:length(df_series$X1)){ 
    start <- df_series[n,1]
    end <- df_series[n,2]
    vec_winds <- df_W$wdir_to[df_W$Date >= start & df_W$Date <= end]
    if(length(vec_winds) == 0) mat_var[n,1] = 0
    else {
    mat_winds <- matrix(ncol = length(vec_winds), nrow = length(vec_winds))
     for (y in 1:length(vec_winds)) {
     mat_winds[,y]= vec_winds - vec_winds[y]
     }
    for (zr in 1:length(vec_winds)) {
      for (zc in 1:length(vec_winds)) {
        if(abs(mat_winds[zr,zc]) > 180) {
          mat_winds[zr,zc] = 360 - abs(mat_winds[zr,zc])
        }}}
    mat_var[n,1] = max(mat_winds)
    }
    write.table(mat_var, file = paste0("02_Angles/Angles_Pup_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
}
}
)

# Pdw
system.time(
  for(i in 1:length(Cities_EU$Urban.Agglomeration)){ 
    File_Name_Series <- paste0("01_Series/series_Pdw_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    File_Name_W_ST4 <- paste0(DIR,"/00_Bulk/", Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_series <- read.table(file = File_Name_Series, sep= ";",header = T) 
    df_series$X1 = as.POSIXct(df_series$X1, tz = 'UTC')
    df_series$X2 = as.POSIXct(df_series$X2, tz = 'UTC')
    df_W <- read.table(file = File_Name_W_ST4, sep= ";",header = T) 
    df_W$Date = as.POSIXct(df_W$Date, tz = 'UTC')
    mat_var <- matrix(ncol=1, nrow = length(df_series$X1))
    
    df_W$wdir_to = mod(atan2(df_W$v,df_W$u)*180/pi + 180,360)
    
    for(n in 1:length(df_series$X1)){ 
      start <- df_series[n,1]
      end <- df_series[n,2]
      vec_winds <- df_W$wdir_to[df_W$Date >= start & df_W$Date <= end]
      if(length(vec_winds) == 0) mat_var[n,1] = 0
      else {
        mat_winds <- matrix(ncol = length(vec_winds), nrow = length(vec_winds))
        for (y in 1:length(vec_winds)) {
          mat_winds[,y]= vec_winds - vec_winds[y]
        }
        for (zr in 1:length(vec_winds)) {
          for (zc in 1:length(vec_winds)) {
            if(abs(mat_winds[zr,zc]) > 180) {
              mat_winds[zr,zc] = 360 - abs(mat_winds[zr,zc])
            }}}
        mat_var[n,1] = max(mat_winds)
      }
      write.table(mat_var, file = paste0("02_Angles/Angles_Pdw_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    }
  }
)

# Pur
system.time(
  for(i in 1:length(Cities_EU$Urban.Agglomeration)){ 
    File_Name_Series <- paste0("01_Series/series_Pur_",  min_dry_period, '_mdp_', Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    File_Name_W_ST4 <- paste0(DIR,"/00_Bulk/", Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_series <- read.table(file = File_Name_Series, sep= ";",header = T) 
    df_series$X1 = as.POSIXct(df_series$X1, tz = 'UTC')
    df_series$X2 = as.POSIXct(df_series$X2, tz = 'UTC')
    df_W <- read.table(file = File_Name_W_ST4, sep= ";",header = T) 
    df_W$Date = as.POSIXct(df_W$Date, tz = 'UTC')
    mat_var <- matrix(ncol=1, nrow = length(df_series$X1))
    
    df_W$wdir_to = mod(atan2(df_W$v,df_W$u)*180/pi + 180,360)
    
    for(n in 1:length(df_series$X1)){ 
      start <- df_series[n,1]
      end <- df_series[n,2]
      vec_winds <- df_W$wdir_to[df_W$Date >= start & df_W$Date <= end]
      if(length(vec_winds) == 0) mat_var[n,1] = 0
      else {
        mat_winds <- matrix(ncol = length(vec_winds), nrow = length(vec_winds))
        for (y in 1:length(vec_winds)) {
          mat_winds[,y]= vec_winds - vec_winds[y]
        }
        for (zr in 1:length(vec_winds)) {
          for (zc in 1:length(vec_winds)) {
            if(abs(mat_winds[zr,zc]) > 180) {
              mat_winds[zr,zc] = 360 - abs(mat_winds[zr,zc])
            }}}
        mat_var[n,1] = max(mat_winds)
      }
      write.table(mat_var, file = paste0("02_Angles/Angles_Pur_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    }
  }
)


# on selectionne que les evenements qui nous interessent, et on en extrait les variables que l'on veut analyser ensuite ( 3 x 8 min = 24 min)
system.time(
  for(i in 1:length(Cities_EU$Urban.Agglomeration)){ 
    File_Name_dif <- paste0("02_Angles/Angles_Pup_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')   
    df_dif <- read.table(file = File_Name_dif, sep= ";",header = T) 
    File_Name_ST4 <- paste0(DIR,"/00_Bulk/", Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_P <- read.table(file = File_Name_ST4, sep= ";",header = T) 
    df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
    df_P[is.na(df_P)] = 0
    series_Pup <-     series_Pup <- drawre(Time_series = df_P[,c(1,3)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_events <- matrix(ncol=4, nrow=length(series_Pup$Rainfall_Characteristics$Number.Event))
    for(n in 1:length(df_dif$V1)){ 
      if(df_dif[n,1] <= Var_angle_max) {
        mat_events[n,1] = length(series_Pup$Rainfall_Events[[n]]$P_up)
        mat_events[n,2] = sum(series_Pup$Rainfall_Events[[n]]$P_up)
        mat_events[n,3] =  max(series_Pup$Rainfall_Events[[n]]$P_up)
        mat_events[n,4] =  mean(series_Pup$Rainfall_Events[[n]]$P_up)
      }
      else {
        mat_events[n,1] = NA
        mat_events[n,2] = NA
        mat_events[n,3] = NA
        mat_events[n,4] = NA
      }
      
    }
    mat_events <-  na.omit(mat_events)
    colnames(mat_events) <- c('lenght', 'sum', 'max', 'mean')
    write.table(mat_events, file = paste0("03_Events/Events_Pup_",  min_dry_period, '_mdp_', Cities_EU$Urban.Agglomeration[i],P_level,'.TXT'), row.names = F, , sep = ";")
  }
)

system.time(
for(i in 1:length(Cities_EU$Urban.Agglomeration)){ 
  File_Name_dif <- paste0("02_Angles/Angles_Pdw_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')   
  df_dif <- read.table(file = File_Name_dif, sep= ";",header = T) 
  File_Name_ST4 <- paste0(DIR,"/00_Bulk/", Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
  df_P <- read.table(file = File_Name_ST4, sep= ";",header = T) 
  df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
  df_P[is.na(df_P)] = 0
  series_Pdw <-     series_Pup <- drawre(Time_series = df_P[,c(1,4)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
  mat_events <- matrix(ncol=4, nrow=length(series_Pdw$Rainfall_Characteristics$Number.Event))
  for(n in 1:length(df_dif$V1)){ 
    if(df_dif[n,1] <= Var_angle_max) {
      mat_events[n,1] = length(series_Pdw$Rainfall_Events[[n]]$P_dw)
      mat_events[n,2] = sum(series_Pdw$Rainfall_Events[[n]]$P_dw)
      mat_events[n,3] =  max(series_Pdw$Rainfall_Events[[n]]$P_dw)
      mat_events[n,4] =  mean(series_Pdw$Rainfall_Events[[n]]$P_dw)
    }
    else {
      mat_events[n,1] = NA
      mat_events[n,2] = NA
      mat_events[n,3] = NA
      mat_events[n,4] = NA
    }

}
  mat_events <-  na.omit(mat_events)
  colnames(mat_events) <- c('lenght', 'sum', 'max', 'mean')
  write.table(mat_events, file = paste0("03_Events/Events_Pdw_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT'), row.names = F, , sep = ";")
}
)


system.time(
  for(i in 1:length(Cities_EU$Urban.Agglomeration)){ 
    File_Name_dif <- paste0("02_Angles/Angles_Pur_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')   
    df_dif <- read.table(file = File_Name_dif, sep= ";",header = T) 
    File_Name_ST4 <- paste0(DIR,"/00_Bulk/", Cities_EU$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_P <- read.table(file = File_Name_ST4, sep= ";",header = T) 
    df_P$Dates <- as.POSIXct(df_P$Dates,tz = 'UTC')   # date au format date
    df_P[is.na(df_P)] = 0
    series_Pur <-     series_Pur <- drawre(Time_series = df_P[,c(1,7)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_events <- matrix(ncol=4, nrow=length(series_Pur$Rainfall_Characteristics$Number.Event))
    for(n in 1:length(df_dif$V1)){ 
      if(df_dif[n,1] <= Var_angle_max) {
        mat_events[n,1] = length(series_Pur$Rainfall_Events[[n]]$P_ur)
        mat_events[n,2] = sum(series_Pur$Rainfall_Events[[n]]$P_ur)
        mat_events[n,3] =  max(series_Pur$Rainfall_Events[[n]]$P_ur)
        mat_events[n,4] =  mean(series_Pur$Rainfall_Events[[n]]$P_ur)
      }
      else {
        mat_events[n,1] = NA
        mat_events[n,2] = NA
        mat_events[n,3] = NA
        mat_events[n,4] = NA
      }
      
    }
    mat_events <-  na.omit(mat_events)
    colnames(mat_events) <- c('lenght', 'sum', 'max', 'mean')
    write.table(mat_events, file = paste0("03_Events/Events_Pur_",  min_dry_period, '_mdp_',Cities_EU$Urban.Agglomeration[i],P_level,'.TXT'), row.names = F, , sep = ";")
  }
)



#####################################################################
######################## 01 - USA ################################
#####################################################################

# LOAD CITIES
Cities_EU <- read.csv("USA/USA_cities.TXT",header=T,sep=';')
Cities_US$Urban.Agglomeration <- gsub(" ","_",Cities_US$Urban.Agglomeration)


# on extrait les evenements

system.time(
  
  for(i in 1:length(Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)
    File_Name_ST4 <- paste0(DIR,'/00_Bulk/',File_start, Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_P <- read.table(file = File_Name_ST4, sep= ";",header = T) 
    df_P$Date <- as.POSIXct(df_P$Date,tz = 'UTC')   # date au format date
    df_P[is.na(df_P)] = 0
    
    series_Pup <- drawre(Time_series = df_P[,c(1,17)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_Pup <- matrix(ncol=2, nrow = length(series_Pup$Rainfall_Characteristics$Starting))
    mat_Pup[,1] = series_Pup$Rainfall_Characteristics$Starting
    mat_Pup[,2] = series_Pup$Rainfall_Characteristics$End
    mat_Pup = data.frame(mat_Pup)
    mat_Pup$X1 <- as.POSIXct(mat_Pup$X1, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    mat_Pup$X2 <- as.POSIXct(mat_Pup$X2, origin = "1970-01-01 00:00:00",  format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    write.table(mat_Pup, file = paste0("01_Series/series_Pup_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    
    series_Pdw <- drawre(Time_series = df_P[,c(1,18)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_Pdw <- matrix(ncol=2, nrow = length(series_Pdw$Rainfall_Characteristics$Starting))
    mat_Pdw[,1] = series_Pdw$Rainfall_Characteristics$Starting
    mat_Pdw[,2] = series_Pdw$Rainfall_Characteristics$End
    mat_Pdw = data.frame(mat_Pdw)
    mat_Pdw$X1 <- as.POSIXct(mat_Pdw$X1, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    mat_Pdw$X2 <- as.POSIXct(mat_Pdw$X2, origin = "1970-01-01 00:00:00",  format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    write.table(mat_Pdw, file = paste0("01_Series/series_Pdw_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    
    series_Pur <- drawre(Time_series = df_P[,c(1,21)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_Pur <- matrix(ncol=2, nrow = length(series_Pur$Rainfall_Characteristics$Starting))
    mat_Pur[,1] = series_Pur$Rainfall_Characteristics$Starting
    mat_Pur[,2] = series_Pur$Rainfall_Characteristics$End
    mat_Pur = data.frame(mat_Pur)
    mat_Pur$X1 <- as.POSIXct(mat_Pur$X1, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    mat_Pur$X2 <- as.POSIXct(mat_Pur$X2, origin = "1970-01-01 00:00:00",  format = "%Y-%m-%d %H:%M:%OS", tz = 'UTC')   # date au format date
    write.table(mat_Pur, file = paste0("01_Series/series_Pur_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    
  }
  
)


# on regarde si le vent varie trop  ( 3 x 8.5 min = 25 min)

# Pup
system.time(
  for(i in 1:length(Cities_US$Urban.Agglomeration)){ 
    File_Name_Series <- paste0("01_Series/series_Pup_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    File_Name_W_ST4 <- paste0(DIR,"/00_Bulk/", File_start, Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_series <- read.table(file = File_Name_Series, sep= ";",header = T) 
    df_series$X1 = as.POSIXct(df_series$X1, tz = 'UTC')
    df_series$X2 = as.POSIXct(df_series$X2, tz = 'UTC')
    df_W <- read.table(file = File_Name_W_ST4, sep= ";",header = T) 
    df_W$Date = as.POSIXct(df_W$Date, tz = 'UTC')
    mat_var <- matrix(ncol=1, nrow = length(df_series$X1))
    
    for(n in 1:length(df_series$X1)){ 
      start <- df_series[n,1]
      end <- df_series[n,2]
      vec_winds <- df_W$wdir_to[df_W$Date >= start & df_W$Date <= end]
      if(length(vec_winds) == 0) mat_var[n,1] = 0
      else {
        mat_winds <- matrix(ncol = length(vec_winds), nrow = length(vec_winds))
        for (y in 1:length(vec_winds)) {
          mat_winds[,y]= vec_winds - vec_winds[y]
        }
        for (zr in 1:length(vec_winds)) {
          for (zc in 1:length(vec_winds)) {
            if(abs(mat_winds[zr,zc]) > 180) {
              mat_winds[zr,zc] = 360 - abs(mat_winds[zr,zc])
            }}}
        mat_var[n,1] = max(mat_winds)
      }
      write.table(mat_var, file = paste0("02_Angles/Angles_Pup_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    }
  }
)

# Pdw
system.time(
  for(i in 1:length(Cities_US$Urban.Agglomeration)){ 
    File_Name_Series <- paste0("01_Series/series_Pdw_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    File_Name_W_ST4 <- paste0(DIR,"/00_Bulk/", File_start,Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_series <- read.table(file = File_Name_Series, sep= ";",header = T) 
    df_series$X1 = as.POSIXct(df_series$X1, tz = 'UTC')
    df_series$X2 = as.POSIXct(df_series$X2, tz = 'UTC')
    df_W <- read.table(file = File_Name_W_ST4, sep= ";",header = T) 
    df_W$Date = as.POSIXct(df_W$Date, tz = 'UTC')
    mat_var <- matrix(ncol=1, nrow = length(df_series$X1))
    
    for(n in 1:length(df_series$X1)){ 
      start <- df_series[n,1]
      end <- df_series[n,2]
      vec_winds <- df_W$wdir_to[df_W$Date >= start & df_W$Date <= end]
      if(length(vec_winds) == 0) mat_var[n,1] = 0
      else {
        mat_winds <- matrix(ncol = length(vec_winds), nrow = length(vec_winds))
        for (y in 1:length(vec_winds)) {
          mat_winds[,y]= vec_winds - vec_winds[y]
        }
        for (zr in 1:length(vec_winds)) {
          for (zc in 1:length(vec_winds)) {
            if(abs(mat_winds[zr,zc]) > 180) {
              mat_winds[zr,zc] = 360 - abs(mat_winds[zr,zc])
            }}}
        mat_var[n,1] = max(mat_winds)
      }
      write.table(mat_var, file = paste0("02_Angles/Angles_Pdw_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    }
  }
)

# Pur
system.time(
  for(i in 1:length(Cities_US$Urban.Agglomeration)){ 
    File_Name_Series <- paste0("01_Series/series_Pur_", File_start, min_dry_period, '_mdp_', Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    File_Name_W_ST4 <- paste0(DIR,"/00_Bulk/", File_start,Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_series <- read.table(file = File_Name_Series, sep= ";",header = T) 
    df_series$X1 = as.POSIXct(df_series$X1, tz = 'UTC')
    df_series$X2 = as.POSIXct(df_series$X2, tz = 'UTC')
    df_W <- read.table(file = File_Name_W_ST4, sep= ";",header = T) 
    df_W$Date = as.POSIXct(df_W$Date, tz = 'UTC')
    mat_var <- matrix(ncol=1, nrow = length(df_series$X1))
    
    for(n in 1:length(df_series$X1)){ 
      start <- df_series[n,1]
      end <- df_series[n,2]
      vec_winds <- df_W$wdir_to[df_W$Date >= start & df_W$Date <= end]
      if(length(vec_winds) == 0) mat_var[n,1] = 0
      else {
        mat_winds <- matrix(ncol = length(vec_winds), nrow = length(vec_winds))
        for (y in 1:length(vec_winds)) {
          mat_winds[,y]= vec_winds - vec_winds[y]
        }
        for (zr in 1:length(vec_winds)) {
          for (zc in 1:length(vec_winds)) {
            if(abs(mat_winds[zr,zc]) > 180) {
              mat_winds[zr,zc] = 360 - abs(mat_winds[zr,zc])
            }}}
        mat_var[n,1] = max(mat_winds)
      }
      write.table(mat_var, file = paste0("02_Angles/Angles_Pur_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT'),row.names = F, , sep = ";")
    }
  }
)


# on selectionne que les evenements qui nous interessent, et on en extrait les variables que l'on veut analyser ensuite ( 3 x 8 min = 24 min)
system.time(
  for(i in 1:length(Cities_US$Urban.Agglomeration)){ 
    File_Name_dif <- paste0("02_Angles/Angles_Pup_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT')   
    df_dif <- read.table(file = File_Name_dif, sep= ";",header = T) 
    File_Name_ST4 <- paste0(DIR,"/00_Bulk/", File_start,Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_P <- read.table(file = File_Name_ST4, sep= ";",header = T) 
    df_P$Date <- as.POSIXct(df_P$Date,tz = 'UTC')   # date au format date
    df_P[is.na(df_P)] = 0
    series_Pup <-     series_Pup <- drawre(Time_series = df_P[,c(1,17)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_events <- matrix(ncol=4, nrow=length(series_Pup$Rainfall_Characteristics$Number.Event))
    for(n in 1:length(df_dif$V1)){ 
      if(df_dif[n,1] <= Var_angle_max) {
        mat_events[n,1] = length(series_Pup$Rainfall_Events[[n]]$P_up)
        mat_events[n,2] = sum(series_Pup$Rainfall_Events[[n]]$P_up)
        mat_events[n,3] =  max(series_Pup$Rainfall_Events[[n]]$P_up)
        mat_events[n,4] =  mean(series_Pup$Rainfall_Events[[n]]$P_up)
      }
      else {
        mat_events[n,1] = NA
        mat_events[n,2] = NA
        mat_events[n,3] = NA
        mat_events[n,4] = NA
      }
      
    }
    mat_events <-  na.omit(mat_events)
    colnames(mat_events) <- c('lenght', 'sum', 'max', 'mean')
    write.table(mat_events, file = paste0("03_Events/Events_Pup_", File_start, min_dry_period, '_mdp_', Cities_US$Urban.Agglomeration[i],P_level,'.TXT'), row.names = F, , sep = ";")
  }
)

system.time(
  for(i in 1:length(Cities_US$Urban.Agglomeration)){ 
    File_Name_dif <- paste0("02_Angles/Angles_Pdw_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT')   
    df_dif <- read.table(file = File_Name_dif, sep= ";",header = T) 
    File_Name_ST4 <- paste0(DIR,"/00_Bulk/", File_start,Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_P <- read.table(file = File_Name_ST4, sep= ";",header = T) 
    df_P$Date <- as.POSIXct(df_P$Date,tz = 'UTC')   # date au format date
    df_P[is.na(df_P)] = 0
    series_Pdw <-     series_Pup <- drawre(Time_series = df_P[,c(1,18)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_events <- matrix(ncol=4, nrow=length(series_Pdw$Rainfall_Characteristics$Number.Event))
    for(n in 1:length(df_dif$V1)){ 
      if(df_dif[n,1] <= Var_angle_max) {
        mat_events[n,1] = length(series_Pdw$Rainfall_Events[[n]]$P_dw)
        mat_events[n,2] = sum(series_Pdw$Rainfall_Events[[n]]$P_dw)
        mat_events[n,3] =  max(series_Pdw$Rainfall_Events[[n]]$P_dw)
        mat_events[n,4] =  mean(series_Pdw$Rainfall_Events[[n]]$P_dw)
      }
      else {
        mat_events[n,1] = NA
        mat_events[n,2] = NA
        mat_events[n,3] = NA
        mat_events[n,4] = NA
      }
      
    }
    mat_events <-  na.omit(mat_events)
    colnames(mat_events) <- c('lenght', 'sum', 'max', 'mean')
    write.table(mat_events, file = paste0("03_Events/Events_Pdw_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT'), row.names = F, , sep = ";")
  }
)


system.time(
  for(i in 1:length(Cities_US$Urban.Agglomeration)){ 
    File_Name_dif <- paste0("02_Angles/Angles_Pur_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT')   
    df_dif <- read.table(file = File_Name_dif, sep= ";",header = T) 
    File_Name_ST4 <- paste0(DIR,"/00_Bulk/", File_start,Cities_US$Urban.Agglomeration[i],P_level,'.TXT')     # Le fichier de la premiere période
    df_P <- read.table(file = File_Name_ST4, sep= ";",header = T) 
    df_P$Date <- as.POSIXct(df_P$Date,tz = 'UTC')   # date au format date
    df_P[is.na(df_P)] = 0
    series_Pur <-     series_Pur <- drawre(Time_series = df_P[,c(1,21)], IETD = min_dry_period, Thres = min_rainfall_depth_threshold)
    mat_events <- matrix(ncol=4, nrow=length(series_Pur$Rainfall_Characteristics$Number.Event))
    for(n in 1:length(df_dif$V1)){ 
      if(df_dif[n,1] <= Var_angle_max) {
        mat_events[n,1] = length(series_Pur$Rainfall_Events[[n]]$P_ur)
        mat_events[n,2] = sum(series_Pur$Rainfall_Events[[n]]$P_ur)
        mat_events[n,3] =  max(series_Pur$Rainfall_Events[[n]]$P_ur)
        mat_events[n,4] =  mean(series_Pur$Rainfall_Events[[n]]$P_ur)
      }
      else {
        mat_events[n,1] = NA
        mat_events[n,2] = NA
        mat_events[n,3] = NA
        mat_events[n,4] = NA
      }
      
    }
    mat_events <-  na.omit(mat_events)
    colnames(mat_events) <- c('lenght', 'sum', 'max', 'mean')
    write.table(mat_events, file = paste0("03_Events/Events_Pur_", File_start, min_dry_period, '_mdp_',Cities_US$Urban.Agglomeration[i],P_level,'.TXT'), row.names = F, , sep = ";")
  }
)




