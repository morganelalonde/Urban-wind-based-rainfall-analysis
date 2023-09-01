
# On charge les librairies

library(ggplot2)
library(readr)
library(ggpubr)

DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep où il y a le fichier de script
setwd(DIR)

DIR_data <- "00_Bulk/"

Cities_US <- read.csv("USA/USA_cities.txt",header=T,sep=';')
Cities_US$Urban.Agglomeration <- gsub(" ","_",Cities_US$Urban.Agglomeration)

# Les matrices de comparaison

mat_reference <- matrix(ncol=7, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_reference) <- c('Up', 'Ur', 'Dw', "Up_Ur", 'Up_Dw', 'data', 'order')
row.names(mat_reference) <- Cities_US$Urban.Agglomeration

mat_700hpa <- matrix(ncol=7, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_700hpa) <- c('Up', 'Ur', 'Dw', "Up_Ur", 'Up_Dw', 'data', 'order')
row.names(mat_700hpa) <- Cities_US$Urban.Agglomeration

mat_daily <- matrix(ncol=7, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_daily) <- c('Up', 'Ur', 'Dw', "Up_Ur", 'Up_Dw', 'data', 'order')
row.names(mat_daily) <- Cities_US$Urban.Agglomeration

mat_angle_45 <- matrix(ncol=7, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_angle_45) <- c('Up', 'Ur', 'Dw', "Up_Ur", 'Up_Dw', 'data', 'order')
row.names(mat_angle_45) <- Cities_US$Urban.Agglomeration

mat_radius_20 <- matrix(ncol=7, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_radius_20) <- c('Up', 'Ur', 'Dw', "Up_Ur", 'Up_Dw', 'data', 'order')
row.names(mat_radius_20) <- Cities_US$Urban.Agglomeration

mat_radius_dym <- matrix(ncol=7, nrow=length(Cities_US$Urban.Agglomeration))
colnames(mat_radius_dym) <- c('Up', 'Ur', 'Dw', "Up_Ur", 'Up_Dw', 'data', 'order')
row.names(mat_radius_dym) <- Cities_US$Urban.Agglomeration

for(i in 1:length( Cities_US$Urban.Agglomeration)){                      # Pour chaque ville (et pas pour chaque ligne)

  File_Name_ST4 <- paste0(DIR_data, "df_Res_TOT_",  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  mat_reference[i,1] <- Pacc_up
  mat_reference[i,2] <- Pacc_ur
  mat_reference[i,3] <- Pacc_dw
  mat_reference[i,4] <- ((Pacc_ur - Pacc_up)/Pacc_up)*100
  mat_reference[i,5] <- ((Pacc_dw - Pacc_up)/Pacc_up)*100
  mat_reference[i,6] <- 'Reference'
  mat_reference[i,7] <- '6'
  
  File_Name_ST4 <- paste0(DIR_data, "df_Res_TOT_",  Cities_US$Urban.Agglomeration[i],'_700.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  mat_700hpa[i,1] <- Pacc_up
  mat_700hpa[i,2] <- Pacc_ur
  mat_700hpa[i,3] <- Pacc_dw
  mat_700hpa[i,4] <- ((Pacc_ur - Pacc_up)/Pacc_up)*100
  mat_700hpa[i,5] <- ((Pacc_dw - Pacc_up)/Pacc_up)*100
  mat_700hpa[i,6] <- '700 hPa pressure level'
  mat_700hpa[i,7] <- '5'
  
  File_Name_ST4 <- paste0(DIR_data, "df_Res_TOT_daily_",  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  mat_daily[i,1] <- Pacc_up
  mat_daily[i,2] <- Pacc_ur
  mat_daily[i,3] <- Pacc_dw
  mat_daily[i,4] <- ((Pacc_ur - Pacc_up)/Pacc_up)*100
  mat_daily[i,5] <- ((Pacc_dw - Pacc_up)/Pacc_up)*100
  mat_daily[i,6] <- 'Daily time step'
  mat_daily[i,7] <- '4'
  
  File_Name_ST4 <- paste0(DIR_data, "df_Res_TOT_angle_45_",  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  mat_angle_45[i,1] <- Pacc_up
  mat_angle_45[i,2] <- Pacc_ur
  mat_angle_45[i,3] <- Pacc_dw
  mat_angle_45[i,4] <- ((Pacc_ur - Pacc_up)/Pacc_up)*100
  mat_angle_45[i,5] <- ((Pacc_dw - Pacc_up)/Pacc_up)*100
  mat_angle_45[i,6] <- '45° angle'
  mat_angle_45[i,7] <- '3'
  
  File_Name_ST4 <- paste0(DIR_data, "df_Res_TOT_radius_20_",  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  mat_radius_20[i,1] <- Pacc_up
  mat_radius_20[i,2] <- Pacc_ur
  mat_radius_20[i,3] <- Pacc_dw
  mat_radius_20[i,4] <- ((Pacc_ur - Pacc_up)/Pacc_up)*100
  mat_radius_20[i,5] <- ((Pacc_dw - Pacc_up)/Pacc_up)*100
  mat_radius_20[i,6] <- '20 km urban radius'
  mat_radius_20[i,7] <- '2'
  
  File_Name_ST4 <- paste0(DIR_data, "df_Res_TOT_radius_dym1_",  Cities_US$Urban.Agglomeration[i],'_850.TXT')     # Le fichier de la premiere période
  df_P <-  read_delim(file = File_Name_ST4, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  Pacc_up <- sum(df_P$P_up, na.rm=T)
  Pacc_ur <- sum(df_P$P_urb, na.rm=T)
  Pacc_dw <- sum(df_P$P_dw, na.rm=T)
  mat_radius_dym[i,1] <- Pacc_up
  mat_radius_dym[i,2] <- Pacc_ur
  mat_radius_dym[i,3] <- Pacc_dw
  mat_radius_dym[i,4] <- ((Pacc_ur - Pacc_up)/Pacc_up)*100
  mat_radius_dym[i,5] <- ((Pacc_dw - Pacc_up)/Pacc_up)*100
  mat_radius_dym[i,6] <- 'Dynamic urban radius'
  mat_radius_dym[i,7] <- '1'
  
   }





mat_to_plot <- rbind(mat_reference,
                     mat_700hpa,
                     mat_daily,
                     mat_angle_45,
                     mat_radius_20,
                     mat_radius_dym)



df_to_plot <- data.frame(mat_to_plot)
df_to_plot$Up  <- as.numeric(df_to_plot$Up)
df_to_plot$Ur  <- as.numeric(df_to_plot$Ur)
df_to_plot$Dw  <- as.numeric(df_to_plot$Dw)
df_to_plot$Up_Ur  <- as.numeric(df_to_plot$Up_Ur)
df_to_plot$Up_Dw  <- as.numeric(df_to_plot$Up_Dw)
df_to_plot$order  <- as.numeric(df_to_plot$order)

png("Results/sensitivity_analysis.png", width=50, height=10, units="cm", res=300)

a <- ggplot(df_to_plot, aes(x = reorder(data, order), y = Up_Ur)) +    # Create boxplot chart in ggplot2
  geom_boxplot(fill='grey')+theme_light(base_size=13)+ coord_flip()+
  xlab("Locations of urban and upwind zones") + ylab("(A) urban precipitation - upwind precipitation")#+ scale_fill_manual(values=c("#E69F00"))

b <- ggplot(df_to_plot, aes(x = reorder(data, order), y = Up_Dw)) +    # Create boxplot chart in ggplot2
  geom_boxplot(fill='grey')+theme_light(base_size=13)+ coord_flip()+
  xlab("Locations of downwind and upwind zones") + ylab("(B) downwind precipitation - upwind precipitation")#+ scale_fill_manual(values=c("#E69F00"))

ggarrange(a, b, ncol = 2)

dev.off()


png("Results/sensitivity_analysis2.png", width=18, height=8, units="cm", res=300)

a <- ggplot(df_to_plot, aes(x = reorder(data, order), y = Up_Ur)) +    # Create boxplot chart in ggplot2
  geom_boxplot(fill='grey', outlier.shape = NA)+theme_light(base_size=10)+ coord_flip()+
  ylim(-10, 22) + xlab("") + ylab("A. Urban precipitation - upwind precipitation for the different locations of \n urban and upwind zones")#+ scale_fill_manual(values=c("#E69F00"))

b <- ggplot(df_to_plot, aes(x = reorder(data, order), y = Up_Dw)) +    # Create boxplot chart in ggplot2
  geom_boxplot(fill='grey', outlier.shape = NA)+theme_light(base_size=10)+ coord_flip()+
  ylim(-10, 22) + xlab("") + ylab("B. Downwind precipitation - upwind precipitation for the different locations of \n downwind and upwind zones")#+ scale_fill_manual(values=c("#E69F00"))

ggarrange(a, b, nrow = 2)

dev.off()











