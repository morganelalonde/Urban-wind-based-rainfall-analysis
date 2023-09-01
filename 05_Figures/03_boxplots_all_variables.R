#######################################################
### BOXPLOTS OF ALL VARIABLES ###
#######################################################

# On charge les librairies
library(readr)


# directory
DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep où il y a le fichier de script
setwd(DIR)

all_var <- read_delim("Results/table_variables_by_city.txt", delim = ";", escape_double = FALSE, trim_ws = TRUE)
all_var$region = NA
all_var$region[1:22] = 'EUROPE'
all_var$region[22:59] = 'USA'

all_var$Lt_Up <- ((all_var$Lt - all_var$Up) / all_var$Up) *100
all_var$Rt_Up <- ((all_var$Rt - all_var$Up) / all_var$Up)*100


plot(all_var$Aerosol_PM10, 
     all_var$Lt_Up)

plot(all_var$Aerosol_PM10, 
     all_var$Rt_Up)

plot(all_var$imperviousness, 
     all_var$Lt_Up)

plot(all_var$imperviousness, 
     all_var$Rt_Up)


cor(all_var$imperviousness, all_var$Rt_Up, method = "spearman")
cor(all_var$imperviousness, all_var$Lt_Up, method = "spearman")
cor(all_var$Aerosol_PM10, all_var$Rt_Up, method = "spearman")
cor(all_var$Aerosol_PM25, all_var$Rt_Up, method = "spearman")
cor(all_var$Aerosol_PM10, all_var$Lt_Up, method = "spearman")


par(mfrow=c(1,3))
boxplot(all_var[,3:11], las=2, ylim = c(0, 2500))
boxplot(all_var[1:22,3:11], las=2, ylim = c(0, 2500))
boxplot(all_var[22:59,3:11], las=2, ylim = c(0, 2500))

ggplot(all_var,aes(x = region, y = imperviousness))+
  geom_boxplot()+
  ylab('')+theme_bw()

ggplot(all_var,aes(x = region, y = roughness))+
  geom_boxplot()+
  ylab('')+theme_bw()

ggplot(all_var,aes(x = region, y = extent))+
  geom_boxplot()+
  ylab('')+theme_bw()

par(mfrow=c(1,3))
boxplot(all_var[,15:26], las=2, ylim = c(-1, 4))
boxplot(all_var[1:22,15:26], las=2, ylim = c(-1, 4))
boxplot(all_var[22:59,15:26], las=2, ylim = c(-1, 4))

par(mfrow=c(1,3))
boxplot(all_var[,27:28], las=2)
boxplot(all_var[1:22,27:28], las=2)
boxplot(all_var[22:59,27:28], las=2)

par(mfrow=c(1,3))
boxplot(all_var[,29:33], las=2, ylim = c(0, 40000))
boxplot(all_var[1:22,29:33], las=2, ylim = c(0, 40000))
boxplot(all_var[22:59,29:33], las=2, ylim = c(0, 40000))

par(mfrow=c(1,3))
boxplot(all_var[,34:36], las=2, ylim = c(-35, 60))
boxplot(all_var[1:22,34:36], las=2, ylim = c(-35, 60))
boxplot(all_var[22:59,34:36], las=2, ylim = c(-35, 60))

par(mfrow=c(1,3))
boxplot(all_var[,37:41], las=2, ylim = c(-35, 30))
boxplot(all_var[1:22,37:41], las=2, ylim = c(-35, 30))
boxplot(all_var[22:59,37:41], las=2, ylim = c(-35, 30))

par(mfrow=c(1,3))
boxplot(all_var[,42:46], las=2, ylim = c(-35, 30))
boxplot(all_var[1:22,42:46], las=2, ylim = c(-35, 30))
boxplot(all_var[22:59,42:46], las=2, ylim = c(-35, 30))

par(mfrow=c(1,3))
boxplot(all_var[,47:51], las=2, ylim = c(0, 13000))
boxplot(all_var[1:22,47:51], las=2, ylim = c(0, 13000))
boxplot(all_var[22:59,47:51], las=2, ylim = c(0, 13000))

par(mfrow=c(1,3))
boxplot(all_var[,52:54], las=2, ylim = c(-35, 60))
boxplot(all_var[1:22,52:54], las=2, ylim = c(-35, 60))
boxplot(all_var[22:59,52:54], las=2, ylim = c(-35, 60))

par(mfrow=c(1,3))
boxplot(all_var[,55:59], las=2, ylim = c(-35, 60))
boxplot(all_var[1:22,55:59], las=2, ylim = c(-35, 60))
boxplot(all_var[22:59,55:59], las=2, ylim = c(-35, 60))

par(mfrow=c(1,3))
boxplot(all_var[,60:64], las=2, ylim = c(-35, 60))
boxplot(all_var[1:22,60:64], las=2, ylim = c(-35, 60))
boxplot(all_var[22:59,60:64], las=2, ylim = c(-35, 60))

par(mfrow=c(1,3))
boxplot(all_var[,65:69], las=2, ylim = c(0, 13000))
boxplot(all_var[1:22,65:69], las=2, ylim = c(0, 13000))
boxplot(all_var[22:59,65:69], las=2, ylim = c(0, 13000))

par(mfrow=c(1,3))
boxplot(all_var[,70:72], las=2, ylim = c(-35, 60))
boxplot(all_var[1:22,70:72], las=2, ylim = c(-35, 60))
boxplot(all_var[22:59,70:72], las=2, ylim = c(-35, 60))

par(mfrow=c(1,3))
boxplot(all_var[,70:72], las=2, ylim = c(-35, 60))
boxplot(all_var[1:22,70:72], las=2, ylim = c(-35, 60))
boxplot(all_var[22:59,70:72], las=2, ylim = c(-35, 60))

par(mfrow=c(1,3))
boxplot(all_var[,73:77], las=2, ylim = c(-35, 40))
boxplot(all_var[1:22,73:77], las=2, ylim = c(-35, 40))
boxplot(all_var[22:59,73:77], las=2, ylim = c(-35, 40))

par(mfrow=c(1,3))
boxplot(all_var[,78:82], las=2, ylim = c(-35, 40))
boxplot(all_var[1:22,78:82], las=2, ylim = c(-35, 40))
boxplot(all_var[22:59,78:82], las=2, ylim = c(-35, 40))

par(mfrow=c(1,3))
boxplot(all_var[,83:87], las=2, ylim = c(0, 13000))
boxplot(all_var[1:22,83:87], las=2, ylim = c(0, 13000))
boxplot(all_var[22:59,83:87], las=2, ylim = c(0, 13000))

par(mfrow=c(1,3))
boxplot(all_var[,88:90], las=2, ylim = c(-35, 60))
boxplot(all_var[1:22,88:90], las=2, ylim = c(-35, 60))
boxplot(all_var[22:59,88:90], las=2, ylim = c(-35, 60))

par(mfrow=c(1,3))
boxplot(all_var[,91:95], las=2, ylim = c(-35, 40))
boxplot(all_var[1:22,91:95], las=2, ylim = c(-35, 40))
boxplot(all_var[22:59,91:95], las=2, ylim = c(-35, 40))

par(mfrow=c(1,3))
boxplot(all_var[,96:100], las=2, ylim = c(-35, 40))
boxplot(all_var[1:22,96:100], las=2, ylim = c(-35, 40))
boxplot(all_var[22:59,96:100], las=2, ylim = c(-35, 40))

par(mfrow=c(1,3))
boxplot(all_var[,101:105], las=2, ylim = c(0, 10000))
boxplot(all_var[1:22,101:105], las=2, ylim = c(0, 10000))
boxplot(all_var[22:59,101:105], las=2, ylim = c(0, 10000))

par(mfrow=c(1,3))
boxplot(all_var[,106:108], las=2, ylim = c(-35, 65))
boxplot(all_var[1:22,106:108], las=2, ylim = c(-35, 65))
boxplot(all_var[22:59,106:108], las=2, ylim = c(-35, 65))

par(mfrow=c(1,3))
boxplot(all_var[,109:113], las=2, ylim = c(-35, 35))
boxplot(all_var[1:22,109:113], las=2, ylim = c(-35, 35))
boxplot(all_var[22:59,109:113], las=2, ylim = c(-35, 35))

par(mfrow=c(1,3))
boxplot(all_var[,114:118], las=2, ylim = c(-35, 35))
boxplot(all_var[1:22,114:118], las=2, ylim = c(-35, 35))
boxplot(all_var[22:59,114:118], las=2, ylim = c(-35, 35))
