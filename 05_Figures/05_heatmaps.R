#######################################################
### ALL PLOTS TO COMPARE IMPACT AT DIFFERENT SCALES ###
#######################################################

# On charge les librairies
library(gplots)
library(readr)

DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep où il y a le fichier de script
setwd(DIR)

# Data

data <- read_delim("~/PhD/Manips/02_Analysis_urban_precip/02_Analyses_Urban_Precipitation/Results/table_variables_by_city.txt", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

europe <- data[1:22,c(42,47,48,49,43,52,50)]
usa <- data[23:59,c(42,47,48,49,43,52,50)]

rownames(europe) <- data$City[1:22]
rownames(usa) <- data$City[23:59]
colnames(europe) = c('UPUR - total precipitation',
                       'UPUR - depth', 
                       'UPUR - intensity',
                       'UPUR - max intensity', 
                       'UPDW - total precipitation',
                       'UPDW - depth',
                       'UPDW - duration')
colnames(usa) = c('UPUR - total precipitation',
                     'UPUR - depth', 
                     'UPUR - intensity',
                     'UPUR - max intensity', 
                     'UPDW - total precipitation',
                     'UPDW - depth',
                     'UPDW - duration')

europe <- as.matrix(europe)
usa <- as.matrix(usa)
rownames(europe) <- data$City[1:22]
rownames(usa) <- data$City[23:59]
europe <- europe[order(row.names(europe)),]
usa <- usa[order(row.names(usa)),]




breaks1 <- c(-30, -20, -15, -10, -2, 2, 10, 15, 20, 30)

png(file="Results/heatmap_europe.png", width = 10, height = 22, units = "cm", res = 600)
heatmap.2(europe, breaks = breaks1, Rowv = F, Colv = F,
          col =  colorRampPalette(c("darkblue","white","darkred")), 
          tracecol = NULL,
          cexRow=0.3,
          cexCol=0.3) 
dev.off()



png(file="Results/heatmap_usa.png", width = 10, height = 30, units = "cm", res = 600)
heatmap.2(usa, breaks = breaks1, Rowv = F, Colv = F,
          col =  colorRampPalette(c("darkblue","white","darkred")), 
          tracecol = NULL,
          cexRow=0.3,
          cexCol=0.3) 
dev.off()
