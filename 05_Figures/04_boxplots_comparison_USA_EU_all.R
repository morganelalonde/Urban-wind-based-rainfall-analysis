#######################################################
### ACITIES BOXPLOTS ###
#######################################################

# On charge les librairies
library(readr)
library(viridis)
library(ggpubr)
DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep où il y a le fichier de script
setwd(DIR)


# Data

data <- read_delim("~/PhD/Manips/02_Analysis_urban_precip/02_Analyses_Urban_Precipitation/Results/table_variables_by_city.txt", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

data_all <- data 
data_all$Zone <- 'All'
data_us_eu <- data_all
data_us_eu$Zone[1:22] <- 'EU'
data_us_eu$Zone[23:59] <- 'USA'

differences <-  c('URUP', 'DWUP')
variables <- c('depth','duration', 'intensity', 'max intensity', 'occurence')
data_all <- data_all[,c(127, 47, 45, 48, 49, 46, 52, 50, 53, 54, 51)]
data_us_eu <- data_us_eu[,c(127, 47, 45, 48, 49, 46, 52, 50, 53, 54, 51)]

to_plot <- matrix(ncol=4, nrow=0)
colnames(to_plot) <- c('Zone','Values','Differences','Variables')


  for (i in 1:length(variables)) {
    to_add <- rbind(data_us_eu[,c(1, (1+i))],data_all[,c(1, (1+i))])
    to_add$Difference = differences[1]
    to_add$variable = variables[i]
    colnames(to_add) <-c('Zone','Values','Differences','Variables')
    to_plot <- rbind(to_plot, to_add)
  }

 for (j in 1:length(variables)) {
  to_add <- rbind(data_us_eu[,c(1, (6+j))],data_all[,c(1, (6+j))])
  to_add$Difference = differences[2]
  to_add$variable = variables[j]
  colnames(to_add) <-c('Zone','Values','Differences','Variables')
  to_plot <- rbind(to_plot, to_add)
  }

URUP <- to_plot[to_plot$Differences == 'URUP',]
DWUP <- to_plot[to_plot$Differences == 'DWUP',]

ttests_URUP <- matrix(ncol = 3, nrow = 5)
row.names(ttests_URUP) <- c('depth','duration', 'intensity', 'max intensity', 'occurence')
colnames(ttests_URUP) <- c('All', 'EU', 'USA')
ttests_URUP[1,1] <- t.test(URUP[URUP$Zone == 'All' & URUP$Variables == 'depth',2])$p.value
ttests_URUP[1,2] <- t.test(URUP[URUP$Zone == 'EU' & URUP$Variables == 'depth',2])$p.value
ttests_URUP[1,3] <- t.test(URUP[URUP$Zone == 'USA' & URUP$Variables == 'depth',2])$p.value
ttests_URUP[2,1] <- t.test(URUP[URUP$Zone == 'All' & URUP$Variables == 'duration',2])$p.value
ttests_URUP[2,2] <- t.test(URUP[URUP$Zone == 'EU' & URUP$Variables == 'duration',2])$p.value
ttests_URUP[2,3] <- t.test(URUP[URUP$Zone == 'USA' & URUP$Variables == 'duration',2])$p.value
ttests_URUP[3,1] <- t.test(URUP[URUP$Zone == 'All' & URUP$Variables == 'intensity',2])$p.value
ttests_URUP[3,2] <- t.test(URUP[URUP$Zone == 'EU' & URUP$Variables == 'intensity',2])$p.value
ttests_URUP[3,3] <- t.test(URUP[URUP$Zone == 'USA' & URUP$Variables == 'intensity',2])$p.value
ttests_URUP[4,1] <- t.test(URUP[URUP$Zone == 'All' & URUP$Variables == 'max intensity',2])$p.value
ttests_URUP[4,2] <- t.test(URUP[URUP$Zone == 'EU' & URUP$Variables == 'max intensity',2])$p.value
ttests_URUP[4,3] <- t.test(URUP[URUP$Zone == 'USA' & URUP$Variables == 'max intensity',2])$p.value
ttests_URUP[5,1] <- t.test(URUP[URUP$Zone == 'All' & URUP$Variables == 'occurence',2])$p.value
ttests_URUP[5,2] <- t.test(URUP[URUP$Zone == 'EU' & URUP$Variables == 'occurence',2])$p.value
ttests_URUP[5,3] <- t.test(URUP[URUP$Zone == 'USA' & URUP$Variables == 'occurence',2])$p.value

ttests_DWUP <- matrix(ncol = 3, nrow = 5)
row.names(ttests_DWUP) <- c('depth','duration', 'intensity', 'max intensity', 'occurence')
colnames(ttests_DWUP) <- c('All', 'EU', 'USA')
ttests_DWUP[1,1] <- t.test(DWUP[DWUP$Zone == 'All' & DWUP$Variables == 'depth',2])$p.value
ttests_DWUP[1,2] <- t.test(DWUP[DWUP$Zone == 'EU' & DWUP$Variables == 'depth',2])$p.value
ttests_DWUP[1,3] <- t.test(DWUP[DWUP$Zone == 'USA' & DWUP$Variables == 'depth',2])$p.value
ttests_DWUP[2,1] <- t.test(DWUP[DWUP$Zone == 'All' & DWUP$Variables == 'duration',2])$p.value
ttests_DWUP[2,2] <- t.test(DWUP[DWUP$Zone == 'EU' & DWUP$Variables == 'duration',2])$p.value
ttests_DWUP[2,3] <- t.test(DWUP[DWUP$Zone == 'USA' & DWUP$Variables == 'duration',2])$p.value
ttests_DWUP[3,1] <- t.test(DWUP[DWUP$Zone == 'All' & DWUP$Variables == 'intensity',2])$p.value
ttests_DWUP[3,2] <- t.test(DWUP[DWUP$Zone == 'EU' & DWUP$Variables == 'intensity',2])$p.value
ttests_DWUP[3,3] <- t.test(DWUP[DWUP$Zone == 'USA' & DWUP$Variables == 'intensity',2])$p.value
ttests_DWUP[4,1] <- t.test(DWUP[DWUP$Zone == 'All' & DWUP$Variables == 'max intensity',2])$p.value
ttests_DWUP[4,2] <- t.test(DWUP[DWUP$Zone == 'EU' & DWUP$Variables == 'max intensity',2])$p.value
ttests_DWUP[4,3] <- t.test(DWUP[DWUP$Zone == 'USA' & DWUP$Variables == 'max intensity',2])$p.value
ttests_DWUP[5,1] <- t.test(DWUP[DWUP$Zone == 'All' & DWUP$Variables == 'occurence',2])$p.value
ttests_DWUP[5,2] <- t.test(DWUP[DWUP$Zone == 'EU' & DWUP$Variables == 'occurence',2])$p.value
ttests_DWUP[5,3] <- t.test(DWUP[DWUP$Zone == 'USA' & DWUP$Variables == 'occurence',2])$p.value

starts_DWUP <- ttests_DWUP
starts_URUP <- ttests_URUP

for (k in 1:3) {
  for (l in 1:5) {
  if(ttests_DWUP[l,k] <= 0.05) {starts_DWUP[l,k] = '*'}  
  if(ttests_DWUP[l,k] <= 0.01) {starts_DWUP[l,k] = '**'}  
  if(ttests_DWUP[l,k] <= 0.001) {starts_DWUP[l,k] = '***'}  
  if(ttests_DWUP[l,k] > 0.05) {starts_DWUP[l,k] = ''} 

  if(ttests_URUP[l,k] <= 0.05) {starts_URUP[l,k] = '*'}  
  if(ttests_URUP[l,k] <= 0.01) {starts_URUP[l,k] = '**'}   
  if(ttests_URUP[l,k] <= 0.001) {starts_URUP[l,k] = '***'}  
  if(ttests_URUP[l,k] > 0.05) {starts_URUP[l,k] = ''} 
   }
}

dist_DWUP <- matrix(ncol = 3, nrow = 5)
dist_URUP <- matrix(ncol = 3, nrow = 5)
row.names(dist_DWUP) <- c('depth','duration', 'intensity', 'max intensity', 'occurence')
row.names(dist_URUP) <- c('depth','duration', 'intensity', 'max intensity', 'occurence')
colnames(dist_DWUP) <- c('All', 'EU', 'USA')
colnames(dist_URUP) <- c('All', 'EU', 'USA')

for (m in 1:length(row.names(ttests_DWUP))) {
  for (n in 1:length(colnames(dist_DWUP))) {
    distance_dwup <- DWUP[DWUP$Zone == colnames(dist_DWUP)[n] & DWUP$Variables == row.names(dist_DWUP)[m],2]
    distance_dwup <- distance_dwup[distance_dwup$Values < (quantile(distance_dwup$Values, probs = 0.75)+(IQR(x = distance_dwup$Values,  type = 7)*1.5)),]
    distance_dwup <- distance_dwup$Values - 1
    dist_DWUP[m,n] <- max(distance_dwup)
    
    distance_urup <- URUP[URUP$Zone == colnames(dist_URUP)[n] & URUP$Variables == row.names(ttests_URUP)[m],2]
    distance_urup <- distance_urup[distance_urup$Values < (quantile(distance_urup$Values, probs = 0.75)+(IQR(x = distance_urup$Values,  type = 7)*1.5)),]
    distance_urup <- distance_urup$Values - 1
    dist_URUP[m,n] <- max(distance_urup)
  }
}

a <- ggplot(URUP,aes(x = Variables, y = Values))+
  geom_boxplot(aes(fill=Zone), outlier.shape = NA)+
  scale_fill_viridis(discrete=TRUE)+
  ylim(-20, 30)+ylab('Difference (in %) between the urban and the upwind zone')+
  theme_bw()+xlab('')+
  coord_flip()
b <- ggplot(DWUP,aes(x = Variables, y = Values))+
  geom_boxplot(aes(fill=Zone), outlier.shape = NA)+
  scale_fill_viridis(discrete=TRUE)+
  ylim(-20, 30)+ylab('Difference (in %) between the downwind and the upwind zone')+
  theme_bw()+xlab('')+
  coord_flip()  

png(file=paste0('Results/boxlots_alt.png'), width = 20, height = 11, units = "cm", res = 600)


a <- ggplot(URUP,aes(x = Variables, y = Values))+
  geom_boxplot(aes(fill=Zone), outlier.shape = NA)+
  scale_fill_viridis(discrete=TRUE)+
  ylim(-20, 30)+xlab('A. Difference (in %) between the urban and the upwind zone')+
  theme_bw(base_size=8)+ylab('')+
  geom_signif(stat = "identity",
              data = data.frame(x = c(0.75), xend = c(0.75), y = dist_URUP[1,1], annotation = c(starts_URUP[1,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1), xend = c(1), y = dist_URUP[1,2], annotation = c(starts_URUP[1,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.25), xend = c(1.25), y = dist_URUP[1,3], annotation = c(starts_URUP[1,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  
  
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.75), xend = c(1.75), y = dist_URUP[2,1], annotation = c(starts_URUP[2,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(2), xend = c(2), y = dist_URUP[2,2], annotation = c(starts_URUP[2,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(2.25), xend = c(2.25), y = dist_URUP[2,3], annotation = c(starts_URUP[2,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  
  
  geom_signif(stat = "identity",
              data = data.frame(x = c(2.75), xend = c(2.75), y = dist_URUP[3,1], annotation = c(starts_URUP[3,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(3), xend = c(3), y = dist_URUP[3,2], annotation = c(starts_URUP[3,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(3.25), xend = c(3.25), y = dist_URUP[3,3], annotation = c(starts_URUP[3,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  
  
  geom_signif(stat = "identity",
              data = data.frame(x = c(3.75), xend = c(3.75), y = dist_URUP[4,1], annotation = c(starts_URUP[4,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(4), xend = c(4), y = dist_URUP[4,2], annotation = c(starts_URUP[4,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(4.25), xend = c(4.25), y = dist_URUP[4,3], annotation = c(starts_URUP[4,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  
  
  geom_signif(stat = "identity",
              data = data.frame(x = c(4.75), xend = c(4.75), y = dist_URUP[5,1], annotation = c(starts_URUP[5,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(5), xend = c(5), y = dist_URUP[5,2], annotation = c(starts_URUP[5,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(5.25), xend = c(5.25), y = dist_URUP[5,3], annotation = c(starts_URUP[5,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))

b <- ggplot(DWUP,aes(x = Variables, y = Values))+
  geom_boxplot(aes(fill=Zone), outlier.shape = NA)+
  scale_fill_viridis(discrete=TRUE)+
  ylim(-20, 30)+xlab('B. Difference (in %) between the downwind and the upwind zone')+
  theme_bw(base_size=8)+ylab('')+
  geom_signif(stat = "identity",
              data = data.frame(x = c(0.75), xend = c(0.75), y = dist_DWUP[1,1], annotation = c(starts_DWUP[1,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1), xend = c(1), y = dist_DWUP[1,2], annotation = c(starts_DWUP[1,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.25), xend = c(1.25), y = dist_DWUP[1,3], annotation = c(starts_DWUP[1,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  
  
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.75), xend = c(1.75), y = dist_DWUP[2,1], annotation = c(starts_DWUP[2,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(2), xend = c(2), y = dist_DWUP[2,2], annotation = c(starts_DWUP[2,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(2.25), xend = c(2.25), y = dist_DWUP[2,3], annotation = c(starts_DWUP[2,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  
  
  geom_signif(stat = "identity",
              data = data.frame(x = c(2.75), xend = c(2.75), y = dist_DWUP[3,1], annotation = c(starts_DWUP[3,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(3), xend = c(3), y = dist_DWUP[3,2], annotation = c(starts_DWUP[3,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(3.25), xend = c(3.25), y = dist_DWUP[3,3], annotation = c(starts_DWUP[3,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  
  
  geom_signif(stat = "identity",
              data = data.frame(x = c(3.75), xend = c(3.75), y = dist_DWUP[4,1], annotation = c(starts_DWUP[4,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(4), xend = c(4), y = dist_DWUP[4,2], annotation = c(starts_DWUP[4,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(4.25), xend = c(4.25), y = dist_DWUP[4,3], annotation = c(starts_DWUP[4,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  
  
  geom_signif(stat = "identity",
              data = data.frame(x = c(4.75), xend = c(4.75), y = dist_DWUP[5,1], annotation = c(starts_DWUP[5,1])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(5), xend = c(5), y = 7, # don´t work on this point I don´t know why dist_DWUP[5,2], 
                                annotation = c(starts_DWUP[5,2])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))+
  geom_signif(stat = "identity",
              data = data.frame(x = c(5.25), xend = c(5.25), y = dist_DWUP[5,3], annotation = c(starts_DWUP[5,3])),
              aes(x = x, xend = xend, y = y, yend = y,  annotation = annotation))

ggarrange(a, b,  ncol = 2, nrow = 1, common.legend = TRUE)

dev.off()
