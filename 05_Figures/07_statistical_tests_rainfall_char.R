#######################################################

#######################################################

# On charge les librairies
library(ggplot2)
library(MASS)
library(FactoMineR)
library(randomForest)
library(party)
library(readr)

DIR = dirname(rstudioapi::getSourceEditorContext()$path) # C'est le rep où il y a le fichier de script
setwd(DIR)

tableau_resultats <- read_delim("Results/table_variables_by_city.txt", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
to_plot <- tableau_resultats[,-1]

# SPEARMAN ---------------------------------------------------------------------

# CORRELATION
mcorto_plot_spearman <- cor(to_plot, method = "spearman")

mcorto_plot_spearman_events <- mcorto_plot_spearman[c(46, 47, 48, 51, 49),c(11,12,13,16,17,26,27)]
mcorto_plot_spearman_events <- round(mcorto_plot_spearman_events, digits = 2)


test <- mcorto_plot_spearman[c(41,42,43),c(11,12,13,14,15,16,17,26,27)]
test <- round(test, digits = 2)
cor.test(x = tableau_resultats$sat_UHI_day, y = tableau_resultats$summer_Ur_Up, method="spearman")
cor.test(x = tableau_resultats$sat_UHI_day, y = tableau_resultats$summer_Dw_Up, method="spearman")
cor.test(x = tableau_resultats$sat_UHI_night, y = tableau_resultats$summer_Ur_Up, method="spearman")
cor.test(x = tableau_resultats$sat_UHI_night, y = tableau_resultats$summer_Dw_Up, method="spearman")

EU_aerosols_1km <- read_delim("Variables/Aerosols/EU_aerosols_1km.TXT", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

USA_aerosols <- read_delim("Variables/Aerosols/aerosols.TXT", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


EU_aerosols_1km$all <- rowMeans(EU_aerosols_1km)

cor(x = EU_aerosols_1km$all[1:21], y = tableau_resultats$Ur_Up[1:21], method = "spearman")
cor(x = EU_aerosols_1km$all[1:21], y = tableau_resultats$Dw_Up[1:21], method = "spearman")
cor.test(x = EU_aerosols_1km$all[1:21], y = tableau_resultats$Ur_Up[1:21], method="spearman")
cor.test(x = EU_aerosols_1km$all[1:21], y = tableau_resultats$Dw_Up[1:21], method="spearman")

inter <- tableau_resultats[-55,]

cor(x = USA_aerosols$aerosolsPM10[-33], y = inter$Ur_Up[23:58], method = "spearman")
cor.test(x = USA_aerosols$aerosolsPM10[-33], y = tableau_resultats$Ur_Up[c(23:58)], method="spearman")
cor(x = USA_aerosols$aerosolsPM10, y = tableau_resultats$Ur_Up[c(23:59)], method = "spearman")
cor.test(x = USA_aerosols$aerosolsPM10, y = tableau_resultats$Ur_Up[c(23:59)], method="spearman")

cor(x = USA_aerosols$aerosolsPM10[-33], y = inter$Dw_Up[23:58], method = "spearman")
cor.test(x = USA_aerosols$aerosolsPM10[-33], y = tableau_resultats$Dw_Up[c(23:58)], method="spearman")


mcorto_sign_spearman_events <- mcorto_plot_spearman_events

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$Ur_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[1,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$Ur_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[1,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$Ur_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[1,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$Ur_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[1,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$`UHI_WC_night`, y = tableau_resultats$Ur_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[1,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$Ur_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[1,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$Ur_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[1,7] <- res_urup$p.value


res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$Ur_Up_intensity, method="spearman")
mcorto_sign_spearman_events[2,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$Ur_Up_intensity, method="spearman")
mcorto_sign_spearman_events[2,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$Ur_Up_intensity, method="spearman")
mcorto_sign_spearman_events[2,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$Ur_Up_intensity, method="spearman")
mcorto_sign_spearman_events[2,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$`UHI_WC_night`, y = tableau_resultats$Ur_Up_intensity, method="spearman")
mcorto_sign_spearman_events[2,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$Ur_Up_intensity, method="spearman")
mcorto_sign_spearman_events[2,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$Ur_Up_intensity, method="spearman")
mcorto_sign_spearman_events[2,7] <- res_urup$p.value

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$Ur_Up_maxintensity, method="spearman")
mcorto_sign_spearman_events[3,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$Ur_Up_maxintensity, method="spearman")
mcorto_sign_spearman_events[3,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$Ur_Up_maxintensity, method="spearman")
mcorto_sign_spearman_events[3,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$Ur_Up_maxintensity, method="spearman")
mcorto_sign_spearman_events[3,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$Ur_Up_maxintensity, method="spearman")
mcorto_sign_spearman_events[3,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$Ur_Up_maxintensity, method="spearman")
mcorto_sign_spearman_events[3,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$Ur_Up_maxintensity, method="spearman")
mcorto_sign_spearman_events[3,7] <- res_urup$p.value

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$Dw_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[4,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$Dw_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[4,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$Dw_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[4,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$Dw_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[4,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$Dw_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[4,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$Dw_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[4,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$Dw_Up_accumulation, method="spearman")
mcorto_sign_spearman_events[4,7] <- res_urup$p.value

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$Dw_Up_duration, method="spearman")
mcorto_sign_spearman_events[5,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$Dw_Up_duration, method="spearman")
mcorto_sign_spearman_events[5,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$Dw_Up_duration, method="spearman")
mcorto_sign_spearman_events[5,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$Dw_Up_duration, method="spearman")
mcorto_sign_spearman_events[5,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$Dw_Up_duration, method="spearman")
mcorto_sign_spearman_events[5,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$Dw_Up_duration, method="spearman")
mcorto_sign_spearman_events[5,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$Dw_Up_duration, method="spearman")
mcorto_sign_spearman_events[5,7] <- res_urup$p.value

# STARS

mcorto_star_spearman_events <- mcorto_sign_spearman_events

for (k in 1:5) {
  for (i in 1:7) {
    
    if(mcorto_sign_spearman_events[k,i] <= 0.05) {mcorto_star_spearman_events[k,i] = '*'}  
    if(mcorto_sign_spearman_events[k,i] <= 0.01) {mcorto_star_spearman_events[k,i] = '**'}  
    if(mcorto_sign_spearman_events[k,i] <= 0.001) {mcorto_star_spearman_events[k,i] = '***'}  
    if(mcorto_sign_spearman_events[k,i] > 0.05) {mcorto_star_spearman_events[k,i] = ''} 
    
  }
}


mcorto_final_spearman_events <- mcorto_plot_spearman_events

for (k in 1:5) {
  for (i in 1:7) {
mcorto_final_spearman_events[k,i] <- paste0(mcorto_plot_spearman_events[k,i], mcorto_star_spearman_events[k,i])
  }  
}

write.table(mcorto_final_spearman_events, 'Results/spearman_events.csv', sep = ';')
