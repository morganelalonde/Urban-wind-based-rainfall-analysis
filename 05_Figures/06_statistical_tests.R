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
mcorto_plot_spearman_all <- mcorto_plot_spearman[c(41,42,43),c(11,12,13,16,17,26,27)]
mcorto_plot_spearman_all <- round(mcorto_plot_spearman_all, digits = 2)
mcorto_plot_spearman_winter <- mcorto_plot_spearman[c(59,60,61),c(11,12,13,18,19,28,29)]
mcorto_plot_spearman_winter <- round(mcorto_plot_spearman_winter, digits = 2)
mcorto_plot_spearman_spring <- mcorto_plot_spearman[c(77,78,79),c(11,12,13,20,21,30,31)]
mcorto_plot_spearman_spring <- round(mcorto_plot_spearman_spring, digits = 2)
mcorto_plot_spearman_summer <- mcorto_plot_spearman[c(95,96,97),c(11,12,13,14,15,22,23,32,33)]
mcorto_plot_spearman_summer <- round(mcorto_plot_spearman_summer, digits = 2)
mcorto_plot_spearman_fall <- mcorto_plot_spearman[c(113,114,115),c(11,12,13,24,25,34,35)]
mcorto_plot_spearman_fall <- round(mcorto_plot_spearman_fall, digits = 2)

# P VALUE
mcorto_sign_spearman_all <- mcorto_plot_spearman_all

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$Ur_Up, method="spearman")
mcorto_sign_spearman_all[1,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$Ur_Up, method="spearman")
mcorto_sign_spearman_all[1,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$Ur_Up, method="spearman")
mcorto_sign_spearman_all[1,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$Ur_Up, method="spearman")
mcorto_sign_spearman_all[1,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$Ur_Up, method="spearman")
mcorto_sign_spearman_all[1,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$Ur_Up, method="spearman")
mcorto_sign_spearman_all[1,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$Ur_Up, method="spearman")
mcorto_sign_spearman_all[1,7] <- res_urup$p.value


res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$Dw_Up, method="spearman")
mcorto_sign_spearman_all[2,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$Dw_Up, method="spearman")
mcorto_sign_spearman_all[2,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$Dw_Up, method="spearman")
mcorto_sign_spearman_all[2,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$Dw_Up, method="spearman")
mcorto_sign_spearman_all[2,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$Dw_Up, method="spearman")
mcorto_sign_spearman_all[2,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$Dw_Up, method="spearman")
mcorto_sign_spearman_all[2,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$Dw_Up, method="spearman")
mcorto_sign_spearman_all[2,7] <- res_urup$p.value

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$Lt_Rt, method="spearman")
mcorto_sign_spearman_all[3,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$Lt_Rt, method="spearman")
mcorto_sign_spearman_all[3,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$Lt_Rt, method="spearman")
mcorto_sign_spearman_all[3,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$Lt_Rt, method="spearman")
mcorto_sign_spearman_all[3,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$Lt_Rt, method="spearman")
mcorto_sign_spearman_all[3,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$Lt_Rt, method="spearman")
mcorto_sign_spearman_all[3,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$Lt_Rt, method="spearman")
mcorto_sign_spearman_all[3,7] <- res_urup$p.value


mcorto_sign_spearman_winter <- mcorto_plot_spearman_winter

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$winter_Ur_Up, method="spearman")
mcorto_sign_spearman_winter[1,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$winter_Ur_Up, method="spearman")
mcorto_sign_spearman_winter[1,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$winter_Ur_Up, method="spearman")
mcorto_sign_spearman_winter[1,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day_winter, y = tableau_resultats$winter_Ur_Up, method="spearman")
mcorto_sign_spearman_winter[1,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night_winter, y = tableau_resultats$winter_Ur_Up, method="spearman")
mcorto_sign_spearman_winter[1,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10_winter, y = tableau_resultats$winter_Ur_Up, method="spearman")
mcorto_sign_spearman_winter[1,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25_winter, y = tableau_resultats$winter_Ur_Up, method="spearman")
mcorto_sign_spearman_winter[1,7] <- res_urup$p.value


res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$winter_Dw_Up, method="spearman")
mcorto_sign_spearman_winter[2,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$winter_Dw_Up, method="spearman")
mcorto_sign_spearman_winter[2,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$winter_Dw_Up, method="spearman")
mcorto_sign_spearman_winter[2,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day_winter, y = tableau_resultats$winter_Dw_Up, method="spearman")
mcorto_sign_spearman_winter[2,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night_winter, y = tableau_resultats$winter_Dw_Up, method="spearman")
mcorto_sign_spearman_winter[2,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10_winter, y = tableau_resultats$winter_Dw_Up, method="spearman")
mcorto_sign_spearman_winter[2,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25_winter, y = tableau_resultats$winter_Dw_Up, method="spearman")
mcorto_sign_spearman_winter[2,7] <- res_urup$p.value

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$winter_Lt_Rt, method="spearman")
mcorto_sign_spearman_winter[3,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$winter_Lt_Rt, method="spearman")
mcorto_sign_spearman_winter[3,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$winter_Lt_Rt, method="spearman")
mcorto_sign_spearman_winter[3,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$winter_Lt_Rt, method="spearman")
mcorto_sign_spearman_winter[3,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$winter_Lt_Rt, method="spearman")
mcorto_sign_spearman_winter[3,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$winter_Lt_Rt, method="spearman")
mcorto_sign_spearman_winter[3,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$winter_Lt_Rt, method="spearman")
mcorto_sign_spearman_winter[3,7] <- res_urup$p.value


mcorto_sign_spearman_spring <- mcorto_plot_spearman_spring

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$spring_Ur_Up, method="spearman")
mcorto_sign_spearman_spring[1,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$spring_Ur_Up, method="spearman")
mcorto_sign_spearman_spring[1,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$spring_Ur_Up, method="spearman")
mcorto_sign_spearman_spring[1,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day_spring, y = tableau_resultats$spring_Ur_Up, method="spearman")
mcorto_sign_spearman_spring[1,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night_spring, y = tableau_resultats$spring_Ur_Up, method="spearman")
mcorto_sign_spearman_spring[1,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10_spring, y = tableau_resultats$spring_Ur_Up, method="spearman")
mcorto_sign_spearman_spring[1,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25_spring, y = tableau_resultats$spring_Ur_Up, method="spearman")
mcorto_sign_spearman_spring[1,7] <- res_urup$p.value


res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$spring_Dw_Up, method="spearman")
mcorto_sign_spearman_spring[2,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$spring_Dw_Up, method="spearman")
mcorto_sign_spearman_spring[2,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$spring_Dw_Up, method="spearman")
mcorto_sign_spearman_spring[2,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day_spring, y = tableau_resultats$spring_Dw_Up, method="spearman")
mcorto_sign_spearman_spring[2,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night_spring, y = tableau_resultats$spring_Dw_Up, method="spearman")
mcorto_sign_spearman_spring[2,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10_spring, y = tableau_resultats$spring_Dw_Up, method="spearman")
mcorto_sign_spearman_spring[2,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25_spring, y = tableau_resultats$spring_Dw_Up, method="spearman")
mcorto_sign_spearman_spring[2,7] <- res_urup$p.value

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$spring_Lt_Rt, method="spearman")
mcorto_sign_spearman_spring[3,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$spring_Lt_Rt, method="spearman")
mcorto_sign_spearman_spring[3,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$spring_Lt_Rt, method="spearman")
mcorto_sign_spearman_spring[3,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$spring_Lt_Rt, method="spearman")
mcorto_sign_spearman_spring[3,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$spring_Lt_Rt, method="spearman")
mcorto_sign_spearman_spring[3,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$spring_Lt_Rt, method="spearman")
mcorto_sign_spearman_spring[3,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$spring_Lt_Rt, method="spearman")
mcorto_sign_spearman_spring[3,7] <- res_urup$p.value

mcorto_sign_spearman_summer <- mcorto_plot_spearman_summer

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$summer_Ur_Up, method="spearman")
mcorto_sign_spearman_summer[1,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$summer_Ur_Up, method="spearman")
mcorto_sign_spearman_summer[1,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$summer_Ur_Up, method="spearman")
mcorto_sign_spearman_summer[1,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$sat_UHI_day, y = tableau_resultats$summer_Ur_Up, method="spearman")
mcorto_sign_spearman_summer[2,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$sat_UHI_night, y = tableau_resultats$summer_Ur_Up, method="spearman")
mcorto_sign_spearman_summer[2,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day_summer, y = tableau_resultats$summer_Ur_Up, method="spearman")
mcorto_sign_spearman_summer[1,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night_summer, y = tableau_resultats$summer_Ur_Up, method="spearman")
mcorto_sign_spearman_summer[1,7] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10_summer, y = tableau_resultats$summer_Ur_Up, method="spearman")
mcorto_sign_spearman_summer[1,8] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25_summer, y = tableau_resultats$summer_Ur_Up, method="spearman")
mcorto_sign_spearman_summer[1,9] <- res_urup$p.value


res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$summer_Dw_Up, method="spearman")
mcorto_sign_spearman_summer[2,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$summer_Dw_Up, method="spearman")
mcorto_sign_spearman_summer[2,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$summer_Dw_Up, method="spearman")
mcorto_sign_spearman_summer[2,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$sat_UHI_day, y = tableau_resultats$summer_Dw_Up, method="spearman")
mcorto_sign_spearman_summer[2,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$sat_UHI_night, y = tableau_resultats$summer_Dw_Up, method="spearman")
mcorto_sign_spearman_summer[2,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day_summer, y = tableau_resultats$summer_Dw_Up, method="spearman")
mcorto_sign_spearman_summer[2,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night_summer, y = tableau_resultats$summer_Dw_Up, method="spearman")
mcorto_sign_spearman_summer[2,7] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10_summer, y = tableau_resultats$summer_Dw_Up, method="spearman")
mcorto_sign_spearman_summer[2,8] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25_summer, y = tableau_resultats$summer_Dw_Up, method="spearman")
mcorto_sign_spearman_summer[2,9] <- res_urup$p.value

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$summer_Lt_Rt, method="spearman")
mcorto_sign_spearman_summer[3,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$summer_Lt_Rt, method="spearman")
mcorto_sign_spearman_summer[3,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$summer_Lt_Rt, method="spearman")
mcorto_sign_spearman_summer[3,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$summer_Lt_Rt, method="spearman")
mcorto_sign_spearman_summer[3,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$summer_Lt_Rt, method="spearman")
mcorto_sign_spearman_summer[3,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$summer_Lt_Rt, method="spearman")
mcorto_sign_spearman_summer[3,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$summer_Lt_Rt, method="spearman")
mcorto_sign_spearman_summer[3,7] <- res_urup$p.value

mcorto_sign_spearman_fall <- mcorto_plot_spearman_fall

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$fall_Ur_Up, method="spearman")
mcorto_sign_spearman_fall[1,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$fall_Ur_Up, method="spearman")
mcorto_sign_spearman_fall[1,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$fall_Ur_Up, method="spearman")
mcorto_sign_spearman_fall[1,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day_fall, y = tableau_resultats$fall_Ur_Up, method="spearman")
mcorto_sign_spearman_fall[1,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$`UHI_WC_night fall`, y = tableau_resultats$fall_Ur_Up, method="spearman")
mcorto_sign_spearman_fall[1,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10_fall, y = tableau_resultats$fall_Ur_Up, method="spearman")
mcorto_sign_spearman_fall[1,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25_fall, y = tableau_resultats$fall_Ur_Up, method="spearman")
mcorto_sign_spearman_fall[1,7] <- res_urup$p.value


res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$fall_Dw_Up, method="spearman")
mcorto_sign_spearman_fall[2,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$fall_Dw_Up, method="spearman")
mcorto_sign_spearman_fall[2,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$fall_Dw_Up, method="spearman")
mcorto_sign_spearman_fall[2,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day_fall, y = tableau_resultats$fall_Dw_Up, method="spearman")
mcorto_sign_spearman_fall[2,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$`UHI_WC_night fall`, y = tableau_resultats$fall_Dw_Up, method="spearman")
mcorto_sign_spearman_fall[2,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10_fall, y = tableau_resultats$fall_Dw_Up, method="spearman")
mcorto_sign_spearman_fall[2,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25_fall, y = tableau_resultats$fall_Dw_Up, method="spearman")
mcorto_sign_spearman_fall[2,7] <- res_urup$p.value

res_urup <- cor.test(x = tableau_resultats$extent, y = tableau_resultats$fall_Lt_Rt, method="spearman")
mcorto_sign_spearman_fall[3,1] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$imperviousness, y = tableau_resultats$fall_Lt_Rt, method="spearman")
mcorto_sign_spearman_fall[3,2] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$roughness, y = tableau_resultats$fall_Lt_Rt, method="spearman")
mcorto_sign_spearman_fall[3,3] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_day, y = tableau_resultats$fall_Lt_Rt, method="spearman")
mcorto_sign_spearman_fall[3,4] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$UHI_WC_night, y = tableau_resultats$fall_Lt_Rt, method="spearman")
mcorto_sign_spearman_fall[3,5] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM10, y = tableau_resultats$fall_Lt_Rt, method="spearman")
mcorto_sign_spearman_fall[3,6] <- res_urup$p.value
res_urup <- cor.test(x = tableau_resultats$Aerosol_PM25, y = tableau_resultats$fall_Lt_Rt, method="spearman")
mcorto_sign_spearman_fall[3,7] <- res_urup$p.value

# STARS
mcorto_star_spearman_all <- mcorto_sign_spearman_all
mcorto_star_spearman_winter <- mcorto_sign_spearman_winter
mcorto_star_spearman_spring <- mcorto_sign_spearman_spring
mcorto_star_spearman_summer <- mcorto_sign_spearman_summer
mcorto_star_spearman_fall <- mcorto_sign_spearman_fall

for (k in 1:3) {
  for (i in 1:7) {
    if(mcorto_sign_spearman_all[k,i] <= 0.05) {mcorto_star_spearman_all[k,i] = '*'}  
    if(mcorto_sign_spearman_all[k,i] <= 0.01) {mcorto_star_spearman_all[k,i] = '**'}  
    if(mcorto_sign_spearman_all[k,i] <= 0.001) {mcorto_star_spearman_all[k,i] = '***'}  
    if(mcorto_sign_spearman_all[k,i] > 0.05) {mcorto_star_spearman_all[k,i] = ''} 
    
    if(mcorto_sign_spearman_winter[k,i] <= 0.05) {mcorto_star_spearman_winter[k,i] = '*'}  
    if(mcorto_sign_spearman_winter[k,i] <= 0.01) {mcorto_star_spearman_winter[k,i] = '**'}  
    if(mcorto_sign_spearman_winter[k,i] <= 0.001) {mcorto_star_spearman_winter[k,i] = '***'}  
    if(mcorto_sign_spearman_winter[k,i] > 0.05) {mcorto_star_spearman_winter[k,i] = ''} 
    
    if(mcorto_sign_spearman_spring[k,i] <= 0.05) {mcorto_star_spearman_spring[k,i] = '*'}  
    if(mcorto_sign_spearman_spring[k,i] <= 0.01) {mcorto_star_spearman_spring[k,i] = '**'}  
    if(mcorto_sign_spearman_spring[k,i] <= 0.001) {mcorto_star_spearman_spring[k,i] = '***'}  
    if(mcorto_sign_spearman_spring[k,i] > 0.05) {mcorto_star_spearman_spring[k,i] = ''} 
    
    if(mcorto_sign_spearman_summer[k,i] <= 0.05) {mcorto_star_spearman_summer[k,i] = '*'}  
    if(mcorto_sign_spearman_summer[k,i] <= 0.01) {mcorto_star_spearman_summer[k,i] = '**'}  
    if(mcorto_sign_spearman_summer[k,i] <= 0.001) {mcorto_star_spearman_summer[k,i] = '***'}  
    if(mcorto_sign_spearman_summer[k,i] > 0.05) {mcorto_star_spearman_summer[k,i] = ''} 
    
    if(mcorto_sign_spearman_fall[k,i] <= 0.05) {mcorto_star_spearman_fall[k,i] = '*'}  
    if(mcorto_sign_spearman_fall[k,i] <= 0.01) {mcorto_star_spearman_fall[k,i] = '**'}  
    if(mcorto_sign_spearman_fall[k,i] <= 0.001) {mcorto_star_spearman_fall[k,i] = '***'}  
    if(mcorto_sign_spearman_fall[k,i] > 0.05) {mcorto_star_spearman_fall[k,i] = ''} 
    
  }
}



# FINAL TABLE
mcorto_final_spearman_all <- mcorto_plot_spearman_all
mcorto_final_spearman_winter <- mcorto_plot_spearman_winter
mcorto_final_spearman_spring <- mcorto_plot_spearman_spring
mcorto_final_spearman_summer <- mcorto_plot_spearman_summer
mcorto_final_spearman_fall <- mcorto_plot_spearman_fall

for (k in 1:3) {
  for (i in 1:7) {
    mcorto_final_spearman_all[k,i] <- paste0(mcorto_plot_spearman_all[k,i], mcorto_star_spearman_all[k,i])
    mcorto_final_spearman_winter[k,i] <- paste0(mcorto_plot_spearman_winter[k,i], mcorto_star_spearman_winter[k,i])
    mcorto_final_spearman_spring[k,i] <- paste0(mcorto_plot_spearman_spring[k,i], mcorto_star_spearman_spring[k,i])
    mcorto_final_spearman_summer[k,i] <- paste0(mcorto_plot_spearman_summer[k,i], mcorto_star_spearman_summer[k,i])
    mcorto_final_spearman_fall[k,i] <- paste0(mcorto_plot_spearman_fall[k,i], mcorto_star_spearman_fall[k,i])
  }  
}


write.csv(mcorto_final_spearman_all, 'Results/spearman_all.csv', sep = ';')
write.csv(mcorto_final_spearman_winter, 'Results/spearman_winter.csv', sep = ';')
write.csv(mcorto_final_spearman_spring, 'Results/spearman_spring.csv', sep = ';')
write.csv(mcorto_final_spearman_summer, 'Results/spearman_summer.csv', sep = ';')
write.csv(mcorto_final_spearman_fall, 'Results/spearman_fall.csv', sep = ';')

