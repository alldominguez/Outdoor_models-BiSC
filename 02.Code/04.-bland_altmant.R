## 01.- bland_altmant

library(quantr)


#####################
### --- Data --- ###
####################
estimates_correlation_data <- read.csv("01.Data/estimates_correlation_data.csv")
dplyr::glimpse(estimates_correlation_data)

########################################
### --- NO2 Bland-Altmant plots --- ###
######################################
BA_no2_hm_lur <- quantr::plot_bland_altman(estimates_correlation_data, 
                                   lcs_column= "no2_hm", 
                                   reference_column = "no2_lur") +
                 scale_x_continuous(breaks = c(0, 30, 60, 90, 120),
                                    limits = c(0, 120)) +
                 ylim(c(-60, 60)) + 
                 xlab(expression(NO[2]~mean~HM ~and~ LUR~(mu*g/m^3))) + 
                 ylab(expression(NO[2]~diff.~HM - LUR~(mu*g/m^3)))


BA_no2_dm_lur <- quantr::plot_bland_altman(estimates_correlation_data,
                                   lcs_column= "no2_dm", 
                                   reference_column = "no2_lur") +
                 scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125),
                                    limits = c(0, 125)) +
                 ylim(c(-60, 60)) + 
                 xlab(expression(NO[2]~mean~DM ~and~ LUR~(mu*g/m^3))) + 
                 ylab(expression(NO[2]~diff.~DM - LUR~(mu*g/m^3))) 


BA_no2_hm_dm <- quantr::plot_bland_altman(estimates_correlation_data,
                                           lcs_column= "no2_hm", 
                                           reference_column = "no2_dm") +
                scale_x_continuous(breaks = c(0, 30, 60, 90, 120),
                                   limits = c(0, 120)) +
                ylim(c(-60, 60)) + 
                xlab(expression(NO[2]~mean~HM ~and~ DM~(mu*g/m^3))) + 
                ylab(expression(NO[2]~diff.~HM - DM~(mu*g/m^3)))




# putting the plots together
BA_NO2 <- BA_no2_hm_lur | BA_no2_dm_lur | BA_no2_hm_dm              
BA_NO2

### --- export the plot --- ###
ggsave(plot = BA_NO2, "03.Outputs/figures/BA_NO2.png",
       dpi = 600, width = 10, height = 3, units = "in") 

#########################################
### --- PM25 Bland-Altmant plots --- ###
#######################################

BA_pm25_hm_lur <- quantr::plot_bland_altman(estimates_correlation_data, 
                                           lcs_column= "pm25_hm", 
                                           reference_column = "pm25_lur") +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50),
                     limits = c(0, 50)) +
  scale_y_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30)) + 
  ylim(c(-30, 30)) + 
  xlab(expression(PM[2.5]~mean~HM ~and~ LUR~(mu*g/m^3))) + 
  ylab(expression(PM[2.5]~diff.~HM - LUR~(mu*g/m^3)))


BA_pm25_dm_lur <- quantr::plot_bland_altman(estimates_correlation_data,
                                           lcs_column= "pm25_dm", 
                                           reference_column = "pm25_lur") +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50),
                     limits = c(0, 50)) +
  ylim(c(-30, 30)) + 
  xlab(expression(PM[2.5]~mean~DM ~and~ LUR~(mu*g/m^3))) + 
  ylab(expression(PM2[2.5]~diff.~DM - LUR~(mu*g/m^3))) 

BA_pm25_hm_dm <- quantr::plot_bland_altman(estimates_correlation_data, 
                                            lcs_column= "pm25_hm", 
                                            reference_column = "pm25_dm") +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40),
                     limits = c(0, 40)) +
  scale_y_continuous(breaks = c(-20, -10, 0, 10, 20)) + 
  ylim(c(-30, 30)) + 
  xlab(expression(PM[2.5]~mean~HM ~and~ DM~(mu*g/m^3))) + 
  ylab(expression(PM[2.5]~diff.~HM - DM~(mu*g/m^3)))


# putting the plots together
BA_PM25 <- BA_pm25_hm_lur | BA_pm25_dm_lur | BA_pm25_hm_dm                 
BA_PM25

### --- export the plot --- ###
ggsave(plot = BA_PM25, "03.Outputs/figures/BA_PM25.png",
       dpi = 600, width = 10, height = 3, units = "in") 


BA_NO2 / BA_PM25

#######################################
### --- BC Bland-Altmant plots --- ###
######################################

BA_bc_hm_lur <- quantr::plot_bland_altman(estimates_correlation_data, 
                                            lcs_column= "bc_hm", 
                                            reference_column = "bc_lur") +
                scale_x_continuous(breaks = c(0, 1, 2, 3, 4),
                                   limits = c(0, 4)) +
                scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) + 
                ylim(c(-3, 3)) + 
                xlab(expression(BC~mean~HM ~and~ LUR~(mu*g/m^3))) + 
                ylab(expression(BC~diff.~HM - LUR~(mu*g/m^3)))


BA_bc_dm_lur <- quantr::plot_bland_altman(estimates_correlation_data,
                                            lcs_column= "bc_dm", 
                                            reference_column = "bc_lur") +
                scale_x_continuous(breaks = c(0, 1, 2, 3, 4),
                                   limits = c(0, 4)) +
                scale_y_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) + 
                ylim(c(-5, 5)) + 
                xlab(expression(BC~mean~DM ~and~ LUR~(mu*g/m^3))) + 
                ylab(expression(BC~diff.~DM - LUR~(mu*g/m^3)))


BA_bc_hm_dm <- quantr::plot_bland_altman(estimates_correlation_data,
                                          lcs_column= "bc_hm", 
                                          reference_column = "bc_dm") +
               scale_x_continuous(breaks = c(0, 1, 2, 3),
                                 limits = c(0, 3)) +
               scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) + 
               ylim(c(-3, 3)) + 
               xlab(expression(BC~mean~HM ~and~ DM~(mu*g/m^3))) + 
               ylab(expression(BC~diff.~HM - DM~(mu*g/m^3)))


# putting the plots together
BA_BC <- BA_bc_hm_lur | BA_bc_dm_lur | BA_bc_hm_dm              
BA_BC


### --- export the plot --- ###
ggsave(plot = BA_BC, "03.Outputs/figures/BA_BC.png",
       dpi = 600, width = 10, height = 3, units = "in") 


######################################################
### --- PM25 constituents Bland-Altmant plots --- ###
#####################################################

BA_pm25_fe_hm_lur <- quantr::plot_bland_altman(estimates_correlation_data, 
                                               lcs_column= "fe_hm", 
                                               reference_column = "fe_lur") +
                     scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75),
                                       limits = c(0, 0.75)) +
                     scale_y_continuous(breaks = c(-1, -0.50, 0, 0.50, 1)) + 
                     ylim(c(-1, 1)) + 
                     xlab(expression(PM[2.5 - Fe]~mean~HM ~and~ LUR~(mu*g/m^3))) + 
                     ylab(expression(PM[2.5 - Fe]~diff.~HM - LUR~(mu*g/m^3)))


BA_pm25_cu_hm_lur <- quantr::plot_bland_altman(estimates_correlation_data, 
                                               lcs_column= "cu_hm", 
                                               reference_column = "cu_lur") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20),
                     limits = c(0, 20)) +
  scale_y_continuous(breaks = c(-20, -10, 0, 10, 20)) + 
  ylim(c(-20, 20)) + 
  xlab(expression(PM[2.5 - Cu]~mean~HM ~and~ LUR~(ng/m^3))) + 
  ylab(expression(PM[2.5 - Cu]~diff.~HM - LUR~(ng/m^3)))


BA_pm25_zn_hm_lur <- quantr::plot_bland_altman(estimates_correlation_data, 
                                               lcs_column= "zn_hm", 
                                               reference_column = "zn_lur") +
  scale_x_continuous(breaks = c(0, 40, 80, 120, 160),
                     limits = c(0, 160)) +
  scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100)) + 
  ylim(c(-100, 100)) + 
  xlab(expression(PM[2.5 - Zn]~mean~HM ~and~ LUR~(ng/m^3))) + 
  ylab(expression(PM[2.5 - Zn]~diff.~HM - LUR~(ng/m^3)))


###############################################
### --- Bland - Altmant plots together --- ###
#############################################

### --- NO2 BA --- ###
BA_no2_hm_lur 
BA_no2_dm_lur
BA_no2_hm_dm

### --- PM25 BA --- ###
BA_pm25_hm_lur
BA_pm25_dm_lur
BA_pm25_hm_dm

### --- BC BA --- ###
BA_bc_hm_lur
BA_bc_dm_lur
BA_bc_hm_dm

### --- PM25 constituents --- ###
BA_pm25_fe_hm_lur
BA_pm25_cu_hm_lur
BA_pm25_zn_hm_lur

#####################################
### --- Putting all together --- ###
###################################


# Combining NO2 BA plots
no2_ba_combined <-  BA_no2_hm_lur + BA_no2_dm_lur + BA_no2_hm_dm

# Combining PM25 BA plots
pm25_ba_combined <-  BA_pm25_hm_lur + BA_pm25_dm_lur + BA_pm25_hm_dm

# Combining BC BA plots
bc_ba_combined <-  BA_bc_hm_lur + BA_bc_dm_lur + BA_bc_hm_dm

# Combining PM25 Constituents plots
pm25_constituents_combined <-  BA_pm25_fe_hm_lur + BA_pm25_cu_hm_lur + BA_pm25_zn_hm_lur

# Final combined plot
final_combined_plot <-  (no2_ba_combined / pm25_ba_combined / bc_ba_combined / pm25_constituents_combined) + 
                         plot_layout(ncol = 1)


final_combined_plot 

# Exporting figure 
ggsave(plot = final_combined_plot, "03.Outputs/figures/final_bland_altmant.png",
       dpi = 600, width = 9, height = 9, units = "in")























