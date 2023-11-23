# 00.- weekly_average_dispersion
pacman::p_load(tidyverse, skimr, caret, tictoc, lubridate,  mice, ggmice, plyr, data.table)


################################################
### --- Define paths and load some data --- ###
###############################################
load("01.Data/FUR_updated/2023_07_28_finalversion_FUR_delivery.RData") # File FUR updated
path_workplace_dm_reconstructed <- "01.Data/BiSC_dispersion_hourly/workplace_reconstructed_correct/" # this is new route in Toni's folder 
path_weekly_workplace <- "01.Data/BiSC_dispersion_hourly/work_weekly_avg/"
