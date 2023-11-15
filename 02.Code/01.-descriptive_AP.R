# Install packages
pacman::p_load(tidyverse, purrr, skimr, caret, tictoc, lubridate,  mice, ggmice, plyr, data.table)


##############################
### --- Load the data --- ###
############################

#############################################
### --- Air pollutants measurements data --- #####################################################################
############################################
no2_measures <- read.csv("01.Data/BiSC_comparison_data/measures/no2_final_model_data.csv")
pm25_measures <- read.csv("01.Data/BiSC_comparison_data/measures/pm25_final_model_data.csv")
bc_measures <- read.csv("01.Data/BiSC_comparison_data/measures/bc_final_model_data.csv")
pm25_constituents_measures <- read.csv("01.Data/BiSC_comparison_data/measures/pm25_constituents_model_data.csv")

# Have a quick look to the data 
dplyr::glimpse(no2_measures)
dplyr::glimpse(pm25_measures)
dplyr::glimpse(bc_measures)
dplyr::glimpse(pm25_constituents_measures)

### --- selection of specific variables --- ###
no2_measures <- no2_measures %>%
                dplyr::select(gid, id, year, week, NO2.DirectMeasurements, lon, lat) 

pm25_measures <- pm25_measures %>% 
                 dplyr::select(gid, site_id, sid, c_biscape, lon, lat)

bc_measures <- bc_measures %>% 
               dplyr::select(gid, site_id, sid, c_biscape, lon, lat)

pm25_constituents_measures <- pm25_constituents_measures %>% 
                              dplyr::select(gid, site_id, sid, 
                                            biscape_Fe2O3,biscape_Cu, biscape_Zn, lon, lat)

### --- rename some variables --- ###
no2_measures <- no2_measures %>% 
  dplyr::rename(no2_measures = NO2.DirectMeasurements)

pm25_measures <- pm25_measures %>% 
  dplyr::rename(pm25_measures = c_biscape) 

bc_measures <- bc_measures %>% 
  dplyr::rename(bc_measures = c_biscape)

pm25_constituents_measures <- pm25_constituents_measures %>% 
  dplyr::rename(fe2o3_measures = biscape_Fe2O3, 
                cu_measures = biscape_Cu, 
                zn_measures = biscape_Zn)

#######################################
### --- Model predictions data --- #################################################################################################
######################################

##############################
### --- Hybrid models --- ###
#############################
no2_hybrid <- read.csv("01.Data/BiSC_comparison_data/predictions/HM/weekly_estimates_no2_hybrid_model.csv")
pm25_hybrid <- read.csv("01.Data/BiSC_comparison_data/predictions/HM/weekly_estimates_pm25_hybrid_model.csv")
bc_hybrid <- read.csv("01.Data/BiSC_comparison_data/predictions/HM/weekly_estimates_bc_hybrid_model.csv")
pm25_constituents_hybrid <- read.csv("01.Data/BiSC_comparison_data/predictions/HM/weekly_estimates_pm25constituents_hybrid_model.csv")

dplyr::glimpse(no2_hybrid)
dplyr::glimpse(pm25_hybrid)
dplyr::glimpse(bc_hybrid)
dplyr::glimpse(pm25_constituents_hybrid)

# --- rename variables --- # 
no2_hybrid <- no2_hybrid %>% 
              dplyr::rename(no2_hm = no2.total_hybridmodel)

pm25_hybrid <- pm25_hybrid %>% 
               dplyr::rename(pm25_hm = pm25.total_hybridmodel)
                
bc_hybrid <- bc_hybrid %>%  
             dplyr::rename(bc_hm = bc.total_hybridmodel)

pm25_constituents_hybrid <- pm25_constituents_hybrid %>% 
                            dplyr::rename(fe_hm = fe.total_hybridmodel,
                                          cu_hm = cu.total_hybridmodel,
                                          zn_hm = zn.total_hybridmodel)


##################################
### --- Dispersion models --- ###
#################################
dm_home_weekly <- read.csv("01.Data/BiSC_dispersion_models/imputed/weekly_estimate_DM_home_imputed.csv")

# quick check of the data
dplyr::glimpse(dm_home_weekly)
skimr::skim(dm_home_weekly)

# dates in proper format
dm_home_weekly$date_start <- as.Date(dm_home_weekly$date_start)
dm_home_weekly$date_end <- as.Date(dm_home_weekly$date_end)

# --- generate separete files --- #
no2_dm <- dm_home_weekly %>% 
          dplyr::select(subject_id, weeks, date_start, date_end, no2.total) %>% 
          dplyr::rename(no2_dm = no2.total)

pm25_dm <- dm_home_weekly %>% 
           dplyr::select(subject_id, weeks, date_start, date_end, pm2.5.total) %>% 
           dplyr::rename(pm25_dm = pm2.5.total)

bc_dm <- dm_home_weekly %>%  
         dplyr::select(subject_id, weeks, date_start, date_end, BC.total) %>% 
         dplyr::rename(bc_dm = BC.total) 

dplyr::glimpse(no2_dm)
dplyr::glimpse(pm25_dm)
dplyr::glimpse(bc_dm)

###########################
### --- LUR models --- ###
#########################

no2_lur <- readRDS("01.Data/BiSC_comparison_data/predictions/LUR/no2_weekly_preg_29092023.rds")
pm25_lur <- readRDS("01.Data/BiSC_comparison_data/predictions/LUR/pm25_weekly_preg_29092023.rds")
bc_lur <- readRDS("01.Data/BiSC_comparison_data/predictions/LUR/bc_weekly_preg_29092023.rds")
fe_lur <- readRDS("01.Data/BiSC_comparison_data/predictions/LUR/fe_weekly_preg_29092023.rds")
cu_lur <- readRDS("01.Data/BiSC_comparison_data/predictions/LUR/cu_weekly_preg_29092023.rds")
zn_lur <- readRDS("01.Data/BiSC_comparison_data/predictions/LUR/zn_weekly_preg_29092023.rds")

# have a quick check to the data 
dplyr::glimpse(no2_lur)
dplyr::glimpse(pm25_lur)
dplyr::glimpse(bc_lur)
dplyr::glimpse(fe_lur)
dplyr::glimpse(cu_lur)
dplyr::glimpse(zn_lur)

# re-arrenge the data 
no2_lur <- no2_lur %>% 
           dplyr::select(subject_id, weeks, date_start, date_end, no2_week_ratio) %>% 
           dplyr::rename(no2_lur = no2_week_ratio)

pm25_lur <- pm25_lur %>% 
            dplyr::select(subject_id, weeks, date_start, date_end, pm25_week_lur_ratio) %>% 
            dplyr::rename(pm25_lur= pm25_week_lur_ratio)

bc_lur <- bc_lur %>% 
          dplyr::select(subject_id, weeks, date_start, date_end, bc_week_ratio) %>% 
          dplyr::rename(bc_lur = bc_week_ratio)

fe_lur <- fe_lur %>% 
          dplyr::select(subject_id, weeks, date_start, date_end)







