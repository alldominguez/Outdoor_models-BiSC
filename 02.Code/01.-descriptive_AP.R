# Install packages
pacman::p_load(tidyverse, purrr, skimr, caret, tictoc, lubridate, 
               mice, ggmice, plyr, data.table, mapview, ggmap, omsdata, sf, sp, patchwork,
               ggpattern, GGally)

##############################
### --- Load the data --- ###
############################

meteo_variables <- readxl::read_xlsx("03.Outputs/predictions/hybrid_data_final_130923.xlsx")
dplyr::glimpse(meteo_variables)

bisc_meteo_weekly <- meteo_variables %>% 
                     dplyr::select(gid, subject_id, weeks, date_start, date_end, 
                                   lon, lat, avg_temperature, avg_relative_humidity, avg_precipitation) 


rio::export(bisc_meteo_weekly, "03.Outputs/predictions/bisc_meteo_weekly.csv")


#######################################################
### --- Load shapefile  Barcelona metropolitan --- ###
#####################################################
bcn_shp <- sf::read_sf("01.Data/BCN_GIS_data/bcn/bcn.shp")
amb_shp <- sf::st_read("01.Data/amb.gpkg")

###############################
### --- Home addresses --- ###
#############################
gid_data <- read.csv("01.Data/hybrid_data_final_040823.csv") # wee need assign to dm and lur data the lon/lat to then plot the predictions (entire pregnancy)
dplyr::glimpse(gid_data)

gid_data <- gid_data %>% dplyr::select(gid, subject_id, weeks, lon, lat)

gid_data$weeks <- dplyr::recode_factor(gid_data$weeks, 
                                       w1 = 'w01', w2 = 'w02', w3 = 'w03', w4 = 'w04', w5 = 'w05',
                                       w6 = 'w06', w7 = 'w07', w8 = 'w08', w9 = 'w09', w10 = 'w10',
                                       w11 = 'w11', w12 = 'w12', w13 = 'w13', w14 = 'w14', w15 = 'w15',
                                       w16 = 'w16', w17 = 'w17', w18 = 'w18', w19 = 'w19', w20 = 'w20',
                                       w21 = 'w21', w22 = 'w22', w23 = 'w23', w24 = 'w24', w25 = 'w25',
                                       w26 = 'w26', w27 = 'w27', w28 = 'w28', w29 = 'w29', w30 = 'w30',
                                       w31 = 'w31', w32 = 'w32', w33 = 'w33', w34 = 'w34', w35 = 'w35',
                                       w36 = 'w36', w37 = 'w37', w38 = 'w38', w39 = 'w39', w40 = 'w40',
                                       w41 = 'w41', w42 = 'w42', w43 = 'w43', w44 = 'w44')

gid_data <- gid_data %>% dplyr::arrange(subject_id, weeks)
dplyr::glimpse(gid_data)

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


# --- select variables --- #
no2_hybrid <- no2_hybrid %>% 
              dplyr::select(gid, subject_id, weeks, date_start, date_end, no2_hm)


pm25_hybrid <- pm25_hybrid %>% 
               dplyr::select(subject_id, weeks, pm25_hm)

bc_hybrid <- bc_hybrid %>% 
             dplyr::select(subject_id, weeks, bc_hm)

pm25_constituents_hybrid <- pm25_constituents_hybrid %>% 
                            dplyr::select(subject_id, weeks, fe_hm, cu_hm, zn_hm)

lon_lat <- no2_dm %>% 
           dplyr::select(subject_id, weeks, lon, lat)

# --- join estimates --- # 
hm_estimates <- no2_hybrid %>%  
                dplyr::inner_join(pm25_hybrid, by = c("subject_id", "weeks")) %>% 
                dplyr::inner_join(bc_hybrid, by = c("subject_id", "weeks")) %>%
                dplyr::inner_join(pm25_constituents_hybrid, by = c("subject_id", "weeks")) %>% 
                dplyr::inner_join(lon_lat, by = c("subject_id", "weeks")) 
                
dplyr::glimpse(hm_estimates)  # 42,894   

### --- Export the data --- ###
rio::export(hm_estimates, "01.Data/hm_estimates.csv")

##################################
### --- Dispersion models --- ###
#################################
dm_home_weekly <- read.csv("01.Data/BiSC_dispersion_models/imputed/weekly_estimate_DM_home_imputed.csv")

no2_dm <- read.csv("01.Data/BiSC_comparison_data/predictions/DM/no2_dm.csv")
pm25_dm <- read.csv("01.Data/BiSC_comparison_data/predictions/DM/pm25_dm.csv") 
bc_dm <- read.csv("01.Data/BiSC_comparison_data/predictions/DM/bc_dm.csv") 

# quick check of the data 
dplyr::glimpse(no2_dm)
dplyr::glimpse(pm25_dm)
dplyr::glimpse(bc_dm)

#######################################
### --- dates in proper format --- ###
#####################################

### --- NO2 --- ###
no2_dm$date_start <- as.Date(no2_dm$date_start)
no2_dm$date_end <- as.Date(no2_dm$date_end)

### --- PM25 --- ###
pm25_dm$date_start <- as.Date(pm25_dm$date_start)
pm25_dm$date_end <- as.Date(pm25_dm$date_end)

### --- BC --- ###
bc_dm$date_start <- as.Date(bc_dm$date_start)
bc_dm$date_end <- as.Date(bc_dm$date_end)

# arrange the no2 data 
no2_dm <- no2_dm %>% dplyr::arrange(subject_id, weeks)

# --- select some variables per each dataset --- # 
no2_dm <- no2_dm %>% 
          dplyr::select(gid, subject_id, weeks, date_start, date_end,
                        NO2.total_DispersionModels, lon, lat) %>% 
          dplyr::rename(no2_dm = NO2.total_DispersionModels)

pm25_dm <- pm25_dm %>%
           dplyr::select(subject_id, weeks, pm2.5.total) %>% 
           dplyr::rename(pm25_dm = pm2.5.total)

bc_dm <- bc_dm %>% 
         dplyr::select(subject_id, weeks, BC.total) %>% 
         dplyr::rename(bc_dm = BC.total)


dplyr::glimpse(no2_dm)
dplyr::glimpse(pm25_dm)
dplyr::glimpse(bc_dm)

### --- Join all DM estimates --- ### 
dm_estimates <- no2_dm %>% 
                dplyr::inner_join(pm25_dm, by = c("subject_id", "weeks")) %>% 
                dplyr::inner_join(bc_dm, by = c("subject_id", "weeks"))

dplyr::glimpse(dm_estimates)

### --- Export data --- ###
rio::export(dm_estimates, "01.Data/dm_estimates.csv")


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
view(no2_lur)
dplyr::glimpse(no2_lur)
dplyr::glimpse(pm25_lur)
dplyr::glimpse(bc_lur)
dplyr::glimpse(fe_lur)
dplyr::glimpse(cu_lur)
dplyr::glimpse(zn_lur)

##################################
### --- Re-ordering weeks --- ###
#################################

# --- NO2 --- #
no2_lur$weeks <-  dplyr::recode_factor(no2_lur$weeks, 
                                       w1 = 'w01', w2 = 'w02', w3 = 'w03', w4 = 'w04', w5 = 'w05',
                                       w6 = 'w06', w7 = 'w07', w8 = 'w08', w9 = 'w09', w10 = 'w10',
                                       w11 = 'w11', w12 = 'w12', w13 = 'w13', w14 = 'w14', w15 = 'w15',
                                       w16 = 'w16', w17 = 'w17', w18 = 'w18', w19 = 'w19', w20 = 'w20',
                                       w21 = 'w21', w22 = 'w22', w23 = 'w23', w24 = 'w24', w25 = 'w25',
                                       w26 = 'w26', w27 = 'w27', w28 = 'w28', w29 = 'w29', w30 = 'w30',
                                       w31 = 'w31', w32 = 'w32', w33 = 'w33', w34 = 'w34', w35 = 'w35',
                                       w36 = 'w36', w37 = 'w37', w38 = 'w38', w39 = 'w39', w40 = 'w40',
                                       w41 = 'w41', w42 = 'w42', w43 = 'w43', w44 = 'w44')

# --- PM25 --- #  
pm25_lur$weeks <- dplyr::recode_factor(pm25_lur$weeks, 
                                       w1 = 'w01', w2 = 'w02', w3 = 'w03', w4 = 'w04', w5 = 'w05',
                                       w6 = 'w06', w7 = 'w07', w8 = 'w08', w9 = 'w09', w10 = 'w10',
                                       w11 = 'w11', w12 = 'w12', w13 = 'w13', w14 = 'w14', w15 = 'w15',
                                       w16 = 'w16', w17 = 'w17', w18 = 'w18', w19 = 'w19', w20 = 'w20',
                                       w21 = 'w21', w22 = 'w22', w23 = 'w23', w24 = 'w24', w25 = 'w25',
                                       w26 = 'w26', w27 = 'w27', w28 = 'w28', w29 = 'w29', w30 = 'w30',
                                       w31 = 'w31', w32 = 'w32', w33 = 'w33', w34 = 'w34', w35 = 'w35',
                                       w36 = 'w36', w37 = 'w37', w38 = 'w38', w39 = 'w39', w40 = 'w40',
                                       w41 = 'w41', w42 = 'w42', w43 = 'w43', w44 = 'w44')


# --- BC --- #
bc_lur$weeks <- dplyr::recode_factor(bc_lur$weeks, 
                                     w1 = 'w01', w2 = 'w02', w3 = 'w03', w4 = 'w04', w5 = 'w05',
                                     w6 = 'w06', w7 = 'w07', w8 = 'w08', w9 = 'w09', w10 = 'w10',
                                     w11 = 'w11', w12 = 'w12', w13 = 'w13', w14 = 'w14', w15 = 'w15',
                                     w16 = 'w16', w17 = 'w17', w18 = 'w18', w19 = 'w19', w20 = 'w20',
                                     w21 = 'w21', w22 = 'w22', w23 = 'w23', w24 = 'w24', w25 = 'w25',
                                     w26 = 'w26', w27 = 'w27', w28 = 'w28', w29 = 'w29', w30 = 'w30',
                                     w31 = 'w31', w32 = 'w32', w33 = 'w33', w34 = 'w34', w35 = 'w35',
                                     w36 = 'w36', w37 = 'w37', w38 = 'w38', w39 = 'w39', w40 = 'w40',
                                     w41 = 'w41', w42 = 'w42', w43 = 'w43', w44 = 'w44')

# --- Fe --- #
fe_lur$weeks <- dplyr::recode_factor(fe_lur$weeks, 
                                     w1 = 'w01', w2 = 'w02', w3 = 'w03', w4 = 'w04', w5 = 'w05',
                                     w6 = 'w06', w7 = 'w07', w8 = 'w08', w9 = 'w09', w10 = 'w10',
                                     w11 = 'w11', w12 = 'w12', w13 = 'w13', w14 = 'w14', w15 = 'w15',
                                     w16 = 'w16', w17 = 'w17', w18 = 'w18', w19 = 'w19', w20 = 'w20',
                                     w21 = 'w21', w22 = 'w22', w23 = 'w23', w24 = 'w24', w25 = 'w25',
                                     w26 = 'w26', w27 = 'w27', w28 = 'w28', w29 = 'w29', w30 = 'w30',
                                     w31 = 'w31', w32 = 'w32', w33 = 'w33', w34 = 'w34', w35 = 'w35',
                                     w36 = 'w36', w37 = 'w37', w38 = 'w38', w39 = 'w39', w40 = 'w40',
                                     w41 = 'w41', w42 = 'w42', w43 = 'w43', w44 = 'w44')



# --- Cu --- #
cu_lur$weeks <- dplyr::recode_factor(cu_lur$weeks, 
                                     w1 = 'w01', w2 = 'w02', w3 = 'w03', w4 = 'w04', w5 = 'w05',
                                     w6 = 'w06', w7 = 'w07', w8 = 'w08', w9 = 'w09', w10 = 'w10',
                                     w11 = 'w11', w12 = 'w12', w13 = 'w13', w14 = 'w14', w15 = 'w15',
                                     w16 = 'w16', w17 = 'w17', w18 = 'w18', w19 = 'w19', w20 = 'w20',
                                     w21 = 'w21', w22 = 'w22', w23 = 'w23', w24 = 'w24', w25 = 'w25',
                                     w26 = 'w26', w27 = 'w27', w28 = 'w28', w29 = 'w29', w30 = 'w30',
                                     w31 = 'w31', w32 = 'w32', w33 = 'w33', w34 = 'w34', w35 = 'w35',
                                     w36 = 'w36', w37 = 'w37', w38 = 'w38', w39 = 'w39', w40 = 'w40',
                                     w41 = 'w41', w42 = 'w42', w43 = 'w43', w44 = 'w44')


# --- Zn --- #
zn_lur$weeks <- dplyr::recode_factor(zn_lur$weeks, 
                                     w1 = 'w01', w2 = 'w02', w3 = 'w03', w4 = 'w04', w5 = 'w05',
                                     w6 = 'w06', w7 = 'w07', w8 = 'w08', w9 = 'w09', w10 = 'w10',
                                     w11 = 'w11', w12 = 'w12', w13 = 'w13', w14 = 'w14', w15 = 'w15',
                                     w16 = 'w16', w17 = 'w17', w18 = 'w18', w19 = 'w19', w20 = 'w20',
                                     w21 = 'w21', w22 = 'w22', w23 = 'w23', w24 = 'w24', w25 = 'w25',
                                     w26 = 'w26', w27 = 'w27', w28 = 'w28', w29 = 'w29', w30 = 'w30',
                                     w31 = 'w31', w32 = 'w32', w33 = 'w33', w34 = 'w34', w35 = 'w35',
                                     w36 = 'w36', w37 = 'w37', w38 = 'w38', w39 = 'w39', w40 = 'w40',
                                     w41 = 'w41', w42 = 'w42', w43 = 'w43', w44 = 'w44')


no2_lur <- no2_lur %>% 
  dplyr::select(subject_id, weeks, date_start, date_end, no2_week_ratio) %>% 
  dplyr::rename(no2_lur = no2_week_ratio)

pm25_lur <- pm25_lur %>% 
  dplyr::select(subject_id, weeks, pm25_week_lur_ratio) %>% 
  dplyr::rename(pm25_lur= pm25_week_lur_ratio)

bc_lur <- bc_lur %>% 
  dplyr::select(subject_id, weeks, bc_week_ratio) %>% 
  dplyr::rename(bc_lur = bc_week_ratio)

fe_lur <- fe_lur %>% 
  dplyr::select(subject_id, weeks, fe_week_ratio) %>% 
  dplyr::rename(fe_lur = fe_week_ratio)

cu_lur <- cu_lur %>% 
  dplyr::select(subject_id, weeks, cu_week_ratio) %>% 
  dplyr::rename(cu_lur = cu_week_ratio)

zn_lur <- zn_lur %>% 
  dplyr::select(subject_id, weeks, zn_week_ratio) %>% 
  dplyr::rename(zn_lur = zn_week_ratio) 


# We use lur data to arrange(sort the data)
no2_lur <- no2_lur %>% dplyr::arrange(subject_id, weeks)

### --- Join all LUR estimates --- ### 
lur_estimates <- no2_lur %>% 
                 dplyr::inner_join(pm25_lur, by = c("subject_id", "weeks")) %>% 
                 dplyr::inner_join(bc_lur, by = c("subject_id", "weeks")) %>% 
                 dplyr::inner_join(fe_lur, by = c("subject_id", "weeks")) %>% 
                 dplyr::inner_join(cu_lur, by = c("subject_id", "weeks")) %>% 
                 dplyr::inner_join(zn_lur, by = c("subject_id", "weeks"))
        

dplyr::glimpse(lur_estimates)
lur_estimates %>% ungroup() %>% skimr::skim() # we need to ungroup first 

# add the longitude and latitude 
lur_estimates <- lur_estimates %>% 
                 dplyr::left_join(gid_data, 
                                  by = c("subject_id", "weeks"))

dplyr::glimpse(lur_estimates)

### --- Export data --- ###
rio::export(lur_estimates, "01.Data/lur_estimates.csv")

##############################################################################################

# Note 1: to plot the estimates we going to group by gid
# Note 2: to summarize the long-term exposure we group by subject_id

####################################
### --- Plotting estimates --- ###
##################################

lur_estimates <- read.csv("01.Data/lur_estimates.csv")


### -- LUR estimates --- ### 
dplyr::glimpse(lur_estimates)

lur_estimates_plot <- lur_estimates %>% 
                      dplyr::ungroup()

# summarise all the pollutants 
lur_estimates_plot <- lur_estimates_plot %>% dplyr::group_by(gid) %>%  dplyr::summarise_all(funs(mean))
dplyr::glimpse(lur_estimates_plot)
lur_estimates_plot <- lur_estimates_plot %>% dplyr::select(gid, subject_id, no2_lur:lat) %>% tidyr::drop_na()

### --- Load shapefile  Barcelona metropolitan --- ###b
bcn_shp <- sf::read_sf("01.Data/BCN_GIS_data/bcn/bcn.shp")
amb_shp <- sf::st_read("01.Data/amb.gpkg")
plot(amb_shp[1])

#####################################
# --- Entire pregnancy LUR --- # 
###################################

lur_data <- st_as_sf(lur_estimates_plot, coords = c("lon", "lat"), crs = 32632)  # replace with your actual CRS

amb_plot <- ggplot() + 
  geom_sf(data = amb_shp, fill = "grey", color = "white")+ 
  theme(plot.title = element_text(color = "black", size = 14, face = "bold")) +
  facet_grid(. ~ "NO2 LUR model") + theme_bw()

amb_plot 

filter_amb_shp <- amb_shp %>% 
                  dplyr::filter(NOMMUNI %in% c("Badalona", "Barcelona", "Esplugues de Llobregat",
                                             "l'Hospitalet de Llobregat", "Sant Joan Despí", "Sant Just Desvern",
                                             "Sant Feliu de Llobregat", "Sant Adrià de Besòs", 
                                             "Santa Coloma de Gramenet", "Ripollet", "Cerdanyola del Vallès",
                                             "Sant Cugat del Vallès", "Sant Boi de Llobregat","Cornellà de Llobregat"))


amb_bcn <- amb_shp %>%  
           dplyr::filter(NOMMUNI %in% c("Barcelona", "Badalona", 
                                        "Esplugues de Llobregat",
                                        "l'Hospitalet de Llobregat", "Sant Adrià de Besòs", 
                                        "Santa Coloma de Gramenet", "Sant Joan Despí", "Sant Just Desvern",
                                        "Cornellà de Llobregat"))




amb_plot_filtered <- ggplot() + 
  geom_sf(data = filter_amb_shp, fill = "grey", color = "white")+ 
  theme(plot.title = element_text(color = "black", size = 14, face = "bold")) +
  facet_grid(. ~ "NO2 LUR model") + theme_bw() +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) 

amb_plot_filtered


amb_bcn <- ggplot() + 
  geom_sf(data = amb_bcn, fill = "grey", color = "white")+ 
  theme(plot.title = element_text(color = "black", size = 14, face = "bold")) +
  facet_grid(. ~ "NO2 LUR model") + theme_bw() +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) 

amb_bcn




# Check CRS of shapefile
print(sf::st_crs(amb_shp))
print(sf::st_crs(lur_estimates_plot))

# Assuming lur_estimates_plot is already loaded and as shown in your output

# Convert lur_estimates_plot to an sf object
lur_estimates_plot_sf <- st_as_sf(lur_estimates_plot, coords = c("lon", "lat"), crs = 25831)

# Now, lur_estimates_plot_sf should have the CRS set to ETRS89 / UTM zone 31N
# You can check it again
print(st_crs(lur_estimates_plot_sf))

# Then you can proceed to plot
# Assuming your shapefile is already loaded and named as 'shapefile' and in the same CRS

ggplot() +
  geom_sf(data = amb_shp) +
  geom_sf(data = lur_estimates_plot_sf, color = "red", size = 0.5)
#####################################################################################



lur_estimates <- read.csv("01.Data/lur_estimates.csv")


# summarise all the pollutants 
lur_estimates_plot <- lur_estimates %>% 
  dplyr::ungroup()

lur_estimates_plot <- lur_estimates_plot %>% dplyr::group_by(gid) %>%  
  dplyr::summarise_all(funs(mean))

dplyr::glimpse(lur_estimates_plot)

lur_estimates_plot <- lur_estimates_plot %>% 
  dplyr::select(gid, subject_id, 
                no2_lur, pm25_lur, bc_lur, fe_lur, cu_lur, zn_lur,
                lon, lat) %>% 
  tidyr::drop_na()


lur_estimates_sf <- st_as_sf(lur_estimates_plot, coords = c("lon", "lat"), crs = 25831)
lur_estimates_wgs84 <- st_transform(lur_estimates_sf, 4326)

# Plotting with ggplot2
dplyr::glimpse(amb_shp)

ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_wgs84, aes(color = no2_lur), size = 0.1) +
  theme_minimal() +
  labs(title = "Barcelona LUR Estimates")

library(sf)
library(ggplot2)

# Lets check the extent of the shapefile
print(st_bbox(amb_shp))

# LUR estimates NO2 
no2_plot_lur <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = no2_lur), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis", direction = -1) + # Using viridis green palette
  #scale_color_distiller(palette = "Spectral", direction = -1) + # Using Spectral palette
  labs(title = "(A) LUR model",  
       color = expression(paste("NO"[2], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  #facet_grid(. ~ "NO2 LUR model") +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),  # Decrease the size of the legend text
        legend.title = element_text(size = 9),  # Decrease the size of the legend title
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),  # Remove the panel border
        panel.background = element_blank())  # Optionally, make the panel background transparent)

no2_plot_lur

# LUR estimates PM25 
pm25_plot_lur <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = pm25_lur), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis", direction = -1) + # Using viridis green palette
  #scale_color_distiller(palette = "Spectral", direction = -1) + # Using Spectral palette
  labs(title = "",
       color = expression(paste("PM"[2.5], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
  #facet_grid(. ~ "PM25 LUR model") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),  # Decrease the size of the legend text
        legend.title = element_text(size = 9),  # Decrease the size of the legend title
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),  # Remove the panel border
        panel.background = element_blank())  # Optionally, make the panel background transparent)

pm25_plot_lur


# LUR estimates BC
bc_plot_lur <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = bc_lur), size = 1, alpha = 0.5) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis", direction = -1) + # Using viridis green palette
  #scale_color_distiller(palette = "Spectral", direction = -1) + # Using Spectral palette
  labs(title = "", 
       color =expression(paste("BC", " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
  #facet_grid(. ~ "BC LUR model") + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),  # Decrease the size of the legend text
        legend.title = element_text(size = 9),  # Decrease the size of the legend title
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),  # Remove the panel border
        panel.background = element_blank())  # Optionally, make the panel background transparent)  # Decrease the size of the legend keys)


# Combine all the plots 
lur_map_plot <- (no2_plot_lur +  pm25_plot_lur + bc_plot_lur)
lur_map_plot

### --- Saving the LUR plot --- ###
ggsave(plot = lur_map_plot, "03.Outputs/figures/lur_map_plot.png", 
       dpi = 600, width = 10, height = 5, units = "in")

#############################################################################################

###################################################
### --- Dispersion exposure estimate plots --- ###
##################################################

dm_estimates <- read.csv("01.Data/dm_estimates.csv")


# summarise all the pollutants 
dm_estimates_plot <- dm_estimates %>% 
                     dplyr::ungroup()

dm_estimates_plot <- dm_estimates_plot %>% dplyr::group_by(gid) %>%  
  dplyr::summarise_all(funs(mean))

dplyr::glimpse(dm_estimates_plot)

dm_estimates_plot <- dm_estimates_plot %>% 
                     dplyr::select(gid, subject_id, no2_dm, pm25_dm, bc_dm, lon, lat) %>% 
                     tidyr::drop_na()


dm_estimates_sf <- st_as_sf(dm_estimates_plot, coords = c("lon", "lat"), crs = 25831)
dm_estimates_wgs84 <- st_transform(dm_estimates_sf, 4326)

library(sf)
library(ggplot2)

# Lets check the extent of the shapefile
print(st_bbox(amb_shp))

# DM estimates NO2 
no2_plot_dm <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = dm_estimates_sf, aes(color = no2_dm), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis", direction = -1) + # Using viridis green palette
  #scale_color_distiller(palette = "Spectral", direction = -1) + # Using Spectral palette
  labs(title = "(B) DM model",  
       color = expression(paste("NO"[2], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
  #facet_grid(. ~ "NO2 DM model") +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),  # Decrease the size of the legend text
        legend.title = element_text(size = 9),  # Decrease the size of the legend title
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),  # Remove the panel border
        panel.background = element_blank())  # Optionally, make the panel background transparent)

# DM estimates PM25 
pm25_plot_dm <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = dm_estimates_sf, aes(color = pm25_dm), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis", direction = -1) + # Using viridis green palette
  #scale_color_distiller(palette = "Spectral", direction = -1) + # Using Spectral palette
  labs(title = "",
       color = expression(paste("PM"[2.5], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
  #facet_grid(. ~ "PM25 LDM model") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),  # Decrease the size of the legend text
        legend.title = element_text(size = 9),  # Decrease the size of the legend title
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),  # Remove the panel border
        panel.background = element_blank())  # Optionally, make the panel background transparent)

# DM estimates BC
bc_plot_dm <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = dm_estimates_sf, aes(color = bc_dm), size = 1, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis", direction = -1) + # Using viridis green palette
  #scale_color_distiller(palette = "Spectral", direction = -1) + # Using Spectral palette
  labs(title = "", 
       color =expression(paste("BC", " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
  #facet_grid(. ~ "BC DM model") + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),  # Decrease the size of the legend text
        legend.title = element_text(size = 9),  # Decrease the size of the legend title
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),  # Remove the panel border
        panel.background = element_blank())  # Optionally, make the panel background transparent)  # Decrease the size of the legend keys)


# Combine all the plots 
dm_map_plot <- (no2_plot_dm +  pm25_plot_dm + bc_plot_dm)
dm_map_plot

####################################
### --- Saving the DM plot --- ###
##################################
ggsave(plot = dm_map_plot, "03.Outputs/figures/dm_map_plot_v2.png", 
       dpi = 600, width = 10, height = 5, units = "in")

#############################################################################################

###############################################
### --- Hybrid exposure estimate plots --- ###
#############################################

hm_estimates <- read.csv("01.Data/hm_estimates.csv")

# summarise all the pollutants 
hm_estimates_plot <- hm_estimates %>% 
                     dplyr::ungroup()

hm_estimates_plot <- hm_estimates_plot %>% 
                     dplyr::group_by(gid) %>%  
                     dplyr::summarise_all(funs(mean))

dplyr::glimpse(hm_estimates_plot)

hm_estimates_plot <- hm_estimates_plot %>% 
                     dplyr::select(gid, subject_id, 
                                   no2_hm, pm25_hm, bc_hm, 
                                   fe_hm, cu_hm, zn_hm, 
                                   lon, lat) %>% 
                     tidyr::drop_na()


hm_estimates_sf <- st_as_sf(hm_estimates_plot, coords = c("lon", "lat"), crs = 25831)
hm_estimates_wgs84 <- st_transform(hm_estimates_sf, 4326)

library(sf)
library(ggplot2)

# Lets check the extent of the shapefile
print(st_bbox(amb_shp))

# HM estimates NO2 
no2_plot_hm <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = hm_estimates_sf, aes(color = no2_hm), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis", direction = -1) + # Using viridis green palette
  #scale_color_distiller(palette = "Spectral", direction = -1) + # Using Spectral palette
  labs(title = "(C) HM model",  
       color = expression(paste("NO"[2], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
  #facet_grid(. ~ "NO2 HM model") +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),  # Decrease the size of the legend text
        legend.title = element_text(size = 9),  # Decrease the size of the legend title
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),  # Remove the panel border
        panel.background = element_blank())  # Optionally, make the panel background transparent)

no2_plot_hm

# HM estimates PM25 
pm25_plot_hm <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = hm_estimates_sf, aes(color = pm25_hm), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis", direction = -1) + # Using viridis green palette
  #scale_color_distiller(palette = "Spectral", direction = -1) + # Using Spectral palette
  labs(title = "",
       color = expression(paste("PM"[2.5], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
  #facet_grid(. ~ "PM25 HM model") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),  # Decrease the size of the legend text
        legend.title = element_text(size = 9),  # Decrease the size of the legend title
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),  # Remove the panel border
        panel.background = element_blank())  # Optionally, make the panel background transparent)

pm25_plot_hm

# HM estimates BC
bc_plot_hm <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = hm_estimates_sf, aes(color = bc_hm), size = 1, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis", direction = -1) + # Using viridis green palette
  #scale_color_distiller(palette = "Spectral", direction = -1) + # Using Spectral palette
  labs(title = "", 
       color =expression(paste("BC", " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
  #facet_grid(. ~ "BC LUR model") + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),  # Decrease the size of the legend text
        legend.title = element_text(size = 9),  # Decrease the size of the legend title
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),  # Remove the panel border
        panel.background = element_blank())  # Optionally, make the panel background transparent)  # Decrease the size of the legend keys)

bc_plot_hm


# Combine all the plots 
hm_map_plot <- (no2_plot_hm +  pm25_plot_hm + bc_plot_hm)
hm_map_plot

####################################
### --- Saving the DM plot --- ###
##################################
ggsave(plot = hm_map_plot, "03.Outputs/figures/hm_map_plot_v2.png", 
       dpi = 600, width = 10, height = 5, units = "in")


### check all the plots ###
lur_map_plot / dm_map_plot/ hm_map_plot

# Note: We need to fix the scales to compare the models 
lur_estimates_sf %>% dplyr::select(no2_lur, pm25_lur, bc_lur) %>% summary()
dm_estimates_sf %>% dplyr::select(no2_dm, pm25_dm, bc_dm) %>% summary()
hm_estimates_sf %>% dplyr::select(no2_hm, pm25_hm, bc_hm) %>% summary()


###################################################################################################

########################
### --- Boxplot --- ###
######################

# --- Read data --- ###
lur_estimates <- read.csv("01.Data/lur_estimates.csv")
dm_estimates <- read.csv("01.Data/dm_estimates.csv")
hm_estimates <- read.csv("01.Data/hm_estimates.csv")

# --- Check the data --- #
dplyr::glimpse(lur_estimates)
dplyr::glimpse(dm_estimates)
dplyr::glimpse(hm_estimates)

# create a label and renaming variables 
lur_estimates <- lur_estimates %>% 
                  dplyr::mutate(model = "LUR") %>% 
                  dplyr::ungroup() %>% 
                  dplyr::rename(no2 = no2_lur,
                                pm25 = pm25_lur,
                                bc = bc_lur,
                                fe = fe_lur, 
                                cu = cu_lur,
                                zn = zn_lur)


dm_estimates <- dm_estimates %>% 
                dplyr::mutate(model = "DM") %>% 
                dplyr::rename(no2 = no2_dm, 
                              pm25 = pm25_dm, 
                              bc = bc_dm)


hm_estimates <- hm_estimates %>% 
                dplyr::mutate(model = "HM") %>% 
                dplyr::rename(no2 = no2_hm, 
                              pm25 = pm25_hm, 
                              bc = bc_hm,
                              fe = fe_hm, 
                              cu = cu_hm,
                              zn = zn_hm)


# Note: Now I add fe, cu, zn to the datset for LUR and HM (we can't do the rbind with dm)
lur_estimates <- lur_estimates %>% dplyr::select(gid, subject_id, weeks, date_start, date_end, 
                                                 no2, pm25, bc,
                                                 #fe, cu, zn,
                                                 model)


dm_estimates <- dm_estimates %>% dplyr::select(gid, subject_id, weeks, date_start, date_end, 
                                                 no2, pm25, bc, model)


hm_estimates <- hm_estimates %>% dplyr::select(gid, subject_id, weeks, date_start, date_end, 
                                               no2, pm25, bc, 
                                               #fe, cu, zn, 
                                               model)


dplyr::glimpse(lur_estimates)
dplyr::glimpse(dm_estimates)
dplyr::glimpse(hm_estimates)

# putting all the estimates together
models_estimates <- rbind(lur_estimates, dm_estimates, hm_estimates)
model_estimates_elements <- rbind(lur_estimates, hm_estimates)

# levels of model variable
models_estimates$model <- as.factor(models_estimates$model)
levels(model_estimates$model)

model_estimates_elements$model <- as.factor(model_estimates_elements$model)
levels(model_estimates_elements$model)

model_estimates_elements$model <- factor(model_estimates_elements$model, 
                                         levels = c("LUR", "HM"),
                                         ordered = TRUE)

levels(model_estimates_elements$model)


models_estimates$model <- factor(models_estimates$model,
                                levels = c("LUR", "DM", "HM"),
                                ordered = TRUE)

dplyr::glimpse(model_estimates)
view(model_estimates_elements)

library(ggplot2)
library(ggpattern)

##################################
### --- NO2 model boxplot --- ###
################################
no2_boxplot <- ggplot2::ggplot(data = models_estimates, 
                mapping = aes(x = model, y = no2, color = model, fill = model)) + 
  geom_boxplot(alpha = 0.7, color = "black") + 
 #scale_color_manual(values = c('LUR' = '#440154', 
 #                               'DM' = '#3b528b', 
  #                              'HM' = '#21918c')) +
  scale_fill_manual(values = c('LUR' = '#440154', 
                               'DM' = '#3b528b', 
                               'HM' = '#21918c')) +
  ylab(bquote(NO[2] ~ (mu*g/m^3))) +
  theme_bw() +
  theme(legend.position = 'none')


### --- PM25 model boxplot --- ### 
pm25_boxplot <- ggplot2::ggplot(data = models_estimates, 
                mapping = aes(x = model, y = pm25, color = model, fill = model)) + 
  geom_boxplot(alpha = 0.7, color = "black") + 
  #scale_color_manual(values = c('LUR' = '#440154', 
  #                               'DM' = '#3b528b', 
  #                              'HM' = '#21918c')) +
  scale_fill_manual(values = c('LUR' = '#440154', 
                               'DM' = '#3b528b', 
                               'HM' = '#21918c')) +
  ylab(bquote(PM[25] ~ (mu*g/m^3))) +
  theme_bw() + 
  theme(legend.position = 'none')

### --- BC model boxplot --- ### 
bc_model_boxplot <- ggplot2::ggplot(data = models_estimates, 
                mapping = aes(x = model, y = bc, color = model, fill = model)) + 
  geom_boxplot(alpha = 0.7, color = "black") + 
  #scale_color_manual(values = c('LUR' = '#440154', 
  #                               'DM' = '#3b528b', 
  #                              'HM' = '#21918c')) +
  scale_fill_manual(values = c('LUR' = '#440154', 
                               'DM' = '#3b528b', 
                               'HM' = '#21918c')) +
  ylab(bquote(BC ~ (mu*g/m^3))) + ylim(c(0, 9)) +
  theme_bw() + 
  theme(legend.position = 'none')

### --- Fe model  boxplot --- ### 
fe_model_boxplot <- ggplot2::ggplot(data = model_estimates_elements, 
                                    mapping = aes(x = model, y = fe, color = model, fill = model)) + 
  geom_boxplot(alpha = 0.7, color = "black") + 
  #scale_color_manual(values = c('LUR' = '#440154', 
  #                               'DM' = '#3b528b', 
  #                              'HM' = '#21918c')) +
  scale_fill_manual(values = c('LUR' = '#440154', 
                               'HM' = '#21918c')) +
  ylab(bquote(Fe ~ (mu*g/m^3))) + 
  #ylim(c(0, 9)) +
  theme_bw() + 
  theme(legend.position = 'none')

fe_model_boxplot

### --- Cu model boxplot --- ### 
cu_model_boxplot <- ggplot2::ggplot(data = model_estimates_elements, 
                                    mapping = aes(x = model, y = cu, color = model, fill = model)) + 
  geom_boxplot(alpha = 0.7, color = "black") + 
  #scale_color_manual(values = c('LUR' = '#440154', 
  #                               'DM' = '#3b528b', 
  #                              'HM' = '#21918c')) +
  scale_fill_manual(values = c('LUR' = '#440154', 
                               'HM' = '#21918c')) +
  ylab(bquote(Cu ~ (n*g/m^3))) + 
  #ylim(c(0, 9)) +
  theme_bw() + 
  theme(legend.position = 'none')

cu_model_boxplot


### --- Mn model boxplot --- ### 
zn_model_boxplot <- ggplot2::ggplot(data = model_estimates_elements, 
                                    mapping = aes(x = model, y = zn, color = model, fill = model)) + 
  geom_boxplot(alpha = 0.7, color = "black") + 
  #scale_color_manual(values = c('LUR' = '#440154', 
  #                               'DM' = '#3b528b', 
  #                              'HM' = '#21918c')) +
  scale_fill_manual(values = c('LUR' = '#440154', 
                               'HM' = '#21918c')) +
  ylab(bquote(Zn ~ (n*g/m^3))) + 
  #ylim(c(0, 9)) +
  theme_bw() + 
  theme(legend.position = 'none')

zn_model_boxplot

### --- Putting al the graphs together --- ###
boxplot_models <- no2_boxplot | pm25_boxplot | bc_model_boxplot
boxplot_models + theme(aspect.ratio = 1)

boxplot_models_constituents <- fe_model_boxplot | cu_model_boxplot | zn_model_boxplot
boxplot_models_constituents + theme(aspect.ratio = 1)


boxplot_all_models_estimates <- boxplot_models / boxplot_models_constituents
boxplot_all_models_estimates
  
### --- saving the figure --- ### 

# NO2, PM25, BC
ggsave(plot = boxplot_models , "03.Outputs/figures/boxplot_models.png",
       dpi = 600, width = 10, height = 3, units = "in")

# FE, CU, ZN
ggsave(plot = boxplot_models_constituents , "03.Outputs/figures/boxplot_models_elements.png",
       dpi = 600, width = 10, height = 3, units = "in")

# NO2, PM25, BC, FE, CU, ZN (all models estimates )
ggsave(plot = boxplot_all_models_estimates, "03.Outputs/figures/boxplot_all_models_estimates.png",
       dpi = 600, width = 10, height = 6, units = "in")

##################################
### --- Correlation plots --- ###
#################################
  
### --- correlation long-term (entire pregnancy exposure period) --- ###

# --- Read data --- ###
lur_estimates <- read.csv("01.Data/lur_estimates.csv")
dm_estimates <- read.csv("01.Data/dm_estimates.csv")
hm_estimates <- read.csv("01.Data/hm_estimates.csv")
  
# --- Check the data --- #
dplyr::glimpse(lur_estimates)
dplyr::glimpse(dm_estimates)
dplyr::glimpse(hm_estimates)

view(lur_estimates_filtered)
view(lur_estimates)
view(dm_estimates)
view(hm_estimates)

# have a quick look 
lur_estimates %>% dplyr::group_by(subject_id)
dm_estimates %>% dplyr::group_by(subject_id)
hm_estimates %>% dplyr::group_by(subject_id)

# check the difference between ids 
unique_ids <- setdiff(lur_estimates$subject_id, union(dm_estimates$subject_id, hm_estimates$subject_id))
unique_ids # 12012911 12024211 12041011
unique_ids  <- c("12012911", "12024211", "12041011", "12015611",
                  "10002711", "10039411", "12003611", "12015611", "12035911")

# now we use unique_ids vector to filter lur_estimates
lur_estimates_filtered <- lur_estimates %>% dplyr::filter(!subject_id %in% unique_ids)
dplyr::glimpse(lur_estimates_filtered) %>% dplyr::group_by(subject_id)

dm_estimates_filtered <- dm_estimates %>% dplyr::filter(!subject_id %in% unique_ids)
dplyr::glimpse(dm_estimates_filtered) %>% dplyr::group_by(subject_id)

hm_estimates_filtered <- hm_estimates %>% dplyr::filter(!subject_id %in% unique_ids)
dplyr::glimpse(hm_estimates_filtered) %>% dplyr::group_by(subject_id)

# select variables 
lur_estimates_filtered 

dm_estimates_filtered <- dm_estimates_filtered %>% 
                         dplyr::select(subject_id, weeks,
                                       no2_dm, pm25_dm, bc_dm)

hm_estimates_filtered <- hm_estimates_filtered %>% 
                         dplyr::select(subject_id, weeks,
                                       no2_hm, pm25_hm, bc_hm,
                                       fe_hm, cu_hm, zn_hm)


estimates_correlation_data <- lur_estimates_filtered %>% 
                              dplyr::inner_join(dm_estimates_filtered, by = c("subject_id", "weeks")) %>% 
                              dplyr::inner_join(hm_estimates_filtered, by = c("subject_id", "weeks"))

dplyr::glimpse(estimates_correlation_data)


#######################################
### --- export correlation data --- ###
#######################################
rio::export(estimates_correlation_data, "01.Data/estimates_correlation_data.csv")

####################################################################################


####################################################
### --- Correlation plot long term exposure --- ###
##################################################
pacman::p_load(GGally)
library("GGally")

estimates_correlation_data <- read.csv("01.Data/estimates_correlation_data.csv")
dplyr::glimpse(estimates_correlation_data)
estimates_correlation_data %>% dplyr::group_by(subject_id)

###################################
### --- Data for the plots --- ###
#################################
dplyr::glimpse(estimates_correlation_data)

# short - term exposure data 
estimates_correlation_data <- estimates_correlation_data %>% 
                              dplyr::select(subject_id, weeks, 
                                            no2_lur:zn_lur, 
                                            no2_dm:zn_hm)

dplyr::glimpse(estimates_correlation_data)

names(estimates_correlation_data) <- c("ID", "weeks", 
                                       "NO2 LUR", "PM25 LUR", "BC LUR",
                                       "FE LUR", "CU LUR", "ZN LUR", 
                                       "NO2 DM", "PM25 DM", "BC DM",
                                       "NO2 HM", "PM25 HM", "BC HM", 
                                       "FE HM", "CU HM", "ZN HM")

# long - term exposure data 
preg_cor_data <- estimates_correlation_data  %>% 
                 dplyr::group_by(ID) %>% 
                 dplyr::summarise(dplyr::across(2:16, mean)) # start from 2 because the data is grouped by subject id this mean less one variable

dplyr::glimpse(preg_cor_data)


############################################
### --- PLotting correlation matrix --- ###
###########################################

### ---  Long - term exposure correlation matrix --- ### 
preg_corr <- ggcorr(preg_cor_data[, 2:16], 
                    method = c("pairwise", "spearman"), 
                    label = TRUE, label_size = 3 , label_round = 2, hjust = 0.85, layout.exp = 2) +
  ggplot2::labs(title = "(A) Long-term exposure (Entire pregnancy)") + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 14))

preg_corr


### ---  Short - term exposure correlation matrix --- ### 
weekly_corr <-  ggcorr(estimates_correlation_data[, 3:17], 
              method = c("pairwise", "spearman"),
              label = TRUE, label_size = 3 , label_round = 2, hjust = 0.85, layout.exp = 2) +
              ggplot2::labs(title = "(B) Short-term exposure (Weekly exposure)") +
              theme(legend.position = "none", 
                    plot.title = element_text(face = "bold", size = 14))

weekly_corr

# plotting things together
correlation_fig <- preg_corr | weekly_corr
correlation_fig

### --- save figure -- ###
ggsave(plot = correlation_fig, "03.Outputs/figures/correlation_fig_v2.png",
       dpi = 600, width = 15, height = 5, units = "in")


#############################################################
### --- Correlation Long-term exposure per pollutant --- ### 
########################################################## 

dplyr::glimpse(preg_cor_data) # long-term dataset
dplyr::glimpse(estimates_correlation_data) # short-term dataset

######################################################
### --- create new dataset long-term exposure --- ###
####################################################
no2_long_exp <-  preg_cor_data %>% dplyr::select(ID, `NO2 LUR`, `NO2 DM`, `NO2 HM`)
pm25_long_exp <- preg_cor_data %>% dplyr::select(ID, `PM25 LUR`, `PM25 DM`, `PM25 HM`)
bc_long_exp <- preg_cor_data %>% dplyr::select(ID, `BC LUR`, `BC DM`, `BC HM`)

dplyr::glimpse(no2_long_exp)
dplyr::glimpse(pm25_long_exp)
dplyr::glimpse(bc_long_exp)


# Generate the correlation plots with reduced margins
no2_long_plot <- ggcorr(no2_long_exp[, 2:4], method = c("pairwise", "spearman"),
                        label = TRUE, label_size = 4, label_round = 2, 
                        hjust = 0.5, vjust = 0.8) +
  ggplot2::labs(title = "(A) Long-term exposure") + 
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))  # Smaller margins

pm25_long_plot <- ggcorr(pm25_long_exp[, 2:4], method = c("pairwise", "spearman"),
                         label = TRUE, label_size = 4, label_round = 2,
                         hjust = 0.5, vjust = 0.8) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))  # Smaller margins

bc_long_plot <- ggcorr(bc_long_exp[, 2:4], method = c("pairwise", "spearman"),
                       label = TRUE, label_size = 4, label_round = 2,
                       hjust = 0.5, vjust = 0.8) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))  # Smaller margins

# Use grid.arrange to place the plots side by side
long_corr_pollutant <- no2_long_plot | pm25_long_plot | bc_long_plot
long_corr_pollutant

### --- save figure --- ###
ggsave(plot = long_corr_pollutant, "03.Outputs/figures/long_corr_pollutant.png",
       dpi = 600, width = 10, height = 5, units = "in")


#######################################################
### --- create new dataset short-term exposure --- ###
######################################################
no2_short_exp <- estimates_correlation_data  %>% dplyr::select(ID, `NO2 LUR`, `NO2 DM`, `NO2 HM`)
pm25_short_exp <- estimates_correlation_data  %>% dplyr::select(ID, `PM25 LUR`, `PM25 DM`, `PM25 HM`)
bc_short_exp <- estimates_correlation_data %>% dplyr::select(ID, `BC LUR`, `BC DM`, `BC HM`)

dplyr::glimpse(no2_short_exp)
dplyr::glimpse(pm25_short_exp)
dplyr::glimpse(bc_short_exp)

# Generate the correlation plots with reduced margins
no2_short_plot <- ggcorr(no2_short_exp[, 2:4], method = c("pairwise", "spearman"),
                        label = TRUE, label_size = 4, label_round = 2, 
                        hjust = 0.5, vjust = 0.8) +
  ggplot2::labs(title = "(B) Short-term exposure") + 
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))  # Smaller margins

pm25_short_plot <- ggcorr(pm25_short_exp[, 2:4], method = c("pairwise", "spearman"),
                         label = TRUE, label_size = 4, label_round = 2,
                         hjust = 0.5, vjust = 0.8) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))  # Smaller margins

bc_short_plot <- ggcorr(bc_short_exp[, 2:4], method = c("pairwise", "spearman"),
                       label = TRUE, label_size = 4, label_round = 2,
                       hjust = 0.5, vjust = 0.8) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))  # Smaller margins

# Use grid.arrange to place the plots side by side
short_corr_pollutant <- no2_short_plot | pm25_short_plot | bc_short_plot
short_corr_pollutant

### --- save figure --- ###
ggsave(plot = short_corr_pollutant, "03.Outputs/figures/short_corr_pollutant.png",
       dpi = 600, width = 10, height = 5, units = "in")

######################################################
### --- long-and-short-term correlation plots --- ### 
####################################################
corrplot_windows <- long_corr_pollutant / short_corr_pollutant
ggsave(plot = corrplot_windows, "03.Outputs/figures/corrplot_windows.png",
       dpi = 600, width = 15, height = 7, units = "in")


####################################
### --- Hexbin scatterplots --- ###
##################################
library("tidyverse")
library("colorspace")
library("lubridate")
library("gridExtra")


# Note: This could be added in the supplementary materials 

########################################
### --- NO2 Hexbin scatterplots --- ###
######################################

NO2_HM_LUR <- estimates_correlation_data %>%
              ggplot() +
              geom_hex(aes(y =`NO2 HM`, x = `NO2 LUR`)) +
              #scale_fill_continuous_sequential(palette = "spectral") +
              scale_fill_distiller(palette = "Spectral") + 
              coord_equal() +
              theme_bw(base_size = 10) + 
              theme(axis.text = element_text(size = 10)) + 
              labs(fill="Count") +
              #xlim(c(0, 150)) +
              #ylim(c(0, 150)) +
              ylab(expression(NO[2] ~ "HM predictions" ~ (mu * g/m^3))) + 
              xlab(expression(NO[2] ~ "LUR predictions" ~ (mu * g/m^3))) +
              theme(aspect.ratio = 1) 
              #theme(legend.position = 'none')

NO2_HM_LUR

NO2_HM_DM <- estimates_correlation_data %>%
  ggplot() +
  geom_hex(aes(y =`NO2 HM`, x = `NO2 DM`)) +
  #scale_fill_continuous_sequential(palette = "spectral") +
  scale_fill_distiller(palette = "Spectral") + 
  coord_equal() +
  theme_bw(base_size = 10) + 
  labs(fill="Count") +
  #xlim(c(0, 100)) +
  #ylim(c(0, 100)) +
  ylab(expression(NO[2] ~ "HM predictions" ~ (mu * g/m^3))) + 
  xlab(expression(NO[2] ~ "DM predictions" ~ (mu * g/m^3))) +
  theme(aspect.ratio = 1)

NO2_HM_DM

# NO2 predictions comparison
NO2_HEX <- NO2_HM_LUR | NO2_HM_DM
NO2_HEX

### --- Export HEXBIN plot --- ### 
ggsave(plot = NO2_HEX, "03.Outputs/figures/NO2_HEX.png",
       dpi = 600, width = 6, height = 5, units = "in")

#########################################
### --- PM25 Hexbin scatterplots --- ###
#######################################

PM25_HM_LUR <- estimates_correlation_data %>%
  ggplot() +
  geom_hex(aes(y =`PM25 HM`, x = `PM25 LUR`)) +
  #scale_fill_continuous_sequential(palette = "spectral") +
  scale_fill_distiller(palette = "Spectral") + 
  coord_equal() +
  theme_bw(base_size = 10) + 
  labs(fill="Count") +
  #xlim(c(0, 60)) +
  #ylim(c(0, 60)) +
  ylab(expression(PM[2.5] ~ "HM predictions" ~ (mu * g/m^3))) + 
  xlab(expression(PM[2.5] ~ "LUR predictions" ~ (mu * g/m^3))) +
  theme(aspect.ratio = 1)  
  #theme(legend.position = 'none')

PM25_HM_LUR

PM25_HM_DM <- estimates_correlation_data %>%
  ggplot() +
  geom_hex(aes(y =`PM25 HM`, x = `PM25 DM`)) +
  #scale_fill_continuous_sequential(palette = "spectral") +
  scale_fill_distiller(palette = "Spectral") + 
  coord_equal() +
  theme_bw(base_size = 10) + 
  labs(fill="Count") +
  #xlim(c(0, 40)) +
  #ylim(c(0, 40)) +
  ylab(expression(PM[2.5] ~ "HM predictions" ~ (mu * g/m^3))) + 
  xlab(expression(PM[2.5] ~ "DM predictions" ~ (mu * g/m^3))) +
  theme(aspect.ratio = 1)

PM25_HM_DM

# PM25 predictions comparison 
PM25_HEX <- PM25_HM_LUR | PM25_HM_DM
PM25_HEX

### --- Export HEXBIN plot --- ###
ggsave(plot = PM25_HEX, "03.Outputs/figures/PM25_HEX.png",
       dpi = 600, width = 6, height = 5, units = "in")

#########################################
### --- BC Hexbin scatterplots --- ###
#######################################

BC_HM_LUR <- estimates_correlation_data %>%
  ggplot() +
  geom_hex(aes(y =`BC HM`, x = `BC LUR`)) +
  #scale_fill_continuous_sequential(palette = "spectral") +
  scale_fill_distiller(palette = "Spectral") + 
  coord_equal() +
  theme_bw(base_size = 10) + 
  labs(fill="Count") +
 #xlim(c(0, 4)) +
 #ylim(c(0, 4)) +
  ylab(expression(BC ~ "HM predictions" ~ (mu * g/m^3))) + 
  xlab(expression(BC ~ "LUR predictions" ~ (mu * g/m^3))) +
  theme(aspect.ratio = 1)  
  #theme(legend.position = 'none')

BC_HM_LUR

BC_HM_DM <- estimates_correlation_data %>%
  ggplot() +
  geom_hex(aes(y =`BC HM`, x = `BC DM`)) +
  #scale_fill_continuous_sequential(palette = "spectral") +
  scale_fill_distiller(palette = "Spectral") + 
  coord_equal() +
  theme_bw(base_size = 10) + 
  labs(fill="Count") +
  #xlim(c(0, 4)) +
  #ylim(c(0, 4)) +
  ylab(expression(BC ~ "HM predictions" ~ (mu * g/m^3))) + 
  xlab(expression(BC ~ "DM predictions" ~ (mu * g/m^3))) +
  theme(aspect.ratio = 1)

BC_HM_DM

# BC predictions comparison 
BC_HEX <- BC_HM_LUR | BC_HM_DM
BC_HEX


### --- Save HEXBIN plot --- ###
ggsave(plot = BC_HEX, "03.Outputs/figures/BC_HEX_v2.png",
       dpi = 600, width = 6, height = 5, units = "in")


# Putting all the figures together 
HEX_all <- NO2_HEX / PM25_HEX / BC_HEX
HEX_all 

### --- Save HEXBIN all plots together --- ###
ggsave(plot = HEX_all, "03.Outputs/figures/HEX_all.png",
       dpi = 600, width = 10, height = 7)


### --- Save HEXBIN LUR together --- ###
HM_LUR_HEX <- NO2_HM_LUR / PM25_HM_LUR / BC_HM_LUR
HM_LUR_HEX

# Exporting figure 
ggsave(plot = HM_LUR_HEX, "03.Outputs/figures/HM_LUR_HEX.png",
       dpi = 600, width = 3, height = 9, units = "in")


### --- Save HEXBIN figures together --- ###
HM_DM_HEX <- NO2_HM_DM / PM25_HM_DM / BC_HM_DM
HM_DM_HEX

ggsave(plot = HM_DM_HEX, "03.Outputs/figures/HM_DM_HEX.png",
       dpi = 600, width = 3, height = 9, units = "in")

####################################### 
### --- Fe Hexbin scatterplots --- ### 
#####################################
FE_HM_LUR <- estimates_correlation_data %>%
  ggplot() +
  geom_hex(aes(y =`FE HM`, x = `FE LUR`)) +
  #scale_fill_continuous_sequential(palette = "spectral") +
  scale_fill_distiller(palette = "Spectral") + 
  coord_equal() +
  theme_bw(base_size = 10) + 
  labs(fill="Count") +
  #xlim(c(0, 4)) +
  #ylim(c(0, 4)) +
  ylab(expression(Fe ~ "HM predictions" ~ (mu * g/m^3))) + 
  xlab(expression(Fe ~ "LUR predictions" ~ (mu * g/m^3))) +
  theme(aspect.ratio = 1) + 
  theme(legend.position = "right")

FE_HM_LUR

#######################################
### --- Cu Hexbin scatterplots --- ###
######################################
CU_HM_LUR <- estimates_correlation_data %>%
  ggplot() +
  geom_hex(aes(y =`CU HM`, x = `CU LUR`)) +
  #scale_fill_continuous_sequential(palette = "spectral") +
  scale_fill_distiller(palette = "Spectral") + 
  coord_equal() +
  theme_bw(base_size = 10) + 
  labs(fill="Count") +
  #xlim(c(0, 4)) +
  #ylim(c(0, 4)) +
  ylab(expression(Cu ~ "HM predictions" ~ (n * g/m^3))) + 
  xlab(expression(Cu ~ "LUR predictions" ~ (n * g/m^3))) +
  theme(aspect.ratio = 1) + 
  theme(legend.position = "right")

CU_HM_LUR

######################################
### --- Zn Hexbin scatterplot --- ### 
#####################################

ZN_HM_LUR <- estimates_correlation_data %>%
  ggplot() +
  geom_hex(aes(y =`ZN HM`, x = `ZN LUR`)) +
  #scale_fill_continuous_sequential(palette = "spectral") +
  scale_fill_distiller(palette = "Spectral") + 
  coord_equal() +
  theme_bw(base_size = 10) + 
  labs(fill="Count") +
  #xlim(c(0, 4)) +
  #ylim(c(0, 4)) +
  ylab(expression(Zn ~ "HM predictions" ~ (n * g/m^3))) + 
  xlab(expression(Zn ~ "LUR predictions" ~ (n * g/m^3))) +
  theme(aspect.ratio = 1) + 
  theme(legend.position = "right")

ZN_HM_LUR

### --- Combining all plots --- ### 
PM25_elements_HEX <- FE_HM_LUR / CU_HM_LUR / ZN_HM_LUR
PM25_elements_HEX

# Exporting figure 
ggsave(plot = PM25_elements_HEX, "03.Outputs/figures/PM25_elements_HEX.png",
       dpi = 600, width = 3, height = 9, units = "in")


### --- Final HEXBIN plot --- ### 
final_HEXBIN <- (NO2_HM_LUR / PM25_HM_LUR / BC_HM_LUR) | (FE_HM_LUR / CU_HM_LUR / ZN_HM_LUR) | (NO2_HM_DM / PM25_HM_DM / BC_HM_DM)

# Exporting figure 
ggsave(plot = final_HEXBIN, "03.Outputs/figures/final_HEXBIN.png",
       dpi = 600, width = 9, height = 9, units = "in")


##################################################
### --- Rescalling MAP legend comparison --- ###############################################
################################################

###########################
### --- NO2 models --- ###
#########################

# Calculate overall min and max for NO2 from the summaries you have
min_no2 <- min(lur_estimates_sf$no2_lur, dm_estimates_sf$no2_dm, hm_estimates_sf$no2_hm, na.rm = TRUE)
max_no2 <- max(lur_estimates_sf$no2_lur, dm_estimates_sf$no2_dm, hm_estimates_sf$no2_hm, na.rm = TRUE)
min_no2
max_no2

# Define the breaks you want to show in the legend
legend_breaks <- c(min_no2, 26, 43, 60, max_no2)
legend_labels <- c(as.integer(min_no2), "26", "43", "60", as.integer(max_no2))


# We use this vector to filtered the data
subjects_out <- c("10051711", "10032711", "12031211", 
                  "11003411", "10038011", "12032011",
                  "12025111", "10052911", "12020611", "12025611")


lur_estimates_sf <- lur_estimates_sf %>%
                    dplyr::filter(!(subject_id %in% subjects_out))

##############################
### --- NO2 LUR model --- ###
#############################
NO2_LUR_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = no2_lur), 
          size = 1.2, alpha = 1) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_no2, max_no2),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(as.integer(min_no2), "26", "43", "60", as.integer(max_no2))  # Format the labels as desired
  ) + # Set the limits to the overall range
  labs(title = expression(paste("(A) NO"[2], " models")), 
       color = expression(paste("NO"[2], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.placement = "outside"
  ) +
  facet_grid(.~'LUR model')

NO2_LUR_model_plot

#############################
### --- NO2 DM model --- ###
############################

# filtering the subject outside the area 
dm_estimates_sf <- dm_estimates_sf %>%
  dplyr::filter(!(subject_id %in% subjects_out))


NO2_DM_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = dm_estimates_sf, aes(color = no2_dm),
          size = 1.2, alpha = 1) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_no2, max_no2),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(as.integer(min_no2), "26", "43", "60", as.integer(max_no2))  # Format the labels as desired
  )  + # Set the limits to the overall range
  labs(title = "",  
       color = expression(paste("NO"[2], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() +  
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 9), 
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12), # Make facet labels bold and align to the left
        strip.background = element_blank(), # Remove background rectangle from facet labels
        strip.placement = "outside") + # Remove background rectangle from facet labels)  +
  facet_grid(.~'Dispersion model')

NO2_DM_model_plot

#################################
### --- NO2 Hybrid model --- ###
################################

# filtering the participants outside of the area
hm_estimates_sf <- hm_estimates_sf %>%
  dplyr::filter(!(subject_id %in% subjects_out))

NO2_HM_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = hm_estimates_sf, aes(color = no2_hm), 
          size = 1.2, alpha = 1) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_no2, max_no2),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(as.integer(min_no2), "26", "43", "60", as.integer(max_no2))  # Format the labels as desired
  )  + # Set the limits to the overall range
  labs(title = "",  
       color = expression(paste("NO"[2], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() + 
  theme(
    plot.title = element_blank(),  # Remove plot title since it's empty
    legend.position = "right",
    legend.title = element_text(margin = margin(b = 5)),  # Push the legend title up
    legend.title.align = 0.5,
    strip.text.x = element_text(face = "bold", hjust = 0, size = 12),  # Left align the facet label
    strip.background = element_blank(),  # Remove the background from the facet label
    legend.text = element_text(size = 10), 
    legend.key.size = unit(1, 'lines'), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  facet_grid(.~'Hybrid model')  # Add the model name as facet label

NO2_HM_model_plot


### --- NO2 models comparison ---- ###
NO2_models_map <- NO2_LUR_model_plot | NO2_DM_model_plot | NO2_HM_model_plot
NO2_models_map

### --- save the figure --- ### 
ggsave(plot = NO2_models_map, "03.Outputs/figures/NO2_models_map_v2.png",
       dpi = 600, width = 10, height = 5)


################################################################
### --- creating an interactive plot to remove some ids --- ###
###############################################################
pacman::p_load(plotly)
library(plotly)
p_interactive <- ggplotly(NO2_HM_model_plot) 

p_interactive$data[[2]]$text <- hm_estimates_sf$subject_id
p_interactive$data[[2]]$hoverinfo <- "text"


###########################
### --- PM25 models --- ###
#########################

# Calculate overall min and max for NO2 from the summaries you have
min_pm25 <- min(lur_estimates_sf$pm25_lur, dm_estimates_sf$pm25_dm, hm_estimates_sf$pm25_hm, na.rm = TRUE)
max_pm25 <- max(lur_estimates_sf$pm25_lur, dm_estimates_sf$pm25_dm, hm_estimates_sf$pm25_hm, na.rm = TRUE)
min_pm25
max_pm25

# Define the breaks you want to show in the legend
legend_breaks <- c(min_pm25, 10, 14, 18, 22, max_pm25)
legend_labels <- c(as.integer(min_pm25), "10", "14", "18", "22", as.integer(max_pm25))

##############################
### --- PM25 LUR model --- ###
#############################
PM25_LUR_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = pm25_lur), size = 1.0, alpha = 1.0) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_pm25, max_pm25),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(as.integer(min_pm25), "10", "14", "18", "22", as.integer(max_pm25))) + # Set the limits to the overall range
  labs(title = expression(paste("(B) PM"[2.5], " models")),   
       color = expression(paste("PM"[2.5], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 9), 
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12), # Make facet labels bold and align to the left
        strip.background = element_blank(), # Remove background rectangle from facet labels
        strip.placement = "outside") + # Remove background rectangle from facet labels)  +
  facet_grid(.~'LUR model')

PM25_LUR_model_plot

#############################
### --- PM25 DM model --- ###
############################
PM25_DM_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = dm_estimates_sf, aes(color = pm25_dm), size = 1.0, alpha = 1.0) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_pm25, max_pm25),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(as.integer(min_pm25), "10", "14", "18", "22", as.integer(max_pm25))) + # Set the limits to the overall range
  labs(title = "",  
       color = expression(paste("PM"[2.5], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() +  
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 9), 
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12), # Make facet labels bold and align to the left
        strip.background = element_blank(), # Remove background rectangle from facet labels
        strip.placement = "outside") + # Remove background rectangle from facet labels)  +
  facet_grid(.~'Dispersion model')

PM25_DM_model_plot

#################################
### --- PM25 Hybrid model --- ###
################################
PM25_HM_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = hm_estimates_sf, aes(color = pm25_hm), size = 1.0, alpha = 1.0) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_pm25, max_pm25),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(as.integer(min_pm25), "10", "14", "18", "22", as.integer(max_pm25))) + # Set the limits to the overall range
  labs(title = "",  
       color = expression(paste("PM"[2.5], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() + 
  theme(
    plot.title = element_blank(),  # Remove plot title since it's empty
    legend.position = "right",
    legend.title = element_text(margin = margin(b = 5)),  # Push the legend title up
    legend.title.align = 0.5,
    strip.text.x = element_text(face = "bold", hjust = 0, size = 12),  # Left align the facet label
    strip.background = element_blank(),  # Remove the background from the facet label
    legend.text = element_text(size = 10), 
    legend.key.size = unit(1, 'lines'), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  facet_grid(.~'Hybrid model')  # Add the model name as facet label

PM25_HM_model_plot 

### --- PM25 models comparison ---- ###
PM25_models_map <-  PM25_LUR_model_plot | PM25_DM_model_plot | PM25_HM_model_plot
PM25_models_map

### --- save the plot --- ###
ggsave(plot = PM25_models_map, "03.Outputs/figures/PM25_models_map_v2.png",
       dpi = 600, width = 10, height = 5, units = "in")


###########################
### --- BC models --- ###
#########################

# Calculate overall min and max for NO2 from the summaries you have
min_bc <- min(lur_estimates_sf$bc_lur, dm_estimates_sf$bc_dm, hm_estimates_sf$bc_hm, na.rm = TRUE)
max_bc <- max(lur_estimates_sf$bc_lur, dm_estimates_sf$bc_dm, hm_estimates_sf$bc_hm, na.rm = TRUE)
min_bc
max_bc

# Define the breaks you want to show in the legend
legend_breaks <- c(min_bc, 0.8, 1.5, 2.3, 3.0, max_bc)
legend_labels <- c(as.integer(min_bc),  "0.8", "1.5", "2.3", "3.0", as.integer(max_bc))

##############################
### --- BC LUR model --- ###
#############################
BC_LUR_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = bc_lur), size = 1.0, alpha = 1.0) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_bc, max_bc),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(round(min_bc, digit = 1),  "0.8", "1.5", "2.3", "3.0", round(max_bc, digit = 1))
    ) + # Set the limits to the overall range
  labs(title = expression(paste("(C) BC models")),   
       color = expression(paste("BC" ," (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 9), 
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12), # Make facet labels bold and align to the left
        strip.background = element_blank(), # Remove background rectangle from facet labels
        strip.placement = "outside") + # Remove background rectangle from facet labels)  +
  facet_grid(.~'LUR model')

BC_LUR_model_plot

#############################
### --- BC DM model --- ###
############################
BC_DM_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = dm_estimates_sf, aes(color = bc_dm), size = 1.0, alpha = 1.0) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_bc, max_bc),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(round(min_bc, digit = 1),  "0.8", "1.5", "2.3", "3.0", round(max_bc, digit = 1))
  ) + # Set the limits to the overall range
  labs(title = "",  
       color = expression(paste("BC", " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() +  
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 9), 
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12), # Make facet labels bold and align to the left
        strip.background = element_blank(), # Remove background rectangle from facet labels
        strip.placement = "outside") + # Remove background rectangle from facet labels)  +
  facet_grid(.~'Dispersion model')

BC_DM_model_plot

#################################
### --- BC Hybrid model --- ###
################################
BC_HM_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = hm_estimates_sf, aes(color = bc_hm), size = 1.0, alpha = 1.0) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_bc, max_bc),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(round(min_bc, digit = 1),  "0.8", "1.5", "2.3", "3.0", round(max_bc, digit = 1))
  ) + # Set the limits to the overall range
  labs(title = "",  
       color = expression(paste("BC", " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() + 
  theme(
    plot.title = element_blank(),  # Remove plot title since it's empty
    legend.position = "right",
    legend.title = element_text(margin = margin(b = 5)),  # Push the legend title up
    legend.title.align = 0.5,
    strip.text.x = element_text(face = "bold", hjust = 0, size = 12),  # Left align the facet label
    strip.background = element_blank(),  # Remove the background from the facet label
    legend.text = element_text(size = 10), 
    legend.key.size = unit(1, 'lines'), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  facet_grid(.~'Hybrid model')  # Add the model name as facet label

BC_HM_model_plot 

### --- PM25 models comparison ---- ###
BC_models_map <-  BC_LUR_model_plot | BC_DM_model_plot | BC_HM_model_plot
BC_models_map

### --- save the plot --- ### 
ggsave(plot = BC_models_map, "03.Outputs/figures/BC_models_map_v2.png",
       dpi = 600, width = 10, height = 5, units = "in")

### --- Putting all the models together with the fixed scale --- ###
all_maps <- NO2_models_map / PM25_models_map / BC_models_map
all_maps


### --- save the plot --- ### 
ggsave(plot = all_maps, "03.Outputs/figures/all_maps.png",
       dpi = 600, width = 10, height = 10, units = "in")


###########################
### --- Fe models --- ###
#########################
dplyr::glimpse(lur_estimates_sf)

# Calculate overall min and max for NO2 from the summaries you have
min_fe <- min(lur_estimates_sf$fe_lur, hm_estimates_sf$fe_hm, na.rm = TRUE)
max_fe <- max(lur_estimates_sf$fe_lur, hm_estimates_sf$fe_hm, na.rm = TRUE)
min_fe
max_fe


# Define the breaks you want to show in the legend
legend_breaks <- c(min_fe, 0.20, 0.32, 0.45, 0.60, max_fe)
legend_labels <- c(as.integer(min_fe), "0.20", "0.32", "0.45", "0.60", as.integer(max_fe))

##############################
### --- Fe LUR model --- ###
#############################
FE_LUR_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = fe_lur), size = 1.0, alpha = 1.0) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_fe, max_fe),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(round(min_fe, digit = 1),  "0.20", "0.32", "0.45", "0.60", round(max_fe, digit = 1))
  ) + # Set the limits to the overall range
  labs(title = expression(paste("(D) Fe models")),   
       color = expression(paste("Fe" ," (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 9), 
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12), # Make facet labels bold and align to the left
        strip.background = element_blank(), # Remove background rectangle from facet labels
        strip.placement = "outside") + # Remove background rectangle from facet labels)  +
  facet_grid(.~'LUR model')

FE_LUR_model_plot


#################################
### --- Fe Hybrid model --- ###
################################
Fe_HM_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = hm_estimates_sf, aes(color = fe_hm), size = 1.0, alpha = 1.0) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_fe, max_fe),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(round(min_fe, digit = 1),  "0.20", "0.32", "0.45", "0.60", round(max_fe, digit = 1))
  ) + # Set the limits to the overall range
  labs(title = "",  
       color = expression(paste("Fer", " (µg/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() + 
  theme(
    plot.title = element_blank(),  # Remove plot title since it's empty
    legend.position = "right",
    legend.title = element_text(margin = margin(b = 5)),  # Push the legend title up
    legend.title.align = 0.5,
    strip.text.x = element_text(face = "bold", hjust = 0, size = 12),  # Left align the facet label
    strip.background = element_blank(),  # Remove the background from the facet label
    legend.text = element_text(size = 10), 
    legend.key.size = unit(1, 'lines'), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  facet_grid(.~'Hybrid model')  # Add the model name as facet label



### --- FE models comparison ---- ###
FE_models_map <-  FE_LUR_model_plot | FE_HM_model_plot | plot_spacer()
FE_models_map

### --- save the plot --- ### 
ggsave(plot = FE_models_map, "03.Outputs/figures/FE_models_map.png",
       dpi = 600, width = 10, height = 5, units = "in")


###########################
### --- Cu models --- ###
#########################
dplyr::glimpse(lur_estimates_sf)
dplyr::glimpse(hm_estimates_sf)

# Calculate overall min and max for NO2 from the summaries you have
min_cu <- min(lur_estimates_sf$cu_lur, hm_estimates_sf$cu_hm, na.rm = TRUE)
max_cu <- max(lur_estimates_sf$cu_lur, hm_estimates_sf$cu_hm, na.rm = TRUE)
min_cu
max_cu


# Define the breaks you want to show in the legend
legend_breaks <- c(min_cu, 4.54, 8.11, 11.68, 15.25, max_cu)
legend_labels <- c(as.integer(min_cu), "4.54", "8.11", "11.68", "15.25", as.integer(max_cu))

##############################
### --- CU LUR model --- ###
#############################
CU_LUR_model_plot <- ggplot() +
  geom_sf(data = amb_bcn, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = cu_lur), size = 1.0, alpha = 1.0) +
  theme_minimal() +
  scale_color_viridis_c(
    option = "viridis", direction = 1, 
    limits = c(min_cu, max_cu),
    breaks = legend_breaks, # Set the breaks for the legend
    labels = c(as.integer(min_cu), "4.54", "8.11", "11.68", "15.25", as.integer(max_cu))
  ) + # Set the limits to the overall range
  labs(title = expression(paste("(D) Cu models")),   
       color = expression(paste("Cu" ," (ng/m"^3*")"))) +
  xlim(c(st_bbox(amb_bcn)[1], st_bbox(amb_bcn)[3])) +
  ylim(c(st_bbox(amb_bcn)[2], st_bbox(amb_bcn)[4])) +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 9), 
        legend.key.size = unit(0.5, 'cm'),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12), # Make facet labels bold and align to the left
        strip.background = element_blank(), # Remove background rectangle from facet labels
        strip.placement = "outside") + # Remove background rectangle from facet labels)  +
  facet_grid(.~'LUR model')

CU_LUR_model_plot





















































###########################################################################################

########################################################
### --- Descriptive table observed measurements --- ###
######################################################
dplyr::glimpse(no2_measures)
dplyr::glimpse(pm25_measures)
dplyr::glimpse(bc_measures)
dplyr::glimpse(pm25_constituents_measures)

##############################################################################################################
### Table 1. Outdoor air pollution concentration distribution for the BiSC-home and BiSCAPE campaigns --- ###
############################################################################################################


no2_measures %>% dplyr::select(NO2.DirectMeasurements) %>% skimr::skim()
pm25_measures %>% dplyr::select(c_biscape) %>% skimr::skim()
bc_measures %>% dplyr::select(c_biscape) %>% skimr::skim()


bc_measures$c_biscape

no2_measures$year <- as.factor(no2_measures$year) 
levels(no2_measures$year) # "2018" "2019" "2020" "2021"


pm25_measures$year <- as.factor(pm25_measures$year)


skimr::skim(no2_measures)
skimr::skim(pm25_measures)
skimr::skim(bc_measures)
skimr::skim(pm25_constituents_measures)

  

view(pm25_measures)
view(no2_measure)





