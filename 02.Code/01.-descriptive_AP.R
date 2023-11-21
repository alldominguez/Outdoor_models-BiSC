# Install packages
pacman::p_load(tidyverse, purrr, skimr, caret, tictoc, lubridate, 
               mice, ggmice, plyr, data.table, mapview, ggmap, omsdata, sf, sp, patchwork)

##############################
### --- Load the data --- ###
############################

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
# Note 2: to summarize the long-term exposure we group by 

####################################
### --- Plotting estimates --- ###
##################################

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

filter_amb_shp <- amb_shp %>% 
                  dplyr::filter(NOMMUNI %in% c("Badalona", "Barcelona", "Esplugues de Llobregat",
                                             "l'Hospitalet de Llobregat", "Sant Joan Despí", "Sant Just Desvern",
                                             "Sant Feliu de Llobregat", "Sant Adrià de Besòs", 
                                             "Santa Coloma de Gramenet", "Ripollet", "Cerdanyola del Vallès",
                                             "Sant Cugat del Vallès", "Sant Boi de Llobregat","Cornellà de Llobregat"))


amb_plot_filtered <- ggplot() + 
  geom_sf(data = filter_amb_shp, fill = "grey", color = "white")+ 
  theme(plot.title = element_text(color = "black", size = 14, face = "bold")) +
  facet_grid(. ~ "NO2 LUR model") + theme_bw() +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) 

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


# First, convert your points to an sf object
lur_estimates_sf <- st_as_sf(lur_estimates_plot, coords = c("lon", "lat"), crs = 25831)

lur_estimates_wgs84 <- st_transform(lur_estimates_sf, 4326)

# Plotting with ggplot2

dplyr::glimpse(amb_shp)

ggplot() +
  geom_sf(data = amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_wgs84, aes(color = no2_lur), size = 0.1) +
  theme_minimal() +
  labs(title = "Barcelona LUR Estimates")

library(sf)
library(ggplot2)

# Lets check the extent of the shapefile
print(st_bbox(amb_shp))

# LUR estimates NO2 
no2_plot_lur <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = no2_lur), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis") + # Using viridis green palette
  labs(title = "(A) LUR model",  
       color = expression(paste("NO"[2], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
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

# LUR estimates PM25 
pm25_plot_lur <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = pm25_lur), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis") + # Using viridis green palette
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

# LUR estimates BC
bc_plot_lur <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = lur_estimates_sf, aes(color = bc_lur), size = 1, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis") + # Using viridis green palette
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

# summarise all the pollutants 
dm_estimates_plot <- dm_estimates %>% 
                     dplyr::ungroup()

dm_estimates_plot <- dm_estimates_plot %>% dplyr::group_by(gid) %>%  dplyr::summarise_all(funs(mean))
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

# LUR estimates NO2 
no2_plot_dm <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = dm_estimates_sf, aes(color = no2_dm), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis") + # Using viridis green palette
  labs(title = "(B) DM model",  
       color = expression(paste("NO"[2], " (µg/m"^3*")"))) +
  xlim(c(st_bbox(filter_amb_shp)[1], st_bbox(filter_amb_shp)[3])) +
  ylim(c(st_bbox(filter_amb_shp)[2], st_bbox(filter_amb_shp)[4])) +
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

# LUR estimates PM25 
pm25_plot_dm <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = dm_estimates_sf, aes(color = pm25_dm), size = 1.0, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis") + # Using viridis green palette
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

# LUR estimates BC
bc_plot_dm <- ggplot() +
  geom_sf(data = filter_amb_shp, fill = "lightgrey", color = "white") +
  geom_sf(data = dm_estimates_sf, aes(color = bc_dm), size = 1, alpha = 0.7) +
  theme_minimal() +
  scale_color_viridis_c(option = "viridis") + # Using viridis green palette
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
dm_map_plot <- (no2_plot_dm +  pm25_plot_dm + bc_plot_dm)
dm_map_plot

####################################
### --- Saving the LUR plot --- ###
##################################
ggsave(plot = dm_map_plot, "03.Outputs/figures/dm_map_plot.png", 
       dpi = 600, width = 10, height = 5, units = "in")



