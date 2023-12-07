# 03.- monitoring stations 
pacman::p_load(tidyverse, purrr, skimr, caret, tictoc, lubridate, 
               mice, ggmice, plyr, data.table, mapview, ggmap, omsdata, sf, sp, patchwork,
               ggpattern, GGally)

###################################
### --- Barcelona Base Map --- ### 
#################################
load("03.Outputs/figures/barcelona_base_map.RData")

####################################
### --- Monitoring stations --- ###
###################################

### --- Coordinates --- ### 
monitoring_coords <- readxl::read_xlsx("01.Data/BiSC_monitoring_stations/monitoring_coordenates.xlsx")
dplyr::glimpse(monitoring_coords)
view(monitoring_coords)

monitoring_coords <- monitoring_coords %>% 
                     dplyr::filter(!station == "obs_fabra")


### --- Mapping monitoring stations --- ### 
  ggmap(barcelona_map2) +
  geom_point(data = monitoring_coords,
             aes(x = longitude, y = latitude), 
             color = '#21918c',
             alpha = 1, 
             size = 2) +
  theme_minimal() +  xlab('') + ylab('') +
  facet_grid(.~'Monitoring Stations')


# Change the names displaye in the map
new_names <- c("Ciutadella", "Eixample", "Gracia", "Hospitalet",
                 "Palau Reial", "Poblenou", "Sants", "Vall Hebron", "Ies Goya",
                 "Pl. Univ", "Zona Univ", "Jardins", "Sagnier", "St. Adria",
                 "Sta. Coloma")
  
monitoring_coords$station <- factor(monitoring_coords$station, levels = unique(monitoring_coords$station), labels = new_names)
  

# here we assing the label direction by hand 
label_direction <- c(0.005, -0.005, 0.005, -0.005, 
                     0.005, 0.005, -0.005, 0.005, 0.005, 
                     0.005, -0.005, 0.005, 0.005, 0.005, 
                     0.005)

monitoring_coords$label_direction <- label_direction

# Plotting the monitoring stations with the 
fig_stations <- ggmap(barcelona_map2) +
                geom_point(data = monitoring_coords,
                           aes(x = longitude, y = latitude), 
                           color = 'firebrick',
                           alpha = 1, 
                           size = 2) +
                geom_label(data = monitoring_coords,
                           aes(x = longitude, y = latitude, label = station), 
                           color = "black",
                           fill = "white",
                           label.size = 0.2,
                           label.padding = unit(0.2, "lines"),
                           hjust = 0.5,
                           vjust = 0.5,
                           size = 2.5,
                           alpha = 0.7,
                           nudge_y = monitoring_coords$label_direction) +  # Use smaller nudge value
                theme_minimal() + 
                xlab('') + 
                ylab('') +
                facet_grid(.~'Monitoring Stations')

fig_stations 


# monitoring station 
monitoring_station_fig <- ggmap(barcelona_map2) +
  geom_point(data = monitoring_coords,
             aes(x = longitude, y = latitude, 
                 color = ifelse(station == "Palau Reial", "LUR background station", "Monitoring stations")), 
             alpha = 1, 
             size = 2.5) +
  geom_label(data = monitoring_coords,
             aes(x = longitude, y = latitude, label = station), 
             color = "black",
             fill = "white",
             label.size = 0.2,
             label.padding = unit(0.2, "lines"),
             hjust = 0.5,
             vjust = 0.5,
             size = 2.5,
             alpha = 0.7,
             nudge_y = monitoring_coords$label_direction) +
  theme_minimal() +
  theme(legend.position = c(0.95, 0.05),  # Adjust this to position the legend inside the map
        legend.justification = c(1, 0),    # Anchor the legend to the bottom right
        legend.background = element_rect(fill = "white", colour = "black"), # White background with black border
        legend.title = element_blank(),    # Remove the legend title
        legend.text = element_text(size = 8),  # Smaller text
        legend.key.size = unit(0.5, 'lines'),  # Smaller keys
        legend.spacing = unit(0.2, 'lines'),   # Reduce spacing between legend items
        legend.box.margin = margin(2, 2, 2, 2)) +  # Reduce margin around the legend box
  xlab('') + 
  ylab('') +
  scale_color_manual(values = c("LUR background station" = "firebrick", "Monitoring stations" = "blue"))

monitoring_station_fig



######################################################
### --- BCN monitoring stations, Time series ---- ###
####################################################
bcn_stations <- read.csv("01.Data/BiSC_monitoring_stations/imputed/bcn_ms_bisc_new_imputed.csv")
dplyr::glimpse(bcn_stations)

bcn_stations <- read.csv("01.Data/BiSC_monitoring_stations/imputed/monitoring_stations_imputed.csv")
dplyr::glimpse(bcn_stations)

bcn_stations$date <- as.Date(bcn_stations$date)

library(tidyr)
library(dplyr)

# Assuming your data frame is named 'data'
dplyr::glimpse(bcn_stations)

no2_stations <- bcn_stations %>% 
                dplyr::select(date, 
                              no2_ciutadella:no2_gracia, 
                              no2_palau_reial:no2_hospitalet,
                              no2_jardins:no2_st_adria)

pm25_stations <- bcn_stations %>% 
                 dplyr::select(date, 
                               pm25_gracia:pm25_palau_reial)


dplyr::glimpse(no2_stations)
dplyr::glimpse(pm25_stations)


library(tidyr)
library(dplyr)
library(ggplot2)

# Reshape NO2 data to long format
long_no2 <- pivot_longer(no2_stations, 
                         cols = starts_with("no2_"),
                         names_to = "station",
                         values_to = "no2_level",
                         names_prefix = "no2_")

# Reshape PM25 data to long format
long_pm25 <- pivot_longer(pm25_stations, 
                          cols = starts_with("pm25_"),
                          names_to = "station",
                          values_to = "pm25_level",
                          names_prefix = "pm25_")


# Plot for NO2
ggplot(long_no2, aes(x = date, y = no2_level, color = station)) +
  geom_line() +
  labs(title = "NO2 Concentrations Over Time", x = "Date", y = "NO2 Level") +
  theme_minimal()

# Plot for PM25
ggplot(long_pm25, aes(x = date, y = pm25_level, color = station)) +
  geom_line() +
  labs(title = "PM25 Concentrations Over Time", x = "Date", y = "PM25 Level") +
  theme_minimal()




library(ggplot2)
library(viridis)


# Define the mapping of old names to new names
name_mapping <- c(ciutadella = "Ciutadella", eixample = "Eixample", gracia = "Gracia", 
                  hospitalet = "Hospitalet", palau_reial = "Palau Reial", 
                  poblenou = "Poblenou", sants = "Sants", vall_hebron = "Vall Hebron", ies_goya = "Ies Goya",
                  pl_univ = "Pl. Univ", zona_univ = "Zona Univ", jardins = "Jardins", 
                  sagnier = "Sagnier", st_adria = "St. Adria", st_coloma = "St. Coloma")


# Rename the stations in the datasets
long_no2$station <- name_mapping[long_no2$station]
long_pm25$station <- name_mapping[long_pm25$station]


# Function to create a color palette with firebrick for Palau Reial
create_palette <- function(data, station_column) {
  unique_stations <- unique(data[[station_column]])
  palette <- setNames(viridis::viridis(length(unique_stations)), unique_stations)
  palette["Palau Reial"] <- "firebrick"
  return(palette)
}

# Create color palette for NO2
no2_palette <- create_palette(long_no2, "station")

# Plot for NO2
no2_ts_plot <- ggplot(long_no2, aes(x = date, y = no2_level, color = station)) +
               geom_line() +
               scale_color_manual(values = no2_palette) +
               labs(x = "", 
                     y = expression(NO[2]*" (µg/m" ^ "3" * ")")) + # Adding subscript and units to y-axis
               facet_wrap(.~station) + 
               theme_bw() +
               theme(legend.position = "none")  

no2_ts_plot

# Create color palette for PM25
pm25_palette <- create_palette(long_pm25, "station")

# Plot for PM25
pm25_ts_plot <- ggplot(long_pm25, aes(x = date, y = pm25_level, color = station)) +
                geom_line() +
                scale_color_manual(values = pm25_palette) +
                labs(x = "Date (days)", 
                     y = expression(PM[2.5]*" (µg/m" ^ "3" * ")")) + # Adding subscript and units to y-axis
                facet_wrap(.~station) + 
                theme_bw() +
                theme(legend.position = "none")  

pm25_ts_plot



# combinint monitoring stations map + time series (no2, pm25)
location_ts_stations <- monitoring_station_fig | (no2_ts_plot / pm25_ts_plot)

### --- Export figure --- ### 
ggsave(plot = location_ts_stations , "03.Outputs/figures/location_ts_stations.png",
       dpi = 600, width = 12, height = 10, units = "in")


###############################################
### --- Supplementary materials paper  --- ### 
#############################################

bcn_stations <- read.csv("01.Data/BiSC_monitoring_stations/imputed/monitoring_stations_imputed.csv")
dplyr::glimpse(bcn_stations)

bcn_stations$date <- as.Date(bcn_stations$date)


### --- NO2 monitoring station --- ###
no2_pr_ts <- bcn_stations %>% dplyr::select(date, 
                                            no2_palau_reial) %>% 
             ggplot(aes(x = date, y = no2_palau_reial)) + 
             geom_line(color = "firebrick") + 
             labs(x = "", 
                  y = expression(NO[2]*" (µg/m" ^ "3" * ")")) + 
             theme_bw() + 
             theme(legend.position = "none") +
             facet_wrap(.~ "Palau Reial")

no2_pr_ts

### --- PM25 monitoring station --- ###
pm25_pr_ts <- bcn_stations %>% dplyr::select(date, 
                                            pm25_palau_reial) %>% 
              ggplot(aes(x = date, y = pm25_palau_reial)) + 
              geom_line(color = "firebrick") + 
              labs(x = "", 
                   y = expression(PM[2.5]*" (µg/m" ^ "3" * ")")) + 
              theme_bw() + 
              theme(legend.position = "none") 

pm25_pr_ts

### --- PM25 monitoring station --- ###
bc_pr_ts <- bcn_stations %>% dplyr::select(date, 
                                           bc_palau_reial) %>% 
            ggplot(aes(x = date, y = bc_palau_reial)) + 
            geom_line(color = "firebrick") + 
            labs(x = "", 
                 y = expression(BC*" (µg/m" ^ "3" * ")")) + 
            theme_bw() + 
            theme(legend.position = "none") 

bc_pr_ts


# all moni otring stations
palau_reial_stations <-  no2_pr_ts / pm25_pr_ts / bc_pr_ts
palau_reial_stations

location_ts_stations <- monitoring_station_fig | (no2_pr_ts / pm25_pr_ts / bc_pr_ts)
location_ts_stations

### --- Export figure --- ### 
ggsave(plot = location_ts_stations , "03.Outputs/figures/location_ts_stations.png",
       dpi = 600, width = 12, height = 10, units = "in")




