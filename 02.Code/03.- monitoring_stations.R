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

names(monitoring_coords) <- c()



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
new_names <- c("Ciutadella", "Eixample", "Gracia", "Hospitalet", "Obs Fabra",
                 "Palau Reial", "Poblenou", "Sants", "Vall Hebron", "Ies Goya",
                 "Pl. Univ", "Zona Univ", "Jardins", "Sagnier", "St. Adria",
                 "Sta. Coloma")
  
monitoring_coords$station <- factor(monitoring_coords$station, levels = unique(monitoring_coords$station), labels = new_names)
  

# here we assing the label direction by hand 
label_direction <- c(0.005, -0.005, 0.005, -0.005, 0.005, 
                     0.005, 0.005, -0.005, 0.005, 0.005, 
                     0.005, -0.005, 0.005, 0.005, 0.005, 
                     0.005)

monitoring_coords$label_direction <- label_direction

# Plotting the monitoring stations with the 
ggmap(barcelona_map2) +
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




