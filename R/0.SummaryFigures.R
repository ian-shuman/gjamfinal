## This script provides visualizations of all variables used in the analysis
## which are included in the supplementary information of Shuman et al. (in prep)

## Authors: I Shuman & AM Willson

rm(list = ls())

library(readr)
library(plyr)
library(RColorBrewer)
library(viridis)
library(stringr)
library(cowplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)

# Map of study region for plotting
states <- ggplot2::map_data('state') |>
  dplyr::filter(region %in% c('indiana', 'illinois'))

### Summary Figures for Y Data

# Load taxon-level ydata
load('GJAMDATA/processed_xydata_2.RData')
# Rename
ydata_all <- ydata
xdata_uncond <- xdata
xdata_uncond$type <- rep("is", nrow(xdata_uncond))
# Load ecosystem-level ydata & xdata
load('GJAMDATA/processed_xydata_2_ecosystem.RData')
# Rename
ydata_eco <- ydata

# Add corner ID to xdata
xdata_uncond_ind <- xdata_uncond |>
  tibble::rownames_to_column(var = 'id')

# Add lat/long to taxon-level data
ydata_all_comb_uncond <- ydata_all |>
  tibble::rownames_to_column(var = 'id') |>
  dplyr::left_join(xdata_uncond_ind, by = 'id') |>
  dplyr::select(c(No.tree:Other.hardwood, lat, long))

# Add lat long to ecosystem-level data
ydata_eco_comb_uncond <- ydata_eco |>
  tibble::rownames_to_column(var = 'id') |>
  dplyr::left_join(xdata_uncond_ind, by = 'id') |>
  dplyr::select(c(Prairie, Savanna, Forest, lat, long))

#Load ydata_eco_comb and ydata_all_comb as from 0.SummaryFigures_OOS.R:
# Load taxon-level yata
load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced.RData')
# Rename
ydata_all <- ydata_oos
# Load ecosystem-level ydata & xdata
load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced_ecosystem.RData')
# Rename
ydata_eco <- ydata_oos

# Add corner ID to xdata
xdata_oos_ind <- xdata_oos |>
  tibble::rownames_to_column(var = 'id')

# Add lat/long to taxon-level data
ydata_all_comb <- ydata_all |>
  tibble::rownames_to_column(var = 'id') |>
  dplyr::left_join(xdata_oos_ind, by = 'id') |>
  dplyr::select(c(Elm:Other.hardwood, lat, long))

# Add lat/long to ecosystem-level data
ydata_eco_comb <- ydata_eco |>
  tibble::rownames_to_column(var = 'id') |>
  dplyr::left_join(xdata_oos_ind, by = 'id') |>
  dplyr::select(c(Prairie, Savanna, Forest, lat, long))


ydata_eco_comb <- rbind(ydata_eco_comb, ydata_eco_comb_uncond)
ydata_all_comb <- rbind(ydata_all_comb, ydata_all_comb_uncond)

xdata_ind <- rbind(xdata_uncond_ind, xdata_oos_ind)
xdata <- rbind(xdata_uncond, xdata_oos)


## Plot

#Plot Ecosystem state


ydata_eco_comb |>
  tidyr::pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Presence') |>
  dplyr::filter(Presence == 1) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = long, y = lat, color = Ecosystem), shape = '.', alpha = 0.7) +
  ggplot2::scale_color_manual(values = c('Prairie' = '#bb5566', 'Savanna' = '#ddaa34', 'Forest' = '#002a53')) +
  ggplot2::labs(color = 'Ecosystem State') +
  ggplot2::guides(color = guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggplot2::ggtitle('Ecosystem distributions')

#Plot Taxa Class
pal <- c('#bb5566',
         '#ddaa34', '#ecd08f',
         '#002a53', '#004488', '#4c7cac', '#8aa9c8', '#c2d2e2', '#dee7f0',
         '#005f5f', '#008b8b', '#38a5a5', '#63b9b9', '#8ecdcd', '#c1e4e4')




ydata_all_comb |>
  tidyr::pivot_longer(1:15, names_to = 'Taxon', values_to = 'Presence') |>
  dplyr::filter(Presence == 1) |>
  dplyr::mutate(Taxon = if_else(Taxon == 'No.tree', 'No tree', Taxon),
         Taxon = if_else(Taxon == 'Black.gum.sweet.gum', 'Black gum/sweet gum', Taxon),
         Taxon = if_else(Taxon == 'Other.conifer', 'Other conifer', Taxon),
         Taxon = if_else(Taxon == 'Other.hardwood', 'Other hardwood', Taxon),
         Taxon = if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/tulip poplar', Taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = long, y = lat, color = Taxon), shape = '.', alpha = 0.7) +
  ggplot2::guides(color = guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::scale_color_manual(values = pal, breaks = c('No tree', 'Oak', 'Hickory',
                                              'Ash', 'Basswood', 'Beech',
                                              'Black gum/sweet gum', 'Dogwood',
                                              'Elm', 'Ironwood', 'Maple', 'Other conifer',
                                              'Other hardwood', 'Poplar/tulip poplar', 
                                              'Walnut')) +
  ggplot2::geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggplot2::ggtitle('Taxon distributions')

## Summary Figures for X Data

# Plot In-Sample v.s. OOS and management areas
#Make management area labels
lat <- c(42.24945, 39.18589, 41.09169, 40.84533, 40.11273, 40.0646, 39.85929, 38.06713, 38.73074, 40.08565, 42.00343, 41.86646, 41.51992, 39.34596, 39.26155, 38.17247, 39.19557, 38.67208, 38.16165, 39.7774, 41.5106, 40.77276)
long <- c(-90.01636, -87.87319, -90.06282, -87.86425, -88.66883, -90.73644, -90.21972, -88.65333, -89.82326, -87.74809, -89.27698, -88.11244, -86.91481, -86.76891, -85.44225, -87.28007, -86.15017, -86.65414, -86.57263, -86.24079, -85.45621, -87.22381)
Area <- c(1:22)
MA_labels <- as.data.frame(cbind(Area, lat, long))
MA_labels <- sf::st_as_sf(MA_labels, coords = c('lat', 'long'))
sf::st_crs(MA_labels) <- 'EPSG:4326'

#Plot Management areas with labels and IS/OOS Coloration
sample <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = long, y = lat, color = type), size = 0.5) +
  ggplot2::geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::geom_text(data = MA_labels, aes(x = long, y = lat, label = Area), color = "black", size = 5, fontface = "bold")+
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  ggplot2::theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = element_text(size = 12),
                 legend.text = element_text(size = 12)) +
  ggplot2::ggtitle('Training and Validation Managment Areas') +
  ggplot2::scale_color_manual(name = "Use", 
                              labels = c("\nTraining \n(In-Sample)\n", "\nValidation \n(Out-of-Sample)\n"), 
                              values = c("blue", "red")) +
  ggplot2::guides(color = guide_legend(override.aes = list(size = 3)))
sample

# Plot Slope
slope <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = long, y = lat, color = Slope), shape = '.') +
  ggplot2::scale_color_gradient(low = 'White', high = 'black', 'Slope (°)', limits = c(0, 50)) +
  ggplot2::geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  ggplot2::theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggplot2::ggtitle('Slope')
slope

# Plot Aspect
aspect <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = long, y = lat, color = direction), shape = '.') +
  ggplot2::labs(color = 'Aspect Direction') +
  ggplot2::guides(color = guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::scale_color_manual(values = c('red', 'yellow', 'darkgreen', 'blue', 'grey')) +
  ggplot2::geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  ggplot2::theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggplot2::ggtitle('Aspect')
aspect

# Plot SWI
swi <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = long, y = lat, color = mean.SWI), shape = '.') +
  viridis::scale_color_viridis(option = 'viridis', direction = -1, str_wrap('SAGA Wetness Index\n', width = 10), limits = c(0, 15)) +
  ggplot2::geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  ggplot2::theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggplot2::ggtitle('SAGA Wetness Index')
swi

#cowplot::plot_grid(slope, aspect, swi, nrow = 2, labels = c("A", "B", "C"))
cowplot::plot_grid(slope, swi, nrow = 1, labels = c("A", "B"))


# Plot CAC
CAC <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = long, y = lat, color = CAC), shape = '.') +
  viridis::scale_color_viridis(option = 'viridis', direction = -1, limits = c(0, 60)) +
  ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::labs(color = expression(paste(CaCO['3'], ' (%)'))) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  ggplot2::ggtitle(expression(bold(CaCO['3'])))+
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12)) 
CAC

# Plot CEC
CEC <- xdata |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = CEC), shape = '.') +
  scale_color_viridis(option = 'viridis', direction = -1, str_wrap('Cation exchange capacity (meq/100g)', width = 14)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Cation exchange capacity')
CEC

# Plot %CLA
cla <- xdata |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = CLA), shape = '.') +
  scale_color_viridis(option = 'viridis', direction = -1, '% clay \n', limits = c(0, 100)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Soil % clay')
cla

# Plot %SAN
san <- xdata |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = SAN), shape = '.') +
  scale_color_viridis(option = 'viridis', direction = -1, '% sand \n', limits = c(0, 100)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Soil % sand')
san

# Plot AWC
AWC <- xdata |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = WAT), shape = '.') +
  scale_color_viridis(option = 'viridis', direction = -1, str_wrap('Available water content (cm/cm)', width = 14), limits = c(0, 80)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Available water content')
AWC

# Plot the presence of Hydric Soils
hydric <- xdata |>
  filter(Hydric == 'Yes') |>
  ggplot() +
  geom_point(aes(x = long, y = lat), shape = '.', color = 'black') +
  scale_fill_manual(values = 'black') +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Presence of hydric soils')
hydric

# Plot the presence of Floodplains
flood <- xdata |>
  filter(Floodplain == 'Yes') |>
  ggplot() +
  geom_point(aes(x = long, y = lat), shape = '.') +
  scale_fill_manual(values = 'black') +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Presence of floodplain')
flood

plot_grid(CAC, CEC, AWC, nrow = 2, labels = c("A", "B", "C"))
plot_grid(cla, san, nrow = 1, labels = c("A", "B"))
plot_grid(hydric, flood, nrow = 1, labels = c("A", "B"))

# Plot Mean Annual Precipitation
precip <- xdata |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = totalPPT), shape = '.') +
  scale_color_distiller(palette = 'Blues', direction = 1, str_wrap('Mean annual precipitation 1895 - 1925 (mm)', width = 14)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Precipitation')
precip

# Plot Mean Annual Temperature
temp <- xdata |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = MeanTEMP), shape = '.') +
  scale_color_viridis(option = 'magma', str_wrap('Mean annual temperature 1895 - 1925 (°C)', width = 14)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Temperature')
temp

plot_grid(precip, temp, nrow = 1, labels = c("A", "B"))


