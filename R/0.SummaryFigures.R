## This script provides visualizations of all variables used in the analysis
## which are included in the supplementary information of Shuman et al. (in prep)

## Authors: I Shuman & AM Willson

rm(list = ls())
library(sf)
library(tibble)
library(dplyr)
library(ggplot2)
library(cowplot)
library(viridis)

# Map of study region for plotting
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

### Summary Figures for Y Data

# Load taxon-level ydata
load('GJAMDATA/processed_xydata_2.RData')
# Rename
ydata_all <- ydata
# Load ecosystem-level ydata & xdata
load('GJAMDATA/processed_xydata_2_ecosystem.RData')
# Rename
ydata_eco <- ydata

# Add corner ID to xdata
xdata_ind <- xdata |>
  tibble::rownames_to_column(var = 'id')

# Make into sf object to convert CRS
xdata_ind2 <- sf::st_as_sf(xdata_ind, coords = c('long', 'lat'))
# Add current CRS
sf::st_crs(xdata_ind2) <- 'EPSG:3175'
# Convert to new CRS
xdata_ind2 <- sf::st_transform(xdata_ind2, crs = 'EPSG:4326')

# Add lat/long to taxon-level data
ydata_all_comb <- ydata_all |>
  tibble::rownames_to_column(var = 'id') |>
  dplyr::left_join(xdata_ind, by = 'id') |>
  dplyr::select(c(No.tree:Other.hardwood, lat, long))

# Add lat long to ecosystem-level data
ydata_eco_comb <- ydata_eco |>
  tibble::rownames_to_column(var = 'id') |>
  dplyr::left_join(xdata_ind, by = 'id') |>
  dplyr::select(c(Prairie, Savanna, Forest, lat, long))

## Plot

#Plot Ecosystem state
ydata_eco_comb |>
  tidyr::pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Presence') |>
  dplyr::filter(Presence == 1) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = Ecosystem), shape = '.', alpha = 0.7) +
  ggplot2::scale_color_manual(values = c('Prairie' = '#bb5566', 'Savanna' = '#ddaa34', 'Forest' = '#002a53')) +
  ggplot2::labs(color = 'Ecosystem State') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Ecosystem distributions')

#Plot Taxa Class
pal <- c('#bb5566',
         '#ddaa34', '#ecd08f',
         '#002a53', '#004488', '#4c7cac', '#8aa9c8', '#c2d2e2', '#dee7f0',
         '#005f5f', '#008b8b', '#38a5a5', '#63b9b9', '#8ecdcd', '#c1e4e4')

ydata_all_comb |>
  tidyr::pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence') |>
  dplyr::filter(Presence == 1) |>
  dplyr::mutate(Taxon = dplyr::if_else(Taxon == 'No.tree', 'No tree', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Black.gum.sweet.gum', 'Black gum/sweet gum', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other conifer', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other hardwood', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/tulip poplar', Taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = long, y = lat, color = Taxon), shape = '.', alpha = 0.7) +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::scale_color_manual(values = pal, breaks = c('No tree', 'Oak', 'Hickory',
                                                       'Ash', 'Basswood', 'Beech',
                                                       'Black gum/sweet gum', 'Dogwood',
                                                       'Elm', 'Ironwood', 'Maple', 'Other conifer',
                                                       'Other hardwood', 'Poplar/tulip poplar', 
                                                       'Walnut')) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Taxon distributions')

## Summary Figures for X Data

# Plot Slope
slope <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = Slope), shape = '.') +
  ggplot2::scale_color_gradient(low = 'lightgrey', high = 'black', 'Slope (°)') +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Slope')
slope

# Plot Aspect
aspect <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = direction), shape = '.') +
  ggplot2::labs(color = 'Aspect Direction') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::scale_color_manual(values = c('red', 'yellow', 'darkgreen', 'blue', 'grey'),
                              breaks = c('E', 'N', 'S', 'W', 'NS')) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Aspect')
aspect

# Plot SWI
swi <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = mean.SWI), shape = '.') +
  viridis::scale_color_viridis(option = 'viridis', direction = -1, stringr::str_wrap('SAGA Wetness Index', width = 10)) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('SAGA Wetness Index')
swi

cowplot::plot_grid(slope, aspect, swi, nrow = 2, labels = c("A", "B", "C"))

# Plot CAC
CAC <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = CAC), shape = '.') +
  viridis::scale_color_viridis(option = 'viridis', direction = -1) +
  ggplot2::labs(color = expression(paste(CaCO['3'], ' (%)'))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle(expression(CaCO['3']))
CAC

# Plot CEC
CEC <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = CEC), shape = '.') +
  viridis::scale_color_viridis(option = 'viridis', direction = -1, stringr::str_wrap('Cation exchange capacity (meq/100g)', width = 14)) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Cation exchange capacity')
CEC

# Plot %CLA
cla <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = CLA), shape = '.') +
  viridis::scale_color_viridis(option = 'viridis', direction = -1, '% clay', limits = c(0, 100)) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Soil % clay')
cla

# Plot %SAN
san <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = SAN), shape = '.') +
  viridis::scale_color_viridis(option = 'viridis', direction = -1, '% sand', limits = c(0, 100)) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Soil % sand')
san

# Plot AWC
AWC <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = WAT), shape = '.') +
  viridis::scale_color_viridis(option = 'viridis', direction = -1, stringr::str_wrap('Available water content (cm/cm)', width = 14)) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Available water content')
AWC

# Plot the presence of Hydric Soils
hydric <- xdata |>
  dplyr::filter(Hydric == 'Yes') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat), shape = '.', color = 'black') +
  ggplot2::scale_fill_manual(values = 'black') +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Presence of hydric soils')
hydric

# Plot the presence of Floodplains
flood <- xdata |>
  dplyr::filter(Floodplain == 'Yes') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat), shape = '.') +
  ggplot2::scale_fill_manual(values = 'black') +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Presence of floodplain')
flood

cowplot::plot_grid(CAC, CEC, AWC, nrow = 2, labels = c("A", "B", "C"))
cowplot::plot_grid(cla, san, nrow = 1, labels = c("A", "B"))
cowplot::plot_grid(hydric, flood, nrow = 1, labels = c("A", "B"))

# Plot Mean Annual Precipitation
precip <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = totalPPT), shape = '.') +
  ggplot2::scale_color_distiller(palette = 'Blues', direction = 1, stringr::str_wrap('Mean annual precipitation 1895 - 1925 (mm)', width = 14)) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Precipitation')
precip

# Plot Mean Annual Temperature
temp <- xdata |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = MeanTEMP), shape = '.') +
  viridis::scale_color_viridis(option = 'magma', stringr::str_wrap('Mean annual temperature 1895 - 1925 (°C)', width = 14)) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Temperature')
temp

cowplot::plot_grid(precip, temp, nrow = 1, labels = c("A", "B"))
