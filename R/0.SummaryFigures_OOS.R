## Script for visualizing out-of-sample regions
## These plots can be compared with the prediction derived from GJAM

## Author: AM Willson & I Shuman

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

# Map of the study region for plotting
states <- map_data('state') |>
  dplyr::filter(region %in% c('indiana', 'illinois'))

### Summary Figures for out-of-sample Y data

# Load taxon-level yata
load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced.RData')
# Rename
ydata_all <- ydata_oos
# Load ecosystem-level ydata & xdata
load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced_ecosystem.RData')
# Rename
ydata_eco <- ydata_oos

# Add corner ID to xdata
xdata_ind <- xdata_oos |>
  rownames_to_column(var = 'id')

# Add lat/long to taxon-level data
ydata_all_comb <- ydata_all |>
  rownames_to_column(var = 'id') |>
  left_join(xdata_ind, by = 'id') |>
  select(c(Elm:Other.hardwood, lat, long))

# Add lat/long to ecosystem-level data
ydata_eco_comb <- ydata_eco |>
  rownames_to_column(var = 'id') |>
  left_join(xdata_ind, by = 'id') |>
  select(c(Prairie, Savanna, Forest, lat, long))

## Plot

# Plot ecosystem state
ydata_eco_comb |>
  pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Presence') |>
  filter(Presence == 1) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = Ecosystem), shape = '.', alpha = 0.7) +
  scale_color_manual(values = c('Prairie' = '#bb5566', 'Savanna' = '#ddaa34', 'Forest' = '#002a53')) +
  labs(color = 'Ecosystem State') +
  guides(color = guide_legend(override.aes = list(shape = 16, size = 7))) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Ecosystem distributions')

# Plot taxon class
pal <- c('#bb5566',
         '#ddaa34', '#ecd08f',
         '#002a53', '#004488', '#4c7cac', '#8aa9c8', '#c2d2e2', '#dee7f0',
         '#005f5f', '#008b8b', '#38a5a5', '#63b9b9', '#8ecdcd', '#c1e4e4')

ydata_all_comb |>
  pivot_longer(Elm:Other.hardwood, names_to = 'Taxon', values_to = 'Presence') |>
  filter(Presence == 1) |>
  mutate(Taxon = if_else(Taxon == 'No.tree', 'No tree', Taxon),
         Taxon = if_else(Taxon == 'Black.gum.sweet.gum', 'Black gum/sweet gum', Taxon),
         Taxon = if_else(Taxon == 'Other.conifer', 'Other conifer', Taxon),
         Taxon = if_else(Taxon == 'Other.hardwood', 'Other hardwood', Taxon),
         Taxon = if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/tulip poplar', Taxon)) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = Taxon), shape = '.', alpha = 0.7) +
  guides(color = guide_legend(override.aes = list(shape = 16, size = 7))) +
  scale_color_manual(values = pal, breaks = c('No tree', 'Oak', 'Hickory',
                                              'Ash', 'Basswood', 'Beech',
                                              'Black gum/sweet gum', 'Dogwood',
                                              'Elm', 'Ironwood', 'Maple', 'Other conifer',
                                              'Other hardwood', 'Poplar/tulip poplar',
                                              'Walnut')) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Taxon distributions')

## Summary figures for x data

# Plot slope
slope <- xdata_oos |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = Slope), shape = '.') +
  scale_color_gradient(low = 'White', high = 'black', 'Slope (°)') +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Slope')
slope

# Plot aspect
aspect <- xdata_oos |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = direction), shape = '.') +
  labs(color = 'Aspect Direction') +
  guides(color = guide_legend(override.aes = list(shape = 16, size = 7))) +
  scale_color_manual(values = c('red', 'yellow', 'darkgreen', 'blue', 'grey')) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Aspect')
aspect

# Plot SWI
swi <- xdata_oos |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = mean.SWI), shape = '.') +
  scale_color_viridis(option = 'viridis', direction = -1, str_wrap('SAGA Wetness Index', width = 10)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('SAGA Wetness Index')
swi

plot_grid(slope, aspect, swi, nrow = 2, labels = c('A', 'B', 'C'),
          rel_widths = c(0.46, 0.54))

# Plot CAC
CAC <- xdata_oos |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = CAC), shape = '.') +
  scale_color_viridis(option = 'viridis', direction = -1) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  labs(color = expression(paste(CaCO['3'], ' (%)'))) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle(expression(CaCO['3']))
CAC

# Plot CEC
CEC <- xdata_oos |>
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
cla <- xdata_oos |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = CLA), shape = '.') +
  scale_color_viridis(option = 'viridis', direction = -1, '% clay', limits = c(0, 100)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Soil % clay')
cla

# Plot %SAN
san <- xdata_oos |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = SAN), shape = '.') +
  scale_color_viridis(option = 'viridis', direction = -1, '% sand', limits = c(0, 100)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Soil % sand')
san

# Plot AWC
AWC <- xdata_oos |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = WAT), shape = '.') +
  scale_color_viridis(option = 'viridis', direction = -1, str_wrap('Available water content (cm/cm)', width = 14)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle('Available water content')
AWC

# Plot the presence of hydric soils
hydric <- xdata_oos |>
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

# Plot the presence of floodplains
flood <- xdata_oos |>
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

plot_grid(CAC, CEC, AWC, nrow = 2, labels = c('A', 'B', 'C'))
plot_grid(cla, san, nrow = 1, labels = c('A', 'B'))
plot_grid(hydric, flood, nrow = 1, labels = c('A', 'B'))

# Plot mean annual precipitation
precip <- xdata_oos |>
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

# Plot mean annual temperature
temp <- xdata_oos |>
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

plot_grid(precip, temp, nrow = 1, labels = c('A', 'B'))
