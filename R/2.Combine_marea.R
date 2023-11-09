## Identifying the closest in-sample management area to each out-of-sample management area

## We included a random effect of management area in our model
## However, our out-of-sample prediction is for  completely different management areas
## We compensate for this by identifying the closest in-sample management area and assuming
## the random effect is insignificant enough to not hurt prediction.

rm(list = ls())

# Load out-of-sample data
load('GJAMDATA/Withheld For Validation/validation_processed_xydata.RData')

# Add a sample of "out-of-sample" and
# manipulate management area column
xdata_oos <- xdata_oos |>
  dplyr::mutate(type = as.factor('oos'),
                marea = as.factor(marea))

# Load in-sample data
load('GJAMDATA/processed_xydata.RData')

# Add the same column to the in-sample
# and manipulate management area
xdata <- xdata |>
  dplyr::mutate(type = as.factor('is'),
                marea = as.factor(marea))

# map of study region for plotting
states <- sf::st_as_sf(maps::map('state', region = c('indiana', 'illinois'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

## Find approximate center of each management area

# combine in-sample and out-of-sample data
xdata <- rbind(xdata, xdata_oos)

# Find the unique managmeent areas in both
mareas <- as.character(unique(xdata$marea))
# Number of management areas
n_marea <- length(mareas)
# Storage
coords <- matrix(, nrow = n_marea, ncol = 3)

# For each management area
for(i in 1:n_marea){
  # Log coordinates
  coords[i,1] <- mareas[i]
  # Subset  data
  sub <- subset(xdata, marea == mareas[i])
  # Find approximate center using average of lat and long
  coords[i,2] <- mean(sub$lat)
  coords[i,3] <- mean(sub$long)
}

# Formatting
colnames(coords) <- c('marea', 'lat', 'long')
coords <- as.data.frame(coords)
coords$lat <- as.numeric(coords$lat)
coords$long <- as.numeric(coords$long)

# Check to make sure the centers are representative
ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::geom_point(data = xdata, ggplot2::aes(x = long, y = lat)) +
  ggplot2::geom_point(data = coords, ggplot2::aes(x = long, y = lat), color = 'red')

# Find the closest in-sample management area for each out-of-sample management area
in_sample <- unique(dplyr::select(subset(xdata, type == 'is'), 'marea'))
out_sample <- unique(dplyr::select(subset(xdata, type == 'oos'), 'marea'))

# Add coordinates to in-sample and out-of-sample dataframes
in_sample <- in_sample |>
  dplyr::left_join(coords, by = 'marea')
out_sample <- out_sample |>
  dplyr::left_join(coords, by = 'marea')

# Find distances between out-of-sample and in-sample management areas
dists <- fields::rdist(dplyr::select(in_sample, lat, long),
                       dplyr::select(out_sample, lat, long))

# Find the closest in-sample management area for each
# out-of-sample management area
closest_points <- apply(dists, 2, which.min)
closest_marea <- in_sample$marea[closest_points]

# Add to dataframe
out_sample$closest <- closest_marea

# Replace management area in out-of-sample data based on out_sample dataframe
xdata_oos <- xdata_oos |>
  dplyr::mutate(marea = dplyr::if_else(marea == 'IL_Forest1', 'IL_Small3', marea),
                marea = dplyr::if_else(marea == 'IL_River1', 'IL_River2', marea),
                marea = dplyr::if_else(marea == 'IL_Small2', 'IL_Prairie1', marea),
                marea = dplyr::if_else(marea == 'IN_Forest3', 'IN_HoosierSouth', marea),
                marea = dplyr::if_else(marea == 'IN_Forest4', 'IN_Forest1', marea),
                marea = dplyr::if_else(marea == 'IN_Indianapolis', 'IN_Forest1', marea),
                marea = dplyr::if_else(marea == 'IN_Prairie1', 'IL_Prairie1', marea))

# Plot to make sure the new management areas make sense
ggplot2::ggplot() +
  ggplot2::geom_point(data = xdata, ggplot2::aes(x = long, y = lat, color = marea)) +
  ggplot2::geom_point(data = xdata_oos, ggplot2::aes(x = long, y = lat, color = marea)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1)

save(xdata_oos, ydata_oos, file = 'GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea.RData')
