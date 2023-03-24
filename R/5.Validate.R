## Validation

## Author: AM Willson

rm(list = ls())

library(gjam)

# Load output from GJAM
load('out/all_taxa-all_cov/all_taxa-all_cov_1.RData')

# Check to make sure convergence looks okay since we only have 1 chain
gjamPlot(out)

# Looks good

# Clean up environment to make sure we are predicting correct data
rm(edata, elist, mlist, xdata, ydata, site_effort)

# Load out of sample data
load('GJAM DATA/Withheld For Validation/validation_process2.RData')

# Where there are NA's in the aspect covariate, make it a random value
# within the range of the data
# I'm doing this because gjamPredict() doesn't tolerate NA and since we 
# don't have any information, just make it random
nas <- length(which(is.na(xdata[,4])))
rands <- runif(n = nas, min = min(xdata[,4], na.rm = T), max = max(xdata[,4], na.rm = T))
xdata[is.na(xdata)] <- rands

# Specify data list
newdata <- list(xdata = xdata, nsim = 1000, ematrix = edata)

# Make prediction
pred <- gjamPredict(output = out, newdata = newdata)

# Extract predictions
pred_yMu <- as.data.frame(pred$sdList$yMu)
pred_wMu <- as.data.frame(pred$sdList$wMu)
pred_yPe <- as.data.frame(pred$sdList$yPe)
pred_wSe <- as.data.frame(pred$sdList$wSe)

# Let's plot to see what we're working with
states <- map_data('state') |>
  filter(region %in% c('indiana', 'illinois'))

pred_yMu |>
  mutate(lat = xdata$lat,
         lon = xdata$long) |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence') |>
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = 'white', color = 'black') +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  geom_point(aes(x = lon, y = lat, color = Presence)) +
  facet_wrap(~Taxon) +
  scale_color_viridis_c(option = 'A')

pred_wMu |>
  mutate(lat = xdata$lat,
         lon = xdata$long) |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence') |>
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = 'white', color = 'black') +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  geom_point(aes(x = lon, y = lat, color = Presence)) +
  facet_wrap(~Taxon) +
  scale_color_viridis_c(option = 'A')

# Do the same with the probability of presence
pred_pr <- as.data.frame(pred$prPresent)

pred_pr |>
  mutate(lat = xdata$lat,
         lon = xdata$long) |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Probability') |>
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = 'white', color = 'black') +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  geom_point(aes(x = lon, y = lat, color = Probability)) +
  facet_wrap(~Taxon) +
  scale_color_viridis_c(option = 'A')

# Now compare with observation
pred_yMu <- pred_yMu |>
  mutate(Index = rownames(ydata)) |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
ydata <- ydata |>
  rownames_to_column(var = 'Index') |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')

comp <- pred_yMu |>
  full_join(ydata, by = c('Index', 'Taxon'))
colnames(comp) <- c('Index', 'Taxon', 'Predicted', 'Observed')

comp |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point() +
  xlim(c(0, 1)) + ylim(c(0, 1)) +
  facet_wrap(~Taxon)
