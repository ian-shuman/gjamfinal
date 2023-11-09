## Out-of-sample prediction for validation
## Can be run for either all taxa or ecosystem level

## Author: AM Willson

rm(list = ls())

# Load output from GJAM
#load('out/All_taxa~all_cov_ASPECT/all_taxa-all_cov_ASPECT_1.RData')
load('out/All_taxa~all_cov_NOASPECT/all_taxa-all_cov_NOASPECT_1.RData')
#load('out/Reduced_taxa~all_cov_ASPECT/reduced_taxa-all_cov_ASPECT_1.RData')
#load('out/Reduced_taxa~all_cov_NOASPECT/reduced_taxa-all_cov_NOASPECT_1.RData')

# Clean up environment to make sure we are predicting correct data
rm(edata, mlist, xdata, ydata, site_effort, form1, nburn, niter)

# Load out of sample data
load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced.RData')
#load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced_ecosystem.RData')

# Specify whether all taxa or reduced taxa
type <- 'all'

if(!exists('xdata_oos')) xdata_oos <- xdata
if(!exists('ydata_oos')) ydata_oos <- ydata

# Modify xdata for use with gjamPredict
xdata <- xdata_oos |>
  dplyr::mutate(Aspect = 1,
                direction = as.factor(direction))

rm(xdata_oos)

# Specify data list
newdata <- list(xdata = xdata, nsim = 1000)

# Make prediction
pred <- gjam::gjamPredict(output = out, newdata = newdata)

# Extract predictions
pred_yMu <- as.data.frame(pred$sdList$yMu)
pred_pr <- as.data.frame(pred$prPresent)

# Map of study region
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

## yMu = predicted y (presence/absence but for some reason on a continuous scale)
if(type == 'all'){
  # Add the coordinates from the xdata
  pred_yMu |>
    dplyr::mutate(lat = xdata$lat,
                  lon = xdata$long) |>
    # Make easier to plot
    tidyr::pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence') |>
    # Make taxon names easier to read
    dplyr::mutate(Taxon = dplyr::if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'No.tree', 'No Tree', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon)) |>
    # Plot presence over space
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Presence)) +
    ggplot2::facet_wrap(~Taxon, nrow = 3, ncol = 5) +
    ggplot2::scale_color_viridis_c(option = 'A') +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                   legend.title = ggplot2::element_text(size = 14),
                   legend.text = ggplot2::element_text(size = 12))
}
# Same procedure if reduced
if(type == 'reduced'){
  pred_yMu |>
    dplyr::mutate(lat = xdata$lat,
                  lon = xdata$long) |>
    tidyr::pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Presence') |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Presence)) +
    ggplot2::facet_wrap(~Ecosystem) +
    ggplot2::scale_color_viridis_c(option = 'A') +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                   legend.title = ggplot2::element_text(size = 14),
                   legend.text = ggplot2::element_text(size = 12))
}

# Do the same with the probability of presence
## This actually represents probability of presence at a given location
if(type == 'all'){
  pred_pr |>
    dplyr::mutate(lat = xdata$lat,
                  lon = xdata$long) |>
    tidyr::pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Probability') |>
    dplyr::mutate(Taxon = dplyr::if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'No.tree', 'No Tree', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon)) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Probability)) +
    ggplot2::facet_wrap(~Taxon, nrow = 3, ncol = 5) +
    ggplot2::scale_color_viridis_c(option = 'A') +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                   legend.title = ggplot2::element_text(size = 14),
                   legend.text = ggplot2::element_text(size = 12))
}
if(type == 'reduced'){
  pred_pr |>
    dplyr::mutate(lat = xdata$lat,
                  lon = xdata$long) |>
    tidyr::pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Probability') |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Probability)) +
    ggplot2::facet_wrap(~Ecosystem) +
    ggplot2::scale_color_viridis_c(option = 'A') +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                   legend.title = ggplot2::element_text(size = 14),
                   legend.text = ggplot2::element_text(size = 12))
}

# Now compare with observation
if(type == 'all'){
  pred_yMu_long <- pred_yMu |>
    # Add index of each corner in each management area from ydata
    dplyr::mutate(Index = rownames(ydata_oos)) |>
    tidyr::pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
  # Do the same for ydata
  ydata_oos_long <- ydata_oos |>
    tibble::rownames_to_column(var = 'Index') |>
    tidyr::pivot_longer(Elm:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
}
if(type == 'reduced'){
  pred_yMu_long <- pred_yMu |>
    dplyr::mutate(Index = rownames(ydata_oos)) |>
    tidyr::pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Presence')
  ydata_oos_long <- ydata_oos |>
    tibble::rownames_to_column(var = 'Index') |>
    tidyr::pivot_longer(Prairie:Savanna, names_to = 'Ecosystem', values_to = 'Presence')
}

if(type == 'all'){
  # Join predicted and observed for predicted presence
  comp_1 <- pred_yMu_long |>
    dplyr::full_join(ydata_oos_long, by = c('Index', 'Taxon'))
  colnames(comp_1) <- c('Index', 'Taxon', 'Predicted', 'Observed')
}
if(type == 'reduced'){
  comp_1 <- pred_yMu_long |>
    dplyr::full_join(ydata_oos_long, by = c('Index', 'Ecosystem'))
  colnames(comp_1) <- c('Index', 'Ecosystem', 'Predicted', 'Observed')
}

if(type == 'all'){
  # do the same for predicted probability of presence
  pred_pr_long <- pred_pr |>
    dplyr::mutate(Index = rownames(ydata_oos)) |>
    tidyr::pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Probability')

  comp <- pred_pr_long |>
    dplyr::full_join(comp_1, by = c('Index', 'Taxon'))
}

if(type == 'reduced'){
  pred_pr_long <- pred_pr |>
    dplyr::mutate(Index = rownames(ydata_oos)) |>
    tidyr::pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Probability')
  
  comp <- pred_pr_long |>
    dplyr::full_join(comp_1, by = c('Index', 'Ecosystem'))
}

if(type == 'all'){
  comp |>
    dplyr::mutate(Taxon = dplyr::if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'No.tree', 'No Tree', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon)) |>
    ggplot2::ggplot(ggplot2::aes(x = Probability, y = Observed)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~Taxon, scales = 'free', nrow = 3, ncol = 5) +
    ggplot2::geom_smooth(method = 'glm', method.args = list(family = 'binomial'), 
                         color = 'maroon', fill = 'maroon') +
    ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                   axis.title = ggplot2::element_text(size = 14),
                   axis.text = ggplot2::element_text(size = 12))
}

if(type == 'reduced'){
  comp |>
    ggplot2::ggplot(ggplot2::aes(x = Probability, y = Observed)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~Ecosystem, scales = 'free') +
    ggplot2::geom_smooth(method = 'glm', method.args = list(family = 'binomial'),
                         color = 'maroon', fill = 'maroon') +
    ggplot2::theme_minimal() +
    ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                   axis.title = ggplot2::element_text(size = 14),
                   axis.text = ggplot2::element_text(size = 12))
}

# Overall model
form <- stats::glm(Observed ~ Probability, family = binomial, data = comp)
summary(form)
# Overall R2
with(summary(form), 1 - deviance/null.deviance)

# Overall with random effect
if(type == 'all'){
  comp$Taxon <- as.factor(comp$Taxon)
  form_rand <- lme4::glmer(Observed ~ Probability + (1|Taxon), family = binomial, data = comp)
}
if(type == 'reduced'){
  comp$Ecosystem <- as.factor(comp$Ecosystem)
  form_rand <- lme4::glmer(Observed ~ Probability + (1|Ecosystem), family = binomial, data = comp)
}

summary(form_rand)
piecewiseSEM::rsquared(form_rand)

# Individual models
if(type == 'all'){
  comp_notree <- comp |>
    dplyr::filter(Taxon == 'No.tree')
  comp_oak <- comp |>
    dplyr::filter(Taxon == 'Oak')
  comp_elm <- comp |>
    dplyr::filter(Taxon == 'Elm')
  comp_hickory <- comp |>
    dplyr::filter(Taxon == 'Hickory')
  comp_ash <- comp |>
    dplyr::filter(Taxon == 'Ash')
  comp_maple <- comp |>
    dplyr::filter(Taxon == 'Maple')
  comp_basswood <- comp |>
    dplyr::filter(Taxon == 'Basswood')
  comp_walnut <- comp |>
    dplyr::filter(Taxon == 'Walnut')
  comp_ironwood <- comp |>
    dplyr::filter(Taxon == 'Ironwood')
  comp_beech <- comp |>
    dplyr::filter(Taxon == 'Beech')
  comp_dogwood <- comp |>
    dplyr::filter(Taxon == 'Dogwood')
  comp_poplar <- comp |>
    dplyr::filter(Taxon == 'Poplar.tulip.poplar')
  comp_gum <- comp |>
    dplyr::filter(Taxon == 'Black.gum.sweet.gum')
  comp_otherconifer <- comp |>
    dplyr::filter(Taxon == 'Other.conifer')
  comp_otherhardwood <- comp |>
    dplyr::filter(Taxon == 'Other.hardwood')
}

if(type == 'reduced'){
  comp_prairie <- comp |>
    dplyr::filter(Ecosystem == 'Prairie')
  comp_savanna <- comp |>
    dplyr::filter(Ecosystem == 'Savanna')
  comp_forest <- comp |>
    dplyr::filter(Ecosystem == 'Forest')
}

if(type == 'all'){
  form_notree <- stats::glm(Observed ~ Probability, family = binomial, data = comp_notree)
  print(paste('no tree:', with(summary(form_notree), 1 - deviance/null.deviance)))

  form_oak <- stats::glm(Observed~Probability, family = binomial, data = comp_oak)
  print(paste('oak:', with(summary(form_oak), 1 - deviance/null.deviance)))

  form_elm <- stats::glm(Observed ~ Probability, family = binomial, data = comp_elm)
  print(paste('elm:', with(summary(form_elm), 1 - deviance/null.deviance)))

  form_hickory <- stats::glm(Observed ~ Probability, family = binomial, data = comp_hickory)
  print(paste('hickory:',with(summary(form_hickory), 1 - deviance/null.deviance)))

  form_ash <- stats::glm(Observed ~ Probability, family = binomial, data = comp_ash)
  print(paste('ash:', with(summary(form_ash), 1 - deviance/null.deviance)))

  form_maple <- stats::glm(Observed ~ Probability, family = binomial, data = comp_maple)
  print(paste('maple:', with(summary(form_maple), 1 - deviance/null.deviance)))

  form_basswood <- stats::glm(Observed ~ Probability, family = binomial, data = comp_basswood)
  print(paste('basswood:', with(summary(form_basswood), 1 - deviance/null.deviance)))
  
  form_beech <- stats::glm(Observed ~ Probability, family = binomial, data = comp_beech)
  print(paste('beech:', with(summary(form_beech), 1 - deviance/null.deviance)))
  
  form_bgsg <- stats:::glm(Observed ~ Probability, family = binomial, data = comp_gum)
  print(paste('black gum/sweet gum:', with(summary(form_bgsg), 1 - deviance/null.deviance)))
  
  form_dogwood <- stats::glm(Observed ~ Probability, family = binomial, data = comp_dogwood)
  print(paste('dogwood:', with(summary(form_dogwood), 1 - deviance/null.deviance)))
  
  form_ironwood <- stats::glm(Observed ~ Probability, family = binomial, data = comp_ironwood)
  print(paste('ironwood:', with(summary(form_ironwood), 1 - deviance/null.deviance)))
  
  form_conifer <- stats::glm(Observed ~ Probability, family = binomial, data = comp_otherconifer)
  print(paste('other conifer:', with(summary(form_conifer), 1 - deviance/null.deviance)))
  
  form_hardwood <- stats::glm(Observed ~ Probability, family = binomial, data = comp_otherhardwood)
  print(paste('other hardwood:', with(summary(form_hardwood), 1 - deviance/null.deviance)))
  
  form_ptp <- stats::glm(Observed ~ Probability, family = binomial, data = comp_poplar)
  print(paste('tulip/tulip poplar:', with(summary(form_ptp), 1 - deviance/null.deviance)))
}

if(type == 'reduced'){
  form_prairie <- stats::glm(Observed ~ Probability, family = binomial, data = comp_prairie)
  print(paste('prairie:', with(summary(form_prairie), 1 - deviance/null.deviance)))
  
  form_savanna <- stats::glm(Observed ~ Probability, family = binomial, data = comp_savanna)
  print(paste('savanna:', with(summary(form_savanna), 1 - deviance/null.deviance)))
  
  form_forest <- stats::glm(Observed ~ Probability, family = binomial, data = comp_forest)
  print(paste('forest:', with(summary(form_forest), 1 - deviance/null.deviance)))
}

# Difference between probability of presence and observed presence/absence over space
if(type == 'all'){
  xdata <- xdata |>
    tibble::rownames_to_column(var = 'Index')
  comp <- comp |>
    dplyr::left_join(xdata, by = 'Index') |>
    dplyr::select(c('Taxon', 'Probability', 'Predicted', 'Observed', 'lat', 'long'))
  
  # Color interpretation:
  # near 0 (white) = probability nearly matches observation
  # near 1 (red) = high probability of presence when absent
  # near -1 (blue) = high probability of absence when present
  comp |>
    dplyr::mutate(difference = Observed - Probability) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(alpha = 0.7, ggplot2::aes(x = long, y = lat, color = difference)) +
    ggplot2::facet_wrap(~Taxon) +
    ggplot2::scale_color_distiller(palette = 'RdBu', limits = c(-1, 1)) +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 12))
}

if(type == 'reduced'){
  xdata <- xdata |>
    tibble::rownames_to_column(var = 'Index')
  comp <- comp |>
    dplyr::left_join(xdata, by = 'Index') |>
    dplyr::select(c('Ecosystem', 'Probability', 'Predicted', 'Observed', 'lat', 'long'))
  
  # Color interpretation:
  # near 0 (white) = probability nearly matches observation
  # near 1 (red)= high probability of presence when absent
  # near -1 (blue) = high probability of absence when present
  comp |>
    dplyr::mutate(difference = Observed - Probability) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(alpha = 0.7, ggplot2::aes(x = long, y = lat, color = difference)) +
    ggplot2::facet_wrap(~Ecosystem) +
    ggplot2::scale_color_distiller(palette = 'RdBu', limits = c(-1, 1)) +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 12))
}
