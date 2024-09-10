## Out-of-sample prediction for validation
## Can be run for either all taxa or ecosystem level

## Author: AM Willson & I Shuman

#getAnywhere(".gjamPrediction") for source code on gjamPredict
rm(list = ls())

library(gjam)
library(lme4)
library(piecewiseSEM)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(pROC)

# Load output from GJAM
#load('out/All_taxa~all_cov_ASPECT/all_taxa-all_cov_ASPECT_1.RData')
#load('out/All_taxa~all_cov_NOASPECT/all_taxa-all_cov_NOASPECT_1.RData') #Use this one for taxon-level final results
#load('out/Reduced_taxa~all_cov_ASPECT/reduced_taxa-all_cov_ASPECT_1.RData')
load('out/Reduced_taxa~all_cov_NOASPECT/reduced_taxa-all_cov_NOASPECT_1.RData') #Use this one for ecosystem-level final results

# Clean up environment to make sure we are predicting correct data
rm(edata, mlist, xdata, ydata, site_effort, form1, nburn, niter)

# Load out of sample data
#load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced.RData') #Use for taxon level
load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced_ecosystem.RData') #Use for ecosystem level

# Specify whether all taxa or reduced taxa
type <- 'reduced'

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
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Presence), shape = ".") +
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
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Presence), shape = ".") +
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

#Plot probability of presence for all regions 
if(type == 'all'){
  OOS_uncond_all = pred_pr |>
    dplyr::mutate(lat = xdata$lat,
           lon = xdata$long) |>
    tidyr::pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Probability') |>
    dplyr::mutate(Taxon = dplyr::if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
           Taxon = dplyr::if_else(Taxon == 'No.tree', 'No Tree', Taxon),
           Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
           Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
           Taxon = dplyr::if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon)) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Probability), shape = ".") +
    ggplot2::facet_wrap(~Taxon, nrow = 3, ncol = 5) +
    ggplot2::scale_color_viridis_c(option = 'A') +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
          legend.title = ggplot2::element_text(size = 14),
          legend.text = ggplot2::element_text(size = 12))
  OOS_uncond_all
}


if(type == 'reduced'){
  OOS_uncond_reduced = pred_pr |>
    dplyr::mutate(lat = xdata$lat,
           lon = xdata$long) |>
    tidyr::pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Probability') |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Probability), shape = ".") +
    ggplot2::facet_wrap(~factor(Ecosystem, levels = c("Prairie", "Savanna", "Forest"))) +
    ggplot2::scale_color_viridis_c(option = 'A') +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
          legend.title = ggplot2::element_text(size = 14),
          legend.text = ggplot2::element_text(size = 12))
  OOS_uncond_reduced
}

#Save for multi-panel plotting in 8.Predict_OOS_conditional.R
if(type == 'reduced'){
  save(OOS_uncond_reduced, file = "out/OOS_uncond_reduced.RData")
}
if(type == 'all'){
  save(OOS_uncond_all, file = "out/OOS_uncond_all.RData")
}

#Plot specific management areas and taxa for use in Figure 5.B-E and Supplement
#Areas highlighted in main text for AOI = "IL_River1" and "IN_Forest4"
#Aesthetics of specific management areas may vary (see below for recommendations)

#Set user parameters
AOI <- "IN_Forest3" #Some management area of interest from the following list: c("IL_Forest1", "IL_River1", "IL_Small2", "IN_Forest3", "IN_Forest4", "IN_Indianapolis", "IN_Prairie1")
if(type == 'all'){
  taxa <- c("No.tree", "Oak") #Some taxa of interest from the following list: c("No.tree", "Oak", "Elm", "Hickory", "Ash", "Maple", "Basswood", "Walnut", "Ironwood", "Beech", "Dogwood", "Poplar.tulip.poplar", "Black.gum.sweet.gum", "Other.conifer", "Other.hardwood")
}

#Convert taxa user parameter into column numbers
convert_to_col_numbers <- function(char_vector, data_frame) {
  col_names <- colnames(data_frame)
  col_numbers <- match(char_vector, col_names)
  return(col_numbers)
}
TOI <- convert_to_col_numbers(taxa, pred_pr)

#Plot with specified management areas/taxa
if(type == 'reduced'){
  pred_pr_zoomed <- dplyr::filter(pred_pr, grepl(paste0(AOI), rownames(pred_pr)) == TRUE)
  xdata_zoomed <- dplyr::filter(xdata, grepl(paste0(AOI), rownames(pred_pr)) == TRUE)
  pred_pr_zoomed |>
    dplyr::mutate(lat = xdata_zoomed$lat,
           lon = xdata_zoomed$long) |>
    tidyr::pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Probability') |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Probability), size = 0.9) +
    ggplot2::facet_wrap(~factor(Ecosystem, levels = c("Prairie", "Savanna", "Forest"))) +
    ggplot2::scale_color_viridis_c(option = 'A') +
    ggplot2::theme_void() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
          legend.title = ggplot2::element_text(size = 14),
          legend.text = ggplot2::element_text(size = 12))
}



if(type == 'all'){
  pred_pr_zoomed <- dplyr::filter(pred_pr, grepl(paste0(AOI), rownames(pred_pr)) == TRUE)
  xdata_zoomed <- dplyr::filter(xdata, grepl(paste0(AOI), rownames(xdata)) == TRUE)
  pred_pr_zoomed <- pred_pr_zoomed |>
    dplyr::mutate(lat = xdata_zoomed$lat,
           lon = xdata_zoomed$long) |>
    tidyr::pivot_longer(TOI, names_to = 'Taxon', values_to = 'Probability') |>
    dplyr::mutate(Taxon = dplyr::if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'No.tree', 'No Tree', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
                  Taxon = dplyr::if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon))
    max_prob <- max(pred_pr_zoomed$Probability)
  pred_pr_zoomed |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, color = Probability), size = 0.5) +
    ggplot2::coord_fixed()+
    ggplot2::facet_wrap(~Taxon, nrow = 2, ncol = 2) +
    ggplot2::scale_color_viridis_c(option = 'A', limits = c(0, max_prob)) + 
    ggplot2::theme_void() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
          legend.title = ggplot2::element_text(size = 14),
          legend.text = ggplot2::element_text(size = 12))
}
#Aesthetic recommendations for plotting different management areas of interest: 
#For IL_River1, use "Oak" and "No.tree" taxa with geom_point(size = 0.5) for visualization
#For IN_Forest4, use "Oak" and "Beech" taxa with geom_point(size = 1.4) for visualization

# Now compare with observation (for all management areas and taxa)
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
  comp
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

#Calculate OvR AUC Score for Ecosystem Level (does not make sense for Taxon level, as multiple taxa can be present at a single point)
if(type == 'reduced'){
  Observed <- comp$Ecosystem[comp$Observed == 1]
  AUC_df <- cbind(pred_pr, Observed)
  AUC_object <- pROC::multiclass.roc(AUC_df$Observed, AUC_df[,1:3])
  AUC_object
  #AUC of reduced conditional is 0.8669
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
  
  form_bgsg <- stats::glm(Observed ~ Probability, family = binomial, data = comp_gum)
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
  # near 0 (white) = probability nearly matches observation (true positive/negative)
  # near 1 (red) = low probability of presence when present (false negative)
  # near -1 (blue) = high probability of presence when absent (false positive)
  comp |>
    dplyr::mutate(difference = Observed - Probability) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = difference), alpha = 0.7) +
    ggplot2::facet_wrap(~Taxon, nrow = 3, ncol = 5) +
    ggplot2::scale_color_distiller(palette = 'RdBu', limits = c(-1, 1)) +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
          legend.title = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 12))
}
compgraph <- if(type == 'reduced'){
  xdata <- xdata |>
    tibble::rownames_to_column(var = 'Index')
  comp <- comp |>
    dplyr::left_join(xdata, by = 'Index') |>
    dplyr::select(c('Ecosystem', 'Probability', 'Predicted', 'Observed', 'lat', 'long'))
  
  # Color interpretation:
  # near 0 (white) = probability nearly matches observation (true positive/negative)
  # near 1 (red) = low probability of presence when present (false negative)
  # near -1 (blue) = high probability of presence when absent (false positive)
  comp |>
    dplyr::mutate(difference = Observed - Probability) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = long, y = lat, color = difference), alpha = 0.7, shape = ".") +
    ggplot2::facet_wrap(~Ecosystem) +
    ggplot2::scale_color_distiller(palette = 'RdBu', limits = c(-1, 1)) +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
    ggplot2::coord_sf(crs = 'EPSG:4326') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 12))
}
compgraph

### Save for use in 9.Visualize_OOS.R to calculate the number of cells correctly/incorrectly predicted by the OOS experiments
comp_envi <- comp
comp_envi$difference <- comp_envi$Observed - comp_envi$Probability

if(type == 'all'){
  saveRDS(comp_envi, file = "out/comp_envi_all.rds")
}
if(type == 'reduced'){
  saveRDS(comp_envi, file = "out/comp_envi_reduced.rds")
}


