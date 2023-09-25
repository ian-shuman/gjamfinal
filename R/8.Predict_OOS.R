## Out-of-sample prediction for validation
## Can be run for either all taxa or ecosystem level

## Author: AM Willson

rm(list = ls())

library(gjam)
library(lme4)
library(piecewiseSEM)
library(dplyr)

# Load output from GJAM
load('out/FINAL_RUNS/All_taxa~all_cov_ASPECT/all_taxa-all_cov_ASPECT_1.RData')
#load('out/FINAL_RUNS/All_taxa~all_cov_NOASPECT/all_taxa-all_cov_NOASPECT_1.RData')
#load('out/FINAL_RUNS/Reduced_taxa~all_cov_ASPECT/reduced_taxa-all_cov_ASPECT_1.RData')
#load('out/FINAL_RUNS/Reduced_taxa~all_cov_NOASPECT/reduced_taxa-all_cov_NOASPECT_1.RData')

# Clean up environment to make sure we are predicting correct data
rm(edata, mlist, xdata, ydata, site_effort)

# Load out of sample data
load('GJAMDATA/Withheld For Validation/validation_process2.RData')
#load('GJAMDATA/Withheld For Validation/validation_process_reduce.RData')

# Specify whether all taxa or reduced taxa
type <- 'all'

# Hack. this has no effect on the results.
xdata <- xdata |>
  dplyr::mutate(Aspect = 1,
                direction = as.factor(direction))

#### Simple validation ####

# Specify data list
newdata <- list(xdata = xdata, nsim = 1000)

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

## yMu = predicted y (presence/absence but for some reason on a continuous scale)
# I think it's on a continuous scale because it's the average across different iterations that
# can be 0 or 1
if(type == 'all'){
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
}
if(type == 'reduced'){
  pred_yMu |>
    mutate(lat = xdata$lat,
           lon = xdata$long) |>
    pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Presence') |>
    ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
    coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
    geom_point(aes(x = lon, y = lat, color = Presence)) +
    facet_wrap(~Ecosystem) +
    scale_color_viridis_c(option = 'A')
}


# Do the same with the probability of presence
## This actually represents probability of presence at a given location
pred_pr <- as.data.frame(pred$prPresent)

if(type == 'all'){
  pred_pr |>
    mutate(lat = xdata$lat,
           lon = xdata$long) |>
    pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Probability') |>
    mutate(Taxon = if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
           Taxon = if_else(Taxon == 'No.tree', 'No Tree', Taxon),
           Taxon = if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
           Taxon = if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
           Taxon = if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon)) |>
    ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
    coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
    geom_point(aes(x = lon, y = lat, color = Probability)) +
    facet_wrap(~Taxon, nrow = 3, ncol = 5) +
    scale_color_viridis_c(option = 'A') +
    theme_void() +
    theme(strip.text = element_text(size = 14, face = 'bold'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
}

if(type == 'reduced'){
  pred_pr |>
    mutate(lat = xdata$lat,
           lon = xdata$long) |>
    pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Probability') |>
    ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
    coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
    geom_point(aes(x = lon, y = lat, color = Probability)) +
    facet_wrap(~Ecosystem) +
    scale_color_viridis_c(option = 'A')
}

# Now compare with observation
if(type == 'all'){
pred_yMu <- pred_yMu |>
  mutate(Index = rownames(ydata)) |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
ydata <- ydata |>
  rownames_to_column(var = 'Index') |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
}

if(type == 'reduced'){
  pred_yMu <- pred_yMu |>
    mutate(Index = rownames(ydata)) |>
    pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Presence')
  ydata <- ydata |>
    rownames_to_column(var = 'Index') |>
    pivot_longer(Prairie:Savanna, names_to = 'Ecosystem', values_to = 'Presence')
}

if(type == 'all'){
comp <- pred_yMu |>
  full_join(ydata, by = c('Index', 'Taxon'))
colnames(comp) <- c('Index', 'Taxon', 'Predicted', 'Observed')
}

if(type == 'reduced'){
  comp <- pred_yMu |>
    full_join(ydata, by = c('Index', 'Ecosystem'))
  colnames(comp) <- c('Index', 'Ecosystem', 'Predicted', 'Observed')
}

if(type == 'all'){
pred_pr <- pred_pr |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Probability') |>
  mutate(Index = ydata$Index)

comp <- pred_pr |>
  full_join(ydata, by = c('Index', 'Taxon'))
colnames(comp) <- c('Taxon', 'Probability', 'Index', 'Observed')
}

if(type == 'reduced'){
  pred_pr <- pred_pr |>
    pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Probability') |>
    mutate(Index = ydata$Index)
  
  comp <- pred_pr |>
    full_join(ydata, by = c('Index', 'Ecosystem'))
  colnames(comp) <- c('Ecosystem', 'Probability', 'Index', 'Observed')
}

if(type == 'all'){
  comp |>
    mutate(Taxon = if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
           Taxon = if_else(Taxon == 'No.tree', 'No Tree', Taxon),
           Taxon = if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
           Taxon = if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
          Taxon = if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon)) |>
    ggplot(aes(x = Probability, y = Observed)) +
    geom_point() +
    facet_wrap(~Taxon, scales = 'free', nrow = 3, ncol = 5) +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'), 
                color = 'maroon', fill = 'maroon') +
    xlab('Predicted') + ylab('Observed') +
    theme_minimal() +
    theme(strip.text = element_text(size = 14, face = 'bold'),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
}

if(type == 'reduced'){
  comp |>
    ggplot(aes(x = Probability, y = Observed)) +
    geom_point() +
    facet_wrap(~Ecosystem, scales = 'free') +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'),
                color = 'maroon', fill = 'maroon') +
    theme_minimal()
}

# Overall model
form <- glm(Observed ~ Probability, family = binomial, data = comp)
summary(form)
# Overall R2
with(summary(form), 1 - deviance/null.deviance)

# Overall with random effect
if(type == 'all'){
comp$Taxon <- as.factor(comp$Taxon)
form_rand <- glmer(Observed ~ Probability + (1|Taxon), family = binomial, data = comp)
}
if(type == 'reduced'){
  comp$Ecosystem <- as.factor(comp$Ecosystem)
  form_rand <- glmer(Observed ~ Probability + (1|Ecosystem), family = binomial, data = comp)
}

summary(form_rand)
piecewiseSEM::rsquared(form_rand)

# Individual models
if(type == 'all'){
comp_notree <- comp |>
  filter(Taxon == 'No.tree')
comp_oak <- comp |>
  filter(Taxon == 'Oak')
comp_elm <- comp |>
  filter(Taxon == 'Elm')
comp_hickory <- comp |>
  filter(Taxon == 'Hickory')
comp_ash <- comp |>
  filter(Taxon == 'Ash')
comp_maple <- comp |>
  filter(Taxon == 'Maple')
comp_basswood <- comp |>
  filter(Taxon == 'Basswood')
comp_walnut <- comp |>
  filter(Taxon == 'Walnut')
comp_ironwood <- comp |>
  filter(Taxon == 'Ironwood')
comp_beech <- comp |>
  filter(Taxon == 'Beech')
comp_dogwood <- comp |>
  filter(Taxon == 'Dogwood')
comp_poplar <- comp |>
  filter(Taxon == 'Poplar.tulip.poplar')
comp_gum <- comp |>
  filter(Taxon == 'Black.gum.sweet.gum')
comp_otherconifer <- comp |>
  filter(Taxon == 'Other.conifer')
comp_otherhardwood <- comp |>
  filter(Taxon == 'Other.hardwood')
}

if(type == 'reduced'){
  comp_prairie <- comp |>
    filter(Ecosystem == 'Prairie')
  comp_savanna <- comp |>
    filter(Ecosystem == 'Savanna')
  comp_forest <- comp |>
    filter(Ecosystem == 'Forest')
}

if(type == 'all'){
form_notree <- glm(Observed ~ Probability, family = binomial, data = comp_notree)
print(paste('no tree:', with(summary(form_notree), 1 - deviance/null.deviance)))

form_oak <- glm(Observed~Probability, family = binomial, data = comp_oak)
print(paste('oak:', with(summary(form_oak), 1 - deviance/null.deviance)))

form_elm <- glm(Observed ~ Probability, family = binomial, data = comp_elm)
print(paste('elm:', with(summary(form_elm), 1 - deviance/null.deviance)))

form_hickory <- glm(Observed ~ Probability, family = binomial, data = comp_hickory)
print(paste('hickory:',with(summary(form_hickory), 1 - deviance/null.deviance)))

form_ash <- glm(Observed ~ Probability, family = binomial, data = comp_ash)
print(paste('ash:', with(summary(form_ash), 1 - deviance/null.deviance)))

form_maple <- glm(Observed ~ Probability, family = binomial, data = comp_maple)
print(paste('maple:', with(summary(form_maple), 1 - deviance/null.deviance)))

form_basswood <- glm(Observed ~ Probability, family = binomial, data = comp_basswood)
print(paste('basswood:', with(summary(form_basswood), 1 - deviance/null.deviance)))
}

if(type == 'reduced'){
  form_prairie <- glm(Observed ~ Probability, family = binomial, data = comp_prairie)
  print(paste('prairie:', with(summary(form_prairie), 1 - deviance/null.deviance)))
  
  form_savanna <- glm(Observed ~ Probability, family = binomial, data = comp_savanna)
  print(paste('savanna:', with(summary(form_savanna), 1 - deviance/null.deviance)))
  
  form_forest <- glm(Observed ~ Probability, family = binomial, data = comp_forest)
  print(paste('forest:', with(summary(form_forest), 1 - deviance/null.deviance)))
}

#### Validation with conditional prediction ####

load('out/FINAL_RUNS/All_taxa~all_cov_ASPECT/all_taxa-all_cov_ASPECT_1.RData')
#load('out/FINAL_RUNS/All_taxa~all_cov_NOASPECT/all_taxa-all_cov_NOASPECT_1.RData')
#load('out/FINAL_RUNS/Reduced_taxa~all_cov_ASPECT/reduced_taxa-all_cov_ASPECT_1.RData')
#load('out/FINAL_RUNS/Reduced_taxa~all_cov_NOASPECT/reduced_taxa-all_cov_NOASPECT_1.RData')

rm(edata, mlist, xdata, ydata, site_effort)

load('GJAMDATA/Withheld For Validation/validation_process2.RData')
#load('GJAMDATA/Withheld For Validation/validation_process_reduce.RData')

xdata <- xdata |> mutate(Aspect = 1)

# Predict half of species conditional on other half
rand <- sample(1:ncol(ydata), size = round(ncol(ydata)/2))
newdata <- list(xdata = xdata,
                ydataCond = ydata[,rand],
                nsim = 1000)
pred2 <- gjamPredict(output = out, newdata = newdata)

# Collect prediction outputs
pred2_yMu <- as.data.frame(pred2$sdList$yMu)
pred2_pr <- as.data.frame(pred2$prPresent)

# Compare with observations
if(type == 'all'){
pred2_yMu <- pred2_yMu |>
  mutate(Index = rownames(ydata)) |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
ydata2 <- ydata |>
  rownames_to_column(var = 'Index') |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
}

if(type == 'reduced'){
  pred2_yMu <- pred2_yMu |>
    mutate(Index = rownames(ydata)) |>
    pivot_longer(Prairie:Forest, names_to = 'Ecosystem', values_to = 'Presence')
  ydata2 <- ydata |>
    rownames_to_column(var = 'Index') |>
    pivot_longer(Prairie:Savanna, names_to = 'Ecosystem', values_to = 'Presence')
}

if(type == 'all'){
comp <- pred_yMu |>
  full_join(ydata2, by = c('Index', 'Taxon'))
colnames(comp) <- c('Index', 'Taxon', 'Predicted', 'Observed')
}

if(type == 'reduced'){
  comp <- pred_yMu |>
    full_join(ydata2, by = c('Index', 'Ecosystem'))
  colnames(comp) <- c('Index', 'Ecosystem', 'Predicted', 'Observed')
}

if(type == 'all'){
  comp |>
    mutate(Taxon = if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
           Taxon = if_else(Taxon == 'No.tree', 'No Tree', Taxon),
           Taxon = if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
           Taxon = if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
          Taxon = if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon)) |>
  ggplot(aes(x = Predicted, y = Observed)) +
    geom_point() +
    facet_wrap(~Taxon) +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'),
                color = 'maroon', fill = 'maroon') +
    theme_minimal() +
    theme(strip.text = element_text(size = 14, face = 'bold'),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
}

if(type == 'reduced'){
  comp |>
    ggplot(aes(x = Predicted, y = Observed)) +
    geom_point() +
    facet_wrap(~Ecosystem) +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'),
                color = 'maroon', fill = 'maroon') +
    theme_minimal()
}

# Overall model
form2 <- glm(Observed ~ Predicted, family = binomial, data = comp)
summary(form2)
# Overall R2
with(summary(form2), 1 - deviance/null.deviance)
# Increase in R2 (marginal)
print(paste0('No conditional prediction: ', with(summary(form), 1 - deviance/null.deviance)))
print(paste0('With conditional prediction: ', with(summary(form2), 1 - deviance/null.deviance)))

# Overall with random effect
if(type == 'all'){
comp$Taxon <- as.factor(comp$Taxon)
form2_rand <- glmer(Observed ~ Predicted + (1|Taxon), family = binomial, data = comp)
}
if(type == 'reduced'){
  comp$Ecosystem <- as.factor(comp$Ecosystem)
  form2_rand <- glmer(Observed ~ Predicted + (1|Ecosystem), family = binomial, data = comp)
}
summary(form2_rand)
piecewiseSEM::rsquared(form2_rand)
# Marginal decrease in R2
print(paste0('No conditional prediction: ', piecewiseSEM::rsquared(form_rand)$Conditional))
print(paste0('With conditional prediction: ', piecewiseSEM::rsquared(form2_rand)$Conditional))

