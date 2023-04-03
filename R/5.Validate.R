## Validation for our all taxa, all cov model
## This is separate from validation for the reduced taxa, all cov model

## Author: AM Willson

rm(list = ls())

library(gjam)
library(lme4)
library(piecewiseSEM)

# Load output from GJAM
load('out/reduced_taxa-all_cov/reduced_taxa-all_cov_1.RData')
#load('out/all_taxa-all_cov/all_taxa-all_cov_1.RData')

# Check to make sure convergence looks okay since we only have 1 chain
#gjamPlot(out)

# Looks good

# Clean up environment to make sure we are predicting correct data
rm(edata, elist, mlist, xdata, ydata, site_effort)

# Load out of sample data
load('GJAM DATA/Withheld For Validation/validation_process_reduce.RData')
#load('GJAM DATA/Withheld For Validation/validation_process2.RData')

# Where there are NA's in the aspect covariate, make it a random value
# within the range of the data
# I'm doing this because gjamPredict() doesn't tolerate NA and since we 
# don't have any information, just make it random
nas <- length(which(is.na(xdata[,4])))
rands <- runif(n = nas, min = min(xdata[,4], na.rm = T), max = max(xdata[,4], na.rm = T))
xdata[is.na(xdata)] <- rands

#### Simple validation ####

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

pred_pr <- pred_pr |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Probability') |>
  mutate(Index = ydata$Index)

comp <- pred_pr |>
  full_join(ydata, by = c('Index', 'Taxon'))
colnames(comp) <- c('Taxon', 'Probability', 'Index', 'Observed')

comp |>
  ggplot(aes(x = Probability, y = Observed)) +
  geom_point() +
  facet_wrap(~Taxon, scales = 'free') +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))

# Overall model
form <- glm(Observed ~ Probability, family = binomial, data = comp)
summary(form)
# Overall R2
with(summary(form), 1 - deviance/null.deviance)

# Overall with random effect
comp$Taxon <- as.factor(comp$Taxon)
form_rand <- glmer(Observed ~ Probability + (1|Taxon), family = binomial, data = comp)
summary(form_rand)
piecewiseSEM::rsquared(form_rand)

# Individual models
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

form_notree <- glm(Observed ~ Predicted, family = binomial, data = comp_notree)
with(summary(form_notree), 1 - deviance/null.deviance)

form_oak <- glm(Observed~Predicted, family = binomial, data = comp_oak)
with(summary(form_oak), 1 - deviance/null.deviance)

form_elm <- glm(Observed ~ Probability, family = binomial, data = comp_elm)
with(summary(form_elm), 1 - deviance/null.deviance)

form_hickory <- glm(Observed ~ Probability, family = binomial, data = comp_hickory)
with(summary(form_hickory), 1 - deviance/null.deviance)

form_ash <- glm(Observed ~ Probability, family = binomial, data = comp_ash)
with(summary(form_ash), 1 - deviance/null.deviance)

form_maple <- glm(Observed ~ Probability, family = binomial, data = comp_maple)
with(summary(form_maple), 1 - deviance/null.deviance)

form_basswood <- glm(Observed ~ Probability, family = binomial, data = comp_basswood)
with(summary(form_basswood), 1 - deviance/null.deviance)

#### Validation with conditional prediction ####

load('out/all_taxa-all_cov/all_taxa-all_cov_1.RData')
rm(edata, elist, mlist, xdata, ydata, site_effort)
load('GJAM DATA/Withheld For Validation/validation_process2.RData')
nas <- length(which(is.na(xdata[,4])))
rands <- runif(n = nas, min = min(xdata[,4], na.rm = T), max = max(xdata[,4], na.rm = T))
xdata[is.na(xdata)] <- rands

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
pred2_yMu <- pred2_yMu |>
  mutate(Index = rownames(ydata)) |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
ydata2 <- ydata |>
  rownames_to_column(var = 'Index') |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')

comp <- pred_yMu |>
  full_join(ydata2, by = c('Index', 'Taxon'))
colnames(comp) <- c('Index', 'Taxon', 'Predicted', 'Observed')

comp |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point() +
  facet_wrap(~Taxon)

# Overall model
form2 <- glm(Observed ~ Predicted, family = binomial, data = comp)
summary(form2)
# Overall R2
with(summary(form2), 1 - deviance/null.deviance)
# Increase in R2 (marginal)
print(paste0('No conditional prediction: ', with(summary(form), 1 - deviance/null.deviance)))
print(paste0('With conditional prediction: ', with(summary(form2), 1 - deviance/null.deviance)))

# Overall with random effect
comp$Taxon <- as.factor(comp$Taxon)
form2_rand <- glmer(Observed ~ Predicted + (1|Taxon), family = binomial, data = comp)
summary(form2_rand)
piecewiseSEM::rsquared(form2_rand)
# Marginal decrease in R2
print(paste0('No conditional prediction: ', piecewiseSEM::rsquared(form_rand)$Conditional))
print(paste0('With conditional prediction: ', piecewiseSEM::rsquared(form2_rand)$Conditional))
