## Conditional prediction

rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(lme4)
library(gjam)

# Load output from GJAM
#load('out/FINAL_RUNS/All_taxa~all_cov_ASPECT/all_taxa-all_cov_ASPECT_1.RData')
#load('out/FINAL_RUNS/All_taxa~all_cov_NOASPECT/all_taxa-all_cov_NOASPECT_1.RData')
#load('out/FINAL_RUNS/Reduced_taxa~all_cov_ASPECT/reduced_taxa-all_cov_ASPECT_1.RData')
load('out/FINAL_RUNS/Reduced_taxa~all_cov_NOASPECT/reduced_taxa-all_cov_NOASPECT_1.RData')

# Clean up environment to make sure we are predicting correct data
rm(edata, mlist, xdata, ydata, site_effort)

# Load out of sample data
#load('GJAMDATA/Withheld For Validation/validation_process2.RData')
load('GJAMDATA/Withheld For Validation/validation_process_reduce.RData')

# Specify whether all taxa or reduced taxa
type <- 'reduced'

#xdata <- xdata |> mutate(Aspect = 1)
xdata <- xdata |> select(-Aspect, -type)

cond_pred <- matrix(, nrow = nrow(ydata), ncol = ncol(ydata))
cond_prob <- matrix(, nrow = nrow(ydata), ncol = ncol(ydata))

for(s in 1:ncol(ydata)){
  new_ydata <- ydata[,-s]
  new_datalist <- list(ydataCond = new_ydata,
                       xdata = xdata,
                       nsim = 1000)
  cond_pred_temp <- gjamPredict(output = out, 
                                newdata = new_datalist)
  cond_pred[,s] <- cond_pred_temp$sdList$yMu[,s]
  cond_prob[,s] <- cond_pred_temp$prPresent[,s]
  print(s/ncol(ydata))
}

if(type == 'all'){
  save(cond_pred, cond_prob, file = 'out/cond_pred_all_taxa.RData')
}
if(type == 'reduced'){
  save(cond_pred, cond_prob, file = 'out/cond_pred_reduced_taxa.RData')
}

if(type == 'all'){
  load('out/cond_pred_all_taxa.RData')
}
if(type == 'reduced'){
  load('out/cond_pred_reduced_taxa.RData')
}

colnames(cond_pred) <- colnames(cond_prob) <- colnames(ydata)

cond_pred <- cbind(cond_pred, xdata$long, xdata$lat)
cond_prob <- cbind(cond_prob, xdata$long, xdata$lat)

if(type == 'all'){
  colnames(cond_pred)[16:17] <- c('long', 'lat')
  colnames(cond_prob)[16:17] <- c('long', 'lat')
}
if(type == 'reduced'){
  colnames(cond_pred)[4:5] <- c('long', 'lat')
  colnames(cond_prob)[4:5] <- c('long', 'lat')
}

cond_pred <- as.data.frame(cond_pred)
cond_prob <- as.data.frame(cond_prob)

states <- map_data('state', region = c('illinois', 'indiana'))

if(type == 'all'){
  cond_pred |>
    pivot_longer(No.tree:Other.hardwood,
                 names_to = 'Taxon',
                 values_to = 'Predicted') |>
    mutate(Taxon = if_else(Taxon == 'No.tree', 'No tree', Taxon),
           Taxon = if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon),
           Taxon = if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
           Taxon = if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
          Taxon = if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon)) |>
    ggplot(aes(x = long, y = lat, color = Predicted)) +
    geom_point() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
    facet_wrap(~Taxon, nrow = 3, ncol = 5) +
    theme_void() +
    theme(strip.text = element_text(size = 14, face = 'bold'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    scale_color_viridis_c(option = 'A') +
    xlab('') + ylab('')

  cond_prob |>
    pivot_longer(No.tree:Other.hardwood,
                 names_to = 'Taxon',
                 values_to = 'Probability') |>
    mutate(Taxon = if_else(Taxon == 'No.tree', 'No Tree', Taxon),
           Taxon = if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/\nTulip Poplar', Taxon),
           Taxon = if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/\nSweet Gum', Taxon),
           Taxon = if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
          Taxon = if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon)) |>
    ggplot(aes(x = long, y = lat, color = Probability)) +
    geom_point() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
    facet_wrap(~Taxon, nrow = 3, ncol = 5) +
    theme_void() +
    theme(strip.text = element_text(size = 14, face = 'bold'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    scale_color_viridis_c(option = 'A') +
    xlab('') + ylab('')
}
if(type == 'reduced'){
  cond_pred |>
    pivot_longer(Prairie:Savanna,
                 names_to = 'Ecosystem',
                 values_to = 'Predicted') |>
    ggplot(aes(x = long, y = lat, color = Predicted)) +
    geom_point() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
    facet_wrap(~Ecosystem) +
    theme_void() +
    theme(strip.text = element_text(size = 14, face = 'bold'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    scale_color_viridis_c(option = 'A') +
    xlab('') + ylab('')
  
  cond_prob |>
    pivot_longer(Prairie:Savanna,
                 names_to = 'Ecosystem',
                 values_to = 'Probability') |>
    ggplot(aes(x = long, y = lat, color = Probability)) +
    geom_point() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
    facet_wrap(~Ecosystem) +
    theme_void() +
    theme(strip.text = element_text(size = 14, face = 'bold'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    scale_color_viridis_c(option = 'A') +
    xlab('') + ylab('')
}

if(type == 'all'){
  pred2_yMu <- cond_pred |>
    mutate(Index = rownames(ydata)) |>
    pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
  ydata2 <- ydata |>
    rownames_to_column(var = 'Index') |>
    pivot_longer(No.tree:Other.hardwood, names_to = 'Taxon', values_to = 'Presence')
}
if(type == 'reduced'){
  pred2_yMu <- cond_pred |>
    mutate(Index = rownames(ydata)) |>
    pivot_longer(Prairie:Savanna, names_to = 'Ecosystem', values_to = 'Presence')
  ydata2 <- ydata |>
    rownames_to_column(var = 'Index') |>
    pivot_longer(Prairie:Savanna, names_to = 'Ecosystem', values_to = 'Presence')
}

if(type == 'all'){
  comp <- pred2_yMu |>
    full_join(ydata2, by = c('Index', 'Taxon')) |>
    select(-long,-lat)
  colnames(comp) <- c('Index', 'Taxon', 'Predicted', 'Observed')
}
if(type == 'reduced'){
  comp <- pred2_yMu |>
    full_join(ydata2, by = c('Index', 'Ecosystem')) |>
    select(-long, -lat)
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
    facet_wrap(~Taxon, nrow = 3, ncol = 5) +
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
    theme_minimal() +
    theme(strip.text = element_text(size = 14, face = 'bold'),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
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
    facet_wrap(~Taxon, nrow = 3, ncol = 5) +
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
    theme_minimal() +
    theme(strip.text = element_text(size = 14, face = 'bold'),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
}
# Overall model
form2 <- glm(Observed ~ Predicted, family = binomial, data = comp)
summary(form2)
# Overall R2
with(summary(form2), 1 - deviance/null.deviance)

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

