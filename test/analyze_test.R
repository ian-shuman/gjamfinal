## This is a simple analysis of a subset of the full dataset in Indiana
## Because we only have a small subset of the data, I was interested in the following
  ## 1. How can we plot the output?
  ## 2. What covariates are ecologically & statistically relevant?
  ## 3. What is the relationship between species presence/absence?
  ## 4. How much do the environmental covariates covary?
  ## 5. How well does the model fit the data?
## This script may function as a template for analyses down the line.

## Note that I have written this step to be run on a local machine
## You can run GJAM on the ND CRC and then download the output from the model
## for local analysis

## Author: AM Willson

rm(list = ls())
library(gjam)
library(caret)
library(tidyverse)
library(reshape2)
library(Hmisc)

load('test/out.RData')

#### GJAM default plots ####

## Before beginning our own analysis, let's take a look at what GJAM already gives us

gjamPlot(out)

# a) observed vs. predicted presence/absence: not very informative
# b) factor environmental covariates are correctly predicted (in our case, hydric soils & floodplain)
# c) continuous environmental covariates are well predicted
# d) species presence/absence is most sensitive to soil texture (sand, silt, clay),
#    followed by P-PET, CaCO3 and whether the area is a floodplain
# e) most parameter chains are not very well converged
# f) species covariances?
# g) mean.SlopeProjected: not much statistical significance (where significance means 95% posterior does not overlap 0)
# h) mean.AspectProjected: not much statistical significance
# i) mean.CAC: beech & no tree negatively correlated with CaCO3
#              elm, basswood, mulberry, locust, ash, oak, walnut, hackberry postively correlated with CaCO3
# j) mean.CEC: beech & ash negatively correlated with cation exchange capacity
#              poplar, ironwood, maple, buckeye, hackberry positively correlated with cation exchange capacity
# k) mean.CLA: not much statistical significance
# l) mean.KSA: beech, poplar/tulippoplar, dogwood negatively correlated with soil hydraulic conductivity
#              walnut, mulberry, locust, elm, poplar, hackberry, no tree positively correlated with soil hydraulic conductivity
# m) mean.SAN: not much statistical significance
# n) mean.SIL: not much statistical significance
# o) mean.WAT: no tree, oak dogwood, birch, other hardwood negatively correlated with water capacity
#              hackberry positively correlated with water capacity
# p) mean.SWI: beech, maple, dogwood negative correlated with SWI
#              ash, hickory, buckeye, sycamore, elm, hackberry positively correlated with SWI
# q) totalPPT: no tree & basswood negatively correlated with total precipitation
#              sycamore, hackberry, maple positively correlated with total precipitation
# r) MeanTEMP: maple, walnut negatively correlated with mean temperature
#              blackgum/sweetgum, dogwood, oak positively correlated with mean temperature
# s) GS.ppet: oak, no tree, walnut, blackgum/sweetgum, mulberry, locust negatively correlated with P-PET
#              blackgum, sycamore, beech positively correlated with P-PET
# t) HydricNo: no tree & beech positively correlated with hydric soils
#              maple, dogwood, buckeye, walnut, hackberry negatively correlated with hydric soils
# u) HydricYes: inverse of plot (t)
# v) FloodplainNo: hackberry, buckeye, sycamore, elm, willow, ironwood, maple, mulberry, walnut, locust, poplar positively correlated with floodplains
#                  poplar/tulippoplar, hickory, blackgum/sweetgum, oak, beech, dogwood, no tree negatively correlated with floodplains
# w) FloodplainYes: inverse of plot (v)
# x) parameters for all environmental covariates together

#### Question 1 ####

# Find how many "presence" for each species
# Both observation and predicted
yobs = as.data.frame(out$inputs$y)
pres_yobs = c()
ypred = as.data.frame(out$prediction$ypredMu)
pres_ypred = c()

for(i in 1:ncol(yobs)){
  pres_yobs[i] = length(which(yobs[,i] == 1))
  pres_ypred[i] = length(which(round(ypred[,i]) == 1))
}

## Note that there are almost never any predicted presences
## This makes any representation of the data difficult
## Hopefully with the larger dataset, we can make better inference

## For now, let's make some confusion matrices for each species
## Note that they are not very interpretable right now

confusion_array = array(data = NA, dim = c(2, 2, 25))
for(i in 1:ncol(yobs)){
  mat = confusionMatrix(data = as.factor(round(out$prediction$ypredMu[,i])), reference = as.factor(out$inputs$y[,i]))
  confusion_array[,,i] = mat$table
}

#### Question 2 ####

## First, let's look at statistically significant correlations between environmental
## covariates and species presence/absence

# Taking table of parameters from GJAM output
sig_beta_table = out$parameters$betaTable

# Filtering by significance and adding new columns for species & covariate
sig_beta_table = sig_beta_table %>%
  filter(sig95 == '*') %>%
  select(-sig95) %>%
  mutate(param = rownames(.)) %>%
  filter(!grepl('intercept', param)) %>%
  mutate(species = sub('_.*', '', param),
         covar = sub('.*_', '', param)) %>%
  select(-param) %>%
  remove_rownames()

## Now we can sort the table in different ways to see different things

# What species has the most significant correlations & what are they?
sig_beta_table %>%
  count(species) %>%
  arrange(desc(n))

sig_beta_table %>%
  arrange(species)

# What covariate has the most significant correlations & what are they?
sig_beta_table %>%
  count(covar) %>%
  arrange(desc(n))

sig_beta_table %>%
  arrange(covar)

## Let's do the same thing but with the standardized parameters
sig_beta_stand_table = out$parameters$betaStandXWTable
sig_beta_stand_table = sig_beta_stand_table %>%
  filter(sig95 == '*') %>%
  select(-sig95) %>%
  mutate(param = rownames(.)) %>%
  filter(!grepl('intercept', param)) %>%
  mutate(species = sub('_.*', '', param),
         covar = sub('.*_', '', param)) %>%
  select(-param) %>%
  remove_rownames()

## This allows us to look at the relative strength of correlations

# What species have the strongest correlations & what are they?
sig_beta_stand_table %>%
  arrange(desc(abs(Estimate))) %>%
  slice_head(n = 20) %>%
  ggplot() +
  geom_point(aes(x = c(1:20), y = abs(Estimate), color = species))

# What covariates have the strongest correlations & what are they?
sig_beta_stand_table %>%
  arrange(desc(abs(Estimate))) %>%
  slice_head(n = 20) %>%
  ggplot() +
  geom_point(aes(x = c(1:20), y = abs(Estimate), color = covar))

## Finally, let's look at the sensitivity of presence/absence to the covariates
sens_table = out$parameters$sensTable
sens_table = sens_table %>%
  mutate(sens = rownames(.)) %>%
  filter(!grepl('intercept', sens)) %>%
  mutate(species = sub('_.*', '', sens),
         covar = sub('.*_', '', sens)) %>%
  select(-sens) %>%
  remove_rownames()

# Sort by highest sensitivity & plot by species
sens_table %>%
  arrange(desc(Estimate)) %>%
  slice_head(n = 20) %>%
  ggplot() +
  geom_point(aes(x = c(1:20), y = Estimate, color = species))

sens_table %>%
  arrange(desc(Estimate)) %>%
  slice_head(n = 20) %>%
  ggplot() +
  geom_point(aes(x = c(1:20), y = Estimate, color = covar))

#### Question 3 ####

## Not only can we look at the relationship between species presence/absence and 
## environmental covariates, but we can also look at correlations between different species
## presence/absence. To do this, we look at the species covariance matrix

# Taking covariance matrix from GJAM output
cov_mat = out$parameters$sigMu

## First, let's just visualize with a heat map
cov_mat_melt = melt(cov_mat)
colnames(cov_mat_melt) = c('Species1', 'Species2', 'Covariance')

cov_mat_melt %>%
  ggplot() +
  geom_tile(aes(x = Species1, y = Species2, fill = Covariance)) +
  scale_fill_viridis(option = 'A') +
  theme(axis.text.x = element_text(angle = 90))

## Now, this is only sort of useful because it accounts for both correlation and variance
## Let's convert our convariance matrix into a correlation matrix

cor_mat = cov2cor(cov_mat)

# Repeat above steps with correlation matrix

cor_mat_melt = melt(cor_mat)
colnames(cor_mat_melt) = c('Species1', 'Species2', 'Correlation')

cor_mat_melt %>%
  ggplot() +
  geom_tile(aes(x = Species1, y = Species2, fill = Correlation)) +
  scale_fill_viridis(option = 'A') +
  theme(axis.text.x = element_text(angle = 90))

#### Question 4 ####

## Let's make a simple correlation matrix for the covariate data and see how strong
## our correlations are

# Grab the data from the GJAM output
covar = out$inputs$xdata

# Remove non-numeric columns
covar = covar %>%
  select(-c(Hydric, Floodplain))

# Correlation matrix
covar_cor = cor(covar, use = 'complete.obs')

# Format and plot as above
covar_cor_melt = melt(covar_cor)
colnames(covar_cor_melt) = c('Covar1', 'Covar2', 'Correlation')

covar_cor_melt %>%
  ggplot() +
  geom_tile(aes(x = Covar1, y = Covar2, fill = Correlation)) +
  scale_fill_viridis(option = 'A') +
  theme(axis.text.x = element_text(angle = 90))

## We can take this a step further and plot by significant correlations

# Redo correlations with p-values
covar_cor = rcorr(as.matrix(covar))

# Separate correlations & p-values
covar_cor_r = covar_cor$r
covar_cor_p = covar_cor$P

# Format & combine
covar_cor_r_melt = melt(covar_cor_r)
colnames(covar_cor_r_melt) = c('Covar1', 'Covar2', 'Correlation')
covar_cor_p_melt = melt(covar_cor_p)
colnames(covar_cor_p_melt) = c('Covar1', 'Covar2', 'p')
covar_cor_melt = covar_cor_r_melt %>%
  full_join(covar_cor_p_melt, by = c('Covar1', 'Covar2'))

# Manipulate correlation by p-value for plotting
covar_cor_melt = covar_cor_melt %>%
  mutate(new_p = case_when(is.na(p) ~ 0,
                           !is.na(p) ~ p))
p_rm = which(covar_cor_melt$new_p > 0.05)
covar_cor_melt$Correlation[p_rm] = NA

# Plot without nonsignificant correlations
covar_cor_melt %>%
  ggplot() +
  geom_tile(aes(x = Covar1, y = Covar2, fill = Correlation)) +
  scale_fill_viridis(option = 'A', na.value = 'lightgray') +
  theme(axis.text.x = element_text(angle = 90))

#### Question 5 ####

# Let's look at DIC
## Alone, DIC bears absolutely no value. Only in comparison with alternative models
out$fit$DIC

# How about design table
out$inputs$designTable
## This shows us that soil sand, silt, and clay are highly correlated (VIF >> 10)
## Only one should be used in the future