rm(list = ls())

library(tidyverse)
library(corrplot)
library(cowplot)

# Load data for given model
# Should load the "combined.RData" file in the
# subfolder of the "out" directory corresponding to the
# model of interest
load('out/all_taxa-all_cov/combined.RData')

## Correlations between taxa and drivers

# Number of columns we're working with
cols <- ncol(bFacGibbs)

# Remove unnecessary columns and generate summary statistics
bFacGibbs_corr <- bFacGibbs |>
  select(-c(chain, iter))
corr_mean <- apply(bFacGibbs_corr, 2, mean, na.rm = T)
corr_sd <- apply(bFacGibbs_corr, 2, sd, na.rm = T)
corr_lower <- apply(bFacGibbs_corr, 2, quantile, probs = 0.025, na.rm = T)
corr_upper <- apply(bFacGibbs_corr, 2, quantile, probs = 0.975, na.rm = T)

# Formatting our summary statistics
corr <- rbind(corr_mean, corr_sd, corr_lower, corr_upper)
rownames(corr) <- c('mean', 'sd', 'lower', 'upper')
corr <- t(corr)
corr <- as.data.frame(corr)
corr <- corr |>
  rownames_to_column(var = 'beta') |>
  mutate(taxon = sub('_.*', '', beta),
         covariate = sub('.*_', '', beta))

# Specify color palette
pal <- c('#bb5566',
         '#ddaa34', '#ecd08f',
         '#002a53', '#004488', '#4c7cac', '#8aa9c8', '#c2d2e2', '#dee7f0',
         '#005f5f', '#008b8b', '#38a5a5', '#63b9b9', '#8ecdcd', '#c1e4e4')
# Plot with free y axis
corr |>
  mutate(covariate = replace(covariate, covariate == 'mean.SlopeProjected', 'Slope'),
         covariate = replace(covariate, covariate == 'mean.AspectProjected', 'Aspect'),
         covariate = replace(covariate, covariate == 'mean.CAC', '[CaCO3]'),
         covariate = replace(covariate, covariate == 'mean.CEC', 'Cation Exchange\nCapacity'),
         covariate = replace(covariate, covariate == 'mean.CLA', 'Soil %Clay'),
         covariate = replace(covariate, covariate == 'mean.SAN', 'Soil %Sand'),
         covariate = replace(covariate, covariate == 'mean.WAT', 'Available Water\nContent'),
         covariate = replace(covariate, covariate == 'mean.SWI', 'Saga Wetness Index'),
         covariate = replace(covariate, covariate == 'totalPPT', 'Precipitation'),
         covariate = replace(covariate, covariate == 'MeanTEMP', 'Temperature'),
         covariate = replace(covariate, covariate == 'HydricYes', 'Hydric Soil'),
         covariate = replace(covariate, covariate == 'FloodplainYes', 'Floodplain')) |>
  filter(covariate != 'FloodplainNo') |>
  filter(covariate != 'HydricNo') |>
  mutate(taxon = replace(taxon, taxon == 'Black.gum.sweet.gum', 'Black Gum/Sweet Gum'),
         taxon = replace(taxon, taxon == 'No.tree', 'No Tree'),
         taxon = replace(taxon, taxon == 'Other.conifer', 'Other Conifer'),
         taxon = replace(taxon, taxon == 'Other.hardwood', 'Other Hardwood'),
         taxon = replace(taxon, taxon == 'Poplar.tulip.poplar', 'Poplar/Tulip Poplar')) |>
  rename(Taxon = taxon) |>
  ggplot() +
  geom_boxplot(aes(x = Taxon, ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper, color = Taxon), stat = 'identity') +
  geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
  facet_wrap(~covariate, scales = 'free_y') +
  xlab('') + ylab('Estimate') +
  scale_x_discrete(limits = c('No Tree',
                              'Hickory', 'Oak',
                              'Ash', 'Basswood', 'Beech', 'Black Gum/Sweet Gum',
                              'Dogwood', 'Elm', 'Ironwood', 'Maple', 'Other Conifer',
                              'Other Hardwood', 'Poplar/Tulip Poplar', 'Walnut')) +
  scale_color_manual(limits = c('No Tree',
                                  'Hickory', 'Oak',
                                  'Ash', 'Basswood', 'Beech', 'Black Gum/Sweet Gum',
                                  'Dogwood', 'Elm', 'Ironwood', 'Maple', 'Other Conifer',
                                  'Other Hardwood', 'Poplar/Tulip Poplar', 'Walnut'), values = pal) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Plot with fixed y axis
corr |>
  mutate(covariate = replace(covariate, covariate == 'mean.SlopeProjected', 'Slope'),
         covariate = replace(covariate, covariate == 'mean.AspectProjected', 'Aspect'),
         covariate = replace(covariate, covariate == 'mean.CAC', '[CaCO3]'),
         covariate = replace(covariate, covariate == 'mean.CEC', 'Cation Exchange\nCapacity'),
         covariate = replace(covariate, covariate == 'mean.CLA', 'Soil %Clay'),
         covariate = replace(covariate, covariate == 'mean.SAN', 'Soil %Sand'),
         covariate = replace(covariate, covariate == 'mean.WAT', 'Available Water\nContent'),
         covariate = replace(covariate, covariate == 'mean.SWI', 'Saga Wetness Index'),
         covariate = replace(covariate, covariate == 'totalPPT', 'Precipitation'),
         covariate = replace(covariate, covariate == 'MeanTEMP', 'Temperature'),
         covariate = replace(covariate, covariate == 'HydricYes', 'Hydric Soil'),
         covariate = replace(covariate, covariate == 'FloodplainYes', 'Floodplain')) |>
  filter(covariate != 'FloodplainNo') |>
  filter(covariate != 'HydricNo') |>
  mutate(taxon = replace(taxon, taxon == 'Black.gum.sweet.gum', 'Black Gum/Sweet Gum'),
         taxon = replace(taxon, taxon == 'No.tree', 'No Tree'),
         taxon = replace(taxon, taxon == 'Other.conifer', 'Other Conifer'),
         taxon = replace(taxon, taxon == 'Other.hardwood', 'Other Hardwood'),
         taxon = replace(taxon, taxon == 'Poplar.tulip.poplar', 'Poplar/Tulip Poplar')) |>
  rename(Taxon = taxon) |>
  ggplot() +
  geom_boxplot(aes(x = Taxon, ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper, color = Taxon), stat = 'identity') +
  geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
  facet_wrap(~covariate) +
  xlab('') + ylab('Estimate') +
  scale_x_discrete(limits = c('No Tree',
                              'Hickory', 'Oak',
                              'Ash', 'Basswood', 'Beech', 'Black Gum/Sweet Gum',
                              'Dogwood', 'Elm', 'Ironwood', 'Maple', 'Other Conifer',
                              'Other Hardwood', 'Poplar/Tulip Poplar', 'Walnut')) +
  scale_color_manual(limits = c('No Tree',
                                  'Hickory', 'Oak',
                                  'Ash', 'Basswood', 'Beech', 'Black Gum/Sweet Gum',
                                  'Dogwood', 'Elm', 'Ironwood', 'Maple', 'Other Conifer',
                                  'Other Hardwood', 'Poplar/Tulip Poplar', 'Walnut'), values = pal) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Do some cleaning of the sensitivity (fSensGibbs)
fSensGibbs_sum <- fSensGibbs |>
  select(-c(chain, iter))
sens_mean <- apply(fSensGibbs_sum, 2, mean, na.rm = T)
sens_sd <- apply(fSensGibbs_sum, 2, sd, na.rm = T)
sens_lower <- apply(fSensGibbs_sum, 2, quantile, probs = 0.025, na.rm = T)
sens_upper <- apply(fSensGibbs_sum, 2, quantile, probs = 0.975, na.rm = T)

sens <- rbind(sens_mean, sens_sd, sens_lower, sens_upper)
rownames(sens) <- c('mean', 'sd', 'lower', 'upper')
sens <- t(sens)
sens <- as.data.frame(sens)
sens <- sens |>
  rownames_to_column(var = 'covar')

# Plot sensitivity
sens |>
  mutate(covar = replace(covar, covar == 'mean.SlopeProjected', 'Slope'),
         covar = replace(covar, covar == 'mean.AspectProjected', 'Aspect'),
         covar = replace(covar, covar == 'mean.CAC', '[CaCO3]'),
         covar = replace(covar, covar == 'mean.CEC', 'Cation Exchange\nCapacity'),
         covar = replace(covar, covar == 'mean.CLA', 'Soil %Clay'),
         covar = replace(covar, covar == 'mean.SAN', 'Soil %Sand'),
         covar = replace(covar, covar == 'mean.WAT', 'Available Water\nContent'),
         covar = replace(covar, covar == 'mean.SWI', 'Saga Wetness Index'),
         covar = replace(covar, covar == 'totalPPT', 'Precipitation'),
         covar = replace(covar, covar == 'MeanTEMP', 'Temperature'),
         covar = replace(covar, covar == 'HydricYes', 'Hydric Soil'),
         covar = replace(covar, covar == 'FloodplainYes', 'Floodplain')) |>
  filter(covar != 'HydricNo') |>
  filter(covar != 'FloodplainNo') |>
  ggplot() +
  geom_boxplot(aes(x = reorder(covar, mean, decreasing = F), 
                   ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper, 
                   color = reorder(covar, mean, decreasing = T)), stat = 'identity') +
  coord_flip() +
  xlab('') + ylab(expression(paste('Sensitivity (', hat(F) ,')'))) +
  theme_minimal() +
  scale_color_manual(values = c('#88ccee', '#88ccee',
                                '#aa4499', '#aa4499', '#aa4499', '#aa4499',
                                '#aa4499', '#aa4499', '#aa4499',
                                '#999932', '#999932', '#999932'), name = '')

## Correlations between taxa

# Remove unnecessary columns
sgibbs_cor <- sgibbs |>
  select(-c(chain, iter))
# Get summary statistics
mean_sgibbs <- apply(sgibbs_cor, 2, mean)
sd_sgibbs <- apply(sgibbs_cor, 2, sd)
lower_sgibbs <- apply(sgibbs_cor, 2, quantile, probs = 0.025)
upper_sgibbs <- apply(sgibbs_cor, 2, quantile, probs = 0.975)

# Need to put into the matrix format
# This gives the index for each entry of the matrix
ind <- rbind(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
             c(2, 16, 17, 17, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29),
             c(3, 17, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42),
             c(4, 18, 31, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54),
             c(5, 19, 32, 44, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65),
             c(6, 20, 33, 45, 56, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75),
             c(7, 21, 34, 46, 57, 67, 76, 77, 78, 79, 80, 81, 82, 83, 84),
             c(8, 22, 35, 47, 58, 68, 77, 85, 86, 87, 88, 89, 90, 91, 92),
             c(9, 23, 36, 48, 59, 69, 78, 86, 93, 94, 95, 96, 97, 98, 99),
             c(10, 24, 37, 49, 60, 70, 79, 87, 94, 100, 101, 102, 103, 104, 105),
             c(11, 25, 38, 50, 61, 71, 80, 88, 95, 101, 106, 107, 108, 109, 110),
             c(12, 26, 39, 51, 62, 72, 81, 89, 96, 102, 107, 111, 112, 113, 114),
             c(13, 27, 40, 52, 63, 73, 82, 90, 97, 103, 108, 112, 115, 116, 117),
             c(14, 28, 41, 53, 64, 74, 83, 91, 98, 104, 109, 113, 116, 118, 119),
             c(15, 29, 42, 54, 65, 75, 84, 92, 99, 105, 110, 114, 117, 119, 120))

# Now we format the output into a matrix
corr_mat <- mean_sgibbs[ind]
corr_mat <- matrix(corr_mat, nrow = 15, ncol = 15)
corr_mat <- cov2cor(corr_mat)
colnames(corr_mat) <- rownames(corr_mat) <- c('No Tree', 'Oak', 'Elm', 'Hickory', 'Ash', 'Maple', 'Basswood', 'Walnut', 'Ironwood', 'Beech', 'Dogwood', 'Poplar/Tulip Poplar', 'Black Gum/Sweet Gum', 'Other Conifer', 'Other Hardwood')

# Remove diagonals to improve visualization
corr_mat[corr_mat == 1] <- NA

# Plot
corrplot(corr_mat, diag = T, type = 'upper', method = 'color', 
         tl.col = 'black', col = brewer.pal(n = 11, name = 'PRGn'))

         