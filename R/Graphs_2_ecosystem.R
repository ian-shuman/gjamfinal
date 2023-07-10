## Figures for Shuman et al. at ecosystem level

## Author: AM Willson

rm(list = ls())

library(tidyverse)
library(corrplot)
library(cowplot)
library(RColorBrewer)

load('out/FINAL_RUNS/Reduced_taxa~all_cov_NOASPECT/combined.RData')

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
pal <- c('#bb5566', '#ddaa34','#002a53')

corr |>
  mutate(covariate = replace(covariate, covariate == 'CAC', '[CACO3]'),
         covariate = replace(covariate, covariate == 'CEC', 'Cation Exchange\nCapacity'),
         covariate = replace(covariate, covariate == 'CLA', 'Soil %Clay'),
         covariate = replace(covariate, covariate == 'SAN', 'Soil %Sand'),
         covariate = replace(covariate, covariate == 'WAT', 'Available Water\nContent'),
         covariate = replace(covariate, covariate == 'mean.SWI', 'Saga Wetness Index'),
         covariate = replace(covariate, covariate == 'totalPPT', 'Precipitation'),
         covariate = replace(covariate, covariate == 'MeanTEMP', 'Temperature'),
         covariate = replace(covariate, covariate == 'HydricYes', 'Hydric Soil'),
         covariate = replace(covariate, covariate == 'FloodplainYes', 'Floodplain'),
         covariate = replace(covariate, covariate == 'directionE', 'East-Facing Slope'),
         covariate = replace(covariate, covariate == 'directionN', 'North-Facing Slope'),
         covariate = replace(covariate, covariate == 'directionS', 'South-Facing Slope'),
         covariate = replace(covariate, covariate == 'directionW', 'West-Facing Slope')) |>
  filter(covariate != 'FloodplainNo') |>
  filter(covariate != 'HydricNo') |>
  filter(covariate != 'directionNS') |>
  rename(Taxon = taxon) |>
  ggplot() +
  geom_boxplot(aes(x = Taxon, ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper, color = Taxon), stat = 'identity') +
  geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
  facet_wrap(~covariate, scales = 'free_y') +
  xlab('') + ylab('Estimate') +
  scale_x_discrete(limits = c('Prairie', 'Savanna', 'Forest')) +
  scale_color_manual(limits = c('Prairie', 'Savanna', 'Forest'), 
                     values = pal) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Plot with fixed y axis
corr |>
  mutate(covariate = replace(covariate, covariate == 'mean.SlopeProjected', 'Slope'),
         covariate = replace(covariate, covariate == 'mean.AspectProjected', 'Aspect'),
         covariate = replace(covariate, covariate == 'mean.CAC', '[CACO3]'),
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
  rename(Taxon = taxon) |>
  ggplot() +
  geom_boxplot(aes(x = Taxon, ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper, color = Taxon), stat = 'identity') +
  geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
  facet_wrap(~covariate) +
  xlab('') + ylab('Estimate') +
  scale_x_discrete(limits = c('Prairie', 'Savanna', 'Forest')) +
  scale_color_manual(limits = c('Prairie', 'Savanna', 'Forest'), 
                     values = pal) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Do some cleaning on the sensitivity (fSensGibbs)
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

sens |>
  mutate(covar = replace(covar, covar == 'CAC', '[CaCO3]'),
         covar = replace(covar, covar == 'CEC', 'Cation Exchange\nCapacity'),
         covar = replace(covar, covar == 'CLA', 'Soil %Clay'),
         covar = replace(covar, covar == 'SAN', 'Soil %Sand'),
         covar = replace(covar, covar == 'WAT', 'Available Water\nContent'),
         covar = replace(covar, covar == 'mean.SWI', 'Saga Wetness Index'),
         covar = replace(covar, covar == 'totalPPT', 'Precipitation'),
         covar = replace(covar, covar == 'MeanTEMP', 'Temperature'),
         covar = replace(covar, covar == 'HydricYes', 'Hydric Soil'),
         covar = replace(covar, covar == 'FloodplainYes', 'Floodplain'),
         covar = replace(covar, covar == 'directionE', 'East-Facing Slope'),
         covar = replace(covar, covar == 'directionW', 'West-Facing Slope'),
         covar = replace(covar, covar == 'directionS', 'South-Facing Slope'),
         covar = replace(covar, covar == 'directionN', 'North-Facing Slope')) |>
  filter(covar != 'HydricNo') |>
  filter(covar != 'FloodplainNo') |>
  filter(covar != 'directionNS') |>
  ggplot() +
  geom_boxplot(aes(x = reorder(covar, mean, decreasing = F),
                   ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper,
                   color = reorder(covar, mean, decreasing = T)), stat = 'identity') +
  coord_flip() +
  xlab('') + ylab(expression(paste('Sensitivity (', hat(F), ')'))) +
  theme_minimal() +
  scale_color_manual(values = c('#88ccee', '#88ccee', 
                                #'#aa4499', 
                                '#999932',
                                #'#aa4499', 
                                '#999932',
                                '#aa4499', #'#aa4499', '#aa4499',
                                '#999932', '#999932', 
                                '#aa4499', '#aa4499',
                                '#999932',
                                '#aa4499'), name = '')

## Correaltions between ecosystems

# Remove unnecessary columns
sgibbs_cor <- sgibbs |>
  select(-c(chain, iter))
# Get summary statitics
mean_sgibbs <- apply(sgibbs_cor, 2, mean)
sd_sgibbs <- apply(sgibbs_cor, 2, sd)
lower_sgibbs <- apply(sgibbs_cor, 2, quantile, probs = 0.025)
upper_sgibbs <- apply(sgibbs_cor, 2, quantile, prob = 0.975)

# Need to put into the matrix format
# This gives the index for each entry of the matrix
ind <- c(1, 2, 3,
         2, 4, 5,
         3, 5, 6)

# Now we format the output into a matrix
corr_mat <- mean_sgibbs[ind]
corr_mat <- matrix(corr_mat, nrow = 3, ncol = 3)
corr_mat <- cov2cor(corr_mat)
colnames(corr_mat) <- rownames(corr_mat) <- c('Prairie', 'Savanna', 'Forest')

# Specify color palette
pal <- c('#364b9a', '#4a7bb7', '#6ea6cd', '#93cae1', '#cde4ef', 
         '#eaeccc', '#feda8b', '#fdb336', '#f67e4b', '#dd3d2d', '#a50026')

# Plot
corrplot(corr_mat, diag = F, type = 'upper', method = 'color',
          tl.col = 'black', col = rev(pal))

# With upper and lower credible intervals
low_mat <- lower_sgibbs[ind]
low_mat <- matrix(low_mat, nrow = 3, ncol = 3)
low_mat <- cov2cor(low_mat)
colnames(low_mat) <- rownames(low_mat) <- c('Prairie', 'Savanna', 'Forest')

upp_mat <- upper_sgibbs[ind]
upp_mat <- matrix(upp_mat, nrow = 3, ncol = 3)
upp_mat <- cov2cor(upp_mat)
colnames(upp_mat) <- rownames(upp_mat) <- c('Prairie', 'Savanna', 'Forest')

corrplot(corr_mat, lowCI.mat = low_mat, uppCI.mat = upp_mat, plotCI = 'rect',
         tl.col = 'black', col = rev(pal), type = 'upper', diag = F)
