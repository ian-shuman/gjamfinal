## Figures associated with Shuman et al. publication in prep

## Author: AM Willson

rm(list = ls())

library(corrplot)
library(cowplot)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(ggplot2)

# Load data for given model
# Should load the "combined.RData" file in the
# subfolder of the "out" directory corresponding to the
# model of interest
load('out/Reduced_taxa~all_cov_NOASPECT/combined.RData')

## All figures are produced for the "no aspect" simulations but  could
## easily be modified to include aspect

type <- 'reduced' # reduced or all

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
if(type == 'all'){
  pal <- c('#bb5566',
           '#ddaa34', '#ecd08f',
           '#002a53', '#004488', '#4c7cac', '#8aa9c8', '#c2d2e2', '#dee7f0',
           '#005f5f', '#008b8b', '#38a5a5', '#63b9b9', '#8ecdcd', '#c1e4e4')
}
if(type == 'reduced'){
  pal <- c('#bb5566', '#ddaa34', '#002a53')
}

# Plot with free y axis
if(type == 'all'){
  for_plotting <- corr |>
    filter(covariate != 'HydricNo') |>
    filter(covariate != 'FloodplainNo') |>
    mutate(taxon = if_else(taxon == 'Black.gum.sweet.gum', 'Black gum/wweet gum', taxon),
           taxon = if_else(taxon == 'No.tree', 'No tree', taxon),
           taxon = if_else(taxon == 'Other.conifer', 'Other conifer', taxon),
           taxon = if_else(taxon == 'Other.hardwood', 'Other hardwood', taxon),
           taxon = if_else(taxon == 'Poplar.tulip.poplar', 'Poplar/tulip poplar', taxon)) |>
    rename(Taxon = taxon)
  
  my_labeller <- as_labeller(x = c(Slope = 'Slope', CAC = 'CaCO[3]',
                                   CEC = "`Cation exchange capacity`",
                                   CLA = '`Soil % clay`', SAN = '`Soil % sand`',
                                   WAT = '`Available water content`',
                                   mean.SWI = '`Saga Wetness Index`',
                                   totalPPT = 'Precipitation',
                                   MeanTEMP = 'Temperature',
                                   HydricYes = '`Hydric soil`',
                                   FloodplainYes = 'Floodplain'), default = label_parsed)
  
  for_plotting |>
    ggplot() +
    geom_boxplot(aes(x = Taxon, ymin = lower, lower = mean - sd, 
                     middle = mean, upper = mean + sd, ymax = upper, 
                     color = Taxon), stat = 'identity') +
    geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
    facet_wrap(~factor(covariate, levels = c('MeanTEMP', 'totalPPT',
                                             'CAC', 'CEC', 'CLA', 'SAN', 'WAT', 'HydricYes',
                                             'mean.SWI', 'Slope', 'FloodplainYes')), 
               labeller = my_labeller, scales = 'free_y',
               nrow = 4, ncol = 3) +
    xlab('') + ylab('Coefficient estimate') +
    scale_x_discrete(limits = c('No tree',
                                'Hickory', 'Oak',
                                'Ash', 'Basswood', 'Beech',
                                'Black gum/sweet gum',
                                'Dogwood', 'Elm',
                                'Ironwood', 'Maple', 'Other conifer',
                                'Other hardwood', 'Poplar/tulip poplar',
                                'Walnut')) +
    scale_color_manual(limits = c('No tree',
                                  'Hickory', 'Oak',
                                  'Ash', 'Basswood',
                                  'Beech', 'Black gum/sweet gum',
                                  'Dogwood', 'Elm',
                                  'Ironwood', 'Maple',
                                  'Other conifer', 'Other hardwood',
                                  'Poplar/tulip poplar', 'Walnut'),
                       values = pal) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text = element_text(size = 14, face = 'bold'),
          legend.title = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.text.y = element_text(size = 12))
}
if(type == 'reduced'){
  for_plotting <- corr |>
    filter(covariate != 'HydricNo') |>
    filter(covariate != 'FloodplainNo') |>
    rename(Ecosystem = taxon)
  
  my_labeller <- as_labeller(x = c(Slope = 'Slope', 'CAC' = 'CaCO[3]',
                                   CEC = '`Cation exchange capacity`',
                                   CLA = '`Soil % clay`', SAN = '`Soil % sand`',
                                   WAT = '`Available water content`',
                                   totalPPT = 'Precipitation',
                                   MeanTEMP = 'Temperature',
                                   HydricYes = '`Hydric soil`',
                                   mean.SWI = '`Saga Wetness Index`',
                                   FloodplainYes = 'Floodplain'), default = label_parsed)
  
  for_plotting |>
    ggplot() +
    geom_boxplot(aes(x = Ecosystem, ymin = lower, lower = mean - sd,
                     middle = mean, upper = mean + sd, ymax = upper,
                     color = Ecosystem), stat = 'identity') +
    geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
    facet_wrap(~factor(covariate, levels = c('MeanTEMP', 'totalPPT', 
                                             'CAC', 'CEC', 'CLA', 'SAN', 'WAT', 'HydricYes',
                                             'mean.SWI', 'Slope', 'FloodplainYes')),
               labeller = my_labeller, scales = 'free_y',
               nrow = 4, ncol = 3) +
    xlab('') + ylab('Coefficient estimate') +
    scale_x_discrete(limits = c('Prairie', 'Savanna', 'Forest')) +
    scale_color_manual(limits = c('Prairie', 'Savanna', 'Forest'),
                       values = pal) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text = element_text(size = 14, face = 'bold'),
          legend.title = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.text.y = element_text(size = 12))
}

# Plot with fixed y axis
# Figure S9
if(type == 'all'){
  for_plotting |>
    ggplot() +
    geom_boxplot(aes(x = Taxon, ymin = lower, lower = mean - sd,
                     middle = mean, upper = mean + sd, ymax = upper,
                     color = Taxon), stat = 'identity') +
    geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
    facet_wrap(~factor(covariate, levels = c('MeanTEMP', 'totalPPT',
                                             'CAC', 'CEC', 'CLA', 'SAN', 'WAT', 'HydricYes',
                                             'mean.SWI', 'Slope', 'FloodplainYes')), labeller = my_labeller, scales = 'fixed') +
    xlab('') + ylab('Coefficient estimate') +
    scale_x_discrete(limits = c('No tree',
                                'Hickory', 'Oak',
                                'Ash', 'Basswood',
                                'Beech', 'Black gum/sweet gum',
                                'Dogwood', 'Elm', 'Ironwood',
                                'Maple', 'Other conifer',
                                'Other hardwood',
                                'Poplar/tulip poplar', 'Walnut')) +
    scale_color_manual(limits = c('No tree',
                                  'Hickory', 'Oak',
                                  'Ash', 'Basswood',
                                  'Beech', 'Black gum/sweet gum',
                                  'Dogwood', 'Elm', 'Ironwood',
                                  'Maple', 'Other conifer',
                                  'Other hardwood',
                                  'Poplar/tulip poplar', 'Walnut'),
                       values = pal) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text = element_text(size = 14, face = 'bold'),
          legend.title = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12))
}
# Figure 4
if(type == 'reduced'){
  for_plotting |>
    ggplot() +
    geom_boxplot(aes(x = Ecosystem, ymin = lower, lower = mean - sd,
                 middle = mean, upper = mean + sd, ymax = upper,
                 color = Ecosystem), stat = 'identity') +
    geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
    facet_wrap(~factor(covariate, levels = c('MeanTEMP', 'totalPPT',
                                             'CAC', 'CEC', 'CLA', 'SAN', 'WAT', 'HydricYes',
                                             'mean.SWI', 'Slope', 'FloodplainYes')), 
               labeller = my_labeller,
               scales = 'fixed') +
    xlab('') + ylab('Coefficient estimate') +
    scale_x_discrete(limits = c('Prairie', 'Savanna', 'Forest')) +
    scale_color_manual(limits = c('Prairie', 'Savanna', 'Forest'),
                       values = pal) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text = element_text(size = 14, face = 'bold'),
          legend.title = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12))
}

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

# Figure S8
if(type == 'all'){
  # Plot sensitivity
  for_plotting2 <- sens |> 
    filter(covar != 'HydricNo') |>
    filter(covar != 'FloodplainNo')
  
  for_plotting2 |>
    ggplot() +
    geom_boxplot(aes(x = reorder(covar, mean, decreasing = F), 
                     ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper, 
                     color = reorder(covar, mean, decreasing = T)), stat = 'identity', show.legend = F) +
    coord_flip() +
    xlab('') + ylab(expression(paste('Sensitivity (', hat(F) ,')'))) +
    theme_minimal() +
    scale_color_manual(values = c('#88ccee', # temperature - climate
                                  '#88ccee', # precipitation - climate
                                  '#999932', # cation exchange capacity - soil
                                  '#aa4499', # floodplain - topography
                                  '#999932', # soil % clay - soil
                                  '#999932', # hydric soil - soil
                                  '#999932', # CaCO3 - soil
                                  '#999932', # available water content - soil
                                  '#999932', # soil % sand - soil
                                  '#aa4499', # saga wetness index - topography
                                  '#aa4499' # slope - topography
    ), name = '') +
    scale_x_discrete(labels = c('totalPPT' = 'Precipitation', 'MeanTEMP' = 'Temperature',
                                'CEC' = 'Cation exchange capacity', 'CLA' = 'Soil % clay',
                                'FloodplainYes' = 'Floodplain', 'HydricYes' = 'Hydric soil',
                                'CAC' = expression(paste('CaC',O[3])), 'WAT' = 'Available water content',
                                'SAN' = 'Soil % sand', 'mean.SWI' = 'Saga Wetness Index', 'Slope' = 'Slope')) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
}

# Figure 3
if(type == 'reduced'){
  for_plotting2 <- sens |>
    filter(covar != 'HydricNo') |>
    filter(covar != 'FloodplainNo')
  
  for_plotting2 |>
    ggplot() +
    geom_boxplot(aes(x = reorder(covar, mean, decreasing = F),
                     ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper,
                     color = reorder(covar, mean, decreasing = T)), stat = 'identity', show.legend = F) +
    coord_flip() +
    xlab('') + ylab(expression(paste('Sensitivity (',hat(F),')'))) +
    theme_minimal() +
    scale_color_manual(values = c('#88ccee', # precipitation - climate
                                  '#999932', # floodplain - topography
                                  '#88ccee', # temperature - climate
                                  '#999932', # hydric soil - soil
                                  '#999932', # caco3 - soil
                                  '#999932', # soil % clay - soil
                                  '#aa4499', # saga wetness index - topography
                                  '#999932', # available water content - soil
                                  '#999932', # cation exchange capacity - soil
                                  '#999932', # soil % sand - soil
                                  '#aa4499' # slope - topography
                                  ), name = '') +
    scale_x_discrete(labels = c('totalPPT' = 'Precipitation', 'MeanTEMP' = 'Temperature',
                                'CEC' = 'Cation exchange capacity', 'CLA' = 'Soil % clay',
                                'FloodplainYes' = 'Floodplain', 'HydricYes' = 'Hydric soil',
                                'CAC' = expression(paste('CaC',O[3])), 'WAT' = 'Available water content',
                                'SAN' = 'Soil % sand', 'mean.SWI' = 'Saga Wetness Index', 'Slope' = 'Slope')) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
}

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
if(type == 'all'){
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
}
if(type == 'reduced'){
  ind <- rbind(c(1, 2, 3),
               c(2, 4, 5),
               c(3, 5, 6))
}

# Now we format the output into a matrix
corr_mat <- mean_sgibbs[ind]
corr_mat <- matrix(corr_mat, nrow = sqrt(length(corr_mat)), ncol = sqrt(length(corr_mat)))
corr_mat <- cov2cor(corr_mat)
if(type == 'all'){
  colnames(corr_mat) <- rownames(corr_mat) <- c('No Tree', 'Oak', 'Elm', 'Hickory', 'Ash', 'Maple', 'Basswood', 'Walnut', 'Ironwood', 'Beech', 'Dogwood', 'Poplar/Tulip Poplar', 'Black Gum/Sweet Gum', 'Other Conifer', 'Other Hardwood')
}
if(type == 'reduced'){
  colnames(corr_mat) <- rownames(corr_mat) <- c('Prairie', 'Savanna', 'Forest')
}

# Specify color palette

pal <- c('#364b9a', '#4a7bb7', '#6ea6cd', '#93cae1', '#cde4ef', 
         '#eaeccc', '#feda8b', '#fdb336', '#f67e4b', '#dd3d2d', '#a50026')

# upper and lower credible intervals
low_mat <- lower_sgibbs[ind]
low_mat <- matrix(low_mat, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
low_mat <- cov2cor(low_mat)
colnames(low_mat) <- rownames(low_mat) <- colnames(corr_mat)

upp_mat <- upper_sgibbs[ind]
upp_mat <- matrix(upp_mat, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
upp_mat <- cov2cor(upp_mat)
colnames(upp_mat) <- rownames(upp_mat) <- colnames(corr_mat)

# Figure 5 & Figure S10
# Plot with uncertainty
corrplot(corr_mat, lowCI.mat = low_mat, uppCI.mat = upp_mat, plotCI = 'circle',
         diag = F, type = 'upper', col = rev(pal), tl.col = 'black', tl.cex = 1.4)
