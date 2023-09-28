## Different options for visualizing correlation structure

## Author: AM Willson

rm(list = ls())

library(dplyr)
library(ggplot2)
library(corrplot)

#### Ecosystem level ####

### NEED TO CHANGE WHEN FINAL RUNS COME BACK
load('out/reduced_taxa-all_cov/combined.RData')

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
ind <- c(1, 3, 2,
         3, 6, 5,
         2, 5, 4)

# Now we format the output into a matrix
corr_mat <- mean_sgibbs[ind]
corr_mat <- matrix(corr_mat, nrow = 3, ncol = 3)
corr_mat <- cov2cor(corr_mat)
colnames(corr_mat) <- rownames(corr_mat) <- c('Prairie', 'Savanna', 'Forest')

# Specify color palette
pal <- c('#364b9a', '#4a7bb7', '#6ea6cd', '#93cae1', '#cde4ef', 
         '#eaeccc', '#feda8b', '#fdb336', '#f67e4b', '#dd3d2d', '#a50026')

## Option 1: circle method, no uncertainty
corrplot(corr_mat, method = 'circle', type = 'upper', col = pal, 
         diag = F, tl.col = 'black', tl.cex = 1.5)

## Option 2: square method, no uncertainty
corrplot(corr_mat, method = 'square', type = 'upper', col = pal,
         diag = F, tl.col = 'black', tl.cex = 1.5)

## Option 3: ellipse method, no uncertainty
corrplot(corr_mat, method = 'ellipse', type = 'upper', col = pal,
         diag = F, tl.col = 'black', tl.cex = 1.5)

## Option 4: number method, no uncertainty
corrplot(corr_mat, method = 'number', type = 'upper', col = pal,
         diag = F, tl.col = 'black', tl.cex = 1.5)

## Option 5: shade method, no uncertainty
corrplot(corr_mat, method = 'shade', type = 'upper', col = pal,
         diag = F, tl.col = 'black', tl.cex = 1.5)

## Option 6: color method, no uncertainty
corrplot(corr_mat, method = 'color', type = 'upper', col = pal,
         diag = F, tl.col = 'black', tl.cex = 1.5)

## Option 7: pie method, no uncertainty
corrplot(corr_mat, method = 'pie', type = 'upper', col = pal,
         diag = F, tl.col = 'black', tl.cex = 1.5)

# With upper and lower credible intervals
low_mat <- lower_sgibbs[ind]
low_mat <- matrix(low_mat, nrow = 3, ncol = 3)
low_mat <- cov2cor(low_mat)
colnames(low_mat) <- rownames(low_mat) <- c('Prairie', 'Savanna', 'Forest')

upp_mat <- upper_sgibbs[ind]
upp_mat <- matrix(upp_mat, nrow = 3, ncol = 3)
upp_mat <- cov2cor(upp_mat)
colnames(upp_mat) <- rownames(upp_mat) <- c('Prairie', 'Savanna', 'Forest')

## Option 8: n plotCI
corrplot(corr_mat, lowCI.mat = low_mat, uppCI.mat = upp_mat,
         plotCI = 'n', type = 'upper', col = pal, diag = F,
         tl.col = 'black', tl.cex = 1.5)

## Option 9: square plotCI
corrplot(corr_mat, lowCI.mat = low_mat, uppCI.mat = upp_mat,
         plotCI = 'square', type = 'upper', col = pal, diag = F,
         tl.col = 'black', tl.cex = 1.5)

## Option 10: circle plotCI
corrplot(corr_mat, lowCI.mat = low_mat, uppCI.mat = upp_mat,
         plotCI = 'circle', type = 'upper', col = pal, diag = F,
         tl.col = 'black', tl.cex = 1.5)

## Option 11: rect plotCI
corrplot(corr_mat, lowCI.mat = low_mat, uppCI.mat = upp_mat,
         plotCI = 'rect', type = 'upper', col = pal, diag = F,
         tl.col = 'black', tl.cex = 1.5)

# Calculate p-values
sd_mat <- sd_sgibbs[ind]
sd_mat <- matrix(sd_mat, ncol = 3, nrow = 3)
sd_mat <- cov2cor(sd_mat)
se <- sd_mat / sqrt(2250)
z <- corr_mat / se
p_mat <- 2*pnorm(-abs(z))

## Option 12: p-values, black out insignificant
corrplot(corr_mat, method = 'circle', p.mat = p_mat, sig.level = 0.001,
         insig = 'blank', type = 'upper', col = pal,
         diag = F, tl.col = 'black', tl.cex = 1.5)

## Option 13: p-values, add asterisks for significance
corrplot(corr_mat, method = 'circle', p.mat = p_mat, sig.level = 0.001,
         insig = 'label_sig', type = 'upper', col = pal,
         diag = F, tl.col = 'black', tl.cex = 1.5)
