rm(list = ls())

library(tidyverse)
library(corrplot)
library(cowplot)

load('out/all_taxa-all_cov_latlong.RData')
out1 <- out
#load('out/reduced_taxa-all_cov+latlong.RData')
#load('out/reduced_taxa-temp+precip_1.RData')
#out1 <- out
#load('out/reduced_taxa-temp+precip_2.RData')
#out2 <- out

### Trace plots

## Quantities in "chains" portion of output
## bFacGibbs: standardized beta means (betaStandXmu)
## bgibbs: beta means (betaMu)
## bgibbsUn: (betaMuUn)
## fSensGibbs: sensitivity (fMu)
## sgibbs: covariance (sigMu)

bFacGibbs <- out1$chains$bFacGibbs
bFacGibbs <- as.data.frame(bFacGibbs)
bFacGibbs$chain <- rep(1, times = nrow(bFacGibbs))
bFacGibbs$iter <- rownames(bFacGibbs)
bFacGibbs2 <- out2$chains$bFacGibbs
bFacGibbs2 <- as.data.frame(bFacGibbs2)
bFacGibbs2$chain <- rep(2, times = nrow(bFacGibbs2))
bFacGibbs2$iter <- rownames(bFacGibbs2)
bFacGibbs <- rbind(bFacGibbs, bFacGibbs2)

bgibbs <- out1$chains$bgibbs
bgibbs <- as.data.frame(bgibbs)
bgibbs$chain <- rep(1, times = nrow(bgibbs))
bgibbs$iter <- rownames(bgibbs)
bgibbs2 <- out2$chains$bgibbs
bgibbs2 <- as.data.frame(bgibbs2)
bgibbs2$chain <- rep(2, times = nrow(bgibbs2))
bgibbs2$iter <- rownames(bgibbs2)
bgibbs <- rbind(bgibbs, bgibbs2)

bgibbsUn <- out1$chains$bgibbsUn
bgibbsUn <- as.data.frame(bgibbsUn)
bgibbsUn$chain <- rep(1, times = nrow(bgibbsUn))
bgibbsUn$iter <- rownames(bgibbsUn)
bgibbsUn2 <- out2$chains$bgibbsUn
bgibbsUn2 <- as.data.frame(bgibbsUn2)
bgibbsUn2$chain <- rep(2, times = nrow(bgibbsUn2))
bgibbsUn2$iter <- rownames(bgibbsUn2)
bgibbsUn <- rbind(bgibbsUn, bgibbsUn2)

fSensGibbs <- out1$chains$fSensGibbs
fSensGibbs <- as.data.frame(fSensGibbs)
fSensGibbs$chain <- rep(1, times = nrow(fSensGibbs))
fSensGibbs$iter <- rownames(fSensGibbs)
fSensGibbs2 <- out2$chains$fSensGibbs
fSensGibbs2 <- as.data.frame(fSensGibbs2)
fSensGibbs2$chain <- rep(2, times = nrow(fSensGibbs2))
fSensGibbs2$iter <- rownames(fSensGibbs2)
fSensGibbs <- rbind(fSensGibbs, fSensGibbs2)

sgibbs <- out1$chains$sgibbs
sgibbs <- as.data.frame(sgibbs)
sgibbs$chain <- rep(1, times = nrow(sgibbs))
sgibbs$iter <- rownames(sgibbs)
sgibbs2 <- out2$chains$sgibbs
sgibbs2 <- as.data.frame(sgibbs2)
sgibbs2$chain <- rep(2, times = nrow(sgibbs2))
sgibbs2$iter <- rownames(sgibbs2)
sgibbs <- rbind(sgibbs, sgibbs2)

bFacGibbs |>
  pivot_longer(cols = No.tree_mean.SlopeProjected:Other.hardwood_FloodplainYes, names_to = 'beta', values_to = 'estimate') |>
  mutate(taxon = sub('_.*', '', beta),
         variable = sub('.*_', '', beta)) |>
  mutate(iter = as.numeric(iter)) |>
  filter(iter > 200) |>
  ggplot(aes(x = iter, y = estimate, color = taxon)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free')

bgibbs |>
  pivot_longer(cols = No.tree_mean.SlopeProjected:Other.hardwood_FloodplainYes, names_to = 'beta', values_to = 'estimate') |>
  mutate(taxon = sub('_.*', '', beta),
         variable = sub('.*_', '', beta)) |>
  mutate(iter = as.numeric(iter)) |>
  filter(iter > 200) |>
  ggplot(aes(x = iter, y = estimate, color = taxon)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free')

bgibbsUn |>
  pivot_longer(cols = No.tree_mean.SlopeProjected:Other.hardwood_FloodplainYes, names_to = 'beta', values_to = 'estimate') |>
  mutate(taxon = sub('_.*', '', beta),
         variable = sub('.*_', '', beta)) |>
  mutate(iter = as.numeric(iter)) |>
  filter(iter > 200) |>
  ggplot(aes(x = iter, y = estimate, color = taxon)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free')

fSensGibbs |>
  pivot_longer(cols = mean.SlopeProjected:FloodplainYes, names_to = 'F', values_to = 'estimate') |>
  mutate(iter = as.numeric(iter)) |>
  filter(iter > 200) |>
  ggplot(aes(x = iter, y = estimate)) +
  geom_line() +
  facet_wrap(~F, scales = 'free')

ints = sample(colnames(sgibbs), 5, replace = F)

sgibbs |>
  pivot_longer(cols = No.tree_No.tree:Other.hardwood_Other.hardwood, names_to = 'sigma', values_to = 'estimate') |>
  mutate(iter = as.numeric(iter)) |>
  filter(iter > 200) |>
  filter(sigma %in% c(ints, iter)) |> 
  ggplot(aes(x = iter, y = estimate)) +
  geom_line() +
  facet_wrap(~sigma, scales = 'free')

## Correlations between taxa and drivers
bFacGibbs_corr <- bFacGibbs[201:1000,1:240]
corr_mean <- apply(bFacGibbs_corr, 2, mean, na.rm = T)
corr_sd <- apply(bFacGibbs_corr, 2, sd, na.rm = T)
corr_lower <- apply(bFacGibbs_corr, 2, quantile, probs = 0.025, na.rm = T)
corr_upper <- apply(bFacGibbs_corr, 2, quantile, probs = 0.975, na.rm = T)

corr <- rbind(corr_mean, corr_sd, corr_lower, corr_upper)
rownames(corr) <- c('mean', 'sd', 'lower', 'upper')

corr <- t(corr)

corr <- as.data.frame(corr)

corr <- corr |>
  rownames_to_column(var = 'beta') |>
  mutate(taxon = sub('_.*', '', beta),
         covariate = sub('.*_', '', beta))

corr |>
  ggplot() +
  geom_boxplot(aes(x = taxon, ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper, color = taxon), stat = 'identity') +
  facet_wrap(~covariate, scales = 'free_y')

## Correlations between taxa

sgibbs_corr <- sgibbs[201:1000, 1:120]
mean_sgibbs <- apply(sgibbs_corr, 2, mean)
sd_sgibbs <- apply(sgibbs_corr, 2, sd)
lower_sgibbs <- apply(sgibbs_corr, 2, quantile, probs = 0.025)
upper_sgibbs <- apply(sgibbs_corr, 2, quantile, probs = 0.975)

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

corr_mat <- mean_sgibbs[ind]
corr_mat <- matrix(corr_mat, nrow = 15, ncol = 15)
corr_mat <- cov2cor(corr_mat)
colnames(corr_mat) <- rownames(corr_mat) <- c('No Tree', 'Oak', 'Elm', 'Hickory', 'Ash', 'Maple', 'Basswood', 'Walnut', 'Ironwood', 'Beech', 'Dogwood', 'Poplar/Tulip Poplar', 'Black Gum/Sweet Gum', 'Other Conifer', 'Other Hardwood')
corrplot(corr_mat, diag = T)
