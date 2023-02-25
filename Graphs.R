rm(list = ls())

library(tidyverse)
library(corrplot)
library(cowplot)

load('out/reduced_taxa-all_cov+latlong.RData')

### Trace plots

## Quantities in "chains" portion of output
## bFacGibbs: standardized beta means (betaStandXmu)
## bgibbs: beta means (betaMu)
## bgibbsUn: (betaMuUn)
## fSensGibbs: sensitivity (fMu)
## sgibbs: covariance (sigMu)

bFacGibbs <- out$chains$bFacGibbs
bgibbs <- out$chains$bgibbs
bgibbsUn <- out$chains$bgibbsUn
fSensGibbs <- out$chains$fSensGibbs
sgibbs <- out$chains$sgibbs

bFacGibbs <- as.data.frame(bFacGibbs)

bFacGibbs |>
  rownames_to_column() |>
  mutate(rowname = as.numeric(rowname)) |>
  pivot_longer(cols = Prairie_totalPPT:Savanna_MeanTEMP, names_to = "beta", values_to = "estimate") |>
  filter(rowname > 200) |>
  ggplot(aes(x = rowname, y = estimate)) +
  geom_line() +
  facet_wrap(~beta, scales = 'free')

bgibbs <- as.data.frame(bgibbs)

bgibbs |>
  rownames_to_column() |>
  mutate(rowname = as.numeric(rowname)) |>
  pivot_longer(cols = Prairie_intercept:Savanna_MeanTEMP, names_to = "beta", values_to = "estimate") |>
  filter(rowname > 200) |>
  ggplot(aes(x = rowname, y = estimate)) +
  geom_line() +
  facet_wrap(~beta, scales = 'free')

bgibbsUn <- as.data.frame(bgibbsUn)

bgibbsUn |>
  rownames_to_column() |>
  mutate(rowname = as.numeric(rowname)) |>
  pivot_longer(cols = Prairie_intercept:Savanna_MeanTEMP, names_to = "beta", values_to = "estimate") |>
  filter(rowname > 200) |>
  ggplot(aes(x = rowname, y = estimate)) +
  geom_line() +
  facet_wrap(~beta, scales = 'free')

fSensGibbs <- as.data.frame(fSensGibbs)

fSensGibbs |>
  rownames_to_column() |>
  mutate(rowname = as.numeric(rowname)) |>
  pivot_longer(cols = totalPPT:MeanTEMP, names_to = 'f', values_to = 'estimate') |>
  filter(rowname > 200) |>
  ggplot(aes(x = rowname, y = estimate)) +
  geom_line() +
  facet_wrap(~f, scales = 'free')

sgibbs <- as.data.frame(sgibbs)

sgibbs |>
  rownames_to_column() |>
  mutate(rowname = as.numeric(rowname)) |>
  pivot_longer(cols = Prairie_Prairie:Savanna_Savanna, names_to = 'sigma', values_to = 'estimate') |>
  filter(rowname > 200) |>
  ggplot(aes(x = rowname, y = estimate)) +
  geom_line() +
  facet_wrap(~sigma, scales = 'free')

### Let's take a look at correlations
mean_cov <- sgibbs |>
  summarize_all(mean)

mean_cov <- as.matrix(mean_cov)
row1 <- mean_cov[,1:3]
row2 <- mean_cov[,c(2,4:5)]
row3 <- mean_cov[,c(3,5:6)]

mean_cov <- rbind(row1, row2, row3)
colnames(mean_cov) <- c('Prairie', 'Forest', 'Savanna')
rownames(mean_cov) <- c('Prairie', 'Forest', 'Savanna')

mean_cor <- cov2cor(mean_cov)

sd_cov <- sgibbs |>
  summarize_all(sd)

sd_cov <- as.matrix(sd_cov)
row1 <- sd_cov[,1:3]
row2 <- sd_cov[,c(2,4:5)]
row3 <- sd_cov[,c(3,5:6)]

sd_cov <- rbind(row1, row2, row3)
colnames(sd_cov) <- c('Prairie', 'Forest', 'Savanna')
rownames(sd_cov) <- c('Prairie', 'Forest', 'Savanna')

lower_mat <- sgibbs |>
  summarize_all(funs(quantile(., probs = 0.05)))
upper_mat <- sgibbs |>
  summarize_all(funs(quantile(., probs = 0.95)))

lower_mat <- as.matrix(lower_mat)
row1 <- lower_mat[,1:3]
row2 <- lower_mat[,c(2,4:5)]
row3 <- lower_mat[,c(3,5:6)]
lower_mat <- rbind(row1, row2, row3)
lower_mat <- cov2cor(lower_mat)
upper_mat <- as.matrix(upper_mat)
row1 <- upper_mat[,1:3]
row2 <- upper_mat[,c(2,4:5)]
row3 <- upper_mat[,c(3,5:6)]
upper_mat <- rbind(row1, row2, row3)
upper_mat <- cov2cor(upper_mat)

corrplot(mean_cor, lowCI.mat = lower_mat, uppCI.mat = upper_mat, diag = T, plotCI = 'circ')

### Correlations between taxa and drivers
Prairie_totalPPT <- as.data.frame(bFacGibbs$Prairie_totalPPT)
Prairie_MeanTEMP <- as.data.frame(bFacGibbs$Prairie_MeanTEMP)
Forest_totalPPT <- as.data.frame(bFacGibbs$Forest_totalPPT)
Forest_MeanTEMP <- as.data.frame(bFacGibbs$Forest_MeanTEMP)
Savanna_totalPPT <- as.data.frame(bFacGibbs$Savanna_totalPPT)
Savanna_MeanTEMP <- as.data.frame(bFacGibbs$Savanna_MeanTEMP)

betas <- matrix(, nrow = 4, ncol = 6)

colnames(betas) <- c('PrairiePPT', 'PrairieT', 'ForestPPT', 'ForestT', 'SavannaPPT', 'SavannaT')
rownames(betas) <- c('Mean', 'SD', 'Lower', 'Upper')

temp <- Prairie_totalPPT |>
  rownames_to_column() |>
  filter(rowname > 250) |>
  column_to_rownames(var = 'rowname') |>
  summarize(mean = mean(`bFacGibbs$Prairie_totalPPT`),
            sd = sd(`bFacGibbs$Prairie_totalPPT`),
            lower = quantile(`bFacGibbs$Prairie_totalPPT`, probs = 0.05),
            upper = quantile(`bFacGibbs$Prairie_totalPPT`, probs = 0.95))
betas[1:4,1] <- as.vector(as.matrix(temp))

temp <- Prairie_MeanTEMP |>
  rownames_to_column() |>
  filter(rowname > 250) |>
  column_to_rownames(var = 'rowname') |>
  summarize(mean = mean(`bFacGibbs$Prairie_MeanTEMP`),
            sd = sd(`bFacGibbs$Prairie_MeanTEMP`),
            lower = quantile(`bFacGibbs$Prairie_MeanTEMP`, probs = 0.05),
            upper = quantile(`bFacGibbs$Prairie_MeanTEMP`, probs = 0.95))
betas[1:4,2] <- as.vector(as.matrix(temp))

temp <- Forest_totalPPT |>
  rownames_to_column() |>
  filter(rowname > 250) |>
  column_to_rownames(var = 'rowname') |>
  summarize(mean = mean(`bFacGibbs$Forest_totalPPT`),
            sd = sd(`bFacGibbs$Forest_totalPPT`),
            lower = quantile(`bFacGibbs$Forest_totalPPT`, probs = 0.05),
            upper = quantile(`bFacGibbs$Forest_totalPPT`, probs = 0.95))
betas[1:4,3] <- as.vector(as.matrix(temp))

temp <- Forest_MeanTEMP |>
  rownames_to_column() |>
  filter(rowname > 250) |>
  column_to_rownames(var = 'rowname') |>
  summarize(mean = mean(`bFacGibbs$Forest_MeanTEMP`),
            sd = sd(`bFacGibbs$Forest_MeanTEMP`),
            lower = quantile(`bFacGibbs$Forest_MeanTEMP`, probs = 0.05),
            upper = quantile(`bFacGibbs$Forest_MeanTEMP`, probs = 0.95))
betas[1:4,4] <- as.vector(as.matrix(temp))

temp <- Savanna_totalPPT |>
  rownames_to_column() |>
  filter(rowname > 250) |>
  column_to_rownames(var = 'rowname') |>
  summarize(mean = mean(`bFacGibbs$Savanna_totalPPT`),
            sd = sd(`bFacGibbs$Savanna_totalPPT`),
            lower = quantile(`bFacGibbs$Savanna_totalPPT`, probs = 0.05),
            upper = quantile(`bFacGibbs$Savanna_totalPPT`, probs = 0.95))
betas[1:4,5] <- as.vector(as.matrix(temp))

temp <- Savanna_MeanTEMP |>
  rownames_to_column() |>
  filter(rowname > 250) |>
  column_to_rownames(var = 'rowname') |>
  summarize(mean = mean(`bFacGibbs$Savanna_MeanTEMP`),
            sd = sd(`bFacGibbs$Savanna_MeanTEMP`),
            lower = quantile(`bFacGibbs$Savanna_MeanTEMP`, probs = 0.05),
            upper = quantile(`bFacGibbs$Savanna_MeanTEMP`, probs = 0.95))
betas[1:4,6] <- as.vector(as.matrix(temp))

p1 <- ggplot() +
  geom_boxplot(aes(x = c('Precipitation', 'Temperature'), ymin = betas[3,1:2], lower = betas[1,1:2] - betas[2,1:2], middle = betas[1,1:2], upper = betas[1,1:2] + betas[2,1:2], ymax = betas[4,1:2]), stat = 'identity') +
  ggtitle('Prairie') +
  xlab('') + ylab(expression(beta)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  ylim(c(-1,1))

p2 <- ggplot() +
  geom_boxplot(aes(x = c('Precipitation', 'Temperature'), ymin = betas[3,3:4], lower = betas[1,3:4] - betas[2,3:4], middle = betas[1,3:4], upper = betas[1,3:4] + betas[2,3:4], ymax = betas[4,3:4]), stat = 'identity') +
  ggtitle('Forest') +
  xlab('') + ylab(expression(beta)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  ylim(c(-1,1))

p3 <- ggplot() +
  geom_boxplot(aes(x = c('Precipitation', 'Temperature'), ymin = betas[3,5:6], lower = betas[1,5:6] - betas[2,5:6], middle = betas[1,5:6], upper = betas[1,5:6] + betas[2,5:6], ymax = betas[4,5:6]), stat = 'identity') +
  ggtitle('Savanna') +
  xlab('') + ylab(expression(beta)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  ylim(c(-1,1))

plot_grid(p1, p2, p3, nrow = 1)
