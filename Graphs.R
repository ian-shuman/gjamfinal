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
  mutate(iter = as.numeric(iter)) |>
  pivot_longer(cols = Prairie_totalPPT:Savanna_MeanTEMP, names_to = "beta", values_to = "estimate") |>
  filter(iter > 200) |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free')

bgibbs |>
  mutate(iter = as.numeric(iter)) |>
  pivot_longer(cols = Prairie_intercept:Savanna_MeanTEMP, names_to = "beta", values_to = "estimate") |>
  filter(iter > 200) |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free')

bgibbsUn |>
  mutate(iter = as.numeric(iter)) |>
  pivot_longer(cols = Prairie_intercept:Savanna_MeanTEMP, names_to = "beta", values_to = "estimate") |>
  filter(iter > 200) |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free')

fSensGibbs |>
  mutate(iter = as.numeric(iter)) |>
  pivot_longer(cols = totalPPT:MeanTEMP, names_to = 'f', values_to = 'estimate') |>
  filter(iter > 200) |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~f, scales = 'free')

sgibbs |>
  mutate(iter = as.numeric(iter)) |>
  pivot_longer(cols = Prairie_Prairie:Savanna_Savanna, names_to = 'sigma', values_to = 'estimate') |>
  filter(iter > 200) |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~sigma, scales = 'free')

### Let's take a look at correlations
mean_cov <- sgibbs |>
  select(-(chain:iter)) |>
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
  select(-(chain:iter)) |>
  summarize_all(sd)

sd_cov <- as.matrix(sd_cov)
row1 <- sd_cov[,1:3]
row2 <- sd_cov[,c(2,4:5)]
row3 <- sd_cov[,c(3,5:6)]

sd_cov <- rbind(row1, row2, row3)
colnames(sd_cov) <- c('Prairie', 'Forest', 'Savanna')
rownames(sd_cov) <- c('Prairie', 'Forest', 'Savanna')

lower_mat <- sgibbs |>
  select(-(chain:iter)) |>
  summarize_all(funs(quantile(., probs = 0.05)))
upper_mat <- sgibbs |>
  select(-(chain:iter)) |>
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
Prairie_totalPPT <- as.data.frame(bFacGibbs |> filter(iter > 250) |> select(Prairie_totalPPT))
Prairie_MeanTEMP <- as.data.frame(bFacGibbs |> filter(iter > 250) |> select(Prairie_MeanTEMP))
Forest_totalPPT <- as.data.frame(bFacGibbs |> filter(iter > 250) |> select(Forest_totalPPT))
Forest_MeanTEMP <- as.data.frame(bFacGibbs |> filter(iter > 250) |> select(Forest_MeanTEMP))
Savanna_totalPPT <- as.data.frame(bFacGibbs |> filter(iter > 250) |> select(Savanna_totalPPT))
Savanna_MeanTEMP <- as.data.frame(bFacGibbs |> filter(iter > 250) |> select(Savanna_MeanTEMP))

betas <- matrix(, nrow = 4, ncol = 6)

colnames(betas) <- c('PrairiePPT', 'PrairieT', 'ForestPPT', 'ForestT', 'SavannaPPT', 'SavannaT')
rownames(betas) <- c('Mean', 'SD', 'Lower', 'Upper')

temp <- Prairie_totalPPT |>
  summarize(mean = mean(Prairie_totalPPT),
            sd = sd(Prairie_totalPPT),
            lower = quantile(Prairie_totalPPT, probs = 0.05),
            upper = quantile(Prairie_totalPPT, probs = 0.95))
betas[1:4,1] <- as.vector(as.matrix(temp))

temp <- Prairie_MeanTEMP |>
  summarize(mean = mean(Prairie_MeanTEMP),
            sd = sd(Prairie_MeanTEMP),
            lower = quantile(Prairie_MeanTEMP, probs = 0.05),
            upper = quantile(Prairie_MeanTEMP, probs = 0.95))
betas[1:4,2] <- as.vector(as.matrix(temp))

temp <- Forest_totalPPT |>
  summarize(mean = mean(Forest_totalPPT),
            sd = sd(Forest_totalPPT),
            lower = quantile(Forest_totalPPT, probs = 0.05),
            upper = quantile(Forest_totalPPT, probs = 0.95))
betas[1:4,3] <- as.vector(as.matrix(temp))

temp <- Forest_MeanTEMP |>
  summarize(mean = mean(Forest_MeanTEMP),
            sd = sd(Forest_MeanTEMP),
            lower = quantile(Forest_MeanTEMP, probs = 0.05),
            upper = quantile(Forest_MeanTEMP, probs = 0.95))
betas[1:4,4] <- as.vector(as.matrix(temp))

temp <- Savanna_totalPPT |>
  summarize(mean = mean(Savanna_totalPPT),
            sd = sd(Savanna_totalPPT),
            lower = quantile(Savanna_totalPPT, probs = 0.05),
            upper = quantile(Savanna_totalPPT, probs = 0.95))
betas[1:4,5] <- as.vector(as.matrix(temp))

temp <- Savanna_MeanTEMP |>
  summarize(mean = mean(Savanna_MeanTEMP),
            sd = sd(Savanna_MeanTEMP),
            lower = quantile(Savanna_MeanTEMP, probs = 0.05),
            upper = quantile(Savanna_MeanTEMP, probs = 0.95))
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
