## Comparing GJAM's predictive power with logistic regression

rm(list = ls())

library(lme4)
library(tidyverse)
library(piecewiseSEM)

load('GJAM DATA/process2.RData')

# Prepare ydata to merge xdata and ydata into one data frame
ydata <- ydata |>
  rownames_to_column(var = 'ID') |>
  pivot_longer(cols = No.tree:Other.hardwood, 
               names_to = 'Taxon', 
               values_to = 'Presence')

# Prepare xdata and merge xdata and ydata
# We also center & scale the xdata to improve model fit
full_data <- xdata |>
  rownames_to_column(var = 'ID') |>
  mutate(mean.SlopeProjected = scale(mean.SlopeProjected, center = TRUE, scale = TRUE),
         mean.AspectProjected = scale(mean.AspectProjected, center = TRUE, scale = TRUE),
         mean.CAC = scale(mean.CAC, center = TRUE, scale = TRUE),
         mean.CEC = scale(mean.CEC, center = TRUE, scale = TRUE),
         mean.CLA = scale(mean.CLA, center = TRUE, scale = TRUE),
         mean.SAN = scale(mean.SAN, center = TRUE, scale = TRUE),
         mean.WAT = scale(mean.WAT, center = TRUE, scale = TRUE),
         mean.SWI = scale(mean.SWI, center = TRUE, scale = TRUE),
         totalPPT = scale(totalPPT, center = TRUE, scale = TRUE),
         MeanTEMP = scale(MeanTEMP, center = TRUE, scale = TRUE)) |>
  full_join(ydata, by = 'ID')

# Run generalized linear mixed effects model
# We are predicting presence of each taxon as a function of
# environmental drivers, with a random effect of taxon identity
# We use the binomial family becaue our presence/absence repsonse is binary
# The random effect is currently on the intercept because we don't have
# any a priori expectation for taxon to relate to the drivers
glm_mod <- glmer(formula = Presence ~ mean.SlopeProjected + mean.AspectProjected +
                mean.CAC + mean.CEC + mean.CLA + mean.SAN + mean.WAT +
                mean.SWI + Hydric + Floodplain + totalPPT + MeanTEMP +
                (1|Taxon), family = binomial, data = full_data)

# Compute rsquared to compare with gjam model
piecewiseSEM::rsquared(glm_mod)

# Look at summary
summary(glm_mod)

# Load out-of-sample data
load('GJAM DATA/Withheld For Validation/validation_process2.RData')

# Reformat data
new_ydata <- ydata |>
  rownames_to_column(var = 'ID') |>
  pivot_longer(cols = No.tree:Other.hardwood, 
               names_to = 'Taxon', 
               values_to = 'Presence')

new_xdata <- xdata |>
  rownames_to_column(var = 'ID') |>
  mutate(mean.SlopeProjected = scale(mean.SlopeProjected, center = TRUE, scale = TRUE),
         mean.AspectProjected = scale(mean.AspectProjected, center = TRUE, scale = TRUE),
         mean.CAC = scale(mean.CAC, center = TRUE, scale = TRUE),
         mean.CEC = scale(mean.CEC, center = TRUE, scale = TRUE),
         mean.CLA = scale(mean.CLA, center = TRUE, scale = TRUE),
         mean.SAN = scale(mean.SAN, center = TRUE, scale = TRUE),
         mean.WAT = scale(mean.WAT, center = TRUE, scale = TRUE),
         mean.SWI = scale(mean.SWI, center = TRUE, scale = TRUE),
         totalPPT = scale(totalPPT, center = TRUE, scale = TRUE),
         MeanTEMP = scale(MeanTEMP, center = TRUE, scale = TRUE)) |>
  full_join(new_ydata, by = 'ID') |>
  select(-Presence)

# Make predictions of out of sample data
glm_preds <- predict(glm_mod, new_xdata)

# Coombine with obeservations
obs_pred <- cbind(new_ydata, glm_preds)
colnames(obs_pred) <- c('ID', 'Taxon', 'Observed', 'Predicted')

# Let's look at how well we predicted
test1 <- glm(Observed ~ Predicted, family = binomial, data = obs_pred)
summary(test1)
with(summary(test1), 1 - deviance/null.deviance)

test2 <- glmer(Observed ~ Predicted + (1|Taxon), family = binomial, data = obs_pred)
summary(test2)
piecewiseSEM::rsquared(test2)

# And quick plots
obs_pred |>
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  facet_wrap(~Taxon)
