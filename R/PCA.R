rm(list = ls())

library(ggfortify)
library(logisticPCA)

# Data processed in steps 1 & 1.5
load('GJAM DATA/process2.RData')

#formula <- as.formula(~No.tree + Oak + Elm + Hickory + Ash + Maple + Basswood + Walnut + 
#                        Black.gum.sweet.gum + Ironwood + Beech + Dogwood + Poplar.tulip.poplar + 
#                        Other.conifer + Other.hardwood)
#data <- ydata

#pca <- prcomp(formula = formula, data  = data, scale = FALSE, center = FALSE)

# One option for pca with binary data
pca <- logisticPCA(ydata, k = 2)

# Autoplot for ggplot
autoplot(pca)

# Ian ran a PCA using a different package
load('~/Downloads/zibppca.RData')

# Extract loadings from output
loadings <- zibppca.result$loadings

loadings <- as.data.frame(loadings)
colnames(loadings) <- c('PC1', 'PC2', 'PC3', 'PC4', 'PC5', 'PC6',
                        'PC7', 'PC8', 'PC9', 'PC10', 'PC11', 'PC12',
                        'PC13', 'PC14', 'PC15')

# Plot with the default species
loadings |>
  select(PC1:PC2) |>
  rownames_to_column() |>
  rename(Taxon = rowname) |>
  ggplot(aes(x = PC1, y = PC2, color = Taxon)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) +
  theme_minimal()

# Assign species to categories
prairie <- c('No.tree')
savanna <- c('Hickory', 'Oak')
late <- c('Beech', 'Dogwood', 'Elm', 'Ironwood', 'Maple')
early <- c('Ash', 'Basswood', 'Black.gum.sweet.gum', 'Poplar.tulip.poplar',
           'Walnut')
conifer <- c('Other.conifer')
hardwood <- c('Other.hardwood')

# Plot with reduced groupings
loadings |>
  rownames_to_column() |>
  rename(Taxon = rowname) |>
  mutate(Group = 'na') |>
  mutate(Group = if_else(Taxon %in% prairie, 'Prairie', Group),
         Group = if_else(Taxon %in% savanna, 'Savanna', Group),
         Group = if_else(Taxon %in% late, 'Late Successional', Group),
         Group = if_else(Taxon %in% early, 'Early Successional', Group),
         Group = if_else(Taxon %in% conifer, 'Conifer', Group),
         Group = if_else(Taxon %in% hardwood, 'Other Hardwood', Group)) |>
  ggplot(aes(x = PC1, y = PC2, color = Group)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) +
  theme_minimal() +
  scale_color_brewer(palette = 'Dark2')

# Extract eigenvalues
eigen <- zibppca.result$eigenvalues
var_explained <- eigen / sum(eigen)
var_explained <- var_explained * 100

# Add to figure
loadings |>
  rownames_to_column() |>
  rename(Taxon = rowname) |>
  mutate(Group = 'na') |>
  mutate(Group = if_else(Taxon %in% prairie, 'Prairie', Group),
         Group = if_else(Taxon %in% savanna, 'Savanna', Group),
         Group = if_else(Taxon %in% late, 'Late Successional', Group),
         Group = if_else(Taxon %in% early, 'Early Successional', Group),
         Group = if_else(Taxon %in% conifer, 'Conifer', Group),
         Group = if_else(Taxon %in% hardwood, 'Other Hardwood', Group)) |>
  ggplot(aes(x = PC1, y = PC2, color = Group)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) +
  theme_minimal() +
  scale_color_brewer(palette = 'Dark2') +
  xlab('PC1 (9.3% variance explained)') + ylab('PC2 (8.04% variance explained)')

