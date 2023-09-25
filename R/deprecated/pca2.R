library(factoextra)
library(ggfortify)
library(logisticPCA)
library(tidyverse)

pca_x <- xdata |>
  select(-c(long, lat, Hydric, Floodplain, marea, mean.AspectProjected))

pc_out <- prcomp(pca_x, scale = T)

autoplot(pc_out)

fviz_eig(pc_out)
#fviz_pca_ind(pc_out)
fviz_pca_var(pc_out)

logsvd_model = logisticSVD(ydata, k = 2)
logsvd_model

plot.lsvd(logsvd_model, type = 'trace')
plot.lsvd(logsvd_model, type = 'loadings')

logpca_model <- logisticPCA(ydata, k = 2)
logpca_model

plot.lpca(logpca_model, type = 'trace')
plot.lpca(logpca_model, type = 'loadings')
