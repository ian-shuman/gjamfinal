## Combine output from multiple independent runs (= chains) of GJAM

## NOTE: All code will run with any of the 4 model choices. The only
## difference is the indexing for making traceplots. Each model
## has a different number of paramters, so the index must be changed
## once an error is thrown to see the last few columns of parameters.

## Author: AM Willson

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

# load R script with modified version of gelman statistic
source('R/utils.R')

# Specify which model you were using
# Options:
  # All_taxa~all_cov_ASPECT
  # All_taxa~all_cov_NOASPECT
  # Reduced_taxa~all_cov_ASPECT
  # Reduced_taxa~all_cov_NOASPECT

type <- 'All_taxa~all_cov_NOASPECT'

# List all the outputs we have for that model
files <- list.files(path = paste0('out/', type))
if('combined.RData' %in% files) files <- files[files != 'combined.RData']

# Pre-specifeid number of chains and interations
nchains <- length(files)
niter <- 1000

# Loop over the files
ind <- 1
for(i in files){
  file_name <- paste0('out/', type, '/', i)
  # Load file
  load(file_name)
  # Initialize with first file
  if(i == files[1]){
    # Save the chains for each of the outputs in separate matrices
    bFacGibbs <- out$chains$bFacGibbs
    bgibbs <- out$chains$bgibbs
    bgibbsUn <- out$chains$bgibbsUn
    fSensGibbs <- out$chains$fSensGibbs
    sgibbs <- out$chains$sgibbs
  }else{
    # Rbind if not the first file
    bFacGibbs <- rbind(bFacGibbs, out$chains$bFacGibbs)
    bgibbs <- rbind(bgibbs, out$chains$bgibbs)
    bgibbsUn <- rbind(bgibbsUn, out$chains$bgibbsUn)
    fSensGibbs <- rbind(fSensGibbs, out$chains$fSensGibbs)
    sgibbs <- rbind(sgibbs, out$chains$sgibbs)
  }
  ind <- ind + 1
}

# Formatting
bFacGibbs <- as.data.frame(bFacGibbs)
bgibbs <- as.data.frame(bgibbs)
bgibbsUn <- as.data.frame(bgibbsUn)
fSensGibbs <- as.data.frame(fSensGibbs)
sgibbs <- as.data.frame(sgibbs)

# Add chain and iteration indices to dataframes
# for plotting and manipulating later
bFacGibbs$chain <- bgibbs$chain <- bgibbsUn$chain <- fSensGibbs$chain <- sgibbs$chain <- rep(1:nchains, each = niter)
bFacGibbs$iter <- bgibbs$iter <- bgibbsUn$iter <- fSensGibbs$iter <- sgibbs$iter <- seq(from = 1, to = niter, by = 1)

# Remove burn in

# Specify your burn in period
burnin <- 200

# Remove burn in
bFacGibbs <- dplyr::filter(bFacGibbs, iter > burnin)
bgibbs <- dplyr::filter(bgibbs, iter > burnin)
bgibbsUn <- dplyr::filter(bgibbsUn, iter > burnin)
fSensGibbs <- dplyr::filter(fSensGibbs, iter > burnin)
sgibbs <- dplyr::filter(sgibbs, iter > burnin)

# Trace plots
bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[1:20], iter, chain)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[1:20],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
  ggplot2::geom_line(show.legend = F) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))

if(type == 'Reduced_taxa~all_cov_NOASPECT'){
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[21:39], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[21:39],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type %in% c('Reduced_taxa~all_cov_ASPECT', 'All_taxa~all_cov_ASPECT', 'All_taxa~all_cov_NOASPECT')){
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[21:40], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[21:40],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type == 'Reduced_taxa~all_cov_ASPECT'){
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[41:54], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[41:54],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type %in% c('All_taxa~all_cov_ASPECT', 'All_taxa~all_cov_NOASPECT')){
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[41:60], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[41:60],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[61:80], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[61:80],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[81:100], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[81:100],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[101:120], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[101:120],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[121:140], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[121:140],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[141:160], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[141:160],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[161:180], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[161:180],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type == 'All_taxa~all_cov_NOASPECT'){
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[181:195], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[181:195],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type == 'All_taxa~all_cov_ASPECT'){
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[181:200], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[181:200],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[201:220], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[201:220],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[221:240], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[221:240],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[241:260], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[241:260],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bFacGibbs |>
    dplyr::select(c(colnames(bFacGibbs)[261:270], iter, chain)) |>
    tidyr::pivot_longer(colnames(bFacGibbs)[261:270],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

# Calculate gelman rubin diagnostic
bFacGibbs_diag <- c()

for(i in 1:(ncol(bFacGibbs)-2)){
  bFacGibbs_diag[i] <- gelman_statistic(bFacGibbs[,i], bFacGibbs$chain)
}

bFacGibbs_diag <- as.data.frame(cbind(colnames(bFacGibbs)[1:(ncol(bFacGibbs)-2)], bFacGibbs_diag))

colnames(bFacGibbs_diag) <- c('Coefficient', 'Diagnostic Statistic')

# If < ~1.2, indicates chain convergence
print(tibble::tibble(bFacGibbs_diag), n = nrow(bFacGibbs_diag))

# Repeat for bgibbs
bgibbs |>
  dplyr::select(c(colnames(bgibbs)[1:20], iter, chain)) |>
  tidyr::pivot_longer(colnames(bgibbs)[1:20],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
  ggplot2::geom_line(show.legend = F) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))

if(type == 'Reduced_taxa~all_cov_NOAPSECT'){
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[21:36], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[21:36],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type %in% c('Reduced_taxa~all_cov_ASPECT', 'All_taxa~all_cov_ASPECT', 'All_taxa~all_cov_NOASPECT')){
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[21:40], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[21:40],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type == 'Reduced_taxa~all_cov_ASPECT'){
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[41:48], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[41:48],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type %in% c('All_taxa~all_cov_ASPECT', 'All_taxa~all_cov_NOASPECT')){
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[41:60], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[41:60],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[61:80], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[61:80],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[81:100], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[81:100],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[101:120], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[101:120],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[121:140], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[121:140],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[141:160], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[141:160],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[161:180], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[161:180],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type == 'All_taxa~all_cov_ASPECT'){
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[181:200], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[181:200],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[201:220], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[201:220],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbs |>
    dplyr::select(c(colnames(bgibbs)[221:240], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbs)[221:240],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

bgibbs_diag <- c()

for(i in 1:(ncol(bgibbs)-2)){
  bgibbs_diag[i] <- gelman_statistic(bgibbs[,i], bgibbs$chain)
}

bgibbs_diag <- as.data.frame(cbind(colnames(bgibbs)[1:(ncol(bgibbs)-2)], bgibbs_diag))

colnames(bgibbs_diag) <- c('Coefficient', 'Diagnostic Statistic')

print(tibble::tibble(bgibbs_diag), n = nrow(bgibbs_diag))

# Repeat for bgibbsUn
bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[1:20], iter, chain)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[1:20],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
  ggplot2::geom_line(show.legend = F) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))

if(type == 'Reduced_taxa~all_cov_NOASPECT'){
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[21:36], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[21:36],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type %in% c('Reduced_taxa~all_cov_ASPECT', 'All_taxa~all_cov_ASPECT', 'All_taxa~all_cov_NOASPECT')){
  bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[21:40], iter, chain)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[21:40],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
  ggplot2::geom_line(show.legend = F) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type == 'Reduced_taxa~all_cov_ASPECT'){
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[41:48], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[41:48],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type %in% c('All_taxa~all_cov_ASPECT', 'All_taxa~all_cov_NOASPECT')){
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[41:60], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[41:60],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[61:80], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[61:80],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[81:100], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[81:100],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[101:120], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[101:120],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[121:140], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[121:140],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[141:160], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[141:160],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[161:180], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[161:180],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type == 'All_taxa~all_cov_ASPECT'){
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[181:200], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[181:200],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[201:220], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[201:220],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  bgibbsUn |>
    dplyr::select(c(colnames(bgibbsUn)[221:240], iter, chain)) |>
    tidyr::pivot_longer(colnames(bgibbsUn)[221:240],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

bgibbsUn_diag <- c()

for(i in 1:(ncol(bgibbsUn) - 2)){
  bgibbsUn_diag[i] <- gelman_statistic(bgibbsUn[,i], bgibbsUn$chain)
}

bgibbsUn_diag <- as.data.frame(cbind(colnames(bgibbsUn[1:(ncol(bgibbsUn)-2)]), bgibbsUn_diag))

colnames(bgibbsUn_diag) <- c('Coefficient', 'Diagnostic Statistic')
print(tibble::tibble(bgibbsUn_diag), n = nrow(bgibbsUn_diag))

if(type %in% c('All_taxa~all_cov_NOASPECT', 'Reduced_taxa~all_cov_NOASPECT')){
  fSensGibbs |>
    dplyr::select(c(colnames(fSensGibbs)[1:13], iter, chain)) |>
    tidyr::pivot_longer(colnames(fSensGibbs)[1:13],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Sensitivity estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type %in% c('All_taxa~all_cov_ASPECT', 'Reduced_taxa~all_cov_ASPECT')){
  fSensGibbs |>
    dplyr::select(c(colnames(fSensGibbs)[1:18], iter, chain)) |>
    tidyr::pivot_longer(colnames(fSensGibbs)[1:18],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Sensitivity Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

fSensGibbs_diag <- c()
for(i in 1:(ncol(fSensGibbs)-2)){
  fSensGibbs_diag[i] <- gelman_statistic(fSensGibbs[,i], fSensGibbs$chain)
}

fSensGibbs_diag <- as.data.frame(cbind(colnames(fSensGibbs)[1:(ncol(fSensGibbs)-2)], fSensGibbs_diag))

colnames(fSensGibbs_diag) <- c('Coefficient', 'Diagnostic Statistic')

print(tibble::tibble(fSensGibbs_diag), n = nrow(fSensGibbs_diag))

if(type %in% c('Reduced_taxa~all_cov_ASPECT', 'Reduced_taxa~all_cov_NOASPECT')){
  sgibbs |>
    dplyr::select(c(colnames(sgibbs)[1:6], iter, chain)) |>
    tidyr::pivot_longer(colnames(sgibbs)[1:6],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

if(type %in% c('All_taxa~all_cov_ASPECT', 'All_taxa~all_cov_NOAPSECT')){
  sgibbs |>
    dplyr::select(c(colnames(sgibbs)[1:20], iter, chain)) |>
    tidyr::pivot_longer(colnames(sgibbs)[1:20],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  sgibbs |>
    dplyr::select(c(colnames(sgibbs)[21:40], iter, chain)) |>
    tidyr::pivot_longer(colnames(sgibbs)[21:40],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  sgibbs |>
    dplyr::select(c(colnames(sgibbs)[41:60], iter, chain)) |>
    tidyr::pivot_longer(colnames(sgibbs)[41:60],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  sgibbs |>
    dplyr::select(c(colnames(sgibbs)[61:80], iter, chain)) |>
    tidyr::pivot_longer(colnames(sgibbs)[61:80],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  sgibbs |>
    dplyr::select(c(colnames(sgibbs)[81:100], iter, chain)) |>
    tidyr::pivot_longer(colnames(sgibbs)[81:100],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
  
  sgibbs |>
    dplyr::select(c(colnames(sgibbs)[101:120], iter, chain)) |>
    tidyr::pivot_longer(colnames(sgibbs)[101:120],
                        names_to = 'beta', values_to = 'estimate') |>
    ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate, color = as.factor(chain))) +
    ggplot2::geom_line(show.legend = F) +
    ggplot2::facet_wrap(~beta, scales = 'free') +
    ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance Estimate') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('#090c10', '#004488', '#ddaa34', '#bb5566'))
}

sgibbs_diag <- c()
for(i in 1:(ncol(sgibbs)-2)){
  sgibbs_diag[i] <- gelman_statistic(sgibbs[,i], sgibbs$chain)
}

sgibbs_diag <- as.data.frame(cbind(colnames(sgibbs)[1:(ncol(sgibbs)-2)], sgibbs_diag))
colnames(sgibbs_diag) <- c('Coefficient', 'Diagnostic Statistic')

print(tibble::tibble(sgibbs_diag), n = nrow(sgibbs_diag))

# If everything looks good, save

save(bFacGibbs, bgibbs, bgibbsUn,
     fSensGibbs, sgibbs, file = paste0('out/', type, '/combined.RData'))
