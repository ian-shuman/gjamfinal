## Run GJAM

## Author: AM Willson

## This script is currently set up to run on ND's CRC cluster
## Minimal changes are required for loading libraries and indicating
## correct working directory when working on a different cluster.
## GJAM is unable to run on a personal machine with typical specs with
## these data

rm(list = ls())

require(gjam, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')

## Change only these variables
setwd('~/gjam-master/')
niter <- 1000
nburn <- 200

## End changes

# Load data from 3.Reduce_ecosystem.R
load('processed_xydata_2_ecosystem.RData')

:wq# Define formula
# Presence/absence at each site is a function of each of the environmental covariates
# with no interactions
form1 <- as.formula(~ Slope + CAC + CEC + CLA + SAN +
                      WAT + mean.SWI + Hydric + Floodplain +
                      totalPPT + MeanTEMP + direction)

# Prepare and run model
mlist <- list(ng = niter, nburnin = nburn, typeNames = 'PA',
              random = 'marea', PREDICTX = F)

out <- gjam::gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

# Save
save.image(file = 'out/reduced_taxa-all_cov_ASPECT.RData')