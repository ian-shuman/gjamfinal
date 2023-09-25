## Run GJAM

## Author: AM Willson

## This s cript is currently set up to run on ND's CRC cluster

rm(list = ls())
require(crayon, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')
require(withr, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')
require(tzdb, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')
require(backports, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')
require(broom, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')
require(tidyverse, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')
require(gjam, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')

## Change only these variables ##
setwd('~/gjam-master/')
niter <- 1000
nburn <- 200

## End changes ##

# Load data from 1.5.Reduce.R
load('process2_FINALSOILS.RData')

#### Running GJAM ####

# Define formula
# Presence/absence at each site is a function of each of the environmental covariates
# with no interactions
form1 <- as.formula(~ Slope + CAC + CEC + CLA + SAN +
                      WAT + mean.SWI + Hydric + Floodplain +
                      totalPPT + MeanTEMP)

# Prepare and run model
mlist <- list(ng = niter, burnin = nburn, typeNames = 'PA',
              random = 'marea', PREDICTX = F)

out <- gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

# Save
save.image(file = 'out/all_taxa-all_cov_NOASPECT.RData')