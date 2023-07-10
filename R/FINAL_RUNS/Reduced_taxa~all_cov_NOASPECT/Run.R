## Run GJAM

## Author: AM Willson

## This script is currently set up to run on ND's CRC cluster

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

# Reduce ydata to have only savanna/prairie/forest taxa
new.ydata <- ydata |>
  mutate(Prairie = No.tree,
         Savanna = Oak + Hickory,
         Forest = Elm + Ash + Maple + Basswood + Walnut +
           Black.gum.sweet.gum + Ironwood + Beech + Dogwood +
           Poplar.tulip.poplar + Other.conifer + Other.hardwood) |>
  mutate(Prairie = as.numeric(Prairie),
         Savanna = as.numeric(Savanna),
         Forest = as.numeric(Forest)) |>
  mutate(Prairie = if_else(Prairie > 1, 1, Prairie),
         Savanna = if_else(Savanna > 1, 1, Savanna),
         Forest = if_else(Forest > 1, 1, Forest)) |>
  select(c(Prairie, Savanna, Forest)) |>
  mutate(Prairie = if_else(Prairie == 1 & Savanna == 1, 0, Prairie),
         Savanna = if_else(Savanna == 1 & Forest == 1, 0, Savanna))

# Check to make sure we have something in every row
zeros <- apply(new.ydata, 1, sum)
zeros <- which(zeros != 1)

# Good to go
if(length(zeros) == 0) ydata <- new.ydata

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
save.image(file = 'out/reduced_taxa-all_cov_NOASPECT.RData')