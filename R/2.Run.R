## Preparing and running GJAM with PLS data

## Author: AM Willson

## This script is currently set up to run on ND's CRC cluster

rm(list = ls())
require(tidyverse, lib.loc = '~/Rlibs')
require(gjam, lib.loc = '~/Rlibs')

## Change only these variables ##
setwd('~/gjam/run/')
effort_type <- 'site' # 'site' or 'full'
niter <- 10
nburn <- 5

## End changes ##

# Load data from 1.5.Reduce.R
load('GJAM DATA/process2.RData')
load('GJAM DATA/effort.RData')

# Reformat effort
if(effort_type == 'site'){edata <- site_effort}
if(effort_type == 'full'){edata <- effort}
elist = list(columns = 1:ncol(ydata),
             values = edata)

#### Running GJAM ####

# Define formula
# Presence/absence at each site is a function of each of the environmental covariates
# with no interactions
form1 = as.formula(~ mean.SlopeProjected + mean.AspectProjected + mean.CAC +
                     mean.CEC + mean.CLA + mean.SAN + mean.WAT + mean.SWI + 
                     Hydric + Floodplain + totalPPT + MeanTEMP)

# Make list of priors for each parameter
#prior <- gjamPriorTemplate(formula = form1,
#                           xdata = xdata, ydata = ydata,
#                           lo = lo, hi = hi)

# Prepare & run model
mlist = list(ng = niter, burnin = nburn, typeNames = 'PA',
             effort = elist, random = c('marea', 'long', 'lat'))

out = gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

# Save
save.image(file = 'run/out.RData')
