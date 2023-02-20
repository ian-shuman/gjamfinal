## Run GJAM

## Author: AM Willson

## This script is currently set up to run on ND's CRC cluster

rm(list = ls())
require(tidyverse, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')
require(gjam, lib.loc = '/afs/crc.nd.edu/user/i/ishuman2/Rlibs2')

## Change only these variables ##
setwd('~/gjam-master/')
effort_type <- 'full' # 'site' or 'full'
niter <- 10
nburn <- 5

## End changes ##

# Load data from 1.5.Reduce.R
load('process2.RData')

# Reformat effort
if(effort_type == 'site'){edata <- site_effort}
if(effort_type == 'full'){edata <- edata}
elist = list(columns = 1:ncol(ydata),
             values = edata)

#### Running GJAM ####

# Define formula
# Presence/absence at each site is a function of each of the environmental covariates
# with no interactions
form1 = as.formula(~ MeanTEMP + totalPPT)

# Make list of priors for each parameter
#prior <- gjamPriorTemplate(formula = form1,
#                           xdata = xdata, ydata = ydata,
#                           lo = lo, hi = hi)

# Prepare & run model
mlist = list(ng = niter, burnin = nburn, typeNames = 'PA',
             effort = elist, random = 'marea',
             PREDICTX = F)

out = gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

# Save
save.image(file = 'out/all_taxa-temp+precip.RData')
