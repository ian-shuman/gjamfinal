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

# Reduce ydata to have only savanna/prairie/forest taxa
new.ydata <- ydata |>
  mutate(Prairie = No.tree,
         Savanna = Oak + Hickory,
         Forest = Elm + Ash + Maple + Basswood + Walnut + Black.gum.sweet.gum +
           Ironwood + Beech + Dogwood + Poplar.tulip.poplar + Other.conifer +
           Other.hardwood) |>
  mutate(Prairie = as.numeric(Prairie),
         Savanna = as.numeric(Savanna),
         Forest = as.numeric(Forest)) |>
  mutate(Savanna = if_else(Savanna > 1, 1, Savanna),
         Forest = if_else(Forest > 1, 1, Forest)) |>
  select(c(Prairie, Forest, Savanna)) |>
  mutate(Savanna = if_else(Savanna == 1 & Forest == 1, 0, Savanna))

# Check to make sure we have something in every row
zeros = apply(new.ydata, 1, sum)
zeros = which(zeros == 0)

# Good to go
ydata <- new.ydata

# Do the same for edata if using "full" effort option
if(effort_type == 'full'){
  new.edata <- edata |>
    rowwise() |>
    mutate(Prairie_dist = No.tree_dist,
           Savanna_dist = mean(c(Oak_dist, Hickory_dist), na.rm = T),
           Forest_dist = mean(c(Elm_dist, Ash_dist, Maple_dist, Basswood_dist,
                                Walnut_dist, Black.gum.sweet.gum_dist,
                                Ironwood_dist, Beech_dist, Dogwood_dist,
                                Poplar.tulip.poplar_dist, Other.conifer_dist,
                                Other.hardwood_dist), na.rm = T))
  
  new.edata <- new.edata |>
    mutate_all(~ifelse(is.nan(.), NA, .)) |>
    select(c(Prairie_dist, Savanna_dist, Forest_dist))
  
  zeros <- apply(new.edata, 1, sum)
  if(length(which(zeros == 0)) == 0){edata <- new.edata}else{print('error')}
}

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
             effort = elist, random = 'marea',
             PREDICTX = F)

out = gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

# Save
save.image(file = 'out/reduced_taxa-all_cov.RData')
