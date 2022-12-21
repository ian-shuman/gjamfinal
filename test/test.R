## This script runs through data processing, preparing GJAM inputs, and running GJAM
## for a sample area in Indiana
## The script may need to be modified to run for the entire area of interest
## because the data processing may be more extensive for the entire dataset

## Note that this script is set up to run on the ND CRC.
## If you wish to run remotely, you will need to load packages using lines 9 & 10
## and define your own working directory
## Otherwise, the code should remain the same

## Author: AM Willson

rm(list = ls())
#library(tidyverse)
#library(gjam)
require(tidyverse, lib.loc = '~/Rlibs')
require(gjam, lib.loc = '~/Rlibs')

# Set working directory
setwd('~/gjam')

#### Prepare data ####

# Load in CSVs
#xdata = read_csv('test/Sample_X_DataStructure.csv')
#ydata = read_csv('test/Sample_Y_DataStructure.csv')

# Save as RData object for future use
#save(xdata, ydata, file = 'test/test.RData')

# Load in RData
load('test/test.RData')

# Format data
# Convert xdata into the correct data types
xdata = xdata %>%
  mutate(uniqueID = as.factor(uniqueID),
         Hydric = as.factor(Hydric),
         Floodplain = as.factor(Floodplain)) %>%
  # Change name of column to not have "_"
  mutate(GS.ppet = GS_ppet) %>%
  select(-GS_ppet) %>%
  # Remove one of the correlated predictor variables
  select(-mean.SIL) %>%
  # Add unique ID numbers to rownames
  column_to_rownames(var = 'uniqueID')


# Convert ydata into factor columns (all are y/n)
ydata = ydata %>%
  mutate(uniqueID = as.factor(uniqueID))%>%
  select(-c(`No data`, Water, `Unknown tree`, `NA`)) %>%
  column_to_rownames(var = 'uniqueID')

# Remove columns with no information
zeros <- apply(ydata, 1, sum)
zeros <- which(zeros == 0)

ydata <- ydata[-zeros,]
xdata <- xdata[-zeros,]

# Add edata
# Since all observations were made in the exact same way, effort will be equal
# Therefore, just make a vector of 1s
#edata = rep(1, length = nrow(xdata))

# Alternative: make effort equal to distance
load('test/effort.RData')
# This could be either species-specific effort at each site or an average effort
#edata <- site_effort
#edata <- edata[-zeros]

edata <- effort
edata <- edata[-zeros,]

# And reformat
elist = list(columns = 1:ncol(ydata),
             values = edata)

#### Running GJAM ####

# Define formula
# Presence/absence at each site is a function of each of the environmental covariates
# with no interactions
form1 = as.formula(~ mean.SlopeProjected + mean.AspectProjected + mean.CAC +
                    mean.CEC + mean.CLA + mean.KSA + mean.SAN +
                    mean.WAT + mean.SWI + Hydric + Floodplain + totalPPT +
                    MeanTEMP + GS.ppet)

# Set up priors
## This is a hack to get uninformative priors on every parameter
## Hack starts here ##
spLo <- "Oak"
sp <- length(spLo)
lo <- vector("list", sp)

# add names to the list
names(lo) <- paste0("totalPPT", spLo)

# add values to the list
lo[1] <- Inf

spHi <- c('Oak')
sp <- length(spHi)
hi <- vector("list", sp)

# add names to the list
names(hi) <- paste0("Hydric_", spHi)

# add values to the list
hi[1:length(hi)] <- Inf
## Hack ends here

# Make list of priors for each parameter
prior <- gjamPriorTemplate(formula = form1,
                          xdata = xdata, ydata = ydata,
                          lo = lo, hi = hi)

# Prepare & run model
mlist = list(ng = 100, burnin = 40, typeNames = 'PA', betaPrior = prior,
             effort = elist)
out = gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

# Save default plots
gjamPlot(out, plotPars = list(SAVEPLOTS = T, outfolder = 'gjamOutput'))

# Save output
# I included this for when we are running on the CRC
save.image(file = 'test/out.RData')