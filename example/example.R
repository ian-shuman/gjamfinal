## Testing GJAM using sample data from Lane Scher
## Date: 08 November 2022

rm(list = ls())
library(gjam)

load('birdData.rdata')

#### Data structure ####

## xdata: predictor variables
## each row is a different site/year combination
## each column is a predictor specific to the site/year combination
  ## geeIDyear: NEON site ID & year (chr. [should be factor if used])
  ## geeID: NEON site ID (chr. [should be factor if used])
  ## elevation: elevation of observation (num., not complete)
  ## landcover: land cover class (chr. [should be factor if used])
  ## summerSMOS: mean soil moisture of summer (num., probably bound?)
  ## precip: total precipitation for previous 12 months (num., 0 bound)
  ## summerLAI: leaf area index during summer (num., possibly bound?)

## ydata: response variables
  ## each row is a site/year combination
  ## each column is a count of a given species

## edata: effort
  ## vector of efforts for each site/year combination in the same order as xdata/ydata
  ## here, effort is in minutes

#### Lane's code ####

ydata <- as.data.frame(gjamTrimY(ydata[,1:ncol(ydata)], maxCols = 50)$y)
ydata <- sapply(ydata, as.numeric)
ydata <- as.data.frame(ydata)

xdata$landcover <- as.factor(xdata$landcover)
xdata$landcover <- relevel(xdata$landcover, 
                           "barren")

elist <- list(columns = 1:ncol(ydata),
              values = edata)

form1 <- as.formula(~ elevation + summerSMOS + precip + landcover)

rlist   <- list(r = 8, N = 20)

spLo <- "great blue heron"
sp <- length(spLo)
lo <- vector("list", sp)

# add names to the list
names(lo) <- paste0("landcoverwetlands_", spLo)

# add values to the list
lo[1] <- 0

spHi <- c("blue jay", "northern parula", "killdeer", "chipping sparrow", "ovenbird")
sp <- length(spHi)
hi <- vector("list", sp)

# add names to the list
names(hi) <- paste0("precip_", spHi)

# add values to the list
hi[1:length(hi)] <- 0

prior <- gjamPriorTemplate(formula = form1, 
                           xdata = xdata, ydata = ydata,
                           lo = lo, hi = hi)

mlist <- list(ng=500, burnin=300, typeNames = 'DA', betaPrior = prior,
              effort = elist, reductList = rlist)

out <- gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

gjamPlot(output = out, plotPars = list(PLOTALLY = T))
