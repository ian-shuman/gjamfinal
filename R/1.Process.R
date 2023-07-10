## Processing training data for use in GJAM

## Author: AM Willson

rm(list = ls())

library(tidyverse)

#### STEP 1 ####

## This section just formats the data, focusing mostly on
## unlisting xdata, edata, and ydata
## and ensuring all columns are present in all matrices in ydata and edata

# Read in file names that we need to loop over
xfiles <- list.files('GJAMDATA/X/New Soils Data/Aspect/')
yfiles <- list.files('GJAMDATA/Y/')

# Storage
xdata_list <- list()
yedata_list <- list()

# Read in files
for(i in 1:length(xfiles)){
  filename <- xfiles[i]
  pathname <- paste0('GJAMDATA/X/New Soils Data/Aspect/',filename)
  xdata_list[[i]] <- read.csv(pathname)
  
  filename <- yfiles[i]
  pathname <- paste0('GJAMDATA/Y/',filename)
  yedata_list[[i]] <- read.csv(pathname)
}

# Initialize unlist
xdata <- xdata_list[[1]]
xdata$filename <- rep(xfiles[1], times = nrow(xdata))

# Unlist
for(i in  2:length(xfiles)){
  dat <- xdata_list[[i]]
  dat$filename <- rep(xfiles[i], times = nrow(dat))
  xdata <- rbind(xdata, dat)
}

ydata_list <- list()
edata_list <- list()

# Separate the ydata and the edata
for(i in 1:length(yfiles)){
  tempdata <- yedata_list[[i]]
  
  ydata_list[[i]] <- tempdata %>%
    select((colnames(tempdata)[!grepl('dist', colnames(tempdata), fixed = T)]))
  
  edata_list[[i]] <- tempdata %>%
    select(colnames(tempdata)[grepl('dist', colnames(tempdata), fixed = T)])
}

# Storage
columns <- c()

# Find names of all columns
for(i in 1:length(yfiles)){
  tempdata <- ydata_list[[i]]
  columns <- c(columns, colnames(tempdata))
}
columns <- unique(columns)

# Make function for "not in"
`%nin%` <- Negate(`%in%`)

# Add columns that don't exist in a given matrix
for(i in 1:length(yfiles)){
  tempdata <- ydata_list[[i]]
  cols <- colnames(tempdata)
  save <- which(columns %nin% cols)
  newcols <- (ncol(tempdata)+1):(ncol(tempdata)+length(save))
  tempdata[,newcols] <- 0
  tempdata <- as.data.frame(tempdata)
  colnames(tempdata)[newcols] <- columns[save]
  ydata_list[[i]] <- tempdata
}

# Do the same for the edata
columns <- c()
for(i in 1:length(yfiles)){
  tempdata <- edata_list[[i]]
  columns <- c(columns, colnames(tempdata))
}
columns <- unique(columns)

for(i in 1:length(yfiles)){
  tempdata <- edata_list[[i]]
  cols <- colnames(tempdata)
  save <- which(columns %nin% cols)
  newcols <- (ncol(tempdata)+1):(ncol(tempdata)+length(save))
  tempdata[,newcols] <- NA
  tempdata <- as.data.frame(tempdata)
  colnames(tempdata)[newcols] <- columns[save]
  edata_list[[i]] <- tempdata
}

# Initialize unlist
ydata <- ydata_list[[1]]
ydata$filename <- rep(yfiles[1], times = nrow(ydata))

# Unlist
for(i in 2:length(yfiles)){
  dat <- ydata_list[[i]]
  dat$filename <- rep(yfiles[i], times = nrow(dat))
  ydata <- rbind(ydata, dat)
}

# Same for edata
edata <- edata_list[[1]]
edata$filename <- rep(yfiles[1], times = nrow(edata))

for(i in 2:length(yfiles)){
  dat <- edata_list[[i]]
  dat$filename <- rep(yfiles[i], times = nrow(dat))
  edata <- rbind(edata, dat)
}

# Remove big unnecessary objects
rm(edata_list, xdata_list, ydata_list, yedata_list)

# Format management area columns
xdata <- xdata |>
  mutate(marea = sub('_X.*', '', filename)) |>
  dplyr::select(-c(filename, x, y)) |>
  mutate(uniqueID = paste0(marea,'_',uniqueID))

ydata <- ydata |>
  mutate(marea = sub('_Y.*', '', filename)) |>
  select(-filename) |>
  mutate(uniqueID = paste0(marea,'_',uniqueID))

edata <- edata |>
  mutate(marea = sub('_Y.*', '', filename)) |>
  select(-filename) |>
  mutate(uniqueID = ydata$uniqueID)

#### STEP 2 ####

## This step takes the processed data and formats it correctly for use
## in GJAM

# Convert xdata into the correct data types
xdata = xdata |>
  mutate(uniqueID = as.factor(uniqueID),
         Hydric = as.factor(Hydric),
         Floodplain = as.factor(Floodplain),
         direction = as.factor(direction)) |>
  # Add unique ID numbers to rownames
  column_to_rownames(var = 'uniqueID')

ydata = ydata |>
  mutate(uniqueID = as.factor(uniqueID)) |>
  # Take out columns that we don't need
  select(-c(chainstree, chainstree2, chainstree3, chainstree4)) |>
  # Take out columns that don't contain any information
  select(-c(No.data, Water, Unknown.tree, Wet, NA., X88888)) |>
  column_to_rownames(var = 'uniqueID') %>%
  # Take out management area
  select(-marea)

# Remove rows with no information
zeros <- apply(ydata, 1, sum)
zeros <- which(zeros == 0)

ydata <- ydata[-zeros,]
xdata <- xdata[-zeros,]

# Save
save(xdata, ydata, file = 'GJAMDATA/process_FINALSOILS.RData')

#### STEP 3 ####

## This section specifically formats distance and calculations effort
## from the edata data frame

dist <- edata

# Replace with something numeric so that we can convert to numeric columns
dist[dist == 'missing'] <- -999
dist[dist == 'illegible'] <- -999

# Convert columns to all be numeric
dist <- dist |>
  select(-c(Unknown.tree_dist, No.data_dist, Water_dist, 
            Wet_dist, NA_dist, X88888_dist)) |>
  mutate(No.tree_dist = as.numeric(No.tree_dist),
         Oak_dist = as.numeric(Oak_dist),
         Elm_dist = as.numeric(Elm_dist),
         Hickory_dist = as.numeric(Hickory_dist),
         Ash_dist = as.numeric(Ash_dist),
         Poplar_dist = as.numeric(Poplar_dist),
         Maple_dist = as.numeric(Maple_dist),
         Sycamore_dist = as.numeric(Sycamore_dist),
         Other.hardwood_dist = as.numeric(Other.hardwood_dist),
         Mulberry_dist = as.numeric(Mulberry_dist),
         Basswood_dist = as.numeric(Basswood_dist),
         Walnut_dist = as.numeric(Walnut_dist),
         Cherry_dist = as.numeric(Cherry_dist),
         Locust_dist = as.numeric(Cherry_dist),
         Hackberry_dist = as.numeric(Hackberry_dist),
         Willow_dist = as.numeric(Willow_dist),
         Buckeye_dist = as.numeric(Buckeye_dist),
         Birch_dist = as.numeric(Birch_dist),
         Black.gum.sweet.gum_dist = as.numeric(Black.gum.sweet.gum_dist),
         Sweet.gum_dist = as.numeric(Sweet.gum_dist),
         Black.gum_dist = as.numeric(Black.gum_dist),
         Ironwood_dist = as.numeric(Ironwood_dist),
         Poplar.tulip.poplar_dist = as.numeric(Poplar.tulip.poplar_dist),
         Dogwood_dist = as.numeric(Dogwood_dist),
         Bald.cypress_dist = as.numeric(Bald.cypress_dist),
         Cedar.juniper_dist = as.numeric(Cedar.juniper_dist),
         Tulip.poplar_dist = as.numeric(Tulip.poplar_dist),
         Tamarack_dist = as.numeric(Tamarack_dist),
         Pine_dist = as.numeric(Pine_dist),
         Alder_dist = as.numeric(Alder_dist),
         Chestnut_dist = as.numeric(Chestnut_dist),
         Beech_dist = as.numeric(Beech_dist)) |>
  select(-marea) |>
  column_to_rownames(var = 'uniqueID')

# Flag columns with missing values
missings <- which(dist == -999, arr.ind = T)
missings <- missings[,2]
missings <- unique(missings)

# Find averages in those columns
av_no.tree <- dist |>
  select(No.tree_dist) |>
  filter(No.tree_dist > -999) |>
  dplyr::summarize(mean = mean(No.tree_dist, na.rm = T))
av_oak <- dist |>
  select(Oak_dist) |>
  filter(Oak_dist > -999) |>
  dplyr::summarize(mean = mean(Oak_dist, na.rm = T))
av_elm <- dist |>
  select(Elm_dist) |>
  filter(Elm_dist > -999) |>
  dplyr::summarize(mean = mean(Elm_dist, na.rm = T))
av_hickory <- dist |>
  select(Hickory_dist) |>
  filter(Hickory_dist > -999) |>
  dplyr::summarize(mean = mean(Hickory_dist, na.rm = T))
av_ash <- dist |>
  select(Ash_dist) |>
  filter(Ash_dist > -999) |>
  dplyr::summarize(mean = mean(Ash_dist, na.rm = T))
av_poplar <- dist |>
  select(Poplar_dist) |>
  filter(Poplar_dist > -999) |>
  dplyr::summarize(mean = mean(Poplar_dist, na.rm = T))
av_maple <- dist |>
  select(Maple_dist) |>
  filter(Maple_dist > -999) |>
  dplyr::summarize(mean = mean(Maple_dist, na.rm = T))
av_sycamore <- dist |>
  select(Sycamore_dist) |>
  filter(Sycamore_dist > -999) |>
  dplyr::summarize(mean = mean(Sycamore_dist, na.rm = T))
av_otherhardwood <- dist |>
  select(Other.hardwood_dist) |>
  filter(Other.hardwood_dist > -999) |>
  dplyr::summarize(mean = mean(Other.hardwood_dist, na.rm = T))
av_mulberry <- dist |>
  select(Mulberry_dist) |>
  filter(Mulberry_dist > -999) |>
  dplyr::summarize(mean = mean(Mulberry_dist, na.rm = T))
av_basswood <- dist |>
  select(Basswood_dist) |>
  filter(Basswood_dist > -999) |>
  dplyr::summarize(mean = mean(Basswood_dist, na.rm = T))
av_walnut <- dist |>
  select(Walnut_dist) |>
  filter(Walnut_dist > -999) |>
  dplyr::summarize(mean = mean(Walnut_dist, na.rm = T))
av_cherry <- dist |>
  select(Cherry_dist) |>
  filter(Cherry_dist > -999) |>
  dplyr::summarize(mean = mean(Cherry_dist, na.rm = T))
av_locust <- dist |>
  select(Locust_dist) |>
  filter(Locust_dist > -999) |>
  dplyr::summarize(mean = mean(Locust_dist, na.rm = T))
av_hackberry <- dist |>
  select(Hackberry_dist) |>
  filter(Hackberry_dist > -999) |>
  dplyr::summarize(mean = mean(Hackberry_dist, na.rm = T))
av_willow <- dist |>
  select(Willow_dist) |>
  filter(Willow_dist > -999) |>
  dplyr::summarize(mean = mean(Willow_dist, na.rm = T))
av_buckeye <- dist |>
  select(Buckeye_dist) |>
  filter(Buckeye_dist > -999) |>
  dplyr::summarize(mean = mean(Buckeye_dist, na.rm = T))
av_birch <- dist |>
  select(Birch_dist) |>
  filter(Birch_dist > -999) |>
  dplyr::summarize(mean = mean(Birch_dist, na.rm = T))
av_bgsg <- dist |>
  select(Black.gum.sweet.gum_dist) |>
  filter(Black.gum.sweet.gum_dist > -999) |>
  dplyr::summarize(mean = mean(Black.gum.sweet.gum_dist, na.rm = T))
av_sweetgum <- dist |>
  select(Sweet.gum_dist) |>
  filter(Sweet.gum_dist > -999) |>
  dplyr::summarize(mean = mean(Sweet.gum_dist, na.rm = T))
av_blackgum <- dist |>
  select(Black.gum_dist) |>
  filter(Black.gum_dist > -999) |>
  dplyr::summarize(mean = mean(Black.gum_dist, na.rm = T))
av_ironwood <- dist |>
  select(Ironwood_dist) |>
  filter(Ironwood_dist > -999) |>
  dplyr::summarize(mean = mean(Ironwood_dist, na.rm = T))
av_poplartulip <- dist |>
  select(Poplar.tulip.poplar_dist) |>
  filter(Poplar.tulip.poplar_dist > -999) |>
  dplyr::summarize(mean = mean(Poplar.tulip.poplar_dist, na.rm = T))
av_beech <- dist |>
  select(Beech_dist) |>
  filter(Beech_dist > -999) |>
  dplyr::summarize(mean = mean(Beech_dist, na.rm = T))
av_dogwood <- dist |>
  select(Dogwood_dist) |>
  filter(Dogwood_dist > -999) |>
  dplyr::summarize(mean = mean(Dogwood_dist, na.rm = T))
av_cypress <- dist |>
  select(Bald.cypress_dist) |>
  filter(Bald.cypress_dist > -999) |>
  dplyr::summarize(mean = mean(Bald.cypress_dist, na.rm = T))
av_cedar <- dist |>
  select(Cedar.juniper_dist) |>
  filter(Cedar.juniper_dist > -999) |>
  dplyr::summarize(mean = mean(Cedar.juniper_dist, na.rm = T))
av_tulip <- dist |>
  select(Tulip.poplar_dist) |>
  filter(Tulip.poplar_dist > -999) |>
  dplyr::summarize(mean = mean(Tulip.poplar_dist, na.rm = T))
av_tamarack <- dist |>
  select(Tamarack_dist) |>
  filter(Tamarack_dist > -999) |>
  dplyr::summarize(mean = mean(Tamarack_dist, na.rm = T))
av_pine <- dist |>
  select(Pine_dist) |>
  filter(Pine_dist > -999) |>
  dplyr::summarize(mean = mean(Pine_dist, na.rm = T))
av_alder <- dist |>
  select(Alder_dist) |>
  filter(Alder_dist > -999) |>
  dplyr::summarize(mean = mean(Alder_dist, na.rm = T))
av_chestnut <- dist |>
  select(Chestnut_dist) |>
  filter(Chestnut_dist > -999) |>
  dplyr::summarize(mean = mean(Chestnut_dist, na.rm = T))

# Replace -999 placeholder with average for the species
dist <- dist |>
  mutate(No.tree_dist = ifelse(No.tree_dist == -999, av_no.tree$mean, No.tree_dist),
         Oak_dist = ifelse(Oak_dist == -999, av_oak$mean, Oak_dist),
         Elm_dist = ifelse(Elm_dist == -999, av_elm$mean, Elm_dist),
         Hickory_dist = ifelse(Hickory_dist == -999, av_hickory$mean, Hickory_dist),
         Ash_dist = ifelse(Ash_dist == -999, av_ash$mean, Ash_dist),
         Poplar_dist = ifelse(Poplar_dist == -999, av_poplar$mean, Poplar_dist),
         Maple_dist = ifelse(Maple_dist == -999, av_maple$mean, Maple_dist),
         Sycamore_dist = ifelse(Sycamore_dist == -999, av_sycamore$mean, Sycamore_dist),
         Other.hardwood_dist = ifelse(Other.hardwood_dist == -999, av_otherhardwood$mean, Other.hardwood_dist),
         Mulberry_dist = ifelse(Mulberry_dist == -999, av_mulberry$mean, Mulberry_dist),
         Basswood_dist = ifelse(Basswood_dist == -999, av_basswood$mean, Basswood_dist),
         Walnut_dist = ifelse(Walnut_dist == -999, av_walnut$mean, Walnut_dist),
         Cherry_dist = ifelse(Cherry_dist == -999, av_cherry$mean, Cherry_dist),
         Locust_dist = ifelse(Locust_dist == -999, av_locust$mean, Locust_dist),
         Hackberry_dist = ifelse(Hackberry_dist == -999, av_hackberry$mean, Hackberry_dist),
         Willow_dist = ifelse(Willow_dist == -999, av_willow$mean, Willow_dist),
         Buckeye_dist = ifelse(Buckeye_dist == -999, av_buckeye$mean, Buckeye_dist),
         Birch_dist = ifelse(Birch_dist == -999, av_birch$mean, Birch_dist),
         Black.gum.sweet.gum_dist = ifelse(Black.gum.sweet.gum_dist == -999, av_bgsg$mean, Black.gum.sweet.gum_dist),
         Sweet.gum_dist = ifelse(Sweet.gum_dist == -999, av_sweetgum$mean, Sweet.gum_dist),
         Black.gum_dist = ifelse(Black.gum_dist == -999, av_blackgum$mean, Black.gum_dist),
         Ironwood_dist = ifelse(Ironwood_dist == -999, av_ironwood$mean, Ironwood_dist),
         Poplar.tulip.poplar_dist = ifelse(Poplar.tulip.poplar_dist == -999, av_poplartulip$mean, Poplar.tulip.poplar_dist),
         Beech_dist = ifelse(Beech_dist == -999, av_beech$mean, Beech_dist),
         Dogwood_dist = ifelse(Dogwood_dist == -999, av_dogwood$mean, Dogwood_dist),
         Bald.cypress_dist = ifelse(Bald.cypress_dist == -999, av_cypress$mean, Bald.cypress_dist),
         Cedar.juniper_dist = ifelse(Cedar.juniper_dist == -999, av_cedar$mean, Cedar.juniper_dist),
         Tulip.poplar_dist = ifelse(Tulip.poplar_dist == -999, av_tulip$mean, Tulip.poplar_dist),
         Tamarack_dist = ifelse(Tamarack_dist == -999, av_tamarack$mean, Tamarack_dist),
         Pine_dist = ifelse(Pine_dist == -999, av_pine$mean, Pine_dist),
         Alder_dist = ifelse(Alder_dist == -999, av_pine$mean, Alder_dist),
         Chestnut_dist = ifelse(Chestnut_dist == -999, av_chestnut$mean, Chestnut_dist))

# Replace 0s with 1 because of divide by zero for inverting
dist[dist==0] <- 1

# Make species-specific effort
effort <- 1/dist

# Make site-aggregated effort
site_effort <- apply(dist, 1, mean, na.rm = T)
site_effort[is.nan(site_effort)] <- NA

# Remove sites with only zeros
site_effort <- site_effort[-zeros]
effort <- effort[-zeros,]

# Save
save(effort, site_effort, file = 'GJAMDATA/effort_fixed.RData')
