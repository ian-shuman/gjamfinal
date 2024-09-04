## Processing withheld data for out-of-sample prediction
## This script mirrors 1.Process.R

## Author: AM Willson

rm(list = ls())

library(dplyr)
library(tibble)
library(tidyr)

#### STEP 1 ####

## This section just formats the data, focusing mainly on
## unlisting xdata, edata, and ydata
## and ensuring all columns are present in all matrices in ydata and edata

# Read in file names that we need to loop over
xfiles <- list.files('GJAMDATA/Withheld For Validation/X/')
yfiles <- list.files('GJAMDATA/Withheld For Validation/Y/')

# Storage
xdata_list <- list()
yedata_list <- list()

# Read in files
for(i in 1:length(xfiles)){
  filename <- xfiles[i]
  pathname <- paste0('GJAMDATA/Withheld For Validation/X/', filename)
  xdata_list[[i]] <- utils::read.csv(pathname)
  xdata_list[[i]]$filename <- filename
  
  filename <- yfiles[i]
  pathname <- paste0('GJAMDATA/Withheld For Validation/Y/', filename)
  yedata_list[[i]] <- utils::read.csv(pathname)
  
  yedata_list[[i]] <- lapply(yedata_list[[i]], as.numeric)
  yedata_list[[i]]$filename <- filename
}

# Unlist
xdata <- do.call(rbind, xdata_list)
yedata <- do.call(dplyr::bind_rows, yedata_list)

# Separate ydata from edata
ydata <- dplyr::select(yedata, colnames(yedata)[!grepl('dist', colnames(yedata), fixed = T)])

# Format management area columns
xdata <- xdata |>
  # Subset the filename character string for
  # just the management area
  dplyr::mutate(marea = sub('_X.*', '', filename)) |>
  # remove filename column and x and y since we have lat and long
  dplyr::select(-c(filename, x, y)) |>
  # make uniqueID that is specific to the corner
  # within each management area
  dplyr::mutate(uniqueID = paste0(marea,'_',uniqueID))

# repeat for ydata
ydata <- ydata |>
  dplyr::mutate(marea = sub('_Y.*', '', filename)) |>
  dplyr::select(-filename) |>
  dplyr::mutate(uniqueID = paste0(marea, '_', uniqueID))

# Join xdata and ydata
full_data <- xdata |>
  dplyr::full_join(ydata, by = 'uniqueID')

# Make into sf object
full_data2 <- sf::st_as_sf(full_data, coords = c('long', 'lat'))
# Add current CRS
sf::st_crs(full_data2) <- 'EPSG:3175'
# Transform to EPSG 4326
full_data2 <- sf::st_transform(full_data2, crs = 'EPSG:4326')
# Transform back to regular dataframe
full_data2 <- sfheaders::sf_to_df(full_data2, fill = TRUE) |>
  dplyr::rename(long = x,
                lat = y) |>
  dplyr::select(colnames(full_data))

# Make sure columns and rows are still in the same order
identical(colnames(full_data), colnames(full_data2))
identical(full_data$uniqueID, full_data2$uniqueID)

# Separate xdata and ydata
# This is done to ensure that both dataframes are in the same
# order for GJAM
xdata <- full_data[1:18]
ydata <- full_data[,c(1,19:55)]

colnames(xdata)[18] <- 'marea'

# Convert xdata into the correct data types
xdata <- xdata |>
  dplyr::mutate(uniqueID = as.factor(uniqueID),
                Hydric = as.factor(Hydric),
                Floodplain = as.factor(Floodplain)) |>
  # Add unique ID numbers to rownames
  tibble::column_to_rownames(var = 'uniqueID')

ydata <- ydata |>
  dplyr::mutate(uniqueID = as.factor(uniqueID)) |>
  # Take out columns that we don't need
  dplyr::select(-c(chainstree, chainstree2, chainstree3, chainstree4)) |>
  # Take out columns that don't contain any information
  dplyr::select(-c(No.data, Water, Unknown.tree, NA.)) |>
  tibble::column_to_rownames(var = 'uniqueID') |>
  # Take out management area
  dplyr::select(-marea.y) |>
  tidyr::replace_na(list(Hackberry = 0, Elm = 0, Oak = 0, Maple = 0,
                         Ash = 0, Hickory = 0, Willow = 0, Poplar = 0,
                         No.tree = 0, Walnut = 0, Basswood = 0, Sycamore = 0,
                         Cherry = 0, Ironwood = 0, Pine = 0, Other.hardwood = 0,
                         Birch = 0, Mulberry = 0, Buckeye = 0, Locust = 0,
                         Poplar.tulip.poplar = 0, Beech = 0, Black.gum.sweet.gum = 0,
                         Dogwood = 0, Black.gum = 0, Bald.cypress = 0,
                         Sweet.gum = 0, Chestnut = 0))

# Remove rows with no information
zeros <- apply(ydata, 1, sum)
zeros <- which(zeros == 0)

ydata <- ydata[-zeros,]
xdata <- xdata[-zeros,]

ydata_oos <- ydata
xdata_oos <- xdata

# Save
save(xdata_oos, ydata_oos, file = 'GJAMDATA/Withheld For Validation/validation_processed_xydata.RData')
