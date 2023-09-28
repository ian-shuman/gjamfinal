## Processing training data for use in GJAM

## Author: AM Willson

rm(list = ls())

library(dplyr)
library(tibble)
library(tidyr)

# Read in file names that we need to loop over
## These data can be accessed in the repository listed in
## the corresponding publication
xfiles <- list.files('GJAMDATA/X/')
yfiles <- list.files('GJAMDATA/Y/')

# Storage
xdata_list <- list()
yedata_list <- list()

# Read in files and put all data into list
for(i in 1:length(xfiles)){
  filename <- xfiles[i]
  pathname <- paste0('GJAMDATA/X/',filename)
  xdata_list[[i]] <- read.csv(pathname)
  xdata_list[[i]]$filename <- filename
  
  filename <- yfiles[i]
  pathname <- paste0('GJAMDATA/Y/',filename)
  yedata_list[[i]] <- read.csv(pathname)
  
  yedata_list[[i]] <- lapply(yedata_list[[i]], as.numeric)
  yedata_list[[i]]$filename <- filename
}

# Unlist
xdata <- do.call(rbind, xdata_list)
yedata <- do.call(bind_rows, yedata_list)

# Separate ydata fram edata
ydata <- select(yedata, colnames(yedata)[!grepl('dist', colnames(yedata), fixed  = T)])

# Format management area columns
xdata <- xdata |>
  # Subset the filename character string for
  # just the management area
  mutate(marea = sub('_X.*', '', filename)) |>
  # remove filename column and x and y since we have
  # lat and long
  dplyr::select(-filename, -x, -y) |>
  # make uniqueID that is specific to the corner
  # within each management area
  mutate(uniqueID = paste0(marea,'_',uniqueID))

# Repeat for ydata
ydata <- ydata |>
  mutate(marea = sub('_Y.*', '', filename)) |>
  select(-filename) |>
  mutate(uniqueID = paste0(marea,'_',uniqueID))

# Join xdata and ydata by the unique ID
full_data <- xdata |>
  full_join(ydata, by = 'uniqueID')

# Separate xdata and ydata
# This is done to ensure that both dataframes are in the
# same order for GJAM
xdata <- full_data[,1:18]
ydata <- full_data[,c(1,19:61)]

colnames(xdata)[18] <- 'marea'

# Convert xdata into the correct data types
xdata <- xdata |>
  mutate(uniqueID = as.factor(uniqueID),
         Hydric = as.factor(Hydric),
         Floodplain = as.factor(Floodplain),
         direction = as.factor(direction)) |>
  # Add unique ID numbers to rownames
  column_to_rownames(var = 'uniqueID')

ydata <- ydata |>
  mutate(uniqueID = as.factor(uniqueID)) |>
  # Take out columns that we don't need
  select(-c(chainstree, chainstree2, chainstree3, chainstree4, marea.y)) |>
  # Take out columns that don't contain any information
  select(-c(No.data, Water, Unknown.tree, Wet, NA., X88888)) |>
  column_to_rownames(var = 'uniqueID') |>
  replace_na(list(No.tree = 0, Oak = 0, Elm = 0, Hickory = 0,
                  Ash = 0, Unknown.tree = 0, Poplar = 0, Maple = 0,
                  Sycamore = 0, Other.hardwood = 0, Mulberry = 0,
                  Basswood = 0, Walnut = 0, Cherry = 0, Locust = 0,
                  Hackberry = 0, Willow = 0, Buckeye = 0, Birch = 0,
                  Black.gum.sweet.gum = 0, Sweet.gum = 0, Black.gum = 0,
                  Ironwood = 0, Poplar.tulip.poplar = 0, Beech = 0,
                  Dogwood = 0, Bald.cypress = 0, Cedar.juniper = 0,
                  Tulip.poplar = 0, Tamarack = 0, Pine = 0, Alder = 0, Chestnut = 0))

# Remove rows with no information
zeros <- apply(ydata, 1, sum)
zeros <- which(zeros == 0)

ydata <- ydata[-zeros,]
xdata <- xdata[-zeros,]

# Save
save(xdata, ydata, file = 'GJAMDATA/processed_xydata.RData')
