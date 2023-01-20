## Reducing ydata to improve computational time

## This script is really awkwardly placed betwen steps 1 & 2 right
## now. This should be updated prior to publication

## Author: AM Willson

rm(list = ls())
library(tidyverse)
library(corrplot)

load('GJAM DATA/process.RData')

# Make into a more usable format
longydata <- ydata |>
  pivot_longer(cols = c(No.tree:Chestnut))
colnames(longydata) <- c('Taxon', 'PA')

# How many presences at a corner does each taxon have?
counts <- longydata |>
  group_by(Taxon) |>
  summarize(count = length(which(PA == 1))) |>
  arrange(desc(count))

# There are < 10 observations of alder, tulip poplar, cedar juniper,
# and chestnut, so those shouldn't be categories
# Let's remove alder, cedar juniper, and chestnut
longydata <- longydata |>
  filter(Taxon != 'Alder',
         Taxon != 'Cedar.juniper',
         Taxon != 'Chestnut')

# Make sure it looks correct
test <- longydata |>
  group_by(Taxon) |>
  summarize(count = length(which(PA == 1))) |>
  arrange(desc(count))
print(test, n = nrow(test))

# Since we have a category of "tulip poplar/poplar" that's much bigger,
# let's combine the "tulip poplar" "poplar" and "tulip poplar/poplar"
# categories
#longydata <- longydata |>
#  mutate(Taxon = replace(Taxon, Taxon == 'Tulip.poplar', 'Poplar.tulip.poplar'),
#         Taxon = replace(Taxon, Taxon == 'Poplar', 'Poplar.tulip.poplar'))

# Make sure it looks correct
test <- longydata |>
  group_by(Taxon) |>
  summarize(count = length(which(PA == 1))) |>
  arrange(desc(count))
print(test, n = nrow(test))

# Now let's plot the number of observations of each taxon
longydata |>
  group_by(Taxon) |>
  mutate(Taxon = replace(Taxon, Taxon == 'No.tree', 'No tree'),
         Taxon = replace(Taxon, Taxon == 'Poplar.tulip.poplar', 'Poplar/Tulip Poplar'),
         Taxon = replace(Taxon, Taxon == 'Black.gum.sweet.gum', 'Black Gum/Sweet Gum'),
         Taxon = replace(Taxon, Taxon == 'Other.hardwood', 'Other Hardwood'),
         Taxon = replace(Taxon, Taxon == 'Black.gum', 'Black Gum'),
         Taxon = replace(Taxon, Taxon == 'Sweet.gum', 'Sweet Gum'),
         Taxon = replace(Taxon, Taxon == 'Bald.cypress', 'Bald Cypress'),
         Taxon = replace(Taxon, Taxon == 'Tulip.poplar', 'Tulip Poplar')) |>
  summarize(count = length(which(PA == 1))) |>
  ggplot(aes(x = reorder(Taxon, count), y = count)) +
  geom_point() +
  coord_flip() +
  ylab('Number of Observations') +
  xlab('') +
  theme_minimal()

## Based on this, I would definitely say that there is a 
## reasonable cut-off around "Walnut" where we could group all other
## species together

## Alternatively, species below "Walnut" could be grouped with a
## functionally similar species above "Walnut" where applicable

## For now, I will group everything from Bald cypress:Black.gum in
## either "Other hardwood" or "Other conifer"

longydata <- longydata |>
  mutate(Taxon = replace(Taxon, Taxon == 'Bald.cypress', 'Other.conifer'),
         Taxon = replace(Taxon, Taxon == 'Pine', 'Other.conifer'),
         Taxon = replace(Taxon, Taxon == 'Birch', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Tamarack', 'Other.conifer'),
         Taxon = replace(Taxon, Taxon == 'Sweet.gum', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Locust', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Willow', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Cherry', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Sycamore', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Buckeye', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Hackberry', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Mulberry', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Black.gum', 'Other.hardwood'))

# Let's visualize again
longydata |>
  group_by(Taxon) |>
  summarize(count = length(which(PA == 1))) |>
  ggplot(aes(x = reorder(Taxon, count), y = count)) +
  geom_point() +
  coord_flip()

## This seems a lot more reasonable now. Let's insert this information
## back into our original "ydata" data object

new.ydata <- ydata |>
  # Make a combined "tulip poplar column"
  mutate(TP = Poplar + Poplar.tulip.poplar + Tulip.poplar) |>
  # If more than on of the above categories was present at one corner
  # we will see a 2 or 3. Make these 1's for presence of the taxon
  mutate(TP = if_else(TP > 1, 1, TP)) |>
  # Remove the old columns
  select(-c(Poplar, Poplar.tulip.poplar, Tulip.poplar)) |>
  # Rename the new column
  rename(Poplar.tulip.poplar = TP) |>
  # Repeat with our category of "other conifers" defined above
  mutate(Other.conifer = Bald.cypress + Pine + Tamarack + Cedar.juniper) |>
  mutate(Other.conifer = if_else(Other.conifer > 1, 1, Other.conifer)) |>
  select(-c(Bald.cypress, Pine, Tamarack, Cedar.juniper)) |>
  # Repeat with our category of "other hardwoods" defined above
  mutate(Other.hardwood.2 = Birch + Sweet.gum + Locust + Willow + Cherry +
           Sycamore + Buckeye + Hackberry + Mulberry + Black.gum + Other.hardwood +
           Alder + Chestnut) |>
  mutate(Other.hardwood.2 = if_else(Other.hardwood.2 > 1, 1, Other.hardwood.2)) |>
  select(-c(Birch, Sweet.gum, Locust, Willow, Cherry, Sycamore,
            Buckeye, Hackberry, Mulberry, Black.gum, Other.hardwood,
            Alder, Chestnut)) |>
  rename(Other.hardwood = Other.hardwood.2)

# Now, we need to make sure that we didn't create any more rows with
# only zeros

zeros <- apply(ydata, 1, sum)
zeros <- which(zeros == 0)

# Let's make sure it looks like we think it does
new.ydata |>
  pivot_longer(cols = c(No.tree:Other.hardwood)) |>
  group_by(name) |>
  summarize(count = length(which(value == 1))) |>
  ggplot(aes(x = reorder(name, count), y = count)) +
  geom_point() +
  coord_flip()

# Looks like we're good!

ydata <- new.ydata

## Now let's look at the xdata
## Are there any variables that are significantly correlated?
## We'll take out the ones that we can't use first
xdatatest <- xdata |>
  select(-c(Hydric, Floodplain, marea, mean.AspectProjected))

# Find covariance and correlation
xcov <- cov(xdatatest)
xcor <- cov2cor(xcov)

# Make the diagonal 0 instead of 1 just for visualization
diag(xcor) <- 0

# Visualize
corrplot(xcor, method = 'circle')

## It looks like we do have some correlated predictors:
## mean.SlopeProjected and mean.SWI
## mean.CLA & mean.SAN
## MeanTEMP & GS.ppet to name a few
## Let's look at this in numbers
xcor <- as.data.frame(xcor)
xcor <- xcor |>
  rownames_to_column() |>
  pivot_longer(cols = c(mean.SlopeProjected:GS.ppet))
colnames(xcor) <- c('Covar1', 'Covar2', 'Corr')

print.xcor <- xcor |>
  arrange(desc(abs(Corr)))

print(print.xcor, n = nrow(print.xcor))

print.xcor |>
  ggplot(aes(x = 1:nrow(print.xcor), y = Corr)) +
  geom_point()

## There are a handful of covariate pairs that have correlation > 0.5
## These should probably be reduced

## Let's just run an experiment
## Take out MeanTEMP, mean.SAN, mean.SlopeProjected
xcor <- xcor |>
  filter(Covar1 != 'MeanTEMP',
         Covar2 != 'MeanTEMP',
         Covar1 != 'mean.SAN',
         Covar2 != 'mean.SAN',
         Covar1 != 'mean.SlopeProjected',
         Covar2 != 'mean.SlopeProjected')

xcor |>
  arrange(desc(abs(Corr))) |>
  ggplot(aes(x = 1:nrow(xcor), y = Corr)) +
  geom_point()

# Now let's put our experiment back into the data file
xdata_red <- xdata |>
  select(-c(MeanTEMP, mean.SAN, mean.SlopeProjected))

# I'm going to save this in a different object for now
save(xdata, ydata, xdata_red, file = 'GJAM DATA/process2.RData')
