## Reducing ydata to improve computational time

## This script is really awkwardly placed betwen steps 1 & 2 right
## now. This should be updated prior to publication

## Author: AM Willson

rm(list = ls())
library(tidyverse)
library(corrplot)

load('GJAM DATA/process.RData')
load('GJAM DATA/effort.RData')

# Make into a more usable format
longydata <- ydata |>
  pivot_longer(cols = c(No.tree:Chestnut))
colnames(longydata) <- c('Taxon', 'PA')

longeffort <- effort |>
  pivot_longer(cols = c(No.tree_dist:Chestnut_dist))
colnames(longeffort) <- c('Taxon', 'Effort')

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
p1 <- longydata |>
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
         Taxon = replace(Taxon, Taxon == 'Locust', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Willow', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Cherry', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Sycamore', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Buckeye', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Hackberry', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Mulberry', 'Other.hardwood'))
## Do the same for the effort

longeffort <- longeffort |>
  mutate(Taxon = replace(Taxon, Taxon == 'Bald.cypress_dist', 'Other.conifer_dist'),
         Taxon = replace(Taxon, Taxon == 'Pine_dist', 'Other.conifer_dist'),
         Taxon = replace(Taxon, Taxon == 'Birch_dist', 'Other.hardwood_dist'),
         Taxon = replace(Taxon, Taxon == 'Tamarack_dist', 'Other.conifer_dist'),
         Taxon = replace(Taxon, Taxon == 'Locust_dist', 'Other.hardwood_dist'),
         Taxon = replace(Taxon, Taxon == 'Willow_dist', 'Other.hardwood_dist'),
         Taxon = replace(Taxon, Taxon == 'Cherry_dist', 'Other.hardwood_dist'),
         Taxon = replace(Taxon, Taxon == 'Sycamore_dist', 'Other.hardwood_dist'),
         Taxon = replace(Taxon, Taxon == 'Buckeye_dist', 'Other.hardwood_dist'),
         Taxon = replace(Taxon, Taxon == 'Hackberry_dist', 'Other.hardwood_dist'),
         Taxon = replace(Taxon, Taxon == 'Mulberry_dist', 'Other.hardwood_dist'))

# Let's visualize again
p2 <- longydata |>
  group_by(Taxon) |>
  summarize(count = length(which(PA == 1))) |>
  ggplot(aes(x = reorder(Taxon, count), y = count)) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  ylab('Number of Observations') + xlab('')

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
  # Do the same with "black gum/sweet gum"
  mutate(BGSG = Black.gum.sweet.gum + Sweet.gum + Black.gum) |>
  mutate(BGSG = if_else(BGSG > 1, 1, BGSG)) |>
  select(-c(Black.gum.sweet.gum, Sweet.gum, Black.gum)) |>
  rename(Black.gum.sweet.gum = BGSG) |>
  # Repeat with our category of "other conifers" defined above
  mutate(Other.conifer = Bald.cypress + Pine + Tamarack + Cedar.juniper) |>
  mutate(Other.conifer = if_else(Other.conifer > 1, 1, Other.conifer)) |>
  select(-c(Bald.cypress, Pine, Tamarack, Cedar.juniper)) |>
  # Repeat with our category of "other hardwoods" defined above
  mutate(Other.hardwood.2 = Birch + Locust + Willow + Cherry +
           Sycamore + Buckeye + Hackberry + Mulberry + Other.hardwood +
           Alder + Chestnut) |>
  mutate(Other.hardwood.2 = if_else(Other.hardwood.2 > 1, 1, Other.hardwood.2)) |>
  select(-c(Birch, Locust, Willow, Cherry, Sycamore,
            Buckeye, Hackberry, Mulberry, Other.hardwood,
            Alder, Chestnut)) |>
  rename(Other.hardwood = Other.hardwood.2)

p3 <- new.ydata |>
  pivot_longer(No.tree:Other.hardwood, names_to = 'taxon', values_to = 'PA') |>
  mutate(taxon = if_else(taxon == 'No.tree', 'No Tree', taxon),
         taxon = if_else(taxon == 'Other.hardwood', 'Other Hardwood', taxon),
         taxon = if_else(taxon == 'Black.gum.sweet.gum', 'Black Gum/Sweet Gum', taxon),
         taxon = if_else(taxon == 'Poplar.tulip.poplar', 'Poplar/Tulip Poplar', taxon),
         taxon = if_else(taxon == 'Other.conifer', 'Other Conifer', taxon)) |>
  group_by(taxon) |>
  summarize(count = length(which(PA == 1))) |>
  ggplot(aes(x = reorder(taxon, count), y =count)) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  xlab('') + ylab('Number of Observations')

# Redo for effort
## This is done in two steps because we have to use the rowwise function to take means
new.edata <- effort |>
  rowwise() |>
  mutate(TP_dist = mean(c(Poplar_dist, Poplar.tulip.poplar_dist, Tulip.poplar_dist), na.rm = T),
         BGSG_dist = mean(c(Black.gum.sweet.gum_dist, Black.gum_dist, Sweet.gum_dist), na.rm = T),
         Other.conifer_dist = mean(c(Bald.cypress_dist, Pine_dist, Tamarack_dist, Cedar.juniper_dist), na.rm = T),
         Other.hardwood.2_dist = mean(c(Birch_dist, Locust_dist, Willow_dist, Cherry_dist,
                                        Sycamore_dist, Buckeye_dist, Hackberry_dist, Mulberry_dist, 
                                        Alder_dist, Chestnut_dist), na.rm = T))

new.edata <- new.edata |>
  mutate_all(~ifelse(is.nan(.), NA, .)) |>
  select(-c(Poplar_dist, Poplar.tulip.poplar_dist, Tulip.poplar_dist,
            Black.gum.sweet.gum_dist, Black.gum_dist, Sweet.gum_dist,
            Bald.cypress_dist, Pine_dist, Tamarack_dist, Cedar.juniper_dist,
            Other.hardwood_dist, Birch_dist, Locust_dist, Willow_dist, Cherry_dist, Sycamore_dist,
            Buckeye_dist, Hackberry_dist, Mulberry_dist, Alder_dist, Chestnut_dist)) |>
  rename(Poplar.tulip.poplar_dist = TP_dist,
         Black.gum.sweet.gum_dist = BGSG_dist,
         Other.hardwood_dist = Other.hardwood.2_dist)

# Now, we need to make sure that we didn't create any more rows with
# only zeros

zeros <- apply(new.ydata, 1, sum)
zeros <- which(zeros == 0)

zeros <- apply(new.edata, 1, sum)
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
edata <- new.edata

# I'm going to save this in a different object for now
save(xdata, ydata, edata, site_effort, file = 'GJAM DATA/process2.RData')
