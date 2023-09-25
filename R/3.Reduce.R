## Reducing ydata to improve computational time

## Author: AM Willson

rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

load('GJAMDATA/process_FINALSOILS.RData')

# Make into a more usable format
longydata <- ydata |>
  pivot_longer(cols = c(No.tree:Chestnut),
              names_to = 'Taxon', values_to = 'PA')

# How many presences does each taxon have across all corners?
counts <- longydata |>
  group_by(Taxon) |>
  summarize(count = length(which(PA == 1))) |>
  arrange(desc(count))
print(counts, n = 32)

# There are < 10 observations of alder, tulip poplar, cedar/juniper,
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
p1

## Based on this, I would definitely say that there is a 
## reasonable cut-off around "Walnut" where we could group all other
## species together

## I will group everything from Bald cypress:Black.gum in
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
         Taxon = replace(Taxon, Taxon == 'Mulberry', 'Other.hardwood'),
         Taxon = replace(Taxon, Taxon == 'Black.gum', 'Black.gum.sweet.gum'),
         Taxon = replace(Taxon, Taxon == 'Poplar', 'Poplar.tulip.poplar'),
         Taxon = replace(Taxon, Taxon == 'Sweet.gum', 'Black.gum.sweet.gum'),
         Taxon = replace(Taxon, Taxon == 'Tulip.poplar', 'Poplar.tulip.poplar'))

# Let's visualize again
p2 <- longydata |>
  group_by(Taxon) |>
  summarize(count = length(which(PA == 1))) |>
  ggplot(aes(x = reorder(Taxon, count), y = count)) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  ylab('Number of Observations') + xlab('')
p2

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
p3

# Now, we need to make sure that we didn't create any more rows with
# only zeros
zeros <- apply(new.ydata, 1, sum)
zeros <- which(zeros == 0)

# Looks like we're good!

ydata <- new.ydata

# I'm going to save this in a different object for now
save(xdata, ydata, file = 'GJAMDATA/process2_FINALSOILS.RData')

