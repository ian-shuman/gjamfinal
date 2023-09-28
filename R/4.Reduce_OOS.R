## Reducing ydata for out-of-sample data to match the in-sample ydata

## Author: AM Willson

rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea.RData')

new.ydata <- ydata_oos |>
  # Make a combined "tulip poplar column"
  mutate(TP = Poplar + Poplar.tulip.poplar) |>
  # If more than one of the above categories was present at one corner
  # we will see a 2 or 3. Make these 1's for presence of the taxon
  mutate(TP = if_else(TP > 1, 1, TP)) |>
  # Remove the old columns
  select(-c(Poplar, Poplar.tulip.poplar)) |>
  # Rename the new column
  rename(Poplar.tulip.poplar = TP) |>
  # Do the same with "black gum/sweet gum"
  mutate(BGSG = Black.gum.sweet.gum + Sweet.gum + Black.gum) |>
  mutate(BGSG = if_else(BGSG > 1, 1, BGSG)) |>
  select(-c(Black.gum.sweet.gum, Sweet.gum, Black.gum)) |>
  rename(Black.gum.sweet.gum = BGSG) |>
  # Repeat with our category of "other conifers"
  mutate(Other.conifer = Bald.cypress + Pine) |>
  mutate(Other.conifer = if_else(Other.conifer > 1, 1, Other.conifer)) |>
  select(-c(Bald.cypress, Pine)) |>
  # Repeat with our category of "other hardwoods"
  mutate(Other.hardwood.2 = Birch + Locust + Willow + Cherry +
           Sycamore + Buckeye + Hackberry + Mulberry + Other.hardwood + Chestnut) |>
  mutate(Other.hardwood.2 = if_else(Other.hardwood.2 > 1, 1, Other.hardwood.2)) |>
  select(-c(Birch, Locust, Willow, Cherry, Sycamore,
            Buckeye, Hackberry, Mulberry, Other.hardwood, Chestnut)) |>
  rename(Other.hardwood = Other.hardwood.2)

new.ydata |>
  pivot_longer(Elm:Other.hardwood, names_to = 'taxon', values_to = 'PA') |>
  mutate(taxon = if_else(taxon == 'No.tree', 'No Tree', taxon),
         taxon = if_else(taxon == 'Other.hardwood', 'Other Hardwood', taxon),
         taxon = if_else(taxon == 'Black.gum.sweet.gum', 'Black Gum/Sweet Gum', taxon),
         taxon = if_else(taxon == 'Poplar.tulip.poplar', 'Poplar/Tulip Poplar', taxon),
         taxon = if_else(taxon == 'Other.conifer', 'Other Conifer', taxon)) |>
  group_by(taxon) |>
  summarize(count = length(which(PA == 1))) |>
  ggplot(aes(x = reorder(taxon, count), y = count)) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  xlab('') + ylab('Number of Observations')

# Now, we need to make sure that we didn't create any more rows with
# only zeros
zeros <- apply(new.ydata, 1, sum)
zeros <- which(zeros == 0)

# Looks like we're good!
ydata_oos <- new.ydata

save(xdata_oos, ydata_oos, file = 'GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced.RData')
