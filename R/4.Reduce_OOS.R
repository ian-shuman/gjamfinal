## Reducing dimensions of validation data to match the training data

## Author: AM Willson

rm(list = ls())
library(tidyverse)
library(corrplot)

load('GJAMDATA/Withheld For Validation/validation_process_fixmarea.RData')

xdata <- xdata_oos

# Reduce dimensions
new.ydata <- ydata_oos |>
  # Make a combined "tulip poplar" column
  mutate(TP = Poplar + Poplar.tulip.poplar) |>
  # If more than one of the above categories was present at one corner
  # we will swee a 2 or 3. Make these 1's for presence of the taxon
  mutate(TP = if_else(TP > 1, 1, TP)) |>
  # Remove the old columns
  select(-c(Poplar, Poplar.tulip.poplar)) |>
  # Rename the new column
  rename(Poplar.tulip.poplar = TP) |>
  # Do the samw with "black gum/sweet gum"
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
           Sycamore + Buckeye + Hackberry + Mulberry + Other.hardwood +
           Chestnut) |>
  mutate(Other.hardwood.2 = if_else(Other.hardwood.2 > 1, 1, Other.hardwood.2)) |>
  select(-c(Birch, Locust, Willow, Cherry, Sycamore,
            Buckeye, Hackberry, Mulberry, Other.hardwood,
            Chestnut)) |>
  rename(Other.hardwood = Other.hardwood.2)

# Redo this for effrot
new.edata <- effort |>
  rowwise() |>
  mutate(TP_dist = mean(c(Poplar_dist, Poplar.tulip.poplar_dist), na.rm = T),
         BGSG_dist = mean(c(Black.gum.sweet.gum_dist, Black.gum_dist, Sweet.gum_dist), na.rm = T),
         Other.conifer_dist = mean(c(Bald.cypress_dist, Pine_dist), na.rm = T),
         Other.hardwood.2_dist = mean(c(Birch_dist, Locust_dist, Willow_dist, Cherry_dist,
                                        Sycamore_dist, Buckeye_dist, Hackberry_dist, Mulberry_dist,
                                        Chestnut_dist), na.rm = T))

new.edata <- new.edata |>
  mutate_all(~ifelse(is.nan(.), NA, .)) |>
  select(-c(Poplar_dist, Poplar.tulip.poplar_dist, 
            Black.gum.sweet.gum_dist, Black.gum_dist, Sweet.gum_dist,
            Bald.cypress_dist, Pine_dist,
            Other.hardwood_dist, Birch_dist, Locust_dist, Willow_dist, Cherry_dist,
            Buckeye_dist, Hackberry_dist, Mulberry_dist, Chestnut_dist)) |>
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
  pivot_longer(cols = c(Elm:Other.hardwood)) |>
  group_by(name) |>
  summarize(count = length(which(value == 1))) |>
  ggplot(aes(x = reorder(name, count), y = count)) +
  geom_point() +
  coord_flip()

# Now we just need to reorder the columns
new.ydata <- new.ydata |>
  select(No.tree, Oak, Elm,
         Hickory, Ash, Maple,
         Basswood, Walnut, Ironwood,
         Beech, Dogwood, Poplar.tulip.poplar,
         Black.gum.sweet.gum, Other.conifer, Other.hardwood)

new.edata <- new.edata |>
  select(No.tree_dist, Oak_dist, Elm_dist,
         Hickory_dist, Ash_dist, Maple_dist,
         Basswood_dist, Walnut_dist, Ironwood_dist,
         Beech_dist, Dogwood_dist, Poplar.tulip.poplar_dist,
         Black.gum.sweet.gum_dist, Other.conifer_dist, Other.hardwood_dist)

# Rename
ydata <- new.ydata
edata <- new.edata

# Save
save(xdata, ydata, file = 'GJAMDATA/Withheld For Validation/validation_process2.RData')
