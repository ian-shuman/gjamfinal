## Creating ecosystem level response data from taxon-level response

## Author: AM Willson

rm(list = ls())

library(dplyr)

load('GJAMDATA/processed_xydata_2.RData')

# Reduce ydata to have only savanna/prairie/forest taxa
new.ydata <- ydata |>
  # No tree is representative of prairies
  mutate(Prairie = No.tree,
         # Oaks and hickories are savanna taxa
         Savanna = Oak + Hickory,
         # All other trees are assumed to be associated with forests
         Forest = Elm + Ash + Maple + Basswood + Walnut +
           Black.gum.sweet.gum + Ironwood + Beech + Dogwood +
           Poplar.tulip.poplar + Other.conifer + Other.hardwood) |>
  # Formatting
  mutate(Prairie = as.numeric(Prairie),
         Savanna = as.numeric(Savanna),
         Forest = as.numeric(Forest)) |>
  # If more than one tree fitting the ecosystem type was present 
  # at a corner, then the column will be greater than 1.
  # since this is presence/absence, make = 1
  mutate(Prairie = if_else(Prairie > 1, 1, Prairie),
         Savanna = if_else(Savanna > 1, 1, Savanna),
         Forest = if_else(Forest > 1, 1, Forest)) |>
  select(c(Prairie, Savanna, Forest)) |>
  # If the corner is marked as both savanna and prairie, make it a
  # savanna because this means that at least one tree was present
  mutate(Prairie = if_else(Prairie == 1 & Savanna == 1, 0, Prairie),
         # if the corner is marked as both savanna and forest, make it a
         # forest because this means at least one forest tree was present
         Savanna = if_else(Savanna == 1 & Forest == 1, 0, Savanna))

# Check to make sure we have something in every row
zeros <- apply(new.ydata, 1, sum)
zeros <- which(zeros != 1)

# Good to go
ydata <- new.ydata

save(ydata, xdata, file = 'GJAMDATA/processed_xydata_2_ecosystem.RData')
