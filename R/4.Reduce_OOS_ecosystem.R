## Make the validation data into ecosystems

## Author: AM Willson

rm(list = ls())

library(dplyr)

load('GJAMDATA/Withheld For Validation/validation_process2.RData')

# Convert to ecosystems as in 3.Reduce_ecosystem.R
ydata <- ydata |>
  mutate(Prairie = No.tree,
         Savanna = Oak + Hickory,
         Forest = Elm + Ash + Maple + Basswood + Walnut + Black.gum.sweet.gum +
           Ironwood + Beech + Dogwood + Poplar.tulip.poplar + Other.conifer +
           Other.hardwood) |>
  mutate(Prairie = as.numeric(Prairie),
         Savanna = as.numeric(Savanna),
         Forest = as.numeric(Forest)) |>
  mutate(Savanna = if_else(Savanna > 1, 1, Savanna),
         Forest = if_else(Forest > 1, 1, Forest)) |>
  select(c(Prairie, Forest, Savanna)) |>
  mutate(Savanna = if_else(Savanna == 1 & Forest == 1, 0, Savanna))

save(xdata, ydata, file = 'GJAMDATA/Withheld For Validation/validation_process2_ecosystem.RData')
