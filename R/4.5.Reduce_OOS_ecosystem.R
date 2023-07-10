## Make the validation data into ecosystems

library(tidyverse)

rm(list = ls())

load('GJAMDATA/Withheld For Validation/validation_process2.RData')

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

edata <- edata |>
  rowwise() |>
  mutate(Prairie_dist = No.tree_dist,
         Savanna_dist = mean(c(Oak_dist, Hickory_dist), na.rm = T),
         Forest_dist = mean(c(Elm_dist, Ash_dist, Maple_dist, Basswood_dist,
                              Walnut_dist, Black.gum.sweet.gum_dist,
                              Ironwood_dist, Beech_dist, Dogwood_dist,
                              Poplar.tulip.poplar_dist, Other.conifer_dist,
                              Other.hardwood_dist), na.rm = T))
  
edata <- edata |>
  mutate_all(~ifelse(is.nan(.), NA, .)) |>
  select(c(Prairie_dist, Savanna_dist, Forest_dist))

save(xdata, edata, ydata, file = 'GJAMDATA/Withheld For Validation/validation_process_reduce.RData')
