## Make the validation data into ecosystems

## Author: AM Willson

rm(list = ls())

library(dplyr)

load('GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced.RData')

# Convert to ecosystems as in 3.Reduce_ecosystem.R
ydata_oos <- ydata_oos |>
  dplyr::mutate(Prairie = No.tree,
                Savanna = Oak + Hickory,
                Forest = Elm + Ash + Maple + Basswood + Walnut + Black.gum.sweet.gum +
                  Ironwood + Beech + Dogwood + Poplar.tulip.poplar + Other.conifer +
                  Other.hardwood) |>
  dplyr::mutate(Prairie = as.numeric(Prairie),
                Savanna = as.numeric(Savanna),
                Forest = as.numeric(Forest)) |>
  dplyr::mutate(Savanna = dplyr::if_else(Savanna > 1, 1, Savanna),
                Forest = dplyr::if_else(Forest > 1, 1, Forest)) |>
  dplyr::select(c(Prairie, Forest, Savanna)) |>
  dplyr::mutate(Savanna = dplyr::if_else(Savanna == 1 & Forest == 1, 0, Savanna))

save(xdata_oos, ydata_oos, file = 'GJAMDATA/Withheld For Validation/validation_processed_xydata_fixmarea_reduced_ecosystem.RData')
