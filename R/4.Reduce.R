## Reducing ydata to improve computational time

## Author: AM Willson

rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

load('GJAMDATA/processed_xydata.RData')

new.ydata <- ydata |>
  # Make a combined "tulip poplar column"
  dplyr::mutate(TP = Poplar + Poplar.tulip.poplar + Tulip.poplar) |>
  # If more than on of the above categories was present at one corner
  # we will see a 2 or 3. Make these 1's for presence of the taxon
  dplyr::mutate(TP = dplyr::if_else(TP > 1, 1, TP)) |>
  # Remove the old columns
  dplyr::select(-c(Poplar, Poplar.tulip.poplar, Tulip.poplar)) |>
  # Rename the new column
  dplyr::rename(Poplar.tulip.poplar = TP) |>
  # Do the same with "black gum/sweet gum"
  dplyr::mutate(BGSG = Black.gum.sweet.gum + Sweet.gum + Black.gum) |>
  dplyr::mutate(BGSG = dplyr::if_else(BGSG > 1, 1, BGSG)) |>
  dplyr::select(-c(Black.gum.sweet.gum, Sweet.gum, Black.gum)) |>
  dplyr::rename(Black.gum.sweet.gum = BGSG) |>
  # Repeat with our category of "other conifers" defined above
  dplyr::mutate(Other.conifer = Bald.cypress + Pine + Tamarack + Cedar.juniper) |>
  dplyr::mutate(Other.conifer = dplyr::if_else(Other.conifer > 1, 1, Other.conifer)) |>
  dplyr::select(-c(Bald.cypress, Pine, Tamarack, Cedar.juniper)) |>
  # Repeat with our category of "other hardwoods" defined above
  dplyr::mutate(Other.hardwood.2 = Birch + Locust + Willow + Cherry +
                  Sycamore + Buckeye + Hackberry + Mulberry + Other.hardwood +
                  Alder + Chestnut) |>
  dplyr::mutate(Other.hardwood.2 = dplyr::if_else(Other.hardwood.2 > 1, 1, Other.hardwood.2)) |>
  dplyr::select(-c(Birch, Locust, Willow, Cherry, Sycamore,
                   Buckeye, Hackberry, Mulberry, Other.hardwood,
                   Alder, Chestnut)) |>
  dplyr::rename(Other.hardwood = Other.hardwood.2)

new.ydata |>
  tidyr::pivot_longer(No.tree:Other.hardwood, names_to = 'taxon', values_to = 'PA') |>
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'No.tree', 'No Tree', taxon),
                taxon = dplyr::if_else(taxon == 'Other.hardwood', 'Other Hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Black.gum.sweet.gum', 'Black Gum/Sweet Gum', taxon),
                taxon = dplyr::if_else(taxon == 'Poplar.tulip.poplar', 'Poplar/Tulip Poplar', taxon),
                taxon = dplyr::if_else(taxon == 'Other.conifer', 'Other Conifer', taxon)) |>
  dplyr::group_by(taxon) |>
  dplyr::summarize(count = length(which(PA == 1))) |>
  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(taxon, count), y = count)) +
  ggplot2::geom_point() +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::xlab('') + ggplot2::ylab('Number of Observations')

# Now, we need to make sure that we didn't create any more rows with
# only zeros
zeros <- apply(new.ydata, 1, sum)
zeros <- which(zeros == 0)

# Looks like we're good!

ydata <- new.ydata

# Save intermediate output
save(xdata, ydata, file = 'GJAMDATA/processed_xydata_2.RData')
