rm(list = ls())

# Read in effort data
dist <- read.csv('test/Sample_effort_DataStructure.csv')

# Take out unnecessary columns (to match ydata)
dist <- dist %>%
  select(-(uniqueID:NA.)) %>%
  select(-c(No.data_dist, Water_dist, Unknown.tree_dist, NA_dist))

# Replace with something numeric so that we can convert to numeric columns
dist[dist == 'missing'] <- -999
dist[dist == 'illegible'] <- -999
  
# Convert columns to all be numeric
dist <- dist %>%
  mutate(Oak_dist = as.numeric(Oak_dist),
         Hickory_dist = as.numeric(Hickory_dist),
         Poplar.tulip.poplar_dist = as.numeric(Poplar.tulip.poplar_dist),
         Ash_dist = as.numeric(Ash_dist),
         Maple_dist = as.numeric(Maple_dist),
         Beech_dist = as.numeric(Beech_dist),
         Ironwood_dist = as.numeric(Ironwood_dist),
         Dogwood_dist = as.numeric(Dogwood_dist),
         Walnut_dist = as.numeric(Walnut_dist),
         Other.hardwood_dist = as.numeric(Other.hardwood_dist))

# Flag columns with missing values
missings <- which(dist == -999, arr.ind = T)
missings <- missings[,2]
missings <- unique(missings)

# Find averages in those columns
av_oak <- dist %>%
  select(Oak_dist) %>%
  filter(Oak_dist > -999) %>%
  summarize(mean = mean(Oak_dist, na.rm = T))
av_hickory <- dist %>%
  select(Hickory_dist) %>%
  filter(Hickory_dist > -999) %>%
  summarize(mean = mean(Hickory_dist, na.rm = T))
av_poplartp <- dist %>%
  select(Poplar.tulip.poplar_dist) %>%
  filter(Poplar.tulip.poplar_dist > -999) %>%
  summarize(mean = mean(Poplar.tulip.poplar_dist, na.rm = T))
av_ash <- dist %>%
  select(Ash_dist) %>%
  filter(Ash_dist > -999) %>%
  summarize(mean = mean(Ash_dist, na.rm = T))
av_maple <- dist %>%
  select(Maple_dist) %>%
  filter(Maple_dist > -999) %>%
  summarize(mean = mean(Maple_dist, na.rm = T))
av_beech <- dist %>%
  select(Beech_dist) %>%
  filter(Beech_dist > -999) %>%
  summarize(mean = mean(Beech_dist, na.rm = T))
av_ironwood <- dist %>%
  select(Ironwood_dist) %>%
  filter(Ironwood_dist > -999) %>%
  summarize(mean = mean(Ironwood_dist, na.rm = T))
av_dogwood <- dist %>%
  select(Dogwood_dist) %>%
  filter(Dogwood_dist > -999) %>%
  summarize(mean = mean(Dogwood_dist, na.rm = T))
av_walnut <- dist %>%
  select(Walnut_dist) %>%
  filter(Walnut_dist > -999) %>%
  summarize(mean = mean(Walnut_dist, na.rm = T))
av_oh <- dist %>%
  select(Other.hardwood_dist) %>%
  filter(Other.hardwood_dist > -999) %>%
  summarize(mean = mean(Other.hardwood_dist, na.rm = T))

# Replace -999 placeholder with average for the species
dist <- dist %>%
  mutate(Oak_dist = ifelse(Oak_dist == -999, av_oak$mean, Oak_dist),
         Hickory_dist = ifelse(Hickory_dist == -999, av_hickory$mean, Hickory_dist),
         Poplar.tulip.poplar_dist = ifelse(Poplar.tulip.poplar_dist == -999, av_poplartp$mean, Poplar.tulip.poplar_dist),
         Ash_dist = ifelse(Ash_dist == -999, av_ash$mean, Ash_dist),
         Maple_dist = ifelse(Maple_dist == -999, av_maple$mean, Maple_dist),
         Beech_dist = ifelse(Beech_dist == -999, av_beech$mean, Beech_dist),
         Ironwood_dist = ifelse(Ironwood_dist == -999, av_ironwood$mean, Ironwood_dist),
         Dogwood_dist = ifelse(Dogwood_dist == -999, av_dogwood$mean, Dogwood_dist),
         Walnut_dist = ifelse(Walnut_dist == -999, av_walnut$mean, Walnut_dist),
         Other.hardwood_dist = ifelse(Other.hardwood_dist == -999, av_oh$mean, Other.hardwood_dist))

# Replace 0s with 0.01 because of divide by zero for inverting
dist[dist==0] <- 0.01

# Make species-specific effort
effort <- 1/dist

# Make site-aggregated effort
site_effort <- apply(dist, 1, mean, na.rm = T)
site_effort[is.nan(site_effort)] <- NA

# Save
save(effort, site_effort, file = 'test/effort.RData')
