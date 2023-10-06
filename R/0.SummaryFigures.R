rm(list=ls(all=TRUE))
library(tidyverse)
library(readr)
library(plyr)
library(RColorBrewer)
library(viridis)
library(stringr)
library(cowplot)
states <- map_data('state') |>
  filter(region %in% c('indiana', 'illinois'))

### Summary Figures for Y Data

#Step 1 - We need to reformat the Y data with lat/long info to match that used in the analyses, 
#so we will be using the same code as on GitHub but with an "older" version of the dataset
#that still has lat/long attached


yfiles <- list.files("/Volumes/Seagate Exp/Data for Chris/Summary Figures/Data/Y")
yedata_list <- list()
for(i in 1:length(yfiles)){
  filename <- yfiles[i]
  pathname <- paste0("/Volumes/Seagate Exp/Data for Chris/Summary Figures/Data/Y/",filename)
  yedata_list[[i]] <- read.csv(pathname)
}
ydata_list <- list()
edata_list <- list()

# Separate the ydata and the edata
for(i in 1:length(yfiles)){
  tempdata <- yedata_list[[i]]
  
  ydata_list[[i]] <- tempdata %>%
    select((colnames(tempdata)[!grepl('dist', colnames(tempdata), fixed = T)]))
  
  edata_list[[i]] <- tempdata %>%
    select(colnames(tempdata)[grepl('dist', colnames(tempdata), fixed = T)])
}

# Storage
columns <- c()

# Find names of all columns
for(i in 1:length(yfiles)){
  tempdata <- ydata_list[[i]]
  columns <- c(columns, colnames(tempdata))
}
columns <- unique(columns)

# Make function for "not in"
`%nin%` <- Negate(`%in%`)

# Add columns that don't exist in a given matrix
for(i in 1:length(yfiles)){
  tempdata <- ydata_list[[i]]
  cols <- colnames(tempdata)
  save <- which(columns %nin% cols)
  newcols <- (ncol(tempdata)+1):(ncol(tempdata)+length(save))
  tempdata[,newcols] <- 0
  tempdata <- as.data.frame(tempdata)
  colnames(tempdata)[newcols] <- columns[save]
  ydata_list[[i]] <- tempdata
}

for(i in 1:length(yfiles)){
  tempdata <- edata_list[[i]]
  cols <- colnames(tempdata)
  save <- which(columns %nin% cols)
  newcols <- (ncol(tempdata)+1):(ncol(tempdata)+length(save))
  tempdata[,newcols] <- NA
  tempdata <- as.data.frame(tempdata)
  colnames(tempdata)[newcols] <- columns[save]
  edata_list[[i]] <- tempdata
}


# Initialize unlist
ydata <- ydata_list[[1]]
ydata$filename <- rep(yfiles[1], times = nrow(ydata))

# Unlist
for(i in 2:length(yfiles)){
  dat <- ydata_list[[i]]
  dat$filename <- rep(yfiles[i], times = nrow(dat))
  ydata <- rbind(ydata, dat)
}

ydata <- ydata |>
  mutate(marea = sub('_Y.*', '', filename)) |>
  select(-filename) |>
  mutate(uniqueID = paste0(marea,'_',uniqueID))

ydata <- ydata |>
  # Take out columns that we don't need
  select(-c(chainstree, chainstree2, chainstree3, chainstree4, marea)) |>
  # Take out columns that don't contain any information
  select(-c(No.data, Water, Unknown.tree, Wet, NA., X88888)) 

longydata <- ydata |>
  pivot_longer(cols = c(No.tree:Chestnut))
colnames(longydata) <- c('uniqueID', 'X', 'Y', 'long', 'lat', 'Taxon', 'PA')


longydata <- longydata |>
  filter(Taxon != 'Alder',
         Taxon != 'Cedar.juniper',
         Taxon != 'Chestnut')
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

new.ydata <- ydata |>
  # Make a combined "tulip poplar column"
  mutate(TP = Poplar + Poplar.tulip.poplar + Tulip.poplar) |>
  # If more than on of the above categories was present at one corner
  # we will see a 2 or 3. Make these 1's for presence of the taxon
  mutate(TP = if_else(TP > 1, 1, TP)) |>
  # Remove the old columns
  select(-c(Poplar, Poplar.tulip.poplar, Tulip.poplar)) |>
  # Rename the new column
  dplyr::rename(Poplar.tulip.poplar = TP) |>
  # Do the same with "black gum/sweet gum"
  mutate(BGSG = Black.gum.sweet.gum + Sweet.gum + Black.gum) |>
  mutate(BGSG = if_else(BGSG > 1, 1, BGSG)) |>
  select(-c(Black.gum.sweet.gum, Sweet.gum, Black.gum)) |>
  dplyr::rename(Black.gum.sweet.gum = BGSG) |>
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
  dplyr::rename(Other.hardwood = Other.hardwood.2)

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

ydata <- new.ydata


##Step 2- Now we will prepare data for plotting
rm(dat, edata_list, new.ydata, p3, tempdata, ydata_list, yedata_list, cols, columns, newcols, pathname, save, longydata)

longydata <- ydata |>
  pivot_longer(cols = c(No.tree:Other.hardwood))
colnames(longydata) <- c('uniqueID', 'X', 'Y', 'long', 'lat', 'class','Class','Taxon','PA')
longydata <- subset(longydata, PA > 0)

desired_order <- c("No.tree", "Oak", "Hickory", "Ash", "Basswood", "Beech", "Black.gum.sweet.gum", "Dogwood", "Elm", "Ironwood", "Maple", "Other.conifer", "Other.hardwood", "Poplar.tulip.poplar", "Walnut")
longydata$Taxon <- factor( as.character(longydata$Taxon), levels=desired_order )


for(i in 1:nrow(ydata)){
  if(ydata$`No.tree`[i] == 1){
    ydata$class[i] <- "Prairie"
  }
  if(ydata$`Elm`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Hickory`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Ash`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Maple`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Basswood`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Walnut`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Ironwood`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Beech`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Dogwood`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Poplar.tulip.poplar`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Black.gum.sweet.gum`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Other.conifer`[i] == 1){
    ydata$class[i] <- "Forest"
  }
  if(ydata$`Other.hardwood`[i] == 1){
    ydata$class[i] <- "Forest"
  }
}
ydata[, 'class'] <- "Savanna"
ydata$Class <- ydata$class



#Setp 3- Now we will actually plot

#Plot Ecosystem Class
ggplot() +
  geom_point(data = ydata, aes(x = long, y = lat, color = Class), shape = ".") +
  scale_color_manual(values = c("Prairie" = '#bb5566', "Savanna" = '#ddaa34', "Forest" = '#002a53'))+
  labs(color = "Ecosystem Class")+
  guides(colour = guide_legend(override.aes = list(shape= 16, size = 7))) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))
#c("Prairie" = 'tan', "Savanna" = "orange", "Forest" = 'darkgreen') alternative palate that's more intuitive



#Plot Taxa Class
pal <- c('#bb5566',
         '#ddaa34', '#ecd08f',
         '#002a53', '#004488', '#4c7cac', '#8aa9c8', '#c2d2e2', '#dee7f0',
         '#005f5f', '#008b8b', '#38a5a5', '#63b9b9', '#8ecdcd', '#c1e4e4')

ggplot() +
  geom_point(data = longydata, aes(x = long, y = lat, color = Taxon), shape = ".") +
  guides(colour = guide_legend(override.aes = list(shape= 16, size = 7))) +
  scale_color_manual(values = pal)+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))



### Summary Figures for X Data
setwd("/Volumes/Seagate Exp/Data for Chris/Summary Figures/Data")
files <- list.files("X/")
setwd("/Volumes/Seagate Exp/Data for Chris/Summary Figures/Data/X")
out <- ldply(files, read_csv)
nrow(out[out$uniqueID == 1, ]) #Verify that all 15 sites were combined
out[out == 'NS'] <- 'No Slope'
desired_order <- c("N", "E", "S", "W", "No Slope")
out$direction <- factor( as.character(out$direction), levels=desired_order )


#Plot Slope
slope <- ggplot() +
  geom_point(data = out, aes(x = long, y = lat, color = Slope), shape = ".") +
  scale_colour_gradient(low = "White", high = "black", "Slope (°)")+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#Plot Aspect
aspect <- ggplot() +
  geom_point(data = out, aes(x = long, y = lat, color = direction), shape = ".") +
  labs(color = "Aspect Direction")+
  guides(colour = guide_legend(override.aes = list(shape= 16, size = 7))) +
  scale_color_manual(values = c("red", "yellow", "darkgreen", "blue", "grey"))+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#Plot SWI
swi <- ggplot() +
  geom_point(data = out, aes(x = long, y = lat, color = mean.SWI), shape = ".") +
  scale_colour_viridis(option = 'viridis', direction = -1, str_wrap("SAGA Wetness Index", width = 10))+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

plot_grid(slope, aspect, swi, nrow = 3, labels = c("A", "B", "C"))

#Plot CAC
CAC <- ggplot() +
  geom_point(data = out, aes(x = long, y = lat, color = CAC), shape = ".") +
  scale_colour_viridis(option = 'viridis', direction = -1, str_wrap("[CaCO3]", width = 14))+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))



#Plot CEC
CEC <- ggplot() +
  geom_point(data = out, aes(x = long, y = lat, color = CEC), shape = ".") +
  scale_colour_viridis(option = 'viridis', direction = -1, str_wrap("Cation Exchange Capacity", width = 14))+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#Plot %CLA
cla <- ggplot() +
  geom_point(data = out, aes(x = long, y = lat, color = CLA), shape = ".") +
  scale_colour_viridis(option = 'viridis', direction = -1, "% Clay", limits = c(0, 100))+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#Plot %SAN
san <- ggplot() +
  geom_point(data = out, aes(x = long, y = lat, color = SAN), shape = ".") +
  scale_colour_viridis(option = 'viridis', direction = -1, "% Sand", limits = c(0, 100))+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#Plot AWC
AWC <- ggplot() +
  geom_point(data = out, aes(x = long, y = lat, color = WAT), shape = ".") +
  scale_colour_viridis(option = 'viridis', direction = -1, str_wrap("Available Water Content", width = 14))+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#Plot the presence of Hydric Soils
out_Hydric <- subset(out, Hydric == "Yes")
hydric <- ggplot() +
  geom_point(data = out_Hydric, aes(x = long, y = lat), shape = ".", color = 'black') +
  scale_fill_manual(values = 'black')+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

plot_grid(CAC, CEC, AWC, nrow = 2, labels = c("A", "B", "C"))
plot_grid(cla, san, nrow = 1, labels = c("A", "B"))
plot_grid(hydric, flood, nrow = 1, labels = c("A", "B"))


#Plot the presence of Floodplains
out <- ldply(files, read_csv)
out_Floodplain <- subset(out, Floodplain == "Yes")
flood <- ggplot() +
  geom_point(data = out_Floodplain, aes(x = long, y = lat), shape = ".") +
  scale_fill_manual(values = 'black')+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#Plot Mean Annual Precipitation
precip <- ggplot() +
geom_point(data = out, aes(x = long, y = lat, color = totalPPT), shape = ".") +
  scale_colour_viridis(option = 'plasma', direction = -1, str_wrap("Mean Annual Precipitation 1895 - 1925 (mm)", width = 14))+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#Plot Mean Annual Temperature
temp <- ggplot() +
  geom_point(data = out, aes(x = long, y = lat, color = MeanTEMP), shape = ".") +
  scale_colour_brewer(palette = 'RdBu', str_wrap("Mean Annual Temperature 1895 - 1925 (°C)", width = 14))+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

plot_grid(precip, temp, nrow = 1, labels = c("A", "B"))


