## Figures associated with OOS prediction in Shuman et al. publication in prep

## Author: I Shuman

rm(list = ls())

library(corrplot)
library(cowplot)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(dplyr)

type <- 'reduced' # reduced or all

if(type == 'all'){
  comp_envi <- readRDS(file = "out/comp_envi_all.rds")
  comp_MES <- readRDS(file = "out/comp_MES_all.rds")
}

if(type == 'reduced'){
  comp_envi <- readRDS(file = "out/comp_envi_reduced.rds")
  comp_MES <- readRDS(file = "out/comp_MES_reduced.rds")
}

if(type == 'reduced'){
#Filter dataframes by ecosystem type
prairie_comp_envi <- dplyr::filter(comp_envi, (Ecosystem == "Prairie" & Observed == 1))
noprairie_comp_envi <- dplyr::filter(comp_envi, (Ecosystem == "Prairie" & Observed == 0))
prairie_comp_MES <- dplyr::filter(comp_MES, (Ecosystem == "Prairie" & Observed == 1))
noprairie_comp_MES <- dplyr::filter(comp_MES, (Ecosystem == "Prairie" & Observed == 0))

savanna_comp_envi <- dplyr::filter(comp_envi, (Ecosystem == "Savanna" & Observed == 1))
nosavanna_comp_envi <- dplyr::filter(comp_envi, (Ecosystem == "Savanna" & Observed == 0))
savanna_comp_MES <- dplyr::filter(comp_MES, (Ecosystem == "Savanna" & Observed == 1))
nosavanna_comp_MES <- dplyr::filter(comp_MES, (Ecosystem == "Savanna" & Observed == 0))

forest_comp_envi <- dplyr::filter(comp_envi, (Ecosystem == "Forest" & Observed == 1))
noforest_comp_envi <- dplyr::filter(comp_envi, (Ecosystem == "Forest" & Observed == 0))
forest_comp_MES <- dplyr::filter(comp_MES, (Ecosystem == "Forest" & Observed == 1))
noforest_comp_MES <- dplyr::filter(comp_MES, (Ecosystem == "Forest" & Observed == 0))

#Filter dataframes so that they only have predicted probability of presence when present data 
prairie.df.un <- data.frame(`Prairie` = prairie_comp_envi$Probability) #for unconditional
savanna.df.un <- data.frame(`Savanna` = savanna_comp_envi$Probability)
forest.df.un <- data.frame(`Forest` = forest_comp_envi$Probability)
whole.df.un <- dplyr::bind_rows(prairie.df.un, savanna.df.un, forest.df.un)
colnames(whole.df.un) <- c("Prairie", "Savanna", "Forest")
whole.df.un.long <- na.omit(pivot_longer(whole.df.un, names_to = "Ecosystem", values_to = "Probability", cols = 1:3))

prairie.df <- data.frame(`Prairie` = prairie_comp_MES$Probability) #for conditional/MES
savanna.df <- data.frame(`Savanna` = savanna_comp_MES$Probability)
forest.df <- data.frame(`Forest` = forest_comp_MES$Probability)
whole.df <- dplyr::bind_rows(prairie.df, savanna.df, forest.df)
colnames(whole.df) <- c("Prairie", "Savanna", "Forest")
whole.df.long <- na.omit(pivot_longer(whole.df, names_to = "Ecosystem", values_to = "Probability", cols = 1:3))


## Calculate the number of cells correctly/incorrectly predicted for the ecosystem level unconditional model
mean(prairie_comp_envi$Probability)
mean(noprairie_comp_envi$Probability)
sd(prairie_comp_envi$Probability)
sd(noprairie_comp_envi$Probability)

nrow(dplyr::filter(prairie_comp_envi, Probability >= 0.5)) #number of correct prairie presence predictions (5433/6396)
nrow(dplyr::filter(prairie_comp_envi, Probability < 0.5)) #number of times prairie predicted absent when present (963/6396)

nrow(dplyr::filter(noprairie_comp_envi, Probability < 0.5)) #number of correct prairie absence predictions (14786/18303
nrow(dplyr::filter(noprairie_comp_envi, Probability >= 0.5)) #number of times prairie predicted present when absent (3517/18303

mean(savanna_comp_envi$Probability)
mean(nosavanna_comp_envi$Probability)
sd(savanna_comp_envi$Probability)
sd(nosavanna_comp_envi$Probability)

nrow(dplyr::filter(savanna_comp_envi, Probability >= 0.5)) #number of correct savanna presence predictions (1491/9211)
nrow(dplyr::filter(savanna_comp_envi, Probability < 0.5)) #number of times savanna predicted absent when present (7720/9211)

nrow(dplyr::filter(nosavanna_comp_envi, Probability < 0.5)) #number of correct savanna absence predictions (15021/15488)
nrow(dplyr::filter(nosavanna_comp_envi, Probability >= 0.5)) #number of times savanna predicted present when absent (467/15488)

nrow(dplyr::filter(forest_comp_envi, Probability >= 0.5)) #number of correct forest presence predictions (7004/9092)
nrow(dplyr::filter(forest_comp_envi, Probability < 0.5)) #number of times forest predicted absent when present (2088/9092)

nrow(dplyr::filter(noforest_comp_envi, Probability < 0.5)) #number of correct forest absence predictions (13881/15607)
nrow(dplyr::filter(noforest_comp_envi, Probability >= 0.5)) #number of times forest predicted present when absent (1726/15607)

mean(forest_comp_envi$Probability)
mean(noforest_comp_envi$Probability)
sd(forest_comp_envi$Probability)
sd(noforest_comp_envi$Probability)


## Calculate the number of cells correctly/incorrectly predicted for the ecosystem level MES model
mean(prairie_comp_MES$Probability)
mean(noprairie_comp_MES$Probability)
sd(prairie_comp_MES$Probability)
sd(noprairie_comp_MES$Probability)

nrow(dplyr::filter(prairie_comp_MES, Probability >= 0.5)) #number of correct prairie presence predictions (6200/6396)
nrow(dplyr::filter(prairie_comp_MES, Probability < 0.5)) #number of times prairie predicted absent when present (194/6396)

nrow(dplyr::filter(noprairie_comp_MES, Probability < 0.5)) #number of correct prairie absence predictions (17439/18303)
nrow(dplyr::filter(noprairie_comp_MES, Probability >= 0.5)) #number of times prairie predicted present when absent (864/18303)


mean(savanna_comp_MES$Probability)
mean(nosavanna_comp_MES$Probability)
sd(savanna_comp_MES$Probability)
sd(nosavanna_comp_MES$Probability)

nrow(dplyr::filter(savanna_comp_MES, Probability >= 0.5)) #number of correct savanna presence predictions (6020/9211)
nrow(dplyr::filter(savanna_comp_MES, Probability < 0.5)) #number of times savanna predicted absent when present (3191/9211)

nrow(dplyr::filter(nosavanna_comp_MES, Probability < 0.5)) #number of correct savanna absence predictions (15488/15488)
nrow(dplyr::filter(nosavanna_comp_MES, Probability >= 0.5)) #number of times savanna predicted present when absent (0/15488)


mean(forest_comp_MES$Probability)
mean(noforest_comp_MES$Probability)
sd(forest_comp_MES$Probability)
sd(noforest_comp_MES$Probability)

nrow(dplyr::filter(forest_comp_MES, Predicted >= 0.5)) #number of correct forest presence predictions (7665/9092)
nrow(dplyr::filter(forest_comp_MES, Predicted < 0.5)) #number of times forest predicted absent when present (1427/9092)

nrow(dplyr::filter(noforest_comp_MES, Predicted < 0.5)) #number of correct forest absence predictions (15562/15607)
nrow(dplyr::filter(noforest_comp_MES, Predicted >= 0.5)) #number of times forest predicted present when absent (45/15607)


##Plot results
#For each corner
unconditional.violin <- ggplot(data = whole.df.un.long, aes(x = Ecosystem, y = Probability))+
  geom_violin()
unconditional.violin
unconditional.box <- ggplot(data = whole.df.un.long, aes(x = Ecosystem, y = Probability))+
  geom_boxplot()
unconditional.box
conditional.violin <- ggplot(data = whole.df.long, aes(x = Ecosystem, y = Probability))+
  geom_violin()
conditional.violin
conditional.box <- ggplot(data = whole.df.long, aes(x = Ecosystem, y = Probability))+
  geom_boxplot()
conditional.box
grid.arrange(unconditional.box, conditional.box, unconditional.violin, conditional.violin, nrow = 2)

#Averaged
averaged.df <- data.frame("Ecosystem" = c("Prairie", "Savanna", "Forest", "Prairie", "Savanna", "Forest"), 
                          "Type" = c(rep("Unconditional", 3), rep("Conditional", 3)),
                          "Mean" = c(mean(prairie_comp_envi$Probability), mean(savanna_comp_envi$Probability), mean(forest_comp_envi$Probability), mean(prairie_comp_MES$Probability), mean(savanna_comp_MES$Probability), mean(forest_comp_MES$Probability)), 
                          "SD" = c(sd(prairie_comp_envi$Probability), sd(savanna_comp_envi$Probability), sd(forest_comp_envi$Probability), sd(prairie_comp_MES$Probability), sd(savanna_comp_envi$Probability), sd(forest_comp_MES$Probability)))
sum.df <- data.frame("Ecosystem" = c("Prairie", "Savanna", "Forest", "Prairie", "Savanna", "Forest"), 
                          "Type" = c(rep("Unconditional", 3), rep("Conditional", 3)),
                          "Mean" = c(mean(prairie_comp_envi$Probability), mean(savanna_comp_envi$Probability), mean(forest_comp_envi$Probability), mean(prairie_comp_MES$Probability) - mean(prairie_comp_envi$Probability), mean(savanna_comp_MES$Probability) - mean(savanna_comp_envi$Probability), mean(forest_comp_MES$Probability) - mean(forest_comp_envi$Probability)), 
                          "SD" = c(sd(prairie_comp_envi$Probability), sd(savanna_comp_envi$Probability), sd(forest_comp_envi$Probability), sd(prairie_comp_MES$Probability), sd(savanna_comp_envi$Probability), sd(forest_comp_MES$Probability)),
                     "Addition" = c(rep(0, 3), mean(prairie_comp_envi$Probability), mean(savanna_comp_envi$Probability), mean(forest_comp_envi$Probability)))

ggplot(sum.df, aes(fill=Ecosystem, alpha = Type, y=Mean, x= Ecosystem))+
  geom_bar(stat = "identity")+
  scale_alpha_manual(values = c(0.5, 1))+
  scale_fill_manual(values = c("Prairie" = '#bb5566', "Savanna" = '#ddaa34', "Forest" = '#002a53'))+
  geom_errorbar(aes(ymin=Mean-SD+Addition, ymax=Mean+SD+Addition), width=.2,position=position_dodge(.9))

dodge <- position_dodge2(reverse = TRUE) 
averaged.df$Ecosystem <- factor(averaged.df$Ecosystem, levels = c("Forest", "Savanna", "Prairie"))
ggplot(averaged.df, aes(fill=Ecosystem, alpha = Type, y=Mean, x= Ecosystem))+ 
  geom_bar(stat = "identity", position=dodge)+
  ylab(label = "Mean Predicted Probability of Presence \n When Present")+
  scale_alpha_manual(values = c(0.5, 1))+ 
  scale_fill_manual(values = c("Prairie" = '#bb5566', "Savanna" = '#ddaa34', "Forest" = '#002a53'))+ 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), position=dodge)+ 
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  theme_classic()


averaged.df$Interaction <- interaction(averaged.df$Ecosystem, averaged.df$Type, sep = "_")
averaged.df$Interaction <- fct_relevel(averaged.df$Interaction, "Prairie_Unconditional", "Prairie_Conditional", "Savanna_Unconditional", "Savanna_Conditional", "Forest_Unconditional", "Forest_Conditional")


averaged.df$Ecosystem  <- factor(averaged.df$Type, levels = c("Prairie", "Savanna", "Forest"))

averaged.df$Interaction <- interaction(averaged.df$Ecosystem, averaged.df$Type, sep = "_")
# Change the order of factor levels
levels(averaged.df$Interaction) <- c("Prairie_Unconditional", "Prairie_Conditional", "Savanna_Unconditional", "Savanna_Conditional", "Forest_Unconditional", "Forest_Conditional")

ggplot(averaged.df, aes(fill=Ecosystem, alpha = Type, y=Mean, x= Interaction))+
  geom_bar(stat = "identity")+
  scale_alpha_manual(values = c(0.5, 1))+
  scale_fill_manual(values = c("Prairie" = '#bb5566', "Savanna" = '#ddaa34', "Forest" = '#002a53'))+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))



}



if(type == 'all'){

### Calculate the number of cells correctly/incorrectly predicted for the taxon level unconditional model
no.tree_comp_envi <- dplyr::filter(comp_envi, (Taxon == "No.tree" & Observed == 1))
tree_comp_envi <- dplyr::filter(comp_envi, (Taxon == "No.tree" & Observed == 0))

mean(no.tree_comp_envi$Probability)
mean(tree_comp_envi$Probability)
sd(no.tree_comp_envi$Probability)
sd(tree_comp_envi$Probability)

nrow(dplyr::filter(no.tree_comp_envi, Probability >= 0.5)) #number of correct no tree predictions (96/6396)
nrow(dplyr::filter(no.tree_comp_envi, Probability < 0.5)) #number of times no tree predicted absent when no trees present (6300/6396)

nrow(dplyr::filter(tree_comp_envi, Probability < 0.5)) #number of correct no tree absence predictions (18303/18303)
nrow(dplyr::filter(tree_comp_envi, Probability >= 0.5)) #number of times no tree predicted present when absent (0/18303)

oak_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Oak" & Observed == 1))
nooak_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Oak" & Observed == 0))

mean(nooak_comp_envi$Probability)
mean(oak_comp_envi$Probability)
sd(nooak_comp_envi$Probability)
sd(oak_comp_envi$Probability)

nrow(dplyr::filter(oak_comp_envi, Probability >= 0.5)) #number of correct oak predictions (4196/10807) correct presence
nrow(dplyr::filter(oak_comp_envi, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative

nrow(dplyr::filter(nooak_comp_envi, Probability < 0.5)) #number of correct oak absence predictions (13892/13892) 
nrow(dplyr::filter(nooak_comp_envi, Probability >= 0.5)) #number of times oak predicted present when absent (0/13892) false positive 

hickory_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Hickory" & Observed == 1))
nohickory_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Hickory" & Observed == 0))

mean(hickory_comp_envi$Probability)
mean(nohickory_comp_envi$Probability)
sd(hickory_comp_envi$Probability)
sd(nohickory_comp_envi$Probability)


nrow(dplyr::filter(hickory_comp_envi, Probability >= 0.5)) #number of correct hickory predictions (0/2894) correct presence
nrow(dplyr::filter(hickory_comp_envi, Probability < 0.5)) #number of times hickory predicted absent when hickory present (2894/2894) false negative

nrow(dplyr::filter(nohickory_comp_envi, Probability < 0.5)) #number of correct hickory absence predictions (21805/21805) 
nrow(dplyr::filter(nohickory_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/21805) false positive

beech_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Beech" & Observed == 1))
nobeech_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Beech" & Observed == 0))

mean(beech_comp_envi$Probability)
mean(nobeech_comp_envi$Probability)
sd(beech_comp_envi$Probability)
sd(nobeech_comp_envi$Probability)

nrow(dplyr::filter(beech_comp_envi, Probability >= 0.5)) #number of correct hickory predictions (0/3751) correct presence
nrow(dplyr::filter(beech_comp_envi, Probability < 0.5)) #number of times hickory predicted absent when hickory present (3751/3751) false negative

nrow(dplyr::filter(nobeech_comp_envi, Probability < 0.5)) #number of correct hickory absence predictions (20948/20948) 
nrow(dplyr::filter(nobeech_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


maple_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Maple" & Observed == 1))
nomaple_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Maple" & Observed == 0))

mean(maple_comp_envi$Probability)
mean(nomaple_comp_envi$Probability)
sd(maple_comp_envi$Probability)
sd(nomaple_comp_envi$Probability)

nrow(dplyr::filter(maple_comp_envi, Probability >= 0.5)) #number of correct hickory predictions (0/3751) correct presence
nrow(dplyr::filter(maple_comp_envi, Probability < 0.5)) #number of times hickory predicted absent when hickory present (3751/3751) false negative

nrow(dplyr::filter(nomaple_comp_envi, Probability < 0.5)) #number of correct hickory absence predictions (20948/20948) 
nrow(dplyr::filter(nomaple_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


bgsg_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Black.gum.sweet.gum" & Observed == 1))
nobgsg_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Black.gum.sweet.gum" & Observed == 0))

mean(bgsg_comp_envi$Probability)
mean(nobgsg_comp_envi$Probability)
sd(bgsg_comp_envi$Probability)
sd(nobgsg_comp_envi$Probability)

nrow(dplyr::filter(bgsg_comp_envi, Probability >= 0.5)) #number of correct hickory predictions (0/3751) correct presence
nrow(dplyr::filter(bgsg_comp_envi, Probability < 0.5)) #number of times hickory predicted absent when hickory present (3751/3751) false negative

nrow(dplyr::filter(nobgsg_comp_envi, Probability < 0.5)) #number of correct hickory absence predictions (20948/20948) 
nrow(dplyr::filter(nobgsg_comp_envi, Probability > 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Elm_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Elm" & Observed == 1))
noElm_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Elm" & Observed == 0))

mean(Elm_comp_envi$Probability)
mean(noElm_comp_envi$Probability)
sd(Elm_comp_envi$Probability)
sd(noElm_comp_envi$Probability)
nrow(dplyr::filter(Elm_comp_envi, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noElm_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Ash_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Ash" & Observed == 1))
noAsh_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Ash" & Observed == 0))

mean(Ash_comp_envi$Probability)
mean(noAsh_comp_envi$Probability)
sd(Ash_comp_envi$Probability)
sd(noAsh_comp_envi$Probability)
nrow(dplyr::filter(Ash_comp_envi, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noAsh_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Bass_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Basswood" & Observed == 1))
noBass_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Basswood" & Observed == 0))

mean(Bass_comp_envi$Probability)
mean(noBass_comp_envi$Probability)
sd(Bass_comp_envi$Probability)
sd(noBass_comp_envi$Probability)
nrow(dplyr::filter(Bass_comp_envi, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noBass_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Dog_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Dogwood" & Observed == 1))
noDog_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Dogwood" & Observed == 0))

mean(Dog_comp_envi$Probability)
mean(noDog_comp_envi$Probability)
sd(Dog_comp_envi$Probability)
sd(noDog_comp_envi$Probability)
nrow(dplyr::filter(Dog_comp_envi, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noDog_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Iron_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Ironwood" & Observed == 1))
noIron_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Ironwood" & Observed == 0))

mean(Iron_comp_envi$Probability)
mean(noIron_comp_envi$Probability)
sd(Iron_comp_envi$Probability)
sd(noIron_comp_envi$Probability)
nrow(dplyr::filter(Iron_comp_envi, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noIron_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive



OtC_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Other.conifer" & Observed == 1))
noOtC_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Other.conifer" & Observed == 0))

mean(OtC_comp_envi$Probability)
mean(noOtC_comp_envi$Probability)
sd(OtC_comp_envi$Probability)
sd(noOtC_comp_envi$Probability)
nrow(dplyr::filter(OtC_comp_envi, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noOtC_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


OtH_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Other.hardwood" & Observed == 1))
noOtH_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Other.hardwood" & Observed == 0))

mean(OtH_comp_envi$Probability)
mean(noOtH_comp_envi$Probability)
sd(OtH_comp_envi$Probability)
sd(noOtH_comp_envi$Probability)
nrow(dplyr::filter(OtH_comp_envi, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noOtH_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive



Pop_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Poplar.tulip.poplar" & Observed == 1))
noPop_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Poplar.tulip.poplar" & Observed == 0))

mean(Pop_comp_envi$Probability)
mean(noPop_comp_envi$Probability)
sd(Pop_comp_envi$Probability)
sd(noPop_comp_envi$Probability)
nrow(dplyr::filter(Pop_comp_envi, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noPop_comp_envi, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive

Wal_comp_envi <- dplyr::filter(comp_envi, (Taxon == "Walnut" & Observed == 1))

  
  
### Calculate the number of cells correctly/incorrectly predicted for the taxon level MES model
no.tree_comp_MES <- dplyr::filter(comp_MES, (Taxon == "No.tree" & Observed == 1))
tree_comp_MES <- dplyr::filter(comp_MES, (Taxon == "No.tree" & Observed == 0))

mean(no.tree_comp_MES$Probability)
mean(tree_comp_MES$Probability)
sd(no.tree_comp_MES$Probability)
sd(tree_comp_MES$Probability)

nrow(dplyr::filter(no.tree_comp_MES, Probability >= 0.5)) #number of correct no tree predictions (258/6396)
nrow(dplyr::filter(no.tree_comp_MES, Probability < 0.5)) #number of times no tree predicted absent when no trees present (6138/6396)

nrow(dplyr::filter(tree_comp_MES, Probability < 0.5)) #number of correct no tree absence predictions (18303/18303)
nrow(dplyr::filter(tree_comp_MES, Probability >= 0.5)) #number of times no tree predicted present when absent (0/18303)

oak_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Oak" & Observed == 1))
nooak_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Oak" & Observed == 0))

mean(oak_comp_MES$Probability)
mean(nooak_comp_MES$Probability)
sd(oak_comp_MES$Probability)
sd(nooak_comp_MES$Probability)

nrow(dplyr::filter(oak_comp_MES, Probability >= 0.5)) #number of correct oak predictions (4196/10807) correct presence
nrow(dplyr::filter(oak_comp_MES, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative

nrow(dplyr::filter(nooak_comp_MES, Probability < 0.5)) #number of correct oak absence predictions (13892/13892) 
nrow(dplyr::filter(nooak_comp_MES, Probability >= 0.5)) #number of times oak predicted present when absent (0/13892) false positive 

hickory_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Hickory" & Observed == 1))
nohickory_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Hickory" & Observed == 0))

mean(hickory_comp_MES$Probability)
mean(nohickory_comp_MES$Probability)
sd(hickory_comp_MES$Probability)
sd(nohickory_comp_MES$Probability)

nrow(dplyr::filter(hickory_comp_MES, Probability >= 0.5)) #number of correct hickory predictions (0/2894) correct presence
nrow(dplyr::filter(hickory_comp_MES, Probability < 0.5)) #number of times hickory predicted absent when hickory present (2894/2894) false negative

nrow(dplyr::filter(nohickory_comp_MES, Probability < 0.5)) #number of correct hickory absence predictions (21805/21805) 
nrow(dplyr::filter(nohickory_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/21805) false positive

beech_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Beech" & Observed == 1))
nobeech_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Beech" & Observed == 0))

mean(beech_comp_MES$Probability)
mean(nobeech_comp_MES$Probability)
sd(beech_comp_MES$Probability)
sd(nobeech_comp_MES$Probability)

nrow(dplyr::filter(beech_comp_MES, Probability >= 0.5)) #number of correct hickory predictions (0/3751) correct presence
nrow(dplyr::filter(beech_comp_MES, Probability < 0.5)) #number of times hickory predicted absent when hickory present (3751/3751) false negative

nrow(dplyr::filter(nobeech_comp_MES, Probability < 0.5)) #number of correct hickory absence predictions (20948/20948) 
nrow(dplyr::filter(nobeech_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


maple_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Maple" & Observed == 1))
nomaple_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Maple" & Observed == 0))

mean(maple_comp_MES$Probability)
mean(nomaple_comp_MES$Probability)
sd(maple_comp_MES$Probability)
sd(nomaple_comp_MES$Probability)

nrow(dplyr::filter(maple_comp_MES, Probability >= 0.5)) #number of correct hickory predictions (0/3751) correct presence
nrow(dplyr::filter(maple_comp_MES, Probability < 0.5)) #number of times hickory predicted absent when hickory present (3751/3751) false negative

nrow(dplyr::filter(nomaple_comp_MES, Probability < 0.5)) #number of correct hickory absence predictions (20948/20948) 
nrow(dplyr::filter(nomaple_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive




bgsg_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Black.gum.sweet.gum" & Observed == 1))
nobgsg_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Black.gum.sweet.gum" & Observed == 0))

mean(bgsg_comp_MES$Probability)
mean(nobgsg_comp_MES$Probability)
sd(bgsg_comp_MES$Probability)
sd(nobgsg_comp_MES$Probability)

nrow(dplyr::filter(bgsg_comp_MES, Probability >= 0.5)) #number of correct hickory predictions (0/3751) correct presence
nrow(dplyr::filter(bgsg_comp_MES, Probability < 0.5)) #number of times hickory predicted absent when hickory present (3751/3751) false negative

nrow(dplyr::filter(nobgsg_comp_MES, Probability < 0.5)) #number of correct hickory absence predictions (20948/20948) 
nrow(dplyr::filter(nobgsg_comp_MES, Probability > 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Elm_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Elm" & Observed == 1))
noElm_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Elm" & Observed == 0))

mean(Elm_comp_MES$Probability)
mean(noElm_comp_MES$Probability)
sd(Elm_comp_MES$Probability)
sd(noElm_comp_MES$Probability)
nrow(dplyr::filter(Elm_comp_MES, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noElm_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Ash_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Ash" & Observed == 1))
noAsh_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Ash" & Observed == 0))

mean(Ash_comp_MES$Probability)
mean(noAsh_comp_MES$Probability)
sd(Ash_comp_MES$Probability)
sd(noAsh_comp_MES$Probability)
nrow(dplyr::filter(Ash_comp_MES, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noAsh_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Bass_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Basswood" & Observed == 1))
noBass_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Basswood" & Observed == 0))

mean(Bass_comp_MES$Probability)
mean(noBass_comp_MES$Probability)
sd(Bass_comp_MES$Probability)
sd(noBass_comp_MES$Probability)
nrow(dplyr::filter(Bass_comp_MES, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noBass_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Dog_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Dogwood" & Observed == 1))
noDog_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Dogwood" & Observed == 0))

mean(Dog_comp_MES$Probability)
mean(noDog_comp_MES$Probability)
sd(Dog_comp_MES$Probability)
sd(noDog_comp_MES$Probability)
nrow(dplyr::filter(Dog_comp_MES, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noDog_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


Iron_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Ironwood" & Observed == 1))
noIron_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Ironwood" & Observed == 0))

mean(Iron_comp_MES$Probability)
mean(noIron_comp_MES$Probability)
sd(Iron_comp_MES$Probability)
sd(noIron_comp_MES$Probability)
nrow(dplyr::filter(Iron_comp_MES, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noIron_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive



OtC_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Other.conifer" & Observed == 1))
noOtC_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Other.conifer" & Observed == 0))

mean(OtC_comp_MES$Probability)
mean(noOtC_comp_MES$Probability)
sd(OtC_comp_MES$Probability)
sd(noOtC_comp_MES$Probability)
nrow(dplyr::filter(OtC_comp_MES, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noOtC_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive


OtH_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Other.hardwood" & Observed == 1))
noOtH_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Other.hardwood" & Observed == 0))

mean(OtH_comp_MES$Probability)
mean(noOtH_comp_MES$Probability)
sd(OtH_comp_MES$Probability)
sd(noOtH_comp_MES$Probability)
nrow(dplyr::filter(OtH_comp_MES, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noOtH_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive



Pop_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Poplar.tulip.poplar" & Observed == 1))
noPop_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Poplar.tulip.poplar" & Observed == 0))

mean(Pop_comp_MES$Probability)
mean(noPop_comp_MES$Probability)
sd(Pop_comp_MES$Probability)
sd(noPop_comp_MES$Probability)
nrow(dplyr::filter(Pop_comp_MES, Probability < 0.5)) #number of times oak predicted absent when oak present (6611/10807) false negative
nrow(dplyr::filter(noPop_comp_MES, Probability >= 0.5)) #number of times hickory predicted present when absent (0/20948) false positive

Wal_comp_MES <- dplyr::filter(comp_MES, (Taxon == "Walnut" & Observed == 1))


#Averaged
averaged.df <- data.frame("Taxon" = rep(c("No Tree", "Oak", "Hickory", "Beech", "Maple", "Black Gum/Sweet Gum", "Elm", "Ash", "Basswood", "Dogwood", "Ironwood", "Other Conifer", "Other Hardwood", "Poplar/Tulip Poplar", "Walnut"), 2), 
                          "Type" = c(rep("Unconditional", 15), rep("Conditional", 15)),
                          "Mean" = c(mean(no.tree_comp_envi$Probability), mean(oak_comp_envi$Probability), mean(hickory_comp_envi$Probability), mean(beech_comp_envi$Probability), mean(maple_comp_envi$Probability), mean(bgsg_comp_envi$Probability), mean(Elm_comp_envi$Probability), mean(Ash_comp_envi$Probability), mean(Bass_comp_envi$Probability), mean(Dog_comp_envi$Probability), mean(Iron_comp_envi$Probability), mean(OtC_comp_envi$Probability), mean(OtH_comp_envi$Probability), mean(Pop_comp_envi$Probability), mean(Wal_comp_envi$Probability),
                                     mean(no.tree_comp_MES$Probability), mean(oak_comp_MES$Probability), mean(hickory_comp_MES$Probability), mean(beech_comp_MES$Probability), mean(maple_comp_MES$Probability), mean(bgsg_comp_MES$Probability), mean(Elm_comp_MES$Probability), mean(Ash_comp_MES$Probability), mean(Bass_comp_MES$Probability), mean(Dog_comp_MES$Probability), mean(Iron_comp_MES$Probability), mean(OtC_comp_MES$Probability), mean(OtH_comp_MES$Probability), mean(Pop_comp_MES$Probability), mean(Wal_comp_MES$Probability)), 
                            "SD" = c(sd(no.tree_comp_envi$Probability), sd(oak_comp_envi$Probability), sd(hickory_comp_envi$Probability), sd(beech_comp_envi$Probability), sd(maple_comp_envi$Probability), sd(bgsg_comp_envi$Probability), sd(Elm_comp_envi$Probability), sd(Ash_comp_envi$Probability), sd(Bass_comp_envi$Probability), sd(Dog_comp_envi$Probability), sd(Iron_comp_envi$Probability), sd(OtC_comp_envi$Probability), sd(OtH_comp_envi$Probability), sd(Pop_comp_envi$Probability), sd(Wal_comp_envi$Probability),
                                     sd(no.tree_comp_MES$Probability), sd(oak_comp_MES$Probability), sd(hickory_comp_MES$Probability), sd(beech_comp_MES$Probability), sd(maple_comp_MES$Probability), sd(bgsg_comp_MES$Probability), sd(Elm_comp_MES$Probability), sd(Ash_comp_MES$Probability), sd(Bass_comp_MES$Probability), sd(Dog_comp_MES$Probability), sd(Iron_comp_MES$Probability), sd(OtC_comp_MES$Probability), sd(OtH_comp_MES$Probability), sd(Pop_comp_MES$Probability), sd(Wal_comp_MES$Probability)))
                          

sum.df <- data.frame("Taxon" = rep(c("No Tree", "Oak", "Hickory", "Beech", "Maple", "Black Gum/Sweet Gum", "Elm", "Ash", "Basswood", "Dogwood", "Ironwood", "Other Conifer", "Other Hardwood", "Poplar/Tulip Poplar", "Walnut"), 2), 
                     "Type" = c(rep("Unconditional", 15), rep("Conditional", 15)),
                     "Mean" = c(mean(no.tree_comp_envi$Probability), mean(oak_comp_envi$Probability), mean(hickory_comp_envi$Probability), mean(beech_comp_envi$Probability), mean(maple_comp_envi$Probability), mean(bgsg_comp_envi$Probability), mean(Elm_comp_envi$Probability), mean(Ash_comp_envi$Probability), mean(Bass_comp_envi$Probability), mean(Dog_comp_envi$Probability), mean(Iron_comp_envi$Probability), mean(OtC_comp_envi$Probability), mean(OtH_comp_envi$Probability), mean(Pop_comp_envi$Probability), mean(Wal_comp_envi$Probability),
                                mean(no.tree_comp_MES$Probability) - mean(no.tree_comp_envi$Probability), mean(oak_comp_MES$Probability) - mean(oak_comp_envi$Probability), mean(hickory_comp_MES$Probability) - mean(hickory_comp_envi$Probability), mean(beech_comp_MES$Probability) - mean(beech_comp_envi$Probability), mean(maple_comp_MES$Probability) - mean(maple_comp_envi$Probability), mean(bgsg_comp_MES$Probability) - mean(bgsg_comp_envi$Probability), mean(Elm_comp_MES$Probability) - mean(Elm_comp_envi$Probability), mean(Ash_comp_MES$Probability) - mean(Ash_comp_envi$Probability), mean(Bass_comp_MES$Probability) - mean(Bass_comp_envi$Probability), mean(Dog_comp_MES$Probability) - mean(Dog_comp_envi$Probability), mean(Iron_comp_MES$Probability) - mean(Iron_comp_envi$Probability), mean(OtC_comp_MES$Probability) - mean(OtC_comp_envi$Probability), mean(OtH_comp_MES$Probability) - mean(OtH_comp_envi$Probability), mean(Pop_comp_MES$Probability) - mean(Pop_comp_envi$Probability), mean(Wal_comp_MES$Probability) - mean(Wal_comp_envi$Probability)), 
                     "SD" = c(sd(no.tree_comp_envi$Probability), sd(oak_comp_envi$Probability), sd(hickory_comp_envi$Probability), sd(beech_comp_envi$Probability), sd(maple_comp_envi$Probability), sd(bgsg_comp_envi$Probability), sd(Elm_comp_envi$Probability), sd(Ash_comp_envi$Probability), sd(Bass_comp_envi$Probability), sd(Dog_comp_envi$Probability), sd(Iron_comp_envi$Probability), sd(OtC_comp_envi$Probability), sd(OtH_comp_envi$Probability), sd(Pop_comp_envi$Probability), sd(Wal_comp_envi$Probability),
                              sd(no.tree_comp_MES$Probability), sd(oak_comp_MES$Probability), sd(hickory_comp_MES$Probability), sd(beech_comp_MES$Probability), sd(maple_comp_MES$Probability), sd(bgsg_comp_MES$Probability), sd(Elm_comp_MES$Probability), sd(Ash_comp_MES$Probability), sd(Bass_comp_MES$Probability), sd(Dog_comp_MES$Probability), sd(Iron_comp_MES$Probability), sd(OtC_comp_MES$Probability), sd(OtH_comp_MES$Probability), sd(Pop_comp_MES$Probability), sd(Wal_comp_MES$Probability)),
                     "Addition" = c(rep(0, 15), mean(no.tree_comp_envi$Probability), mean(oak_comp_envi$Probability), mean(hickory_comp_envi$Probability), mean(beech_comp_envi$Probability), mean(maple_comp_envi$Probability), mean(bgsg_comp_envi$Probability), mean(Elm_comp_envi$Probability), mean(Ash_comp_envi$Probability), mean(Bass_comp_envi$Probability), mean(Dog_comp_envi$Probability), mean(Iron_comp_envi$Probability), mean(OtC_comp_envi$Probability), mean(OtH_comp_envi$Probability), mean(Pop_comp_envi$Probability), mean(Wal_comp_envi$Probability)))

ggplot(sum.df, aes(fill=Taxon, alpha = Type, y=Mean, x= Taxon))+
  geom_bar(stat = "identity")+
  scale_alpha_manual(values = c(0.5, 1))+
  scale_fill_manual(values = c("No Tree" = '#bb5566',
                               "Oak" = '#ddaa34', "Hickory" = '#ecd08f',
                               "Ash" = '#002a53', "Basswood" = '#004488', "Beech" = '#4c7cac', "Black Gum/Sweet Gum" = '#8aa9c8', "Dogwood" = '#c2d2e2', "Elm" = '#dee7f0',
                               "Ironwood" = '#005f5f', "Maple" = '#008b8b', "Other Conifer" = '#38a5a5', "Other Hardwood" = '#63b9b9', "Poplar/Tulip Poplar" = '#8ecdcd', "Walnut" = '#c1e4e4'))+
  geom_errorbar(aes(ymin=Mean-SD+Addition, ymax=Mean+SD+Addition), width=.2,position=position_dodge(.9))


dodge <- position_dodge2(reverse = TRUE)
ggplot(averaged.df, aes(fill=Taxon, alpha = Type, y=Mean, x= Taxon))+
  geom_bar(stat = "identity", position=dodge)+
  scale_alpha_manual(values = c(0.5, 1))+
  scale_fill_manual(values = c("No Tree" = '#bb5566',
                               "Oak" = '#ddaa34', "Hickory" = '#ecd08f',
                               "Ash" = '#002a53', "Basswood" = '#004488', "Beech" = '#4c7cac', "Black Gum/Sweet Gum" = '#8aa9c8', "Dogwood" = '#c2d2e2', "Elm" = '#dee7f0',
                               "Ironwood" = '#005f5f', "Maple" = '#008b8b', "Other Conifer" = '#38a5a5', "Other Hardwood" = '#63b9b9', "Poplar/Tulip Poplar" = '#8ecdcd', "Walnut" = '#c1e4e4'))+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), position=dodge)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))






}

























