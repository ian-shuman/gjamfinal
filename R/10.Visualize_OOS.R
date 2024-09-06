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
library(pROC)

type <- 'all' # reduced or all

if(type == 'all'){
  comp_envi <- readRDS(file = "out/comp_envi_all.rds")
  comp_MES <- readRDS(file = "out/comp_MES_all.rds")
}

if(type == 'reduced'){
  comp_envi <- readRDS(file = "out/comp_envi_reduced.rds")
  comp_MES <- readRDS(file = "out/comp_MES_reduced.rds")
}

#Initialze
ecosystems <- c("Prairie", "Savanna", "Forest")
whole.df.un <- data.frame()
whole.df <- data.frame()
mean_envi <- c()
sd_envi <- c()
mean_MES <- c()
sd_MES <- c()

#Calculate Statistics and Generate Figures to Assess Ecosystem-Level OOS Prediction Experiments
if(type == 'reduced'){
  for(i in 1:length(ecosystems)){
    #Filter dataframe by ecosystem type
    ecosystem_comp_envi <- dplyr::filter(comp_envi, (Ecosystem == as.character(ecosystems[i]) & Observed == 1))
    noecosystem_comp_envi <- dplyr::filter(comp_envi, (Ecosystem == as.character(ecosystems[i]) & Observed == 0))
    ecosystem_comp_MES <- dplyr::filter(comp_MES, (Ecosystem == as.character(ecosystems[i]) & Observed == 1))
    noecosystem_comp_MES <- dplyr::filter(comp_MES, (Ecosystem == as.character(ecosystems[i]) & Observed == 0))
    
    #Filter dataframes so that they only have predicted probability of presence when present data 
    ecosystem.df.un <- data.frame(ecosystem_comp_envi$Probability) #for unconditional
    colnames(ecosystem.df.un) <- paste0(ecosystems[i])
    whole.df.un <- dplyr::bind_rows(whole.df.un, ecosystem.df.un)
    
    ecosystem.df <- data.frame(ecosystem_comp_MES$Probability) #for conditional/MES
    colnames(ecosystem.df) <- paste0(ecosystems[i])
    whole.df <- dplyr::bind_rows(whole.df, ecosystem.df)
    
    ## Calculate the number of cells correctly/incorrectly predicted for the ecosystem level unconditional model
    print(paste("Mean Probability of Presence when Present for the", paste(ecosystems[i]), "Ecosytem given Unconditional Prediction is:"))
    print(mean(ecosystem_comp_envi$Probability))
    print(paste("Standard Deviation of the Probability of Presence when Present for the", paste(ecosystems[i]), "Ecosytem given Unconditional Prediction is:"))
    print(stats::sd(ecosystem_comp_envi$Probability))
    print(paste("Mean Probability of Presence when Absent for the", paste(ecosystems[i]), "Ecosytem given Unconditional Prediction is:"))
    print(mean(noecosystem_comp_envi$Probability))
    print(paste("Standard Deviation of the Probability of Presence when Absent for the", paste(ecosystems[i]), "Ecosytem given Unconditional Prediction is:"))
    print(stats::sd(noecosystem_comp_envi$Probability))
    
    #Store values for plotting
    mean_envi <- c(mean_envi, mean(ecosystem_comp_envi$Probability))
    sd_envi <- c(sd_envi, stats::sd(ecosystem_comp_envi$Probability))
    
    print(paste("Number of corners where", paste(ecosystems[i]), "was predicted as a true positive / Number of observed presences for unconditional prediction:"))
    print(paste(nrow(dplyr::filter(ecosystem_comp_envi, Probability >= 0.5)), "/", nrow(ecosystem_comp_envi))) 
    print(paste("Number of corners where", paste(ecosystems[i]), "was predicted as a false negative / Number of observed presences for unconditional prediction:"))
    print(paste(nrow(dplyr::filter(ecosystem_comp_envi, Probability < 0.5)), "/", nrow(ecosystem_comp_envi))) 
    print(paste("Number of corners where", paste(ecosystems[i]), "was predicted as a true negative / Number of observed absences for unconditional prediction:"))
    print(paste(nrow(dplyr::filter(noecosystem_comp_envi, Probability < 0.5)), "/", nrow(noecosystem_comp_envi))) 
    print(paste("Number of corners where", paste(ecosystems[i]), "was predicted as a false positive / Number of observed absences for unconditional prediction:"))
    print(paste(nrow(dplyr::filter(noecosystem_comp_envi, Probability >= 0.5)), "/", nrow(noecosystem_comp_envi)))
    
    ## Calculate the number of cells correctly/incorrectly predicted for the ecosystem level coditional model
    print(paste("Mean Probability of Presence when Present for the", paste(ecosystems[i]), "Ecosytem given Conditional Prediction is:"))
    print(mean(ecosystem_comp_MES$Probability))
    print(paste("Standard Deviation of the Probability of Presence when Present for the", paste(ecosystems[i]), "Ecosytem given Conditional Prediction is:"))
    print(stats::sd(ecosystem_comp_MES$Probability))
    print(paste("Mean Probability of Presence when Absent for the", paste(ecosystems[i]), "Ecosytem given Conditional Prediction is:"))
    print(mean(noecosystem_comp_MES$Probability))
    print(paste("Standard Deviation of the Probability of Presence when Absent for the", paste(ecosystems[i]), "Ecosytem given Conditional Prediction is:"))
    print(stats::sd(noecosystem_comp_MES$Probability))
    
    #Store values for plotting
    mean_MES <- c(mean_MES, mean(ecosystem_comp_MES$Probability))
    sd_MES <- c(sd_MES, stats::sd(ecosystem_comp_MES$Probability))
    
    print(paste("Number of corners where", paste(ecosystems[i]), "was predicted as a true positive / Number of observed presences for conditional prediction:"))
    print(paste(nrow(dplyr::filter(ecosystem_comp_MES, Probability >= 0.5)), "/", nrow(ecosystem_comp_MES))) 
    print(paste("Number of corners where", paste(ecosystems[i]), "was predicted as a false negative / Number of observed presences for conditional prediction:"))
    print(paste(nrow(dplyr::filter(ecosystem_comp_MES, Probability < 0.5)), "/", nrow(ecosystem_comp_MES))) 
    print(paste("Number of corners where", paste(ecosystems[i]), "was predicted as a true negative / Number of observed absences for conditional prediction:"))
    print(paste(nrow(dplyr::filter(noecosystem_comp_MES, Probability < 0.5)), "/", nrow(noecosystem_comp_MES))) 
    print(paste("Number of corners where", paste(ecosystems[i]), "was predicted as a false positive / Number of observed absences for conditional prediction:"))
    print(paste(nrow(dplyr::filter(noecosystem_comp_MES, Probability >= 0.5)), "/", nrow(noecosystem_comp_MES)))
  }
  colnames(whole.df.un) <- c("Prairie", "Savanna", "Forest")
  whole.df.un.long <- stats::na.omit(tidyr::pivot_longer(whole.df.un, names_to = "Ecosystem", values_to = "Probability", cols = 1:3))
  colnames(whole.df) <- c("Prairie", "Savanna", "Forest")
  whole.df.long <- stats::na.omit(tidyr::pivot_longer(whole.df, names_to = "Ecosystem", values_to = "Probability", cols = 1:3))
  
  ##Plot results
  #For each corner
  unconditional.violin <- ggplot2::ggplot(data = whole.df.un.long, ggplot2::aes(x = Ecosystem, y = Probability))+
    ggplot2::geom_violin()
  unconditional.box <- ggplot2::ggplot(data = whole.df.un.long, ggplot2::aes(x = Ecosystem, y = Probability))+
    ggplot2::geom_boxplot()
  conditional.violin <- ggplot2::ggplot(data = whole.df.long, ggplot2::aes(x = Ecosystem, y = Probability))+
    ggplot2::geom_violin()
  conditional.box <- ggplot2::ggplot(data = whole.df.long, ggplot2::aes(x = Ecosystem, y = Probability))+
    ggplot2::geom_boxplot()
  gridExtra::grid.arrange(gridExtra::arrangeGrob(unconditional.box, unconditional.violin, top = "Unconditional", ncol = 2), gridExtra::arrangeGrob(conditional.box, conditional.violin, top = "Conditional", ncol = 2))
  
  #Averaged
  averaged.df <- data.frame("Ecosystem" = c("Prairie", "Savanna", "Forest", "Prairie", "Savanna", "Forest"), 
                            "Type" = c(rep("Unconditional", 3), rep("Conditional", 3)),
                            "Mean" = c(mean_envi, mean_MES), 
                            "SD" = c(sd_envi, sd_MES))
  sum.df <- data.frame("Ecosystem" = c("Prairie", "Savanna", "Forest", "Prairie", "Savanna", "Forest"), 
                       "Type" = c(rep("Unconditional", 3), rep("Conditional", 3)),
                       "Mean" = c(mean_envi, mean_MES[1] - mean_envi[1], mean_MES[2] - mean_envi[2], mean_MES[3] - mean_envi[3]),
                       "SD" = c(sd_envi, sd_MES),
                       "Addition" = c(rep(0, 3), mean_envi))
  
  sum.plot <- ggplot2::ggplot(sum.df, ggplot2::aes(fill=Ecosystem, alpha = Type, y=Mean, x= Ecosystem))+
                ggplot2::geom_bar(stat = "identity")+
                ggplot2::scale_alpha_manual(values = c(0.5, 1))+
                ggplot2::scale_fill_manual(values = c("Prairie" = '#bb5566', "Savanna" = '#ddaa34', "Forest" = '#002a53'))+
                ggplot2::geom_errorbar(ggplot2::aes(ymin=Mean-SD+Addition, ymax=Mean+SD+Addition), width=.2,position=position_dodge(.9))
  
  dodge <- position_dodge2(reverse = TRUE) 
  averaged.df$Ecosystem <- factor(averaged.df$Ecosystem, levels = c("Forest", "Savanna", "Prairie"))
  averaged.plot <- ggplot2::ggplot(averaged.df, ggplot2::aes(fill=Ecosystem, alpha = Type, y=Mean, x= Ecosystem))+ 
                      ggplot2::geom_bar(stat = "identity", position=dodge)+
                      ggplot2::ylab(label = "Mean Predicted Probability of Presence \n When Present")+
                      ggplot2::scale_alpha_manual(values = c(0.5, 1))+ 
                      ggplot2::scale_fill_manual(values = c("Prairie" = '#bb5566', "Savanna" = '#ddaa34', "Forest" = '#002a53'))+ 
                      ggplot2::geom_errorbar(ggplot2::aes(ymin=Mean-SD, ymax=Mean+SD), position=dodge)+ 
                      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust=1))+
                      ggplot2::theme_classic()
  print(sum.plot)
  print(averaged.plot)
}

#Initialize
taxa <- c("No Tree", "Oak", "Hickory", "Beech", "Maple", "Black Gum/Sweet Gum", "Elm", "Ash", "Basswood", "Dogwood", "Ironwood", "Other Conifer", "Other Hardwood", "Poplar/Tulip Poplar", "Walnut")
whole.df.un <- data.frame()
whole.df <- data.frame()
mean_envi <- c()
sd_envi <- c()
mean_MES <- c()
sd_MES <- c()

#Calculate Statistics and Generate Figures to Assess Taxon-Level OOS Prediction Experiments
if(type == 'all'){
  #Rename taxa
  comp_envi <- comp_envi |> dplyr::mutate(Taxon = dplyr::if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/Sweet Gum', Taxon),
                Taxon = dplyr::if_else(Taxon == 'No.tree', 'No Tree', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/Tulip Poplar', Taxon))
  comp_MES <- comp_MES |> dplyr::mutate(Taxon = dplyr::if_else(Taxon == 'Black.gum.sweet.gum', 'Black Gum/Sweet Gum', Taxon),
                                          Taxon = dplyr::if_else(Taxon == 'No.tree', 'No Tree', Taxon),
                                          Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other Conifer', Taxon),
                                          Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other Hardwood', Taxon),
                                          Taxon = dplyr::if_else(Taxon == 'Poplar.tulip.poplar', 'Poplar/Tulip Poplar', Taxon))
  
  #Set color pallate for plotting
  pal <- c('#bb5566',
            '#ddaa34', '#ecd08f',
            '#002a53', '#004488', '#4c7cac', '#8aa9c8', '#c2d2e2', '#dee7f0',
            '#005f5f', '#008b8b', '#38a5a5', '#63b9b9', '#8ecdcd', '#c1e4e4')
  
  #Calculate summary statistics (probability of presence and confusion matrices)
  for(i in 1:length(taxa)){
    #Filter dataframe by Taxon
    taxon_comp_envi <- dplyr::filter(comp_envi, (Taxon == as.character(taxa[i]) & Observed == 1))
    notaxon_comp_envi <- dplyr::filter(comp_envi, (Taxon == as.character(taxa[i]) & Observed == 0))
    taxon_comp_MES <- dplyr::filter(comp_MES, (Taxon == as.character(taxa[i]) & Observed == 1))
    notaxon_comp_MES <- dplyr::filter(comp_MES, (Taxon == as.character(taxa[i]) & Observed == 0))
    
    #Filter dataframes so that they only have predicted probability of presence when present data 
    taxon.df.un <- data.frame(taxon_comp_envi$Probability) #for unconditional
    colnames(taxon.df.un) <- paste0(taxa[i])
    whole.df.un <- dplyr::bind_rows(whole.df.un, taxon.df.un)
    
    taxon.df <- data.frame(taxon_comp_MES$Probability) #for conditional/MES
    colnames(taxon.df) <- paste0(taxa[i])
    whole.df <- dplyr::bind_rows(whole.df, taxon.df)
    
    ## Calculate the number of cells correctly/incorrectly predicted for the taxon level unconditional model
    print(paste("Mean Probability of Presence when Present for the", paste(taxa[i]), "Taxon given Unconditional Prediction is:"))
    print(mean(taxon_comp_envi$Probability))
    print(paste("Standard Deviation of the Probability of Presence when Present for the", paste(taxa[i]), "Taxon given Unconditional Prediction is:"))
    print(stats::sd(taxon_comp_envi$Probability))
    print(paste("Mean Probability of Presence when Absent for the", paste(taxa[i]), "Taxon given Unconditional Prediction is:"))
    print(mean(notaxon_comp_envi$Probability))
    print(paste("Standard Deviation of the Probability of Presence when Absent for the", paste(taxa[i]), "Taxon given Unconditional Prediction is:"))
    print(stats::sd(notaxon_comp_envi$Probability))
    
    #Store values for plotting
    mean_envi <- c(mean_envi, mean(taxon_comp_envi$Probability))
    sd_envi <- c(sd_envi, stats::sd(taxon_comp_envi$Probability))
    
    print(paste("Number of corners where", paste(taxa[i]), "was predicted as a true positive / Number of observed presences for unconditional prediction:"))
    print(paste(nrow(dplyr::filter(taxon_comp_envi, Probability >= 0.5)), "/", nrow(taxon_comp_envi))) 
    print(paste("Number of corners where", paste(taxa[i]), "was predicted as a false negative / Number of observed presences for unconditional prediction:"))
    print(paste(nrow(dplyr::filter(taxon_comp_envi, Probability < 0.5)), "/", nrow(taxon_comp_envi))) 
    print(paste("Number of corners where", paste(taxa[i]), "was predicted as a true negative / Number of observed absences for unconditional prediction:"))
    print(paste(nrow(dplyr::filter(notaxon_comp_envi, Probability < 0.5)), "/", nrow(notaxon_comp_envi))) 
    print(paste("Number of corners where", paste(taxa[i]), "was predicted as a false positive / Number of observed absences for unconditional prediction:"))
    print(paste(nrow(dplyr::filter(notaxon_comp_envi, Probability >= 0.5)), "/", nrow(notaxon_comp_envi)))
    
    ## Calculate the number of cells correctly/incorrectly predicted for the taxon level conditional model
    print(paste("Mean Probability of Presence when Present for the", paste(taxa[i]), "Taxon given Conditional Prediction is:"))
    print(mean(taxon_comp_MES$Probability))
    print(paste("Standard Deviation of the Probability of Presence when Present for the", paste(taxa[i]), "Taxon given Conditional Prediction is:"))
    print(stats::sd(taxon_comp_MES$Probability))
    print(paste("Mean Probability of Presence when Absent for the", paste(taxa[i]), "Taxon given Conditional Prediction is:"))
    print(mean(notaxon_comp_MES$Probability))
    print(paste("Standard Deviation of the Probability of Presence when Absent for the", paste(taxa[i]), "Taxon given Conditional Prediction is:"))
    print(stats::sd(notaxon_comp_MES$Probability))
    
    #Store values for plotting
    mean_MES <- c(mean_MES, mean(taxon_comp_MES$Probability))
    sd_MES <- c(sd_MES, stats::sd(taxon_comp_MES$Probability))
    
    print(paste("Number of corners where", paste(taxa[i]), "was predicted as a true positive / Number of observed presences for conditional prediction:"))
    print(paste(nrow(dplyr::filter(taxon_comp_MES, Probability >= 0.5)), "/", nrow(taxon_comp_MES))) 
    print(paste("Number of corners where", paste(taxa[i]), "was predicted as a false negative / Number of observed presences for conditional prediction:"))
    print(paste(nrow(dplyr::filter(taxon_comp_MES, Probability < 0.5)), "/", nrow(taxon_comp_MES))) 
    print(paste("Number of corners where", paste(taxa[i]), "was predicted as a true negative / Number of observed absences for conditional prediction:"))
    print(paste(nrow(dplyr::filter(notaxon_comp_MES, Probability < 0.5)), "/", nrow(notaxon_comp_MES))) 
    print(paste("Number of corners where", paste(taxa[i]), "was predicted as a false positive / Number of observed absences for conditional prediction:"))
    print(paste(nrow(dplyr::filter(notaxon_comp_MES, Probability >= 0.5)), "/", nrow(notaxon_comp_MES)))
  }
  colnames(whole.df.un) <- c("No Tree", "Oak", "Hickory", "Beech", "Maple", "Black Gum/Sweet Gum", "Elm", "Ash", "Basswood", "Dogwood", "Ironwood", "Other Conifer", "Other Hardwood", "Poplar/Tulip Poplar", "Walnut")
  whole.df.un.long <- stats::na.omit(tidyr::pivot_longer(whole.df.un, names_to = "Taxon", values_to = "Probability", cols = 1:15))
  colnames(whole.df) <- c("No Tree", "Oak", "Hickory", "Beech", "Maple", "Black Gum/Sweet Gum", "Elm", "Ash", "Basswood", "Dogwood", "Ironwood", "Other Conifer", "Other Hardwood", "Poplar/Tulip Poplar", "Walnut")
  whole.df.long <- stats::na.omit(tidyr::pivot_longer(whole.df, names_to = "Taxon", values_to = "Probability", cols = 1:15))
  
  ##Plot results
  #For each corner
  unconditional.violin <- ggplot2::ggplot(data = whole.df.un.long, ggplot2::aes(x = Taxon, y = Probability))+
    ggplot2::geom_violin() + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  unconditional.box <- ggplot2::ggplot(data = whole.df.un.long, ggplot2::aes(x = Taxon, y = Probability))+
    ggplot2::geom_boxplot() + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  conditional.violin <- ggplot2::ggplot(data = whole.df.long, ggplot2::aes(x = Taxon, y = Probability))+
    ggplot2::geom_violin() + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  conditional.box <- ggplot2::ggplot(data = whole.df.long, ggplot2::aes(x = Taxon, y = Probability))+
    ggplot2::geom_boxplot() + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  gridExtra::grid.arrange(gridExtra::arrangeGrob(unconditional.box, unconditional.violin, top = "Unconditional", ncol = 2), gridExtra::arrangeGrob(conditional.box, conditional.violin, top = "Conditional", ncol = 2))
  
  #Averaged
  averaged.df <- data.frame("Taxon" = c("No Tree", "Oak", "Hickory", "Beech", "Maple", "Black Gum/Sweet Gum", "Elm", "Ash", "Basswood", "Dogwood", "Ironwood", "Other Conifer", "Other Hardwood", "Poplar/Tulip Poplar", "Walnut"), 
                            "Type" = c(rep("Unconditional", 15), rep("Conditional", 15)),
                            "Mean" = c(mean_envi, mean_MES), 
                            "SD" = c(sd_envi, sd_MES))
  sum.df <- data.frame("Taxon" = c("No Tree", "Oak", "Hickory", "Beech", "Maple", "Black Gum/Sweet Gum", "Elm", "Ash", "Basswood", "Dogwood", "Ironwood", "Other Conifer", "Other Hardwood", "Poplar/Tulip Poplar", "Walnut"), 
                       "Type" = c(rep("Unconditional", 15), rep("Conditional", 15)),
                       "Mean" = c(mean_envi, mean_MES[1] - mean_envi[1], mean_MES[2] - mean_envi[2], mean_MES[3] - mean_envi[3], mean_MES[4] - mean_envi[4], mean_MES[5] - mean_envi[5], mean_MES[6] - mean_envi[6], mean_MES[7] - mean_envi[7], mean_MES[8] - mean_envi[8], mean_MES[9] - mean_envi[9], mean_MES[10] - mean_envi[10], mean_MES[11] - mean_envi[11], mean_MES[12] - mean_envi[12], mean_MES[13] - mean_envi[13], mean_MES[14] - mean_envi[14], mean_MES[15] - mean_envi[15]),
                       "SD" = c(sd_envi, sd_MES),
                       "Addition" = c(rep(0, 15), mean_envi))
  
  sum.plot <- ggplot2::ggplot(sum.df, ggplot2::aes(fill=Taxon, alpha = Type, y=Mean, x= Taxon))+
    ggplot2::geom_bar(stat = "identity")+
    ggplot2::scale_alpha_manual(values = c(0.5, 1))+
    ggplot2::scale_fill_manual(limits = c('No Tree',
                                           'Oak', 'Hickory',
                                           'Ash', 'Basswood', 'Beech',
                                           'Black Gum/Sweet Gum',
                                           'Dogwood', 'Elm',
                                           'Ironwood', 'Maple', 'Other Conifer',
                                           'Other Hardwood', 'Poplar/Tulip Poplar',
                                           'Walnut'),
                                values = pal) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=Mean-SD+Addition, ymax=Mean+SD+Addition), width=.2,position=position_dodge(.9))+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  dodge <- position_dodge2(reverse = TRUE) 
  averaged.df$Taxon <- factor(averaged.df$Taxon, levels = c("No Tree", "Oak", "Hickory", "Beech", "Maple", "Black Gum/Sweet Gum", "Elm", "Ash", "Basswood", "Dogwood", "Ironwood", "Other Conifer", "Other Hardwood", "Poplar/Tulip Poplar", "Walnut"))
  averaged.plot <- ggplot2::ggplot(averaged.df, ggplot2::aes(fill=Taxon, alpha = Type, y=Mean, x= Taxon))+ 
    ggplot2::geom_bar(stat = "identity", position=dodge)+
    ggplot2::ylab(label = "Mean Predicted Probability of Presence \n When Present")+
    ggplot2::scale_alpha_manual(values = c(0.5, 1))+ 
    ggplot2::scale_fill_manual(limits = c('No Tree',
                                           'Oak', 'Hickory',
                                           'Ash', 'Basswood', 'Beech',
                                           'Black Gum/Sweet Gum',
                                           'Dogwood', 'Elm',
                                           'Ironwood', 'Maple', 'Other Conifer',
                                           'Other Hardwood', 'Poplar/Tulip Poplar',
                                           'Walnut'),
                                values = pal) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=Mean-SD, ymax=Mean+SD), position=dodge)+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust=1))
  print(sum.plot)
  print(averaged.plot)
}
