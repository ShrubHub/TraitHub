# Tundra Trait Team Data Cleaning Script
# by Anne Bjorkman and Isla Myers-Smith
# 15 November 2017
# primary contact: Anne Bjorkman <annebj@gmail.com>

# Please see:
# Bjorkman, et al. Tundra Trait Team: A database of plant traits spanning the tundra biome. Submitted to Global Ecology and Biogeography (November 2017)

# Packages ----
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)

# Import uncleaned data

ttt.unclean <- read.csv("data_raw/TTT_uncleaned_v1.csv", stringsAsFactors = F)

## Step 1: individual records against the entire distribution (except plant height and leaf area)
ttt.clean2 <- ttt.unclean %>% dplyr::mutate(nrow=row_number()) %>% group_by(Trait) %>% dplyr::mutate(
  mean_all = round(((sum(Value) - Value)/(n()-1)),5), 
  n_all = n(), 
  median_all = median(Value),
  sd_all = sd(Value), 
  ErrorRisk_all = round((abs(Value-mean_all)/sd_all),4),
  ErrorRiskMedian_all = round((abs(Value-median_all)/sd_all),4))

ttt.clean3 <- ttt.clean2[(is.finite(ttt.clean2$ErrorRisk_all) & ttt.clean2$ErrorRisk_all > 8 & ttt.clean2$Trait %in% c("Plant height, reproductive","Plant height, vegetative","Leaf area") == F) == FALSE,]

## Step 2: datasets-by-species against species distribution
ttt.clean4 <- ttt.clean3 %>% group_by(Trait, AccSpeciesName) %>% dplyr::mutate(
  ndataset = length(unique(SiteName)), 
  mean_species = round(((sum(Value) - Value)/(n()-1)),5), 
  median_species = median(Value),
  sd_species = sd(Value), 
  n_species = n(), 
  ErrorRisk_species = round((abs(Value-mean_species)/sd_species),4),
  ErrorRiskMedian_species = round((abs(Value-median_species)/sd_species),4)) %>% 
  
  group_by(Trait, Genus) %>% dplyr::mutate(
    mean_genus = round(((sum(Value) - Value)/(n()-1)),5),
    median_genus = median(Value),
    sd_genus = sd(Value), 
    n_genus = n(), 
    ErrorRisk_genus = round((abs(Value-mean_genus)/sd_genus),4),
    ErrorRiskMedian_genus = round((abs(Value-median_genus)/sd_genus),4)) %>% 
  
  group_by(Trait, AccSpeciesName, SiteName) %>% dplyr::mutate(
    mean_spdataset = round(((sum(mean_species) - mean_species)/(n()-1)),5), 
    median_spdataset = median(Value),
    sd_spdataset = sd(Value), 
    n_spdataset = n(), 
    ErrorRisk_spdataset = round((abs(mean_spdataset-mean_species)/sd_species),4),
    ErrorRiskMedian_spdataset = round((abs(median_spdataset-median_species)/sd_species),4)) %>% 
  
  group_by(Trait, Genus, SiteName) %>% dplyr::mutate(
    mean_gspdataset = round(((sum(mean_genus) - mean_genus)/(n()-1)),5), 
    median_gspdataset = median(Value),
    sd_gspdataset = sd(Value), 
    n_gdataset = n(), 
    ErrorRisk_gspdataset = round((abs(mean_spdataset-mean_genus)/sd_genus),4),
    ErrorRiskMedian_gspdataset = round((abs(median_spdataset-median_genus)/sd_genus),4))

# > 4 datasets per species
ttt.clean5 <- ttt.clean4[(ttt.clean4$ndataset >= 4 & is.finite(ttt.clean4$ErrorRisk_spdataset) & ttt.clean4$ErrorRisk_spdataset > 3) == FALSE,] 

## Step 3: datasets-by-species agaist genus distribution
ttt.clean6 <- ttt.clean5[(ttt.clean5$ndataset >= 1 & ttt.clean5$ndataset < 4 & is.finite(ttt.clean5$ErrorRisk_gspdataset) & ttt.clean5$ErrorRisk_gspdataset > 3.5) == FALSE,]

## Step 4: individual records against the species distribution
ttt.clean7 <- ttt.clean6 %>% group_by(Trait, AccSpeciesName) %>% dplyr::mutate(
  ndataset = length(unique(SiteName)), 
  mean_species = round(((sum(Value) - Value)/(n()-1)),5), 
  mad_species = mad(Value, constant=1), 
  sd_species = sd(Value), n_species = n(), 
  ErrorRisk_species = round((abs(Value-mean_species)/sd_species),4))

# 1 - 3 records - leave everything as-is

# 4 - 9 records
ttt.clean8 <- ttt.clean7[(ttt.clean7$n_species >= 4 & ttt.clean7$n_species < 10 & is.finite(ttt.clean7$ErrorRisk_species) & ttt.clean7$ErrorRisk_species > 2.25) == FALSE,]

# 10 - 19 records
ttt.clean9 <- ttt.clean8[(ttt.clean8$n_species >= 10 & ttt.clean8$n_species < 20 & is.finite(ttt.clean8$ErrorRisk_species) & ttt.clean8$ErrorRisk_species > 2.75) == FALSE,]

# 20 - 29 records
ttt.clean10 <- ttt.clean9[(ttt.clean9$n_species >= 20 & ttt.clean9$n_species < 30 & is.finite(ttt.clean9$ErrorRisk_species) & ttt.clean9$ErrorRisk_species > 3.25) == FALSE,]

# >30 records
ttt.clean.final <- ttt.clean10[(ttt.clean10$n_species >= 30 & is.finite(ttt.clean10$ErrorRisk_species) & ttt.clean10$ErrorRisk_species > 4) == FALSE,]

ttt.clean <- as.data.frame(ttt.clean.final)

nrow(ttt.clean)
nrow(ttt.unclean)

nrow(ttt.unclean)-nrow(ttt.clean) #number of observations removed


# VIEW CLEANED DATA ----

# Check data removal
ttt.clean$NotRemoved <- 1
unmatched <- merge(ttt.unclean, ttt.clean, all=T)
unmatched$NotRemoved[is.na(unmatched$NotRemoved)] <- 0
unmatched$NotRemoved <- factor(unmatched$NotRemoved, levels = c(1,0))

# Histograms of kept vs. removed values per species and trait
# NOTE: this can take a long time to run

pdf("figures/TTT_cleaning.pdf")

d_ply(unmatched[unmatched$Trait %in% c("Leaf area per leaf dry mass (specific leaf area, SLA)","Plant height, vegetative","Leaf nitrogen (N) content per leaf dry mass","Leaf area"),], .(Trait,AccSpeciesName), function (x){
  ggplot(data=x)+
    geom_histogram(aes(Value,fill=NotRemoved),bins=200)+
    theme_bw()+
    ggtitle(paste(unique(x$AccSpeciesName),unique(x$Trait),sep="_") )
}, .print = T)

dev.off()


# SAVE CLEANED DATA ----

ttt.clean <- plyr::ddply(ttt.clean, .(Trait, AccSpeciesName), transform,
                         nObsSpp = length(Value[!is.na(Value)]))

ttt.clean$ErrorRisk_species[ttt.clean$nObsSpp < 10] <- NA
colnames(ttt.clean)[which(colnames(ttt.clean)=="ErrorRisk_species")] <- "ErrorRisk"

ttt.clean <- ttt.clean[,c("AccSpeciesName","OriginalName","IndividualID","Latitude","Longitude","Elevation","SiteName","SubsiteName","Treatment","DayOfYear","Year","DataContributor","ValueKindName","Trait","Value","Units","ErrorRisk","Comments")]

write.csv(ttt.clean, file = "data_clean/TTT_cleaned_dataset.csv")


# CONVERT FROM LONG FORMAT TO WIDE ----

# All traits measured on the same individual will be in the same row, traits in columns
# Note that the resulting file will only contain trait values but NOT the Error Risk or Units (because these vary by trait within individuals)

library(reshape2)

ttt.wide <- dcast(ttt.clean[,c(colnames(ttt.clean)[which(colnames(ttt.clean) %in% c("ErrorRisk","Units")==F)])], AccSpeciesName+OriginalName+IndividualID+Latitude+Longitude+Elevation+SiteName+SubsiteName+Treatment+DayOfYear+Year+DataContributor+ValueKindName+Comments ~ Trait, value.var = "Value")

