# Tundra Trait Team Data Cleaning Script
# by Anne Bjorkman and Isla Myers-Smith
# 9 October 2017
# primary contact: Anne Bjorkman <annebj@gmail.com>

# Please see:
# Bjorkman, et al. Tundra Trait Team: A database of plant traits spanning the tundra biome. Submitted to Global Ecology and Biogeography in October 2017

# Packages ----
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)

# Load data ----
load("data_raw/teamtraits_AllTraits_AllObs_WideFormat.RData")
head(teamtraits)

teamtraits$IndividualID <- seq(1:nrow(teamtraits))
colnames(teamtraits)[which(colnames(teamtraits)=="SpeciesName")] <- "OriginalName"

teamtraits.long<-melt(teamtraits[,c("IndividualID","AccSpeciesName","OriginalName","SLA","PlantHeight_Veg","PlantHeight_Reproductive","SeedMass_dry","LeafN","LDMC","StemSpecificDensity","LeafP","LeafArea","LeafFreshMass","LeafDryMass","StemDiameter","Leaf.d15N","Leaf.d13c","LeafC","LCID","C.N.ratio","RSratio","RootingDepth","Lat","Lon","Altitude","SiteName","SubsiteName","Treatment","DOY_measured","Year_measured","DataContributor","Data_coPIs","File.name","Comments","ValueKindName","DataEnterer")],id=c("IndividualID","AccSpeciesName","OriginalName","Lat","Lon","Altitude","SiteName","SubsiteName","Treatment","DOY_measured","Year_measured","DataContributor","Data_coPIs","File.name","Comments","ValueKindName","DataEnterer"))
head(teamtraits.long)
nrow(teamtraits.long) # 778050 rows in long form
colnames(teamtraits.long)[which(colnames(teamtraits.long) %in% c("variable","value"))]<-c("Trait","Value")
teamtraits.long<-within(teamtraits.long,Trait<-as.character(Trait))

teamtraits.long<-teamtraits.long[!is.na(teamtraits.long$Value),] # because there is a row for every trait for every individual, many missing

# Remove untrustworthy data ----

# Remove seed mass data from Salix arctica Rebecca Klady because units uncertain and Papaver from me because values seem off (units problem?)
teamtraits.long <- teamtraits.long[c(teamtraits.long$DataContributor=="Anne Bjorkman" & teamtraits.long$AccSpeciesName=="Papaver radicatum" & teamtraits.long$Trait=="SeedMass")==F,]
teamtraits.long <- teamtraits.long[c(teamtraits.long$DataContributor=="Rebecca Klady" & teamtraits.long$AccSpeciesName=="Salix arctica" & teamtraits.long$Trait=="SeedMass")==F,]

# Remove SLA data from Marko's Niwot database
teamtraits.long <- teamtraits.long[c(teamtraits.long$DataContributor=="Marko_Spasojevic" & teamtraits.long$Trait == "SLA")==F,]

# Remove LCID because no one knows what it is
teamtraits.long <- teamtraits.long[teamtraits.long$Trait != "LCID",]

# Remove all treatment data
teamtraits.long <- teamtraits.long[teamtraits.long$Treatment %in% c("none","control","None","Control","Warming over time","Meadow_ field","Vikhireva-Vasilkova et al._ 1964","Pond_ field","Ridge_ field","Basin_ field","Trough_ field") | is.na(teamtraits.long$Treatment),]

# Make pretty names ----
teamtraits.long$TraitPretty[teamtraits.long$Trait=="RootingDepth"] <- "Rooting depth"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="RSratio"] <- "Root:shoot ratio"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="StemSpecificDensity"] <- "Stem density (SSD)"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="C.N.ratio"] <- "Leaf C:N ratio"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="Leaf.d13c"] <- "Leaf d13C"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="StemDiameter"] <- "Stem diameter"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="Leaf.d15N"] <- "Leaf d15N"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="SeedMass_dry"] <- "Seed mass"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="LeafArea"] <- "Leaf area"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="LeafC"] <- "Leaf C"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="LeafDryMass"] <- "Leaf dry mass"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="LeafFreshMass"] <- "Leaf fresh mass"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="LeafN"] <- "Leaf N"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="LeafP"] <- "Leaf P"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="PlantHeight_Reproductive"] <- "Height, repro."
teamtraits.long$TraitPretty[teamtraits.long$Trait=="PlantHeight_Veg"] <- "Height, veg."
teamtraits.long$TraitPretty[teamtraits.long$Trait=="SLA"] <- "Specific leaf area (SLA)"
teamtraits.long$TraitPretty[teamtraits.long$Trait=="LDMC"] <- "LDMC"

# Make names that match with TRY
teamtraits.long$TraitTRY[teamtraits.long$Trait=="RootingDepth"] <- "Rooting depth"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="RSratio"] <- "Root/shoot ratio"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="StemSpecificDensity"] <- "Stem dry mass per stem fresh volume (stem specific density, SSD)"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="C.N.ratio"] <- "Leaf carbon/nitrogen (C/N) ratio"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="Leaf.d13c"] <- "Leaf carbon (C) isotope discrimination (delta 13C)"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="StemDiameter"] <- "Stem diameter"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="Leaf.d15N"] <- "Leaf nitrogen (N) isotope signature (delta 15N)"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="SeedMass_dry"] <- "Seed dry mass"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="LeafArea"] <- "Leaf area"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="LeafC"] <- "Leaf carbon (C) content per leaf dry mass"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="LeafDryMass"] <- "Leaf dry mass"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="LeafFreshMass"] <- "Leaf fresh mass"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="LeafN"] <- "Leaf nitrogen (N) content per leaf dry mass"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="LeafP"] <- "Leaf phosphorus (P) content per leaf dry mass"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="PlantHeight_Reproductive"] <- "Plant height, reproductive"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="PlantHeight_Veg"] <- "Plant height, vegetative"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="SLA"] <- "Leaf area per leaf dry mass (specific leaf area, SLA)"
teamtraits.long$TraitTRY[teamtraits.long$Trait=="LDMC"] <- "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)"

# Add units
teamtraits.long$Units[teamtraits.long$Trait=="RootingDepth"] <- "cm"
teamtraits.long$Units[teamtraits.long$Trait=="RSratio"] <- "ratio"
teamtraits.long$Units[teamtraits.long$Trait=="StemSpecificDensity"] <- "mg/mm3"
teamtraits.long$Units[teamtraits.long$Trait=="C.N.ratio"] <- "ratio"
teamtraits.long$Units[teamtraits.long$Trait=="Leaf.d13c"] <- "parts per thousand"
teamtraits.long$Units[teamtraits.long$Trait=="StemDiameter"] <- "m"
teamtraits.long$Units[teamtraits.long$Trait=="Leaf.d15N"] <- "parts per thousand"
teamtraits.long$Units[teamtraits.long$Trait=="SeedMass_dry"] <- "mg"
teamtraits.long$Units[teamtraits.long$Trait=="LeafArea"] <- "mm2"
teamtraits.long$Units[teamtraits.long$Trait=="LeafC"] <- "mg/g"
teamtraits.long$Units[teamtraits.long$Trait=="LeafDryMass"] <- "mg"
teamtraits.long$Units[teamtraits.long$Trait=="LeafFreshMass"] <- "g"
teamtraits.long$Units[teamtraits.long$Trait=="LeafN"] <- "mg/g"
teamtraits.long$Units[teamtraits.long$Trait=="LeafP"] <- "mg/g"
teamtraits.long$Units[teamtraits.long$Trait=="PlantHeight_Reproductive"] <- "m"
teamtraits.long$Units[teamtraits.long$Trait=="PlantHeight_Veg"] <- "m"
teamtraits.long$Units[teamtraits.long$Trait=="SLA"] <- "mm2/mg"
teamtraits.long$Units[teamtraits.long$Trait=="LDMC"] <- "ratio"

# CLEAN DATASET ----

# Add genus name
teamtraits.long$Genus<-as.vector(sapply(strsplit(teamtraits.long$AccSpeciesName," ",fixed=FALSE), "[", 1))

teamtraits.clean <- teamtraits.long

## Step 1: individual records against the entire distribution (except plant height and leaf area)
teamtraits.clean2 <- teamtraits.clean %>% dplyr::mutate(nrow=row_number()) %>% group_by(Trait) %>% dplyr::mutate(
  mean_all = round(((sum(Value) - Value)/(n()-1)),5), 
  n_all = n(), 
  median_all = median(Value),
  sd_all = sd(Value), 
  ErrorRisk_all = round((abs(Value-mean_all)/sd_all),4),
  ErrorRiskMedian_all = round((abs(Value-median_all)/sd_all),4))

teamtraits.clean3 <- teamtraits.clean2[(is.finite(teamtraits.clean2$ErrorRisk_all) & teamtraits.clean2$ErrorRisk_all > 8 & teamtraits.clean2$Trait %in% c("PlantHeight","LeafArea") == F) == FALSE,]

## Step 2: datasets-by-species against species distribution
teamtraits.clean4 <- teamtraits.clean3 %>% group_by(Trait, AccSpeciesName) %>% dplyr::mutate(
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
teamtraits.clean5 <- teamtraits.clean4[(teamtraits.clean4$ndataset >= 4 & is.finite(teamtraits.clean4$ErrorRisk_spdataset) & teamtraits.clean4$ErrorRisk_spdataset > 3) == FALSE,] 

## Step 3: datasets-by-species agaist genus distribution
teamtraits.clean6 <- teamtraits.clean5[(teamtraits.clean5$ndataset >= 1 & teamtraits.clean5$ndataset < 4 & is.finite(teamtraits.clean5$ErrorRisk_gspdataset) & teamtraits.clean5$ErrorRisk_gspdataset > 3.5) == FALSE,] #CHECK THIS!

## Step 4: individual records against the species distribution
teamtraits.clean7 <- teamtraits.clean6 %>% group_by(Trait, AccSpeciesName) %>% dplyr::mutate(
  ndataset = length(unique(SiteName)), 
  mean_species = round(((sum(Value) - Value)/(n()-1)),5), 
  mad_species = mad(Value, constant=1), 
  sd_species = sd(Value), n_species = n(), 
  ErrorRisk_species = round((abs(Value-mean_species)/sd_species),4))

# 1 - 3 records - leave everything as-is

# 4 - 9 records
teamtraits.clean8 <- teamtraits.clean7[(teamtraits.clean7$n_species >= 4 & teamtraits.clean7$n_species < 10 & is.finite(teamtraits.clean7$ErrorRisk_species) & teamtraits.clean7$ErrorRisk_species > 2.25) == FALSE,]

# 10 - 19 records
teamtraits.clean9 <- teamtraits.clean8[(teamtraits.clean8$n_species >= 10 & teamtraits.clean8$n_species < 20 & is.finite(teamtraits.clean8$ErrorRisk_species) & teamtraits.clean8$ErrorRisk_species > 2.75) == FALSE,]

# 20 - 29 records
teamtraits.clean10 <- teamtraits.clean9[(teamtraits.clean9$n_species >= 20 & teamtraits.clean9$n_species < 30 & is.finite(teamtraits.clean9$ErrorRisk_species) & teamtraits.clean9$ErrorRisk_species > 3.25) == FALSE,]

# >30 records
teamtraits.clean.final <- teamtraits.clean10[(teamtraits.clean10$n_species >= 30 & is.finite(teamtraits.clean10$ErrorRisk_species) & teamtraits.clean10$ErrorRisk_species > 4) == FALSE,]

teamtraits.clean <- as.data.frame(teamtraits.clean.final)

nrow(teamtraits.clean) #66308
nrow(teamtraits.long) #66752

# VIEW CLEANED DATA ----

# Check data removal
teamtraits.clean$NotRemoved <- 1
unmatched <- merge(teamtraits.long, teamtraits.clean, all=T)
unmatched$NotRemoved[is.na(unmatched$NotRemoved)] <- 0
nrow(unmatched[unmatched$NotRemoved==0,])
unmatched$NotRemoved <- factor(unmatched$NotRemoved, levels = c(1,0))

# Graph of kept vs. removed values
pdf("figures/TTT_cleaning.pdf")
d_ply(unmatched[unmatched$Trait %in% c("SLA","PlantHeight","LeafN","LeafArea","LDMC","SeedMass"),], .(Trait,AccSpeciesName), function (x){
ggplot(data=x)+geom_histogram(aes(Value,fill=NotRemoved),bins=200)+theme_bw()+ggtitle(paste(unique(x$AccSpeciesName),unique(x$Trait),sep="_") )
}, .print = T)
dev.off()

# SAVE CLEANED DATA ----

ttt.clean <- teamtraits.clean[,c("IndividualID","AccSpeciesName","OriginalName","Lat","Lon","Altitude","SiteName","SubsiteName","DOY_measured","Year_measured","DataContributor","File.name","Comments","ValueKindName","Trait","TraitPretty","TraitTRY","Value","Units","ErrorRisk_species")]

ttt.clean <- plyr::ddply(ttt.clean, .(Trait, AccSpeciesName), transform,
                         nObsSpp = length(Value[!is.na(Value)]))

ttt.clean$ErrorRisk_species[ttt.clean$nObsSpp < 10] <- NA

ttt.save <- ttt.clean[,c("IndividualID","AccSpeciesName","OriginalName","Lat","Lon","Altitude","SiteName","SubsiteName","DOY_measured","Year_measured","DataContributor","Comments","ValueKindName","TraitTRY","Value","Units","ErrorRisk_species")]
colnames(ttt.save)[which(colnames(ttt.save)=="TraitTRY")] <- "Trait"
colnames(ttt.save)[which(colnames(ttt.save)=="ErrorRisk_species")] <- "ErrorRisk"
colnames(ttt.save)[which(colnames(ttt.save)=="DOY_measured")] <- "DayOfYear"
colnames(ttt.save)[which(colnames(ttt.save)=="Year_measured")] <- "Year"
colnames(ttt.save)[which(colnames(ttt.save)=="Lat")] <- "Latitude"
colnames(ttt.save)[which(colnames(ttt.save)=="Lon")] <- "Longitude"
colnames(ttt.save)[which(colnames(ttt.save)=="Altitude")] <- "Elevation"

ttt.save <- ttt.save[,c("AccSpeciesName","OriginalName","IndividualID","Latitude","Longitude","Elevation","SiteName","SubsiteName","DayOfYear","Year","DataContributor","ValueKindName","Trait","Value","Units","ErrorRisk","Comments")]

save(ttt.save, file = "data_clean/TTT_cleaned_dataset.RData")
write.csv(ttt.save, file = "data_clean/TTT_cleaned_dataset.csv")

# GET DATASET STATISTICS ----

nrow(ttt.clean) # 66308 observations
length(unique(ttt.clean$Trait)) # 18 traits
length(unique(ttt.clean$AccSpeciesName)) # 538 species minus 10 "sp" equals 528 species

# percent of observations with lat/lon info
nrow(ttt.clean[is.na(ttt.clean$Lat) | is.na(ttt.clean$Lon),])/nrow(ttt.clean) # 0.27 % don't have lat/long

# count of unique sites
length(unique(paste(round(ttt.clean$Lat,1),round(ttt.clean$Lon,1), sep="_"))) # 191
length(unique(ttt.clean$SiteName)) # 198
ttt.clean$SiteCode <- paste("Site",round(ttt.clean$Lat,1),round(ttt.clean$Lon,1), sep="_")

# average number of observations per site
obs.per.site <- ddply(ttt.clean, c("SiteCode","Trait"), summarise,
                      count = length(Value[!is.na(Value)]))
mean(obs.per.site$count) # 141.7 observations per trait per site
median(obs.per.site$count) # 36.5 median
max(obs.per.site$count) # 2555 max (plant height at Barrow)

# percent of observations on individual
unique(ttt.clean$ValueKindName)
nrow(ttt.clean[ttt.clean$ValueKindName %in% c("Site specific mean","Plot mean","Maximum in plot"),])/nrow(ttt.clean) # 4.7%
