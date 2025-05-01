######## packages ########
library(ggplot2)
library(RColorBrewer)
library(tibble)
library(dplyr)
library(tidyselect)
library(tidyverse)
installed.packages()

setwd("C:/Users/chand/Documents/data/Nachusa_2020/bombus_R/data_analysis")
bumble2020 <- read.csv("bumble_2020.csv")
summary(bumble2020)

############################################

simple_bumble_dataset <- data.frame(species = bumble2020$bee_species, 
                             sex = bumble2020$sex, 
                             flower = bumble2020$flower_species)

unique(simple_bumble_dataset$flower)


{
    bimac <- subset(simple_bumble_dataset, species == "bimaculatus")
    gris  <- subset(simple_bumble_dataset, species == "griseocollis")
    rufo <- subset(simple_bumble_dataset, species == "rufocinctus")
    affinis <- subset(simple_bumble_dataset, species == "affinis")
    pens <- subset(simple_bumble_dataset, species == "pensylvanicus")
    auri <- subset(simple_bumble_dataset, species == "auricomus")
    imp <- subset(simple_bumble_dataset, species == "impatiens")
    fervidus <- subset(simple_bumble_dataset, species == "fervidus")
    auri_pens <- subset(simple_bumble_dataset, species == "auri/pens")
    vagans <- subset(simple_bumble_dataset, species == "vagans")
    citrinus <- subset(simple_bumble_dataset, species == "citrinus")
    fraternus <- subset(simple_bumble_dataset, species == "fraternus")
    
    floral_preferences <- data.frame(table(simple_bumble_dataset$flower))
    print(floral_preferences)
    floral_preferences$griseocollis <- match(floral_preferences$flowergris$flower)
    
    
  floral_preferences$bimaculatus <- table(bimac$flower)
  floral_preferences$griseocollis <- table(gris$flower)
  floral_preferences$rufocinctus <- table(rufo$flower)
  floral_preferences$affinis <- table(affinis$flower)
  floral_preferences$pensylvanicus <- table(pens$flower)
  floral_preferences$auricomus <- table(auri$flower)
  floral_preferences$impatiens <- table(imp$flower)
  floral_preferences$fervidus <- table(fervidus$flower)
  floral_preferences$vagans <- table(vagans$flower)
  floral_preferences$auri_pens <- table(auri_pens$flower)
  floral_preferences$citrinus <- table(citrinus$flower)
  floral_preferences$fraternus <- table(fraternus$flower)
  
  colnames(floral_preferences) <- c("flower", "all_species","griseocollis", "bimaculatus", "rufocinctus",
                                    "affinis", "pensylvanicus", "auricomus", 
                                    "impatiens", "fervidus", "vagans", "auri_pens", "citrinus", "fraternus")
as.vector(floral_preferences$flower)

floral_preferences <- arrange(floral_preferences, desc(all_species))

}

simplified_floral_preferences <- data.frame()

simplified_floral_preferences <- subset(floral_preferences[c(1,2,5),1:14])


Dalea_spp <- data.frame(table(floral_preferences[3, -1] + floral_preferences[9, -1]))
Dalea_spp[,14] <- NULL
Dalea_spp <- add_column(Dalea_spp, "flower" = "Dalea_spp.", .before = 1)
simplified_floral_preferences <- rbind(simplified_floral_preferences, Dalea_spp)

Liatris_spp <- data.frame(table(floral_preferences[4, -1] + floral_preferences[11, -1]))
Liatris_spp[,14] <- NULL
Liatris_spp <- add_column(Liatris_spp, "flower" = "Liatris_spp.", .before = 1)
simplified_floral_preferences <- rbind(simplified_floral_preferences, Liatris_spp)

Other_spp <- subset(floral_preferences[c(-1,-2,-4,-5,-9,-11),2:14])
Other_spp_sum <- data.frame(all_species = NA, x = NA, x= NA,
                            x = NA, x = NA, x= NA,
                            x = NA, x = NA, x= NA,
                            x = NA, x = NA, x= NA,
                            x = NA)
Other_spp_sum[1,1:13]<- colSums(Other_spp)
Other_spp_sum <- add_column(Other_spp_sum, "flower" = "Other", .before = 1)
colnames(Other_spp_sum) <- c("flower", "all_species","griseocollis", "bimaculatus", "rufocinctus",
                                  "affinis", "pensylvanicus", "auricomus", 
                                  "impatiens", "fervidus", "vagans", "auri_pens", "citrinus", "fraternus")
simplified_floral_preferences <- rbind(simplified_floral_preferences, Other_spp_sum)
key_floral_preferences_melted <- melt(simplified_floral_preferences, id=c("flower"), 
                           value.name = "count", 
                           variable.name = "Bombus_species")

key_floral_preferences_melted$relative_freq <- NA

{
key_floral_preferences_melted[1:6,4] <- as.integer(key_floral_preferences_melted[1:6,3]) / sum(as.integer(key_floral_preferences_melted[1:6,3])) 
key_floral_preferences_melted[7:12,4] <- as.integer(key_floral_preferences_melted[7:12,3]) / sum(as.integer(key_floral_preferences_melted[7:12,3])) 
key_floral_preferences_melted[13:18,4] <- as.integer(key_floral_preferences_melted[13:18,3]) / sum(as.integer(key_floral_preferences_melted[13:18,3])) 
key_floral_preferences_melted[19:24,4] <- as.integer(key_floral_preferences_melted[19:24,3]) / sum(as.integer(key_floral_preferences_melted[19:24,3])) 
key_floral_preferences_melted[25:30,4] <- as.integer(key_floral_preferences_melted[25:30,3]) / sum(as.integer(key_floral_preferences_melted[25:30,3])) 
key_floral_preferences_melted[31:36,4] <- as.integer(key_floral_preferences_melted[31:36,3]) / sum(as.integer(key_floral_preferences_melted[31:36,3])) 
key_floral_preferences_melted[37:42,4] <- as.integer(key_floral_preferences_melted[37:42,3]) / sum(as.integer(key_floral_preferences_melted[37:42,3])) 
key_floral_preferences_melted[43:48,4] <- as.integer(key_floral_preferences_melted[43:48,3]) / sum(as.integer(key_floral_preferences_melted[43:48,3])) 
key_floral_preferences_melted[49:54,4] <- as.integer(key_floral_preferences_melted[49:54,3]) / sum(as.integer(key_floral_preferences_melted[49:54,3])) 
key_floral_preferences_melted[55:60,4] <- as.integer(key_floral_preferences_melted[55:60,3]) / sum(as.integer(key_floral_preferences_melted[55:60,3])) 
key_floral_preferences_melted[61:66,4] <- as.integer(key_floral_preferences_melted[61:66,3]) / sum(as.integer(key_floral_preferences_melted[61:66,3])) 
key_floral_preferences_melted[67:72,4] <- as.integer(key_floral_preferences_melted[67:72,3]) / sum(as.integer(key_floral_preferences_melted[67:72,3])) 
key_floral_preferences_melted[73:78,4] <- as.integer(key_floral_preferences_melted[73:78,3]) / sum(as.integer(key_floral_preferences_melted[73:78,3])) 

key_floral_preferences_melted[,4] <- round(key_floral_preferences_melted[,4], digits = 2)
}

key_floral_preferences_melted <- key_floral_preferences_melted[-c(1:6,61:66,73:78),]
key_floral_preferences_melted <- key_floral_preferences_melted[,order(colnames(key_floral_preferences_melted))]
key_floral_preferences_melted <- arrange(key_floral_preferences_melted, Bombus_species)
as.character(key_floral_preferences_melted$Bombus_species)

##### reordering key_floral_preferences_melted #######
{
floral_pref_melt_reorder <- data.frame()
## affinis ## 
floral_pref_melt_reorder[1:6,1:4] <- key_floral_preferences_melted[19:24, 1:4]
## impatiens ##
floral_pref_melt_reorder[7:12,1:4] <- key_floral_preferences_melted[37:42, 1:4]
## bimaculatus ##
floral_pref_melt_reorder[13:18,1:4] <- key_floral_preferences_melted[7:12, 1:4]
## vagans ##
floral_pref_melt_reorder[19:24,1:4] <- key_floral_preferences_melted[49:54, 1:4]
## fervidus ##
floral_pref_melt_reorder[25:30,1:4] <- key_floral_preferences_melted[43:49, 1:4]
## pensylvanicus ##
floral_pref_melt_reorder[31:36,1:4] <- key_floral_preferences_melted[25:30, 1:4]
## auricomus ##
floral_pref_melt_reorder[37:42,1:4] <- key_floral_preferences_melted[31:36, 1:4]
## griseocollis ##
floral_pref_melt_reorder[43:48,1:4] <- key_floral_preferences_melted[1:6, 1:4]
## rufocinctus ## 
floral_pref_melt_reorder[49:54,1:4] <- key_floral_preferences_melted[13:18, 1:4]
## citrinus ##
floral_pref_melt_reorder[55:60,1:4] <- key_floral_preferences_melted[55:60, 1:4]
}

floral_pref_melt_reorder$Bombus_species <- factor(floral_pref_melt_reorder$Bombus_species,
                                                  levels = c("affinis", "impatiens", "bimaculatus",
                                                             "vagans", "fervidus", "pensylvanicus",
                                                             "auricomus", "griseocollis", "rufocinctus", "citrinus"))

(
ggplot(data = floral_pref_melt_reorder, 
       aes(x = Bombus_species, y = relative_freq, fill = flower))
  + geom_bar(aes(fill = flower), 
             position = position_fill(reverse = TRUE), 
             stat = "identity", 
             colour = "black")
  + theme(axis.text.x = element_text(size = 10, angle = 55, hjust = 1), 
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  + scale_x_discrete(labels = c("*affinis", "impatiens", "bimaculatus",
                                  "vagans", "*fervidus", "*pensylvanicus",
                                  "auricomus", "griseocollis", "rufocinctus", "†citrinus"))
  + xlab("Bombus Species")
  + ylab("Proportion of Flower Visits")
  + ggtitle("Bumble Bee Foraging Preferences")
  + guides(fill = guide_legend(reverse = TRUE))
  + labs(fill = "Flower Species")
  + scale_fill_manual(values = c("skyblue1", "steelblue1","plum3","mediumpurple1", "lightsalmon","grey88"),
                      labels = c("Baptisia alba", "Cirsium discolor", "Monarda fistulosa", "Liatris sp.", "Dalea sp.", "Other"))
)
