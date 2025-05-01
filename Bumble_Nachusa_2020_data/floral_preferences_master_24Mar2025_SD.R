##### Reading in Packages #####
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(performance)

#read in model packages + tools

library(glmmTMB)
library(car)
library(multcomp)
library(performance)
library(MASS)
library(MuMIn)


##### Reading in Raw Data #####

setwd("C:/Users/chand/Documents/data/Nachusa_2020/bombus_R/data_analysis")
bumble2020 <- read.csv("bumble_2020_srg.csv", header = TRUE)
floral2020 <- read.csv("Nachusa_floral_2020.csv", header = TRUE)
ground_cover2020 <- read.csv("Nachusa_ground_cover2020.csv", header = TRUE)
cover_class_1000m_buffer <- read.csv("final_1000m_planting_buffers.csv", header = TRUE)
cover_class_500m_buffer <- read.csv("cover_class_500m_buffer_final.csv", header = TRUE)
cover_class_250m_buffer <- read.csv("cover_class_250m_buffer_final2.csv", header = TRUE)
cover_class_sites <- table(list(unique(cover_class_1000m_buffer$site)))

bumble2020$visit_number <- NULL
floral2020$visit_number <- NULL
colnames(bumble2020)[colnames(bumble2020) == "survery_start"] <- "start_time"
colnames(bumble2020)[colnames(bumble2020) == "survey_end"] <- "end_time"


bumble2020[,1:8] <- lapply(bumble2020[,1:8] , factor)

##### Creating an Empty Dataframe #####


#put in new dataset with site/date

bumble2020$date_site_month = as_factor(paste(bumble2020$date,":", bumble2020$site,":",bumble2020$month)) # we did 229 total surveys! Wow! 

Species=as.factor(c("bimaculatus", "grisecollis", "rufocinctus",
          "affinis", "pensylvanicus", "auricomus", 
          "impatiens", "fervidus", "vagans", "auri/pens", "citrinus", "fraternus"))

Flowers = data.frame(table(bumble2020$flower_species))

empty = expand.grid("bee_species" = Species, "flower_species" = Flowers$Var1, "date_site_month" = levels(bumble2020$date_site_month))
empty1 = separate(data = empty, col = "date_site_month", into = c("date","site","month"),  sep = ":") #separate year and season, so it can be merged

empty1[,1:5] <- lapply(empty1[,1:5] , factor) #make factor

##### Cleaning Data of White Space #####

empty1= empty1%>% mutate_if(is.factor, funs(factor(trimws(.)))) 
bumble2020= bumble2020%>% mutate_if(is.factor, funs(factor(trimws(.)))) 
ground_cover2020= ground_cover2020%>% mutate_if(is.factor, funs(factor(trimws(.)))) 
floral2020= floral2020%>% mutate_if(is.factor, funs(factor(trimws(.)))) 


########## Creating the Bumble Bee Dataset! ###########

survey_site_list <- table(list(unique(bumble2020$site)))
date_list <- table(list(unique(bumble2020$date)))
species_list <- data.frame(table(list(unique(bumble2020$bee_species)))) # 11 species total
flowers_visited <- list(unique(bumble2020$flower_species)) # 75 species of plants visited, not including "na", "None", Solidago spp., and Bidens spp.

bumble_data1 <- bumble2020 %>% group_by(site, date, bee_species, flower_species, month) %>% 
  summarise(
    abun = n_distinct(Unique_ID))
bumble_data1$abun = as.character(bumble_data1$abun)
bumble_data1$bee_species = as.factor(bumble_data1$bee_species)
bumble_data1$flower_species = as.factor(bumble_data1$flower_species)



bumble_final = left_join(empty1, bumble_data1, by=c("bee_species", 
                                                    "flower_species",  
                                                    "date", 
                                                    "site", 
                                                    "month")) #this original had "all = TRUE, but it seemed to be causing an error 3/24/25 SD)



bumble_final[is.na(bumble_final)] <- 0
bumble_final$abun = as.numeric(bumble_final$abun)
  ##### Joining Floral Data #####
  
floral2020$total_flowers <- floral2020$avg_flow_inflorescene * floral2020$num_flower
  
  floral2020[,1:3] <- lapply(floral2020[,1:3], factor)
  
  
  floral2020_1 = floral2020 %>% group_by(date, site) %>%
    summarise(
      flower_richness = n_distinct(flower_species, na.rm = TRUE),
      inflorescene_abun = sum(num_flower, na.rm=TRUE),
      total_flowers = sum(total_flowers)) %>% ungroup()
  
  bumble_final<- left_join(bumble_final, floral2020_1, by = c("date", "site"))
  
  
floral_summary = floral2020 %>% group_by(flower_species) %>%
    summarise(inflorescene_abun = sum(num_flower, na.rm=TRUE),
      total_flowers = sum(total_flowers)) %>% ungroup()
  
  ##### Standardizing floral data #####
  
  bumble_final$flower_richness_standardized <- (bumble_final$flower_richness - mean(bumble_final$flower_richness, na.rm = TRUE))/sd(bumble_final$flower_richness, na.rm = TRUE)
  
  bumble_final$inflorescene_abun_standardized <- (bumble_final$inflorescene_abun-mean(bumble_final$inflorescene_abun, na.rm = TRUE))/sd(bumble_final$inflorescene_abun, na.rm = TRUE)
  
  bumble_final$total_flowers_standardized <- (bumble_final$total_flowers-mean(bumble_final$total_flowers, na.rm = TRUE))/sd(bumble_final$total_flowers, na.rm = TRUE)
  
  
  ##### Joining Ground Cover Data #####
  
  colnames(ground_cover2020)[colnames(ground_cover2020) == "Litter.Depth"] <- "litter_depth"
  colnames(ground_cover2020)[colnames(ground_cover2020) == "Site"] <- "site"
  
  ground_cover2020[,1:3] <- lapply(ground_cover2020[,1:3], factor)
  
  ground_cover_site_summary <- ground_cover2020 %>% group_by(site) %>% 
    summarise(bare = mean(Bare),
              rocky = mean(Rocky),
              litter = mean(Litter),
              grass = mean(Grass),
              forb = mean(Forb),
              litter_depth_cm = mean(litter_depth)) %>% ungroup()
  
  bumble_final = left_join(bumble_final, ground_cover_site_summary, by = c("site"))
  
  bumble_final$litter_depth_standardized <- scale(bumble_final$litter_depth_cm)
  
  
  ############### Land Cover classes! #############
  
  site_buffer_cover_class <- data.frame(list(unique(cover_class_1000m_buffer$site)))
  
  colnames(site_buffer_cover_class)[1] <- "site"
  
##### 1000 meter buffers #####
  
classes_spread <- cover_class_1000m_buffer %>% spread(cover_class, class_percentage)
classes_spread[is.na(classes_spread)] <- 0

classified_1000m_buffer <- aggregate(x=classes_spread, 
                            by = list(classes_spread$site), max, 
                            drop = TRUE)

classified_1000m_buffer$Group.1 <- NULL

colnames(classified_1000m_buffer) <- c("site", "area_ha,","area_m","cover_class_area",
                                       "unclassified",
                                       "prairie",
                                       "forest",
                                       "agriculture",
                                       "pasture",
                                       "pond",
                                       "river",
                                       "urban")
##### 500 meter buffers #####

classes_spread_500m <- cover_class_500m_buffer %>% spread(cover_class, class_percentage)
classes_spread_500m[is.na(classes_spread_500m)] <- 0

classified_500m_buffer <- aggregate(x=classes_spread_500m, 
                             by = list(classes_spread_500m$site), max, 
                             drop = TRUE)

classified_500m_buffer$Group.1 <- NULL

colnames(classified_500m_buffer) <- c("site", "area_ha,","area_m","cover_class_area",
                                       "prairie",
                                       "forest",
                                       "agriculture",
                                       "pasture",
                                       "pond",
                                       "river",
                                       "urban")

##### 250 meter buffers #####

classes_spread_250m <- cover_class_250m_buffer %>% spread(cover_class, class_percentage)
classes_spread_250m[is.na(classes_spread_250m)] <- 0

classified_250m_buffer <- aggregate(x=classes_spread_250m, 
                                    by = list(classes_spread_250m$site), max, 
                                    drop = TRUE)

classified_250m_buffer$Group.1 <- NULL

colnames(classified_250m_buffer) <- c("site","area_m","cover_class_area",
                                      "prairie",
                                      "forest",
                                      "agriculture",
                                      "pasture",
                                      "pond",
                                      "urban")

##### Joining land cover data to bumble_final #####

### 1000m ###

bumble_final = left_join(bumble_final, classified_1000m_buffer, by=c("site"))#,  all=TRUE)

colnames(bumble_final)[colnames(bumble_final) %in% c("unclassified",
                                                    "prairie",
                                                    "forest",
                                                    "agriculture",
                                                    "pasture",
                                                    "pond",
                                                    "river",
                                                    "urban")] <- c("unclassified_1000m",
                                                                   "prairie_1000m",
                                                                   "forest_1000m",
                                                                   "agriculture_1000m",
                                                                   "pasture_1000m",
                                                                   "pond_1000m",
                                                                   "river_1000m",
                                                                   "urban_1000m")

### 500m ###

bumble_final = left_join(bumble_final, classified_500m_buffer, by=c("site"))#,  all=TRUE)

colnames(bumble_final)[colnames(bumble_final) %in% c("prairie",
                                                     "forest",
                                                     "agriculture",
                                                     "pasture",
                                                     "pond",
                                                     "river",
                                                     "urban")] <- c("prairie_500m",
                                                                    "forest_500m",
                                                                    "agriculture_500m",
                                                                    "pasture_500m",
                                                                    "pond_500m",
                                                                    "river_500m",
                                                                    "urban_500m")

### 250m ###

bumble_final = left_join(bumble_final, classified_250m_buffer, by=c("site"))#,  all=TRUE)

colnames(bumble_final)[colnames(bumble_final) %in% c("prairie",
                                                     "forest",
                                                     "agriculture",
                                                     "pasture",
                                                     "pond",
                                                     "urban")] <- c("prairie_250m",
                                                                    "forest_250m",
                                                                    "agriculture_250m",
                                                                    "pasture_250m",
                                                                    "pond_250m",
                                                                    "urban_250m")


bumble_final[,c(1:4)] <- lapply(bumble_final[,c(1:4)] , factor)

######## Combining pasture + Prairie  land covers ############

bumble_final$prairie_1000m <- bumble_final$prairie_1000m + bumble_final$pasture_1000m
bumble_final$pasture_1000m <- NULL

bumble_final$prairie_500m <- bumble_final$prairie_500m + bumble_final$pasture_500m
bumble_final$pasture_500m <- NULL

bumble_final$prairie_250m <- bumble_final$prairie_250m + bumble_final$pasture_250m
bumble_final$pasture_250m <- NULL

 
#### Getting some floral information ### 
flow <- bumble_final %>% group_by(flower_species) %>% summarize(total_bumble_visits = sum(abun),
                                                        prop_bumble_visits = sum(total_bumble_visits))



#read packages

  library(glmmTMB)
  library(performance)
  
  library(lme4)
  



########## Pearson Correlation Tests ###################  

# cor(x, y, method = "pearson")
# cor.test(x, y, method = "pearson")

#### Prairie vs Agriculture ####
cor.test(bumble_final$prairie_1000m, bumble_final$agriculture_1000m, method = "pearson")
ag_prairie_1000m <- plot(bumble_final$prairie_1000m, bumble_final$agriculture_1000m) 
ag_prairie_1000m <- abline(lm(bumble_final$prairie_1000m ~ bumble_final$agriculture_1000m))


cor.test(bumble_final$prairie_500m, bumble_final$agriculture_500m, method = "pearson")
ag_prairie_500m <- plot(bumble_final$prairie_500m, bumble_final$agriculture_500m) 
ag_prairie_500m <- abline(a=58.01, b = -0.70)

lines(bumble_final$prairie_500m, bumble_final$forest_500m, fitted(fit), col="purple")
lm(bumble_final$prairie_500m ~ bumble_final$agriculture_500m)

cor.test(bumble_final$prairie_250m, bumble_final$agriculture_250m, method = "pearson")
ag_prairie_250m <- plot(bumble_final$prairie_250m, bumble_final$agriculture_250m)
ag_prairie_250m  <- abline(lm(bumble_final$prairie_250m ~ bumble_final$agriculture_250m))

#ALL CORRELATED

#### Ground Cover Stuff ####

cor.test(bumble_final$grass, bumble_final$forb, method = "pearson")
grass_vs_forb <- plot(bumble_final$grass, bumble_final$forb)

# CORRELATED 


cor.test(bumble_final$grass, bumble_final$litter, method = "pearson")
# 	Pearson's product-moment correlation

# data:  bumble_final$grass and bumble_final$litter
# t = 3.121, df = 808, p-value = 0.001866
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.04056225 0.17669830
# sample estimates:
#  cor 
# 0.109142 

grass_vs_litter <- plot(bumble_final$grass, bumble_final$litter)
grass_vs_litter <- abline(lm(bumble_final$grass ~ bumble_final$litter))

# NOT CORRELATED (?)

cor.test(bumble_final$litter, bumble_final$litter_depth_standardized, method = "pearson")
litter_vs_litterdepth <- plot(bumble_final$litter, bumble_final$litter_depth_standardized)
litter_vs_litterdepth <- abline(a = 0.3229532 , b = 0.4405626)

# NOT CORRELATED (?)


cor.test(bumble_final$flower_richness_standardized, bumble_final$inflorescene_abun_standardized, method = "pearson")
flower_richness_vs_infloresecene_abun <- plot(bumble_final$flower_richness, bumble_final$inflorescene_abun_standardized)
litter_vs_litterdepth <- abline(bumble_final$flower_richness ~ bumble_final$inflorescene_abun_standardized)



########### Model 1000m landscape #############################################################
  
  bumble_model_1000m = glmmTMB(data = bumble_final, abun ~ 
                           prairie_1000m +
                           forest_1000m +
                           grass +
                           litter * litter_depth_standardized +
                           flower_richness_standardized + 
                           month * inflorescene_abun_standardized +
                           (1|site) + (1|bee_species), family="nbinom2")

check_overdispersion(bumble_model_1000m) #only works for poisson. nbinom2 fits best

summary(bumble_model_1000m)

check_collinearity(bumble_model_1000m) #no collinearity

Anova(bumble_model_1000m, type="III")

AICc(bumble_model_1000m) #3131.483


 #dredge(bumble_model_1000m) #22 top models!!! Should we do some averaging? Probably.
  
########### Model 500m landscape #############################################################

bumble_model_500m = glmmTMB(data = bumble_final, abun ~ 
                               prairie_500m +
                               forest_500m +
                               grass +
                               litter * litter_depth_standardized +
                               flower_richness_standardized + 
                               month * inflorescene_abun_standardized +
                               (1|site) + (1|bee_species), family="nbinom2")

summary(bumble_model_500m)

check_collinearity(bumble_model_500m)

Anova(bumble_model_500m)

AICc(bumble_model_500m) #3131.345. basically the same.


########### Model 250m landscape #############################################################

bumble_model_250m = glmmTMB(data = bumble_final, abun ~ 
                              prairie_250m +
                              forest_250m +
                              grass +
                              litter * litter_depth_standardized +
                              flower_richness_standardized + 
                              month * inflorescene_abun_standardized +
                              (1|site) + (1|bee_species), family="nbinom2")

summary(bumble_model_250m)

check_collinearity(bumble_model_250m)

Anova(bumble_model_250m)

AICc(bumble_model_250m) #3130.063. basically the same.



############### Individual Species Models #############################################################

imp_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "impatiens"), abun ~
                      prairie_1000m +
                      forest_1000m +
                      grass +
                      litter * litter_depth_standardized +
                      flower_richness_standardized + 
                      month * inflorescene_abun_standardized +
                      (1|site), family="nbinom2")

summary(imp_model)
Anova(imp_model, type="III")
AICc(imp_model)

gris_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "griseocollis"), abun ~
                       prairie_1000m +
                       forest_1000m +
                       grass +
                       litter * litter_depth_standardized +
                       flower_richness_standardized + 
                       month * inflorescene_abun_standardized +
                       (1|site), family="nbinom2")

summary(gris_model)
Anova(gris_model, type="III")


auri_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "auricomus"), abun ~
                       prairie_1000m +
                       forest_1000m +
                       grass +
                       litter * litter_depth_standardized +
                       flower_richness_standardized + 
                       month * inflorescene_abun_standardized +
                       (1|site), family="nbinom2")

summary(auri_model)
Anova(auri_model, type="III")

rufo_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "rufocinctus"), abun ~
                       prairie_1000m +
                       forest_1000m +
                       grass +
                       litter * litter_depth_standardized +
                       flower_richness_standardized + 
                       month * inflorescene_abun_standardized +
                       (1|site), family="nbinom2")

summary(rufo_model)
Anova(rufo_model, type="III")


ferv_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "fervidus"), abun ~
                       prairie_1000m +
                       forest_1000m +
                       grass +
                       litter * litter_depth_standardized +
                       flower_richness_standardized + 
                       month * inflorescene_abun_standardized +
                       (1|site), family="nbinom2")

summary(ferv_model)
Anova(ferv_model, type="III")

####### Plots and Visualization ########

### Testing out some things, averaging some values ###
library(plotrix)
month_averages <- bumble_final %>% group_by(month) %>% mutate(., month = factor(month, levels = c("June",
                                                                                                  "July",
                                                                                                  "August",
                                                                                                  "September"))) %>% 
  summarise(
    avg_inflor_abun = mean(inflorescene_abun_standardized, na.rm = TRUE),
    avg_inflor_abun_SD = sd(inflorescene_abun_standardized, na.rm = TRUE),
    avg_inflor_SE = std.error(inflorescene_abun_standardized, na.rm = TRUE),
    avg_bumble_abun = mean(abun, na.rm = TRUE),
    avg_bumble_abun_SD = sd(abun, na.rm = TRUE),
    avg_bumble_abun_SE = std.error(abun, na.rm = TRUE))

day_averages <- bumble_final %>% group_by(date, month) %>% mutate(., date = factor(date)) %>% 
  summarise(
    avg_inflor_abun = mean(inflorescene_abun_standardized, na.rm = TRUE),
    avg_inflor_abun_SD = sd(inflorescene_abun_standardized, na.rm = TRUE),
    avg_inflor_SE = std.error(inflorescene_abun_standardized, na.rm = TRUE),
    avg_bumble_abun = mean(abun, na.rm = TRUE),
    avg_bumble_abun_SD = sd(abun, na.rm = TRUE),
    avg_bumble_abun_SE = std.error(abun, na.rm = TRUE))



bumble_daily_abun <- bumble_final %>% group_by(date, month) %>% summarise(sum_abun_daily = sum(abun))
{
  daily_bumble_abundance_plot <- ggplot(bumble_daily_abun, aes(x = date,
                                              y = sum_abun_daily,
                                              color = month, 
                                              group = 1)) +
    geom_point(aes(color = month))+ stat_smooth(position = "identity") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    ggtitle("Daily Mean Bumble Bee Abundance") +
    scale_colour_discrete(limits = c("June", "July", "August", "September"))
  daily_bumble_abundance_plot
}

####### First plots ###################
{
inflor_abun_month_plot <- ggplot(month_averages, aes(x = month, y = avg_inflor_abun, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax=avg_inflor_abun+avg_inflor_SE, 
                    ymin=avg_inflor_abun-avg_inflor_SE),
                position=position_dodge(0), 
                width=.1,size=.75, 
                data= month_averages)
inflor_abun_month_plot
  }

{
bumble_abun_month_plot <- ggplot(month_averages, aes(x = month, y = avg_bumble_abun, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax=avg_bumble_abun+avg_bumble_abun_SE, 
                    ymin=avg_bumble_abun-avg_bumble_abun_SE),
                position=position_dodge(0), 
                width=.1,size=.75, 
                data= month_averages)
bumble_abun_month_plot
}


###### Average Daily Inflorescenes Abundance Line plot #####
{
  flower_abun_day <- ggplot(day_averages, aes(x = date,
                                              y = avg_inflor_abun,
                                              color = month, 
                                              group = 1)) +
    geom_point(aes(color = month))+ stat_smooth(position = "identity") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    ggtitle("Daily Mean Inflorescence Abundance") +
    scale_fill_brewer(palette = "Greys", labels = c("Pre-event", "Post-event"))
  flower_abun_day
}

######## Average Daily Bumble Bees Line Plot #######

bumble_daily_abun <- bumble_final %>% group_by(date, month) %>% summarise(sum_abun_daily = sum(abun))
{
  daily_bumble_abundance_plot <- ggplot(bumble_daily_abun, aes(x = date,
                                                               y = sum_abun_daily,
                                                               color = month, 
                                                               group = 1)) +
    geom_point(aes(color = month)) + 
    stat_smooth(position = "identity") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    ggtitle("Daily Bumble Bee Abundance") +
    scale_colour_discrete(limits = c("June", "July", "August", "September"),
                          guide = guide_legend(title = NULL)) +
    xlab("Date") +
    ylab("Abundance") +
    theme(axis.line = element_line(colour = "black"))
  
  daily_bumble_abundance_plot
}

{
  bumble_avg_abun_day <- ggplot(day_averages, aes(x = date,
                                              y = avg_bumble_abun,
                                              color = month, 
                                              group = 1)) +
    geom_point(aes(color = month))+ stat_smooth(position = "identity") +
    geom_line()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    ggtitle("Daily Mean Bumble Bee Abundance") +
    scale_colour_discrete(limits = c("June", "July", "August", "September"))
  bumble_avg_abun_day
}

############################################################################################

#{
#flower_abun_month <- ggplot(bumble_final, aes(x = date, y = inflorescene_abun_standardized, color = month)) +
#geom_point(aes(color = month))+ stat_smooth(position = "identity") +
#    geom_line()+
#theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#flower_abun_month
#}

#### June Observations #### 
june_observations <- data.frame(filter(bumble_final, month == "June"))
june_obs1 <- june_observations %>% group_by(bee_species, flower_species) %>% 
  summarise(
    count = sum(abun),)
june_obs2 <- filter(june_obs1, count > 0)
june_species_proportions <- june_obs2 %>% group_by(bee_species) %>% summarise(total = sum(count))
june_obs3 <- left_join(june_obs2, june_species_proportions,  by=c("bee_species"), all= TRUE)
june_obs3$proportion <- round(june_obs3$count/june_obs3$total, digits = 3)


flw_pref_June <- ggplot(june_obs3, aes(x = flower_species, y = proportion, fill = bee_species )) +
  geom_bar(stat = "identity", color = "black") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
flw_pref_June

#### July Observations #### 
july_observations <- data.frame(filter(bumble_final, month == "July"))
july_obs1 <- july_observations %>% group_by(bee_species, flower_species) %>% 
  summarise(
    count = sum(abun),)
july_obs2 <- filter(july_obs1, count > 0)
july_species_proportions <- july_obs2 %>% group_by(bee_species) %>% summarise(total = sum(count))
july_obs3 <- left_join(july_obs2, july_species_proportions,  by=c("bee_species"), all= TRUE)
july_obs3$proportion <- round(july_obs3$count/july_obs3$total, digits = 3)


flw_pref_july <- ggplot(july_obs3, aes(x = flower_species, y = proportion, fill = bee_species )) +
  geom_bar(position = "dodge", stat = "identity", color = "black") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
flw_pref_july

#### August Observations #### 
august_observations <- data.frame(filter(bumble_final, month == "August"))
august_obs1 <- august_observations %>% group_by(bee_species, flower_species) %>% 
  summarise(
    count = sum(abun),)
august_obs2 <- filter(august_obs1, count > 0)
august_species_proportions <- august_obs2 %>% group_by(bee_species) %>% summarise(total = sum(count))
august_obs3 <- left_join(august_obs2, august_species_proportions,  by=c("bee_species"), all= TRUE)
august_obs3$proportion <- round(august_obs3$count/august_obs3$total, digits = 3)


flw_pref_august <- ggplot(august_obs3, aes(x = flower_species, y = proportion, fill = bee_species )) +
  geom_bar(position = "dodge", stat = "identity", color = "black") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
flw_pref_august

#### September Observations #### 
september_observations <- data.frame(filter(bumble_final, month == "September"))
september_obs1 <- september_observations %>% group_by(bee_species, flower_species) %>% 
  summarise(
    count = sum(abun),)
september_obs2 <- filter(september_obs1, count > 0)
september_species_proportions <- september_obs2 %>% group_by(bee_species) %>% summarise(total = sum(count))
september_obs3 <- left_join(september_obs2, september_species_proportions,  by=c("bee_species"), all= TRUE)
september_obs3$proportion <- round(september_obs3$count/september_obs3$total, digits = 3)


flw_pref_september <- ggplot(september_obs3, aes(x = flower_species, y = proportion, fill = bee_species )) +
  geom_bar(position = "dodge", stat = "identity", color = "black") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
flw_pref_september



