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
bumble2020 <- read.csv("bumble_2020.csv", header = TRUE)
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
bumble2020 <- add_column(bumble2020, ID = 1:7073, .before = "bee_species")


########## Creating the Bumble Bee Dataset! ###########



bumble_data <- bumble2020 %>% group_by(bee_species, date, .drop = FALSE) %>% 
  summarise(
            total_abun = n_distinct(ID)) #do we need surveyor, start, end, temp, other weather? Does it affect this stuff? I might add it in later to be safe


survey_site_list <- table(list(unique(bumble2020$site)))
date_list <- table(list(unique(bumble2020$date)))
species_list <- data.frame(table(list(unique(bumble2020$bee_species))))


bumble_data03 <- bumble2020 %>% 
  filter(!duplicated(paste0(pmax(date, site), pmin(date, site))))

bumble_data04 <- select(bumble_data03, date, site, start_time, end_time)
bumble_date05 <- bumble_data04 %>% slice(rep(1:n(), each = 15))
bumble_data06 <- mutate(bumble_date05, bee_species = rep(species_list$Var1, times = 74))




site_date_data <- bumble2020 %>% group_by(site, date,bee_species) %>% 
  summarise(
    abun = n_distinct(ID))







bumble_final = left_join(bumble_data06, site_date_data, by=c("date",
                                                   "site",
                                                   "bee_species"),  all=TRUE)

  bumble_final[is.na(bumble_final)] <- 0



  ##### Joining Floral Data #####
  
  colnames(floral2020)[colnames(floral2020) == "visit_number"] <- "visit"
  
  floral2020 <- add_column(floral2020, total_flowers = NA)
  floral2020$total_flowers <- floral2020$avg_flow_inflorescene * floral2020$num_flower
  
  floral2020[,1:3] <- lapply(floral2020[,1:3], factor)
  
  floral2020_1 = floral2020 %>% group_by(date, site) %>%
    summarise(
      flower_richness = n_distinct(flower_species, na.rm = TRUE),
      inflorescene_abun = sum(num_flower, na.rm=TRUE),
      total_flowers = sum(total_flowers)) %>% ungroup()
  
  bumble_final<- left_join(bumble_final, floral2020_1, by = c("date", "site"))
  
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
classes_spread2<- classes_spread %>% select(-area_ha, -area_m, -cover_class_area)
classes_spread2[is.na(classes_spread2)] <- 0

classified_1000m_buffer <- aggregate(x=classes_spread2, 
                            by = list(classes_spread2$site), max, 
                            drop = TRUE)

classified_1000m_buffer$Group.1 <- NULL

colnames(classified_1000m_buffer) <- c("site",
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
classes_spread_500m_2<- classes_spread_500m %>% select(-area_ha, -area_m, -cover_class_area)
classes_spread_500m_2[is.na(classes_spread_500m_2)] <- 0

classified_500m_buffer <- aggregate(x=classes_spread_500m_2, 
                             by = list(classes_spread_500m_2$site), max, 
                             drop = TRUE)

classified_500m_buffer$Group.1 <- NULL

colnames(classified_500m_buffer) <- c("site",
                                       "prairie",
                                       "forest",
                                       "agriculture",
                                       "pasture",
                                       "pond",
                                       "river",
                                       "urban")

##### 250 meter buffers #####

classes_spread_250m <- cover_class_250m_buffer %>% spread(cover_class, class_percentage)
classes_spread_250m_2<- classes_spread_250m %>% select(-area, -class_area)
classes_spread_250m_2[is.na(classes_spread_250m_2)] <- 0

classified_250m_buffer <- aggregate(x=classes_spread_250m_2, 
                                    by = list(classes_spread_250m_2$site), max, 
                                    drop = TRUE)

classified_250m_buffer$Group.1 <- NULL

colnames(classified_250m_buffer) <- c("site",
                                      "prairie",
                                      "forest",
                                      "agriculture",
                                      "pasture",
                                      "pond",
                                      "urban")

##### Joining land cover data to bumble_final #####

### 1000m ###

bumble_final = left_join(bumble_final, classified_1000m_buffer, by=c("site"),  all=TRUE)

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

bumble_final = left_join(bumble_final, classified_500m_buffer, by=c("site"),  all=TRUE)

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

bumble_final = left_join(bumble_final, classified_250m_buffer, by=c("site"),  all=TRUE)

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


bumble_final[,c(2,5)] <- lapply(bumble_final[,c(2,5)] , factor)
  
  
  #responses: Bombus abundance, Bombus richness, all the species individually, PERMANOVA: community composition
  #model structure: response ~ week/cp42_period + temp_standardized + bare + grass + forb + litter + litter_depth + flower_richness_standardized + inflorescence_abun_standardized + (1|site)
  
  #abunmodel1=glmmTMB(data=finaldata, abun~year+plottype+Order+avgtemp90_standardized+grass_standardized+propbare_standardized+flrabun_standardized+Method+(1|Site), family="nbinom1")
  #abunmodel2=glmmTMB(data=finaldata, abun~year+plottype+Order+avgtemp90_standardized+grass_standardized+propbare_standardized+flrabun_standardized+Method+(1|Site), family="nbinom2")
  
  library(glmmTMB)
  library(performance)
  
  library(lme4)
  
  # bumble_model = glmmTMB(data = final_data, abun ~ 
   #                        date + 
  #                       time +
   #                        landscape +
    #                       ground_cover +
     #                      nestingstuff +
    #                       flower_richness_standardized + 
     #                      inflorescene_abun_standardized +
      #                     (1|site) + (1|bee_species), family = ??????)




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
                           litter +
                           litter_depth_standardized +
                           flower_richness_standardized + 
                           inflorescene_abun_standardized +
                           (1|site) + (1|bee_species) + (1|date), family="nbinom2")
summary(bumble_model_1000m)

  
########### Model 500m landscape #################################################################################
  
  bumble_model_500m = glmmTMB(data = bumble_final, abun ~ 
                                 prairie_500m +
                                 forest_500m +
                                 grass +
                                 litter +
                                 litter_depth_standardized +
                                 flower_richness_standardized + 
                                 inflorescene_abun_standardized +
                                 (1|site) + (1|bee_species) + (1|date), family ="nbinom2")
  summary(bumble_model_500m)
  

########### Model 250m landscape ###########################################################################
  
  bumble_model_250m = glmmTMB(data = bumble_final, abun ~
                                 prairie_250m +
                                 forest_250m +
                                 grass +
                                 litter +
                                 litter_depth_standardized +
                                 flower_richness_standardized + 
                                 inflorescene_abun_standardized +
                                 (1|site) + (1|bee_species) + (1|date), family ="nbinom2")
  
  summary(bumble_model_250m)
  
  
  ############### Individual Species Models #############################################################
  
  bumble_final %>% filter(., bee_species == "impatiens")
  
  imp_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "impatiens"), abun ~
                                prairie_1000m +
                                forest_1000m +
                                litter +
                                flower_richness_standardized + 
                                inflorescene_abun_standardized +
                                (1|site) + (1|date), family ="nbinom2")
  
  summary(imp_model)
  
  
  gris_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "griseocollis"), abun ~
                        prairie_1000m +
                        forest_1000m +
                        litter +
                        flower_richness_standardized + 
                        inflorescene_abun_standardized +
                        (1|site) + (1|date), family ="nbinom2")
  
  summary(gris_model)
  
  
  auri_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "auricomus"), abun ~
                         prairie_1000m +
                         forest_1000m +
                         flower_richness_standardized + 
                         inflorescene_abun_standardized +
                         (1|site) + (1|date), family ="nbinom2")
  
  summary(auri_model)
  
  rufo_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "rufocinctus"), abun ~
                         prairie_1000m +
                         forest_1000m +
                         flower_richness_standardized + 
                         inflorescene_abun_standardized +
                         (1|site) + (1|date), family ="nbinom2")
  
  summary(rufo_model)
  
  
  ferv_model = glmmTMB(data = bumble_final %>% filter(., bee_species == "fervidus"), abun ~
                         prairie_1000m +
                         forest_1000m +
                         flower_richness_standardized + 
                         inflorescene_abun_standardized +
                         (1|site) + (1|date), family ="nbinom2")
  
  summary(ferv_model)
  
  
  
###############################################################################################
  #
  #
  #
  #
  #
  #
  #
######################## PAST THIS POINT is random stuff but there could be useful code in here
  ###################### so I'm not deleting it ###############################################
##### Creating an Empty Dataframe #####


#put in new dataset with site/date

bumble_data$date_site_visit_etc = paste(bumble_data$date,"x", bumble_data$site)
bumble_data$date_site_visit_etc = as.factor(bumble_data$date_site_visit_etc) #were there any dates with no observations? If so, this approach may not work?

Species=c("bimaculatus", "grisecollis", "rufocinctus",
           "affinis", "pensylvanicus", "auricomus", 
           "impatiens", "fervidus", "vagans", "auri/pens", "citrinus", "fraternus")

empty = expand.grid("date_site_visit_etc" = levels(bumble_data$date_site_visit_etc))
empty1= separate(data = empty, col = "date_site_visit_etc", into = c("date",
                                                                     "site"), sep = "x") #separate year and season, so it can be merged

empty1[,1:2] <- lapply(empty1[,1:2] , factor) #make factor

##### Cleaning Data of White Space #####

empty1= empty1%>% mutate_if(is.factor, funs(factor(trimws(.)))) 
bumble_data= bumble_data%>% mutate_if(is.factor, funs(factor(trimws(.)))) 
ground_cover2020= ground_cover2020%>% mutate_if(is.factor, funs(factor(trimws(.)))) 
floral2020= floral2020%>% mutate_if(is.factor, funs(factor(trimws(.)))) 

bumble_data$date_site_visit_etc <- NULL

bumble_data2 = left_join(empty1, bumble_data, by=c("date",
                                                   "site"), all=TRUE)
bumble_data2$visit_number = NULL

blah <- match(bumble_data2, Species, nomatch = 0)

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
            litter_depth = mean(litter_depth)) %>% ungroup()

bumble_data3 = left_join(bumble_data2, ground_cover_site_summary, by = c("site"))
bumble_data3$Date <- NULL
bumble_data3$Quadrat <- NULL

##### Joining Floral Abundance #####

colnames(floral2020)[colnames(floral2020) == "visit_number"] <- "visit"

floral2020 <- add_column(floral2020, total_flowers = NA)
floral2020$total_flowers <- floral2020$avg_flow_inflorescene * floral2020$num_flower

floral2020[,1:3] <- lapply(floral2020[,1:3], factor)

floral2020_1 = floral2020 %>% group_by(date, site) %>%
  summarise(
    flower_richness = n_distinct(flower_species, na.rm = TRUE),
    inflorescene_abun = sum(num_flower, na.rm=TRUE),
    total_flowers = sum(total_flowers)) %>% ungroup()

final_data = left_join(bumble_data_final, floral2020_1, by = c("date", "site"))
final_data <- final_data %>% group_by(date, site)

final_data$temp <- as.numeric(final_data$temp)



##### Classifying CP-42 Date Ranges #####

library(lubridate)
final_data$date<- as.Date(final_data$date,"%m/%d/%Y")

final_data <- final_data %>%
  add_column(week = (isoweek(final_data$date)),
             .after = "date") 
as.vector(final_data$week)
final_data <- final_data %>%
  add_column(cp42_period = NA, .after = "week")

final_data$week <- as.integer(final_data$week)

final_data <- final_data %>%
  mutate(cp42_period = case_when(
    week %in% 25:27 ~ "Mid_1",
    week %in% 28:29 ~ "Mid_2",
    week %in% 30:33 ~ "Late_1",
    week %in% 34:37 ~ "Late_2",
    week %in% 38:40 ~ "Late_3"
  ))



summary(final_data$week)




##### Standardizing Counts #####


final_data$flower_richness_standardized <- (final_data$flower_richness - mean(final_data$flower_richness, na.rm = TRUE))/sd(final_data$flower_richness, na.rm = TRUE)

final_data$inflorescene_abun_standardized <- (final_data$inflorescene_abun-mean(final_data$inflorescene_abun, na.rm = TRUE))/sd(final_data$inflorescene_abun, na.rm = TRUE)

final_data$total_flowers_standardized <- (final_data$total_flowers-mean(final_data$total_flowers, na.rm = TRUE))/sd(final_data$total_flowers, na.rm = TRUE)

final_data$temp_standardized <- (final_data$temp-mean(final_data$temp, na.rm = TRUE))/sd(final_data$temp, na.rm = TRUE)

final_data$litter_depth <- scale(final_data$litter_depth)


######### Separating Data Frame / Creating tidy data frames #############

##### Bombus and floral associations #####

bombus_pref <- data.frame()


##### Models #####


#responses: Bombus abundance, Bombus richness, all the species individually, PERMANOVA: community composition
#model structure: response ~ week/cp42_period + temp_standardized + bare + grass + forb + litter + litter_depth + flower_richness_standardized + inflorescence_abun_standardized + (1|site)

#abunmodel1=glmmTMB(data=finaldata, abun~year+plottype+Order+avgtemp90_standardized+grass_standardized+propbare_standardized+flrabun_standardized+Method+(1|Site), family="nbinom1")
#abunmodel2=glmmTMB(data=finaldata, abun~year+plottype+Order+avgtemp90_standardized+grass_standardized+propbare_standardized+flrabun_standardized+Method+(1|Site), family="nbinom2")

library(glmmTMB)
library(performance)

library(lme4)

bumble_model = glmmTMB(data = final_data, abun ~ 
                      date + 
                      time +
                      landscape +
                      ground_cover +
                      nestingstuff +
                      flower_richness_standardized + 
                      inflorescene_abun_standardized +
                      (1|site) + (1|bee_species), family = ??????)

imp_model
auricomus_model







summary(pens_model)  #should we put species in here? Is the distribution correct? we should look at correlations, 
#and also try some sort of model trimming procedure, this is a lot of factors
#also, interactions??

fervidus_model = glm(data = final_data, fervidus ~ cp42_period + 
                   temp_standardized +
                   bare + 
                   grass + 
                   forb + 
                   litter + 
                   litter_depth + 
                   flower_richness_standardized + 
                   inflorescene_abun_standardized, family = poisson)
summary(fervidus_model)

pens_model2 = lm(data = final_data, pensylvanicus ~ litter_depth)
summary(pens_model2)
plot(pens_model2)

abunmodel3=glmmTMB(data=finaldata, abun ~ year*plottype*Order+grass_standardized+propbare_standardized+flrabun_standardized+(1|Site),family="nbinom2")


