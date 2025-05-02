######## packages ########
library(ggplot2)
library(RColorBrewer)
library(tibble)
library(dplyr)
library(tidyselect)
library(tidyverse)


###
setwd("C:/Users/chand/Documents/data/Nachusa_2020/bombus_R/data_analysis")
bumble2020 <- read.csv("bumble_2020.csv")
floral2020 <- read.csv("Nachusa_floral_2020.csv", header = TRUE)
summary(bumble2020)


############################################
### Looking at bumble bees on flowers ###
simple_bumble_dataset <- data.frame(species = bumble2020$bee_species, 
                                    sex = bumble2020$sex, 
                                    flower = bumble2020$flower_species) #simplified dataset for bumble bee and flower choices

simple_bumble_dataset <- simple_bumble_dataset %>% filter(species != "None", flower != "None") # removing the two surveys where bumbles were not found, and also where bumble bees were not on flowers

n_distinct(simple_bumble_dataset$flower) # 76 unique flower species visited,  not counting Solidago sp. 
n_distinct(simple_bumble_dataset$species) # 11 unique bumble bee species, with some just to species 


total_bumble_visits <- simple_bumble_dataset %>% count(species, sort = TRUE) #How many bumble bees of each species did we count? 

total_flow_species <- simple_bumble_dataset %>% 
  group_by(species) %>%
  summarize(species_visited = n_distinct(flower)) %>%  
  arrange(desc(species_visited)) # how many species of flower did each bumble bee species visit? 

total_bumble_visits <- left_join(total_bumble_visits, total_flow_species) # combining

total_bumble_visits <- total_bumble_visits %>% mutate(prop = n/7060) %>% mutate(across(c('prop'), round, 2)) #calculating proportion of bumble visits per flower

#unusued code
#total_bumble_visits_sex <- simple_bumble_dataset %>% count(species, sex, sort = TRUE)
#flow_pref_all_bumbles <- simple_bumble_dataset %>% count(species, sex, flower, sort = TRUE)


#### Flower analysis

total_floral <- simple_bumble_dataset %>% count(flower, sort = TRUE) %>% rename(bee.visits = n) #how many bumble bees visited each flower? total_flower is main dataframe  

floral2020$date = as.Date(floral2020$date, format = '%m/%d/%Y') #date for raw data

floral2020$month <- format(as.Date(floral2020$date, format="%d/%m/%Y"),"%m") # add month column to raw data

floral2020$date_site_month = as_factor(paste(floral2020$date,":", floral2020$site,":",floral2020$month)) #combining to get num of unique surveys
n_distinct(floral2020$date_site_month) #229 unique surveys

floral2020$date_site_month_flower = as_factor(paste(floral2020$date,":", floral2020$site,":",floral2020$month, ":", floral2020$flower_species))

flowers_surveys <- data.frame() #data frame to calculate how many times did we detect different flower species during plant surveys?
flowers_surveys <- data.frame(table(unique(floral2020$date_site_month_flower)))
flowers_surveys= separate(data = flowers_surveys, col = "Var1", into = c("date","site","month", "flower"),  sep = ":") #getting column full of flowers to calculate how many times they were found in different surveys

floral2020= floral2020%>% mutate_if(is.factor, funs(factor(trimws(.)))) #factorize

flower_total_survey <- count(flowers_surveys, flower, sort = TRUE) # count how many times a flower was detected at least once on a survey
flower_total_survey$flower <- as.factor(flower_total_survey$flower) #factorize
flower_total_survey= flower_total_survey%>% mutate_if(is.factor, funs(factor(trimws(.)))) #delete white space

total_floral <- left_join(total_floral, flower_total_survey, by= "flower") #join with main dataframe for these data
total_floral <- total_floral %>% rename(flower.survey = n)


simple_bumble_dataset %>% count(species, flower, sort = TRUE) # now I want to know how many species visited each flower


simple_bumble_dataset$flower_bumble = as_factor(paste(simple_bumble_dataset$flower,":", simple_bumble_dataset$species))
flowers_bumble <- data.frame(table(unique(simple_bumble_dataset$flower_bumble)))
flowers_bumble= separate(data = flowers_bumble, col = "Var1", into = c('flower', 'species'),  sep = ":")
flowers_bumble$flower <- as.factor(flowers_bumble$flower)
flowers_bumble$species <- as.factor(flowers_bumble$species)
flowers_bumble= flowers_bumble%>% mutate_if(is.factor, funs(factor(trimws(.)))) 
flowers_bumble <- flowers_bumble %>% filter(species != "auri/pens", species != "Bombus sp.", species != "rufo/vagans") # removing these for this analysis
flowers_bumble <- count(flowers_bumble, flower, sort = TRUE)

total_floral <- left_join(total_floral, flowers_bumble, by= "flower")
total_floral <- total_floral %>% rename(num.bumble.sp = n)



