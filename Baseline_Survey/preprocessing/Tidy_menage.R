library(tidyverse)
library(dplyr)
library(data.table)

rm(list=ls())
load("~/GitHub/Baseline_Survey/data/menage.RData")

# Remote White Space ########################
dfList <- list(menage)

dfList <- lapply(dfList, function(x) {
  cols = names(x)[vapply(x, is.character, logical(1))]
  x[,cols] <- lapply(x[,cols], trimws)
  x
} )

menage = dfList[[1]]
# Administrative data #############################################################

#Changing the name of administrative columns
##Remove unecessary administrative columns
menage <- menage %>% select(c(-hh3, -hh2, -hh6, -hh6a,
                              -hh7, -hh7a, -hh8, -hh8a,
                              -hh9, -hh9a, -hh4,
                              -hh5jj, -hh5mm, -hh5mm,
                              -hh5aa, -hh11hh, -hh11mn,
                              -hh46_autre, -hhaux,
                              -hh47, -hh56hh, -hh56mn,
                              -finhh, -fs6ay, -fs6az
                              ))

## Remove "Specify other" / aggregate of 'check all' questions
menage <- menage %>% select(-contains(c("autre", "nr")),-ws6, -st4, -uf3, -fs6a, -fs5b, -tn18b)
  
##Rename Administrative info
detach("package:plyr", unload=TRUE)

menage <- menage %>% rename(village_code = hh1,
                            village_name = hh1a,
                            reserve_section = hhstrate,
                            permission = hh12,
                            completeness= hh46,
                            household_members= hh48,
                            women15T49 = hh49,
                            children_U5 = hh51,
                            women15T49_c = hh53,
                            children_U5_c = hh55)


## Change Completeness
menage <- menage %>% mutate(completeness = ifelse(completeness == "Completé", TRUE, FALSE))
menage <- menage %>% mutate(permission = ifelse(permission == "OUI", TRUE, FALSE))

#Matrix with the questions/KPIs. The second column indicates the divisor
bool_questions = c("hc6a" , "percent", "Percentage of households who have a cellphone", "Household characteristics",
                   "hc6b", "percent", "Percentage of households who have a show", "Household characteristics",
                   "hc6c", "percent", "Percentage of households who have a bicycle", "Household characteristics",
                   "hc6d", "percent", "Percentage of households who have a motorcycle/scooter", "Household characteristics",
                   "hc6e", "percent", "Percentage of households who have a boat/canoe", "Household characteristics",
                   "hc6f", "percent", "Percentage of households who have a net", "Household characteristics",
                   "hc6g", "percent", "Percentage of households who have a cart with an animal", "Household characteristics",
                   "hc6h", "percent", "Percentage of households who have a car/truck/van", "Household characteristics",
                   "hc6i", "percent", "Percentage of households who have a motor boat", "Household characteristics",
                   "hc6j", "percent", "Percentage of households who have a plow", "Household characteristics",
                   "hc6k", "percent", "Percentage of households who have a sewing machine", "Household characteristics",
                   "hc6l", "percent", "Percentage of households who have a tractor/tiller", "Household characteristics",
                   "hc7", 'percent', 'Percentage of households who has electricity', 'Household characteristics',
                   'hc8', 'percent', 'Percentage of households who own land', 'Household characteristics',
                   'hc12a', 'percent', 'Percentage of households who grow irrigated rice', 'Household characteristics',
                   'hc12b', 'percent', 'Percentage of households who grow rainfed rice', 'Household characteristics',
                   'hc12c', 'percent', 'Percentage of households who grow cassava rice', 'Household characteristics',
                   'hc12d', 'percent', 'Percentage of households who grow sweet potato rice', 'Household characteristics',
                   'hc12e', 'percent', 'Percentage of households who grow cash crop', 'Household characteristics',
                   'hc12x', 'percent', 'Percentage of households who grow other crops', 'Household characteristics',
                   'hc13', 'percent', 'Percentage of households who grow modern rice', 'Household characteristics',
                   'hc14a', 'percent', 'Percentage of households who grow modern irrigated rice', "Household characteristics",
                   'hc14b', 'percent', 'Percentage of households who grow modern rainbed rice', 'Household characteristics',
                   'hc21', 'percent', 'Percentage of households who own shoes/sandals', "Household characteristics",
                   'hc22a', 'percent', 'Percentage of households who have a landline telephone', "Household characteristics",
                   "hc22b", 'percent', "Percentage of households who have a radio", 'Household characteristics',
                   'hc22c', 'percent', "Percentage of households who have a table", 'Household characteristics',
                   'hc22d', 'percent', 'Percentage of households who have a chair', 'Household characteristics',
                   'hc22e', 'percent', 'Percentage of households who have reads', 'Household characteristics',
                   'hc22f', 'percent', 'Percentage of households who have an armchair/sofa', 'Household characteristics',
                   'hc22g', 'percent', 'Percentage of households who have a television', 'Household characteristics',
                   'hc22h', 'percent', 'Percentage of households who have a CD/DVD/DIVX player', 'Household characteristics',
                   'hc24', 'percent', 'Percentage of households who have a computer tablet', 'Household characteristics',
                   'hc26', 'percent', 'Percentage of households who have internet access', 'Household characteristics',
                   'hc27', 'percent', 'Percentage of households who have a bank account', 'Household characteristics',
                   'ws6a', 'percent', 'Percentage of households who boil water to make it healthier to drink', 'Water and Sanitation',
                   'ws6b', 'percent', 'Percentage of households who add bleach or chlorine to water to make it healthier to drink', 'Water and Sanitation',
                   'ws6c', 'percent', 'Percentage of households who filter water through a cloth to make it healthier to drink', 'Water and Sanitation',
                   'ws6d', 'percent', 'Percentage of households who use a water filter to make it healthier to drink', 'Water and Sanitation',
                   'ws6e', 'percent', 'Percentage of households who use solar to make it healthier to drink', 'Water and Sanitation',
                   'ws6f', 'percent', 'Percentage of households who disinfect water to make it healthier to drink', 'Water and Sanitation',
                   'ws6x', 'percent', 'Percentage of households who use other methods to make water healthier to drink', 'Water and Sanitation',
                   'tn1', 'percent', 'Percentage of households who have mosquito nets', 'Possession and Use of Mosquito Nets',
                   'tn17a', 'percent', 'Percentage of households who use mosquito nets to scare away mosquitos when they sleep', 'Possession and Use of Mosquito Nets',
                   'tn17b', 'percent', 'Percentage of households who use mosquito nets  to fish', 'Possession and Use of Mosquito Nets',
                   'tn17c', 'percent', 'Percentage of households who use mosquito nets to prevent poultry from going into the garden', 'Possession and Use of Mosquito Nets',
                   'tn17a', 'percent', 'Percentage of households who use mosquito nets to for other reasons', 'Possession and Use of Mosquito Nets',
                   'tn18ba', 'percent', 'Percentage of households who avoid mosquitos using spray', 'Possession and Use of Mosquito Nets',
                   'tn18bb', 'percent', 'Percentage of households who avoid mosquitos by removing puddles', 'Possession and Use of Mosquito Nets',
                   'tn18bc', 'percent', 'Percentage of households who avoid mosquitos by removing bushes of mosquitos', 'Possession and Use of Mosquito Nets',
                   'tn18bd', 'percent', 'Percentage of households who avoid mosquitos using repellants', 'Possession and Use of Mosquito Nets',
                   'tn18be', 'percent', 'Percentage of households who avoid mosquitos using other methods', 'Possession and Use of Mosquito Nets',
                   'st1', 'percent', 'Percentage of households who had diarrhea in the last three months', 'Health Care and Treatment',
                   'st2', 'percent', 'Percentage of households who had a fever in the last three months', 'Health Care and Treatment',
                   'st3', 'percent', 'Percentage of households who had a cough in the last three months', 'Health Care and Treatment',
                   'st4a', 'percent', 'Percentage of households who go to a government hospital when they become ill', 'Health Care and Treatment',
                   'st4b', 'percent', 'Percentage of households who go to a government health center when they become ill', 'Health Care and Treatment',
                   'st4c', 'percent', 'Percentage of households who go to a government post health facility when they become ill', 'Health Care and Treatment',
                   'st4d', 'percent', 'Percentage of households who go to a community center when they become ill', 'Health Care and Treatment',
                   'st4e', 'percent', 'Percentage of households who go to a public mobile clinic when they become ill', 'Health Care and Treatment',
                   'st4f', 'percent', 'Percentage of households who go to other public facility when they become ill', 'Health Care and Treatment',
                   'st4g', 'percent', 'Percentage of households who go to a private hospital when they become ill', 'Health Care and Treatment',
                   'st4h', 'percent', 'Percentage of households who go to a private doctor when they become ill', 'Health Care and Treatment',
                   'st4i', 'percent', 'Percentage of households who go to a private pharmacy when they become ill', 'Health Care and Treatment',
                   'st4j', 'percent', 'Percentage of households who go to a community health agent when they become ill', 'Health Care and Treatment',
                   'st4k', 'percent', 'Percentage of households who go to a private mobile clinic when they become ill', 'Health Care and Treatment',
                   'st4l', 'percent', 'Percentage of households who go to other private facility when they become ill', 'Health Care and Treatment',
                   'st4m', 'percent', 'Percentage of households who go to family/friends when they become ill', 'Health Care and Treatment',
                   'st4n', 'percent', 'Percentage of households who go to a shop/market/street when they become ill', 'Health Care and Treatment',
                   'st4o', 'percent', 'Percentage of households who go to a traditional practitioner when they become ill', 'Health Care and Treatment',
                   'st4x', 'percent', 'Percentage of households who seek other help when they become ill', 'Health Care and Treatment',
                   'st7', 'percent', 'Percentage of households who could not follow treatment due to high costs', 'Health Care and Treatment',
                   'st8a', 'percent', 'Percentage of households who could not follow treatment due to transportation costs', 'Health Care and Treatment',
                   'st8b', 'percent', 'Percentage of households who could not follow treatment due to medicine costs', 'Health Care and Treatment',
                   'st8c', 'percent', 'Percentage of households who could not follow treatment due to health services themselves', 'Health Care and Treatment',
                   'st8x', 'percent', 'Percentage of households who could not follow treatment due to other costs', 'Health Care and Treatment',
                   'fs1', 'percent', 'Percentage of households worried their food supply might run out', 'Food supply',
                   'fs2', 'percent', 'Percentage of households lived without food in the household', 'Food supply',
                   'fs3', 'percent', 'Percentage of households gone to bed hungry at night', 'Food supply',
                   'fs4', 'percent', 'Percentage of households gone a full day and night without eating', 'Food supply',
                   'fs5a', 'percent', 'Percentage of households who ate food without rice', 'Food supply',
                   'fs6aa', 'percent', 'Percentage of households who could not eat rice with all meals in January in the last twelve months', 'Food supply',
                   'fs6ab', 'percent', 'Percentage of households who could not eat rice with all meals in February in the last twelve months', 'Food supply',
                   'fs6ac', 'percent', 'Percentage of households who could not eat rice with all meals in March in the last twelve months', 'Food supply',
                   'fs6ad', 'percent', 'Percentage of households who could not eat rice with all meals in April in the last twelve months', 'Food supply',
                   'fs6ae', 'percent', 'Percentage of households who could not eat rice with all meals in May in the last twelve months', 'Food supply',
                   'fs6af', 'percent', 'Percentage of households who could not eat rice with all meals in June in the last twelve months', 'Food supply',
                   'fs6ag', 'percent', 'Percentage of households who could not eat rice with all meals in July in the last twelve months', 'Food supply',
                   'fs6ah', 'percent', 'Percentage of households who could not eat rice with all meals in August in the last twelve months', 'Food supply',
                   'fs6ai', 'percent', 'Percentage of households who could not eat rice with all meals in September in the last twelve months', 'Food supply',
                   'fs6aj', 'percent', 'Percentage of households who could not eat rice with all meals in October in the last twelve months', 'Food supply',
                   'fs6ak', 'percent', 'Percentage of households who could not eat rice with all meals in November in the last twelve months', 'Food supply',
                   'fs6al', 'percent', 'Percentage of households who could not eat rice with all meals in December in the last twelve months', 'Food supply',
                   'fs7a', 'percent', 'Percentage of households who ate wild potatos when they had nothing else to eat', 'Food Supply',
                   'fs7b', 'percent', 'Percentage of households who ate via when they had nothing else to eat', 'Food Supply',
                   'fs7x', 'percent', 'Percentage of households who ate other food when they had nothing else to eat', 'Food Supply',
                   'fs8', 'percent', 'Percentage of households who sold assets to buy food', 'Food Supply',
                   'fs10', 'percent', 'Percentage of households who had to choose between eating or obtaining medical care in the last twelve months', 'Food Supply',
                   'fs11', 'percent', 'Percentage of households who chose to eat rather than seek care', 'Food Supply',
                   'uf1', 'percent', 'Percentage of households who entered the forest in the last twelve months', 'Forest Use',
                   'uf2a', 'percent', 'Percentage of households who entered the Coastal plot II of the reserve', "Forest Use",
                   'uf2b', 'percent', 'Percentage of households who entered plot I of the reserve', "Forest Use",
                   'uf2c', 'percent', 'Percentage of households who entered the Classified forest', "Forest Use",
                   'uf3a', 'percent', 'Percentage of households who entered the forest for rice in wetland areas', "Forest Use",
                   'uf3b', 'percent', 'Percentage of households who entered the forest for gather mahampy weaving', "Forest Use",
                   'uf3c', 'percent', 'Percentage of households who entered the forest to go from Point A to B', "Forest Use",
                   'uf3d', 'percent', 'Percentage of households who entered the forest for hunting or trapping', "Forest Use",
                   'uf3e', 'percent', 'Percentage of households who entered the forest for cutting trees', "Forest Use",
                   'uf3f', 'percent', 'Percentage of households who entered the forest to collect cord wood from dead trees', "Forest Use",
                   'uf3g', 'percent', 'Percentage of households who entered the forest for fruit gathering', "Forest Use",
                   'uf3h', 'percent', 'Percentage of households who entered the forest for catching birds', "Forest Use",
                   'uf3i', 'percent', 'Percentage of households who entered the forest to harvest donkeys', "Forest Use",
                   'uf3j', 'percent', 'Percentage of households who entered the forest to dig wild potatos', "Forest Use",
                   'uf3x', 'percent', 'Percentage of households who entered the forest for other reasons', "Forest Use",
                   'uf3a', 'percent', 'Percentage of households who entered the forest for rice in wetland areas', "Forest Use",
                   'uf9a', 'percent', 'Percentage of households who did not enter the forest because they did not need to', "Forest Use",
                   'uf9b', 'percent', 'Percentage of households who did not enter the forest because they were not authorized entry', "Forest Use",
                   'uf9c', 'percent', 'Percentage of households who did not enter the forest because they were afraid', "Forest Use",
                   'uf9d', 'percent', 'Percentage of households who did not enter the forest because they did not have the opportunity to', "Forest Use",
                   'uf9e', 'percent', 'Percentage of households who did not enter the forest because it was too far', "Forest Use",
                   'uf9x', 'percent', 'Percentage of households who did not enter the forest because of other reasons', "Forest Use",
                   )