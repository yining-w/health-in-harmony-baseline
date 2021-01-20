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
                              -finhh, -fs6ay, -fs6az, -uf26x
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
                            children_U5_c = hh55) %>%
  ##make boolean
  mutate_at(.vars=c(hc13, hc14a, hc14b, hc7,fs1, fs2, fs3, fs4, fs7a, fs7b, fs7x, uf1, uf10, uf12, uf27, uf28, fs11),
            ~case_when(
              . == "Traditional" ~ TRUE,
              . == "Modern" ~ FALSE,
              . == "Yes, connected to the public network" ~ TRUE,
              . == "Yes, outside the network" ~ TRUE,
              . == "Oui" ~ TRUE,
              . == "Non" ~ FALSE,
              . == "No" ~ FALSE,
              . == "Yes, Often" ~ TRUE,
              . == "Yes, Sometimes" ~ TRUE,
              . == "Yes, Rarely" ~ TRUE,
              . == "No, Never" ~ TRUE,
              . == "Oui, Souvent" ~ TRUE,
              . == "Oui, rarement" ~ TRUE,
              . == "Non, Jamais" ~ FALSE,
              . == "Pas Sure" ~ FALSE,
              . == "Ne Sait Pas" ~ FALSE,
              . == "Bonne" ~ TRUE,
              . == "Mauvaise" ~ FALSE,
              . == "Eat" ~ TRUE
              . == "Treatment" ~ FALSE,
              TRUE ~ NA
            )) %>% 
  ##when there is a letter TRUE else NA
  mutate_at(.vars = c("birth_assist_med",
                      "birth_assist_nurse",
                      "birth_assist_assist",
                      "birth_assist_trad",
                      "birth_assist_comm",
                      "birth_assist_close",
                      "birth_assist_other",
                      "birth_assist_noone"),
            #~ifelse(grepl('[A-Z]', .), TRUE, NA)) %>%
            ~ case_when(
              grepl('[A-Z]', .) ~ TRUE,
              !is.na(.) ~ FALSE,
              TRUE ~ NA,
            )) 


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
                   'uf10', 'percent', 'Percentage of households who used anything from the forest to pay for healthcare in the last twelve months', 'Forest Use',
                   'uf12', 'percent', 'Percentage of people aware of rules governing forest entry', 'Forest Use',
                   'uf13', 'percent', 'Percentage of people who think there are benefits to living in the forest', 'Forest Use',
                   'uf14a', 'percent', 'Percentage of people who think of water source as a benefit to living in the forest', 'Forest Use',
                   'uf14b', 'percent', 'Percentage of people who think of animal protein as a benefit to living in the forest', 'Forest Use',
                   'uf14c', 'percent', 'Percentage of people who think of construction materials as a benefit to living in the forest', 'Forest Use',
                   'uf14d', 'percent', 'Percentage of people who think of firewood as a benefit to living in the forest', 'Forest Use',
                   'uf14e', 'percent', 'Percentage of people who think of source of fruit as a benefit to living in the forest', 'Forest Use',
                   'uf14f', 'percent', 'Percentage of people who think of source of seeds as a benefit to living in the forest', 'Forest Use',
                   'uf14g', 'percent', 'Percentage of people who think of medicinal plants as a benefit to living in the forest', 'Forest Use',
                   'uf14h', 'percent', 'Percentage of people who think of clean air as a benefit to living in the forest', 'Forest Use',
                   'uf14i', 'percent', 'Percentage of people who think of rain source as a benefit to living in the forest', 'Forest Use',
                   'uf14j', 'percent', 'Percentage of people who think of shade as a benefit to living in the forest', 'Forest Use',
                   'uf14k', 'percent', 'Percentage of people who think of tourism as a benefit to living in the forest', 'Forest Use',
                   'uf14l', 'percent', 'Percentage of people who think of cultural spaces as a benefit to living in the forest', 'Forest Use',
                   'uf14x', 'percent', 'Percentage of people who think of other benefits to living in the forest', 'Forest Use',
                   'uf15', 'percent', 'Percentage of people who think there are downsides to living in the forest', 'Forest Use',
                   'uf16a', 'percent', 'Percentage of people who think of diseases as a downside to living in the forest', 'Forest Use',
                   'uf16b', 'percent', 'Percentage of people who think of wild animals as a downside to living in the forest', 'Forest Use',
                   'uf16c', 'percent', 'Percentage of people who think of bad spirits as a downside to living in the forest', 'Forest Use',
                   'uf16d', 'percent', 'Percentage of people who think of insects as a downside to living in the forest', 'Forest Use',
                   'uf16e', 'percent', 'Percentage of people who think of restrictions as a downside to living in the forest', 'Forest Use',
                   'uf16f', 'percent', 'Percentage of people who think of fires as a downside to living in the forest', 'Forest Use',
                   'uf16x', 'percent', 'Percentage of people who think of other downsides to living in the forest', 'Forest Use',
                   'uf16a', 'percent', 'Percentage of people who think of diseases as a downside to living in the forest', 'Forest Use',
                   'uf19a', 'percent', 'Percentage of people who think there are tree cutting threats from the forest', 'Forest Use',
                   'uf19a', 'percent', 'Percentage of people who think there are bush fire threats from the forest', 'Forest Use',
                   'uf19c', 'percent', 'Percentage of people who think there are burnt agriculture from the forest', 'Forest Use',
                   'uf19d', 'percent', 'Percentage of people who think there are threats to wet rice from the forest', 'Forest Use',
                   'uf19e', 'percent', 'Percentage of people who think there are threats to charcoal from the forest', 'Forest Use',
                   'uf19f', 'percent', 'Percentage of people who think there are threats to hunting from the forest', 'Forest Use',
                   'uf19x', 'percent', 'Percentage of people who think there are other threats from the forest', 'Forest Use',
                   'uf20', 'percent', 'Percentage of people who think the forest should be protected', 'Forest Use',
                   'uf21a', 'percent', 'Percentage of people who think the forest should be protected for its water source', 'Forest Use',
                   'uf21b', 'percent', 'Percentage of people who think the forest should be protected for its mosquito population', 'Forest Use',
                   'uf21c', 'percent', 'Percentage of people who think the forest should be protected for its tourist destination', 'Forest Use',
                   'uf21d', 'percent', 'Percentage of people who think the forest should be protected for its clean air', 'Forest Use',
                   'uf21e', 'percent', 'Percentage of people who think the forest should be protected for its shade', 'Forest Use',
                   'uf21f', 'percent', 'Percentage of people who think the forest should be protected for its agricultural uses', 'Forest Use',
                   'uf22', 'percent', 'Percentage of people who think it is acceptable to cut trees', 'Forest Use',
                   'uf23a', 'percent', 'Percentage of people who think it is unacceptable to cut trees because it endangers animals', 'Forest Use',
                   'uf23b', 'percent', 'Percentage of people who think it is unacceptable to cut trees because of various reasons', 'Forest Use',
                   'uf23c', 'percent', 'Percentage of people who think it is unacceptable to cut trees because they should not sell them', 'Forest Use',
                   'uf23d', 'percent', 'Percentage of people who think it is unacceptable to cut trees because it is illegal', 'Forest Use',
                   'uf23e', 'percent', 'Percentage of people who think it is unacceptable to cut trees because it is customary', 'Forest Use',
                   'uf23x', 'percent', 'Percentage of people who think it is unacceptable to cut trees because of other reasons', 'Forest Use',
                   'uf24a', 'percent', 'Percentage of people who think it is acceptable to cut trees because it will not endanger the forest', 'Forest Use',
                   'uf24b', 'percent', 'Percentage of people who think it is acceptable to cut trees for various reasons', 'Forest Use',
                   'uf24c', 'percent', 'Percentage of people who think it is acceptable to cut trees for need', 'Forest Use',
                   'uf24d', 'percent', 'Percentage of people who think it is acceptable to cut trees to construct housing', 'Forest Use',
                   'uf24e', 'percent', 'Percentage of people who think it is acceptable to cut trees because it is customary', 'Forest Use',
                   'uf24x', 'percent', 'Percentage of people who think it is acceptable to cut trees for other reasons', 'Forest Use',
                   'uf26a', 'percent', 'Percentage of people who think communities will stop cutting trees and hunting if they could cut costs associated with care', 'Forest Use',
                   'uf26b', 'percent', 'Percentage of people who think communities will continue cutting trees and hunting if they could cut costs associated with care', 'Forest Use',
                   'uf26c', 'percent', 'Percentage of people who think communities are not cutting trees in the forest', 'Forest Use',
                   'uf26d', 'percent', 'Percentage of people who think communities are not hunting in the forest', 'Forest Use',
                   'uf26e', 'percent', 'Percentage of people who think communities need the forest for other survival needs', 'Forest Use',
                   'uf27', 'percent', 'Percentage of people who think the drill is a good thing', 'Forest Use',
                   'uf28', 'percent', 'Percentage of people who want the forest to best there for future generations', 'Forest Use'
                   )

bool_questions = matrix(bool_questions, ncol = 4, byrow = TRUE)

#Function to compute the sum and total for each question in each village
bool_type <- function(df, col){
  #Creating the new dataframe
  df <- df %>% select(village_code, all_of(col)) %>%
    group_by(village_code) %>%
    summarise_all(.funs = list(value = ~ sum(., na.rm = TRUE),
                               total = ~ sum(!is.na(.))))
  
  return(df)
}

#Apply the bool_type function to each column in bool_questions        
bool_df <- apply(bool_questions, 1, function(x){
  bool_type(tableau_de_menage, x[1]) %>%
    mutate(question = x[3],
           topic = x[4])
})

#From list to dataframe
bool_df <- bind_rows(bool_df) %>%
  select(village_code, topic, question, value, total) %>%
  mutate(type = "Percentage")
                   