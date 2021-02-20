library(tidyverse)
library(dplyr)
library(data.table)

rm(list=ls())
load(here("Baseline_Survey/data/MENAGE.RData"))

# Remote White Space ########################
dfList <- list(menage)

dfList <- lapply(dfList, function(x) {
  cols = names(x)[vapply(x, is.character, logical(1))]
  x[,cols] <- lapply(x[,cols], trimws)
  x
} )

menage = dfList[[1]]
menage[] <- lapply(menage, type.convert, as.is = TRUE)

##make all obs upper case
menage = data.frame(lapply(menage, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

##remove special characters
menage <- data.frame(lapply(menage, function(x) {
                  gsub("/", "", x)
              }))

menage <- data.frame(lapply(menage, function(x) {
  gsub(",", "", x)
}))

menage = data.frame(lapply(menage, function(x) {
  gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x)
}))

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
                              -finhh, -fs6ay, -fs6az, -uf26x, -uf26
                              ))

## Remove "Specify other" / aggregate of 'check all' questions
menage <- menage %>% select(-contains(c("autre", "nr")),-ws6, -st4, -uf3, -fs6a, -fs5b, -tn18b)

##Rename Administrative info
#detach("package:plyr", unload=TRUE)
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
  mutate(hc10 = ifelse(hc10 > 95, NA, hc10),
         hc11 = ifelse(hc11 > 998, NA, hc11)) 


  ##make boolean
menage <- menage %>% 
  mutate_at(.vars=c("permission", "hc6a",
            "hc6b", "hc6c", "hc6d", "hc6e",
            "hc6f", "hc6g", "hc6h", "hc6i",
            "hc6j", "hc6k", "hc6l", "hc8",
            "hc12a", "hc12b", "hc12c", "hc12d",
            "hc12e", "hc12x", "hc21", "hc22a",
            "hc22b", "hc22c", "hc22d", "hc22e", "hc22f",
            "hc22g", "hc22h", "hc24", "hc26", "hc27",
            "ws5", "uf13",
            "tn1", "tn17a", "tn17b",
            "tn17c", "tn17x", "tn18a", "st1", "st2",
            "st3", "st5", "st6a", "st6b", "st6c", 
            "st7", "st8a", "st8b", "st8c", "st8x",
            "fs5a", "fs8", "fs10", "uf2a", "uf2b",
            "uf2c", "uf6a", "uf6b", "uf6c", "uf6d",
            "uf6e", "uf6f", "uf6g", "uf6h", "uf6i",
            "uf6j", "uf6x", "uf9a", "uf9b", "uf9c",
            "uf9d", "uf9e", "uf9x",
            "uf14a", "uf14b", "uf14c",
            "uf14d", "uf14e", "uf14f", "uf14g", "uf14h",
            "uf14i", "uf14j", "uf14k", "uf14l", "uf14x",
            "uf15", "uf16a", "uf16b", "uf16c", "uf16d",
            "uf16e", "uf16f", "uf16x", "uf17", "uf18",
            "uf19a", "uf19b", "uf19c", "uf19d", "uf19e",
            "uf19f", "uf19x", "uf20", "uf21a", "uf21b",
            "uf21c", "uf21d", "uf21e", "uf21f", "uf22",
            "uf23a", "uf23b", "uf23c", "uf23d", "uf23e",
            "uf23x", "uf24a", "uf24b", "uf24c", "uf24d",
            "uf24e", "uf24x", "uf25",
            "hc13", "hc14a", "hc14b", 
            "hc7","fs1", "fs2", "fs3", 
            "fs4", "fs7a", "fs7b", "fs7x", 
            "uf1", "uf10", "uf12", 
            "uf27", "uf28", "fs11", "hc16", "st6a", "st6b", "st6c",
            "uf17"),
            ~case_when(
              . == "OUI TRES MENACE" ~ TRUE,
              . == "OUI PEU MENACE" ~ TRUE,
              . == "SOMETIMES" ~ TRUE,
              . == "OUI PARFOIS" ~ TRUE,
              . == "OUI SOUVENTTRES" ~ TRUE,
              . == "OUI QUELQUEFOISUN PEU" ~ TRUE,
              . == "NONJAMAIS" ~ FALSE,
              . == "MODERNE" ~ FALSE,
              . == "OUI EN DEHORS DU RESEAU GÉNERATEURPANNEAU SOLAIRESYSTÈME ISOLÉ" ~ TRUE,
              . == "OUI CONNECTÉ AU RESEAU PUBLIC" ~ TRUE,
              . == "OUI QUELQUEFOISUN PEU" ~ TRUE,	
              . == "OUI SOUVENT  TRES" ~ TRUE,
              . == "OUI" ~ TRUE,
              . == "NON" ~ FALSE,
              . == "OUI SOUVENT TRES" ~ TRUE,
              . == "OUI SOUVENT" ~ TRUE,
              . == "OUI RAREMENT" ~ TRUE,
              . == "OUI QUELQUEFOIS" ~ TRUE,
              . == "NON JAMAIS" ~ FALSE,
              . == "PAS SURE" ~ NA,
              . == "NE SAIT PAS" ~ NA,
              . == "BONNE" ~ TRUE,
              . == "MAUVAISE" ~ FALSE,
              . == "EAT" ~ TRUE,
              . == "TREATMENT" ~ FALSE,
              . == "NON REPONSE" ~ NA,
              TRUE ~ NA
            ))  
  ##when there is a letter TRUE else NA

menage <- menage %>% 
  mutate_at(.vars = c("ws6a",
                      "ws6b",
                      "ws6c",
                      "ws6d",
                      "ws6e",
                      "ws6f",
                      "ws6x",
                      "tna8ba",
                      "tn18bb",
                      "tn18bc",
                      "tn18bd",
                      "tn18bx",
                      "st4a",
                      "st4b",
                      "st4c",
                      "st4d",
                      "st4e",
                      "st4f",
                      "st4g",
                      'st4h',
                      "st4i",
                      "st4j",
                      "st4k",
                      "st4l",
                      "st4m",
                      "st4n",
                      "st4o",
                      "st4x",
                      "fs6aa",
                      "fs6ab",
                      "fs6ac",
                      'fs6ad',
                      "fs6ae",
                      "fs6af",
                      "fs6ag",
                      'fs6ah',
                      'fs6ai',
                      'fs6aj',
                      'fs6ak',
                      'fs6al',
                      "uf3a",
                      "uf3b",
                      "uf3c",
                      "uf3d",
                      "uf3e",
                      "uf3f",
                      "uf3g",
                      "uf3h",
                      "uf3i",
                      "uf3j",
                      "uf3x",
                      "uf26a",
                      "uf26b",
                      "uf26c",
                      "uf26d",
                      "uf26e"),
            ~ case_when(
              grepl('[A-Z]', .) ~ TRUE,
              !is.na(.) ~ FALSE,
              TRUE ~ NA,
            )) 


## Change Completeness
menage <- menage %>% mutate(completeness = ifelse(completeness == "Completé", "Complete", "Incomplete"))

#####################################################################
# Boolean ###########################################################
#####################################################################

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
                   'ws5', 'percent', "Percentage of households who make their water safer to drink", 'Water and Sanitation',
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
                   'tna8ba', 'percent', 'Percentage of households who avoid mosquitos using spray', 'Possession and Use of Mosquito Nets',
                   'tn18bb', 'percent', 'Percentage of households who avoid mosquitos by removing puddles', 'Possession and Use of Mosquito Nets',
                   'tn18bc', 'percent', 'Percentage of households who avoid mosquitos by removing bushes of mosquitos', 'Possession and Use of Mosquito Nets',
                   'tn18bd', 'percent', 'Percentage of households who avoid mosquitos using repellants', 'Possession and Use of Mosquito Nets',
              #     'tn18be', 'percent', 'Percentage of households who avoid mosquitos using other methods', 'Possession and Use of Mosquito Nets',
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
                   "st6a", "percent", "Percentage of households whose transport cost was difficult to cover for treatment", "Health Care and Treatment",
                   "st6b", "percent", "Percentage of households whose medicine cost was difficult to cover for treatment", "Health Care and Treatment",
                   "st6c", "percent", "Percentage of households whose health cost was difficult to cover for treatment", "Health Care and Treatment",
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

#menage$village_code <- as.factor(menage$village_code) # Convert character column to factor

#Apply the bool_type function to each column in bool_questions        
bool_df <- apply(bool_questions, 1, function(x){
  bool_type(menage, x[1]) %>%
    mutate(question = x[3],
           topic = x[4])
})

#From list to dataframe
bool_df <- bind_rows(bool_df) %>%
  select(village_code, topic, question, value, total) %>%
  mutate(type = "Percentage")

############################################################################
# Average ##################################################################
############################################################################
#Columns need to be numerical
#NA means no response (be careful because some NA need to be change to 0)

avg_questions = c("household_members", "Average number of household members", "Household characteristics",
                  "hc10", "Average hectares of land", "Household characteristics",
                  "hc11", "Average hectare used for agriculture", "Household characteristics",
                  "hc15a", "Average months to grow irrigated rice", "Household characteristics",
                  "hc15b", "Average months to grow rainfed rice", "Household characteristics",
                  "hc20a", "Average number of dairy cows or bulls", "Household characteristics",
                  "hc20b", "Average number of Zebus", "Household characteristics",
                  "hc20c", "Average number of Goats", "Household characteristics",
                  "hc20d", "Average number of Sheep", "Household characteristics",
                  "hc20e", "Average number of chicken", "Household characteristics",
                  "hc20f", "Average number of pigs", "Household characteristics",
                  "hc20g", "Average number of other poultry", "Household characteristics",
                  "hc20x", "Average number of other animals", "Household characteristics",
                  "hc23", "Average number of rooms used for sleeping", "Household characteristics",
                  "hc29", "Average total income in the last twelve months", "Household characteristics",
                  "ws4", "Average time to go to water source and return", "Water and Sanitation",
                  "tn2", "Average number of mosquito nets", "Possession and Use of Mosquito Nets",
                  "fs6b", "Average number of months without rice in meals", "Food supply"
                  )

menage_avg_cols = c("hc10", "hc11", "hc15a", "hc15b", "hc20a", "hc20b", "hc20c",
                    "hc20d", "hc20e", "hc20f", "hc20g", "hc20x", "hc23", "hc29",
                    "ws4", "tn2", "fs6b", "household_members")

menage[menage_avg_cols] <- sapply(menage[menage_avg_cols], as.numeric)

avg_questions = matrix(avg_questions, ncol = 3, byrow = TRUE)

avg_questions = as.data.frame(avg_questions)
names(avg_questions) <- c("variable", "question", "topic")

#Calculating the total column
avg_df_total <- menage %>% 
  select(village_code, avg_questions[,1]) %>%
  group_by(village_code) %>%
  summarise_all(~sum(!is.na(.))) %>%
  gather(key = "variable", value = "total", -village_code)

#Calculating the average column==
avg_df_avg <- menage %>% 
  select(village_code, avg_questions[,1]) %>%
  group_by(village_code) %>%
  summarise_all(~mean(., na.rm = TRUE)) %>%
  gather(key = "variable", value = "value", -village_code)


#Join the two dataframes
avg_df <- avg_df_avg %>% left_join(avg_df_total, by = c("village_code", "variable")) %>%
  mutate(value = ifelse(!is.nan(value), value, NA)) %>%
  left_join(avg_questions, by = "variable") %>%
  select(-variable) %>%
  select(village_code, topic, question, value, total) %>%
  mutate(type = "Average")

###############################################################################
# Categorical one column ######################################################
###############################################################################
#Columns needs to factor, with all the levels
#NA means no response
#Matirx with the categorical questions in just one column
menage = menage %>% select(-c(uf18)) ## all NAs
menage <- menage %>%
  mutate(
         hc1 = ifelse(hc1 %in% c("NSP", "NON RESPONSE"), NA_character_, as.character(hc1)),
         hc1 = fct_recode(hc1,
                            "Mat" = "NATTE",
                            "Bamboo" = "PALME  BAMBOU",
                            "Ceramic Tiles" = "CARRELAGE EN CERAMIQUE",
                            "Earth  Sand" = "TERRE  SABLE",
                            "Wood Planks" = "PLANCHES DE BOIS",
                          "Polished Wood" = 'PARQUET EN BOIS OU BOIS POLI',
                          'Dung' = "BOUSE",
                          "Cement"= "CIMENT",
                          "Vinyl or Asphalt Strips" = 'BANDES DE VINYLE OU D?ASPHALTE',
                          'Carpet or Rug' = 'MOQUETTE  TAPIS'),
         hc1 = factor(hc1, levels = c("Mat",
                                          "Bamboo",
                                          "Ceramic Tiles",
                                          "Earth / Sand",
                                          "Wood Planks",
                                          "Polished Wood",
                                          "Dung",
                                          "Cement",
                                          "Vinyl or Asphalt Strips",
                                          "Carpet or Rug")),
         hc2 = ifelse(hc2 == "NON REPONSE", NA_character_, as.character(hc2)),
         hc2 = fct_recode(hc2,
                          "Mat" = "NATTE",
                          "Bamboo" = "CHAUME  FEUILLE DE PALME PALMIER  BAMBOU  ZOZORO",
                          "Grass" = "MOTTES D'HERBES",
                          "Wood Planks" = "PLANCHES DE BOIS",
                          "Metal or Aluminium" = "TOLE  METAL  ALUMINIUM",
                          "No Roof" = "PAS DE TOIT"),
         hc2 = factor(hc2, levels = c("Mat", "Bamboo", "Grass", 
                                      "Wood Planks", "Metal or Aluminium", "No Roof")),
         hc3 = ifelse(hc3 == "NON REPONSE", NA_character_, as.character(hc3)),
         hc3 = fct_recode(hc3,
                          "Bamboo" = "CANE  PALME  TRONCS  ZOZORO",
                          "No wall" = "PAS DE MURS",
                          "Plates" = "CONTRE PLAQUE",
                          "Mud" = "BOUE",
                          "Wood Planks" = "PLANCHES DE BOISBARDEUX",
                          "Uncovered Adobe" = "ADOBE NON RECOUVERTBANCO",
                          "Other" = "AUTRE"),
         hc3 = factor(hc3, levels = c("Bamboo", "No wall", "Plates", "Mud", "Wood Planks",
                                      "Uncovered Adobe", "Other")),
         hc4a = ifelse(hc4a == "NON REPONSE", NA_character_, as.character(hc4a)),
         hc4a = fct_recode(hc4a,
                           "Open fire" = "Feu sur Trois pierres  feu ouvert",
                           "Traditional solid fuel stove" = "Cuisiniere traditionnelle a combustible solide",
                           "Solar cooker" = "Cuisiniere solaire",
                           "Solid Fuel Stove" = "Cuisiniere a combustible solide",
                           "No meal prepared" = "PAS DE REPAS PRÉPARÉ DANS MENAGE",
                           "Liquid Fuel Cooker" = "Cuisiniere a combustible liquide",
                           "Liquid Gas Cooker" = "Cuisiniere a gaz liquide (GPL)"),
         hc4a = factor(hc4a, levels = c("Open fire", "Traditional solid fuel stove",
                                        "Solar cooker", "Solid Fuel Stove", "No meal prepared",
                                        "Liquid Fuel Cooker", "Liquid Gas Cooker")),
         hc4b = ifelse(hc4b == "NON REPONSE", NA_character_, as.character(hc4b)),
         hc4b = fct_recode(hc4b,
                           "Wood" = "BOIS",
                           "Biomass" = "BIOMASS MANUFACTUREE (GRANULES) OU COPEAUX DE BOIS",
                           "Charcoal" = "CHARBON DE BOIS",
                           "Animal Waste" = "BOUSE D?ANIMAUX  DECHETS",
                           "Grass" = "RESIDUS AGRICOLES  HERBES PAILLES ARBUSTES",
                           "Diesel" = "ESSENCE  DIESEL",
                           "Alcohol" = "ALCOOL  ETHANOL",
                           "Petroleum" = "PETROLE  PARAFFINE"),
         hc4b = factor(hc4b, levels = c("Wood", "Biomass", "Charcoal",
                                        "Animal Waste", "Grass", "Diesel",
                                        "Alcohol", "Petroleum")),
         hc5 = ifelse(hc5 == "NON REPONSE", NA_character_, as.character(hc5)),
         hc5 = fct_recode(hc5, 
                          "In a non separate room in the main house" = "DANS LA MAISON PRINCIPALE  DANS UNE PIECE NON SEPAREE",
                          "In a separate room in the main house" = "DANS LA MAISON PRINCIPALE  DANS UNE PIECE SEPARE",
                          "In a separate building" = "DANS UN BATIMENT SEPARE",
                          "Outside on a veranda or a covered porch" = "DEHORS  SUR UNE VERANDA OU UN PORCHE COUVERT",
                          "Outdoors" = "DEHORS A LAIR LIBRE"),
         hc5 = factor(hc5, levels = c("In a non separate room in the main house",
                                      "In a separate room in the main house",
                                      "In a separate building",
                                      "Outside on a veranda or a covered porch",
                                      "Outdoors")),
         hc9 = ifelse(hc4b == "NON REPONSE", NA_character_, as.character(hc4b)),
         hc9 = fct_recode(hc9,
                          "No titles" = "AUCUN TITREES",
                          "Partially titled" = "PARTIELLEMENT TITREES",
                          "Totally titled" = "TOUS TITREES"),
         hc9 = factor(hc9, levels = c("No titles", "Partially titled", "Totally titled")),
         hc28 = ifelse(hc28 == "Non Reponse", NA_character_, as.character(hc28)),
         hc28 = fct_recode(hc28,
                           "Other" = 'AUTRE',
                           'Employed' = 'EMPLOYÉ  (GOUVERNEMENT, ONG, AUTRE?)',
                           'No income' = 'PAS DE REVENU',
                           "Small Business Owner" = "PROPRIÉTAIRE D'UNE PETITE ENTREPRISE",
                           "Sell honey"='VENDRE DU MIEL',
                           "Sell crafts" = "VENTE D'ARTISANAT",
                           'Sell wood' = 'VENTE DE BOIS',
                           "Sell other forest products" = "VENTE D'AUTRES PRODUITS FORESTIERS",
                           "Sell firewood" =  "VENTE DE BOIS DE CHAUFFAGE",
                           "Sell charcoal" =  "VENTE DE CHARBON DEBOIS",
                           "Sell fish" = "VENTE DE POISSON",
                           "Sell ravinala or thatch" = 'VENTE DE RAVINALA  CHAUME',
                           "Sell agricultural products" = "VENTE DE PRODUITS AGRICOLES MANIOC TUBERCULES PAR EXEMPLE",
                           "Sell crops"= "VENTE DE CULTURES DE RENTE CAFÉ VANILLE, CLOU DE GIROFLE PAR EXEMPLE",
                           "Sell prepared meals"= "VENTE DE PLATS PRÉPARÉS  PLATS FRITS  GÂTEAUX DE POISSON PAR EXEMPLE"),
         hc28 = factor(hc28, levels = c("Other", "Employed", "No income",
                                        "Small Business Owner", "Sell honey",
                                        "Sell crafts", "Sell wood",
                                        "Sell other forest products", "Sell firewood",
                                        "Sell charcoal", "Sell fish",
                                        "Sell ravinala or thatch", "Sell agricultural products",
                                        "Sell crops", "Sell prepared meals")),
         ws1 = ifelse(ws1 == "NON REPONSE", NA_character_, as.character(ws1)),
         ws1 = fct_recode(ws1, 
                          "unprotected source" = 'SOURCE SOURCE NON PROTEGEE',
                          'protected source' = "SOURCE SOURCE PROTEGEE",
                          'Surface water' = "EAU DE SURFACE RIVIERE BARRAGE LAC MARE COURANT CANAL SYSTEME D?IRRIGATION",
                          'Protected hollow well' =  'PUITS CREUSE PAS PROTEGE',
                          "Unprotected hollow well" =  'PUITS CREUSE PAS PROTEGE',
                          'Conditioned bottled water' = 'EAU CONDITIONNEE EAU EN BOUTEILLE',
                          'Hollow well' = 'PUITS CREUSE PROTEGE',
                          'Tap in the garden' = 'ROBINET DANS LA CONCESSIONJARDIN PARCELLE',
                          'Tap in the housing' = 'ROBINET DANS LE LOGEMENT',
                          'Public tap or fountain terminal' = 'ROBINET ROBINET PUBLICBORNE FONTAINE',
                          'Pump or drilling wells' ='PUITS A POMPEFORAGE'),
         ws1 = factor(ws1, levels = c("unprotected source", "protected source",
                                       "Surface water", "Protected hollow well",
                                       "Unprotected hollow well", "Conditioned bottled water",
                                       "Hollow well", "Tap in the garden", "Tap in the housing")),
         ws7 = ifelse(ws7 == "NON REPONSE", NA_character_, as.character(ws7)),
         ws7 = fct_recode(ws7,
                          "Livestock" = "BETAIL",
                          "Goods owned by household" = "BIENS POSSEDES PAR LE MENAGE",
                          "Land" = "TERRAIN"),
         ws7 = factor(ws7, levels = c("Livestock", "Goods owned by household", "Land")),
         fs9 = ifelse(fs9 == "NON REPONSE", NA_character_, as.character(fs9)),
         fs9 = fct_recode(fs9,
                          "Nature" = 'PAS DE TOILETTES NATURECHAMPS',
                          'Open pit' = 'LATRINE A FOSSE  LATRINE A FOSSE SANS DALLEFOSSE OUVERTE',
                          'non washable slab' = 'LATRINE A FOSSE  LATRINE A FOSSE AVEC DALLE NON LAVABLE',
                          'washable slab' = 'LATRINE A FOSSE  LATRINE A FOSSE AVEC DALLE LAVABLE',
                          'Other' =  'AUTRE',
                          'Suspended toilets' = 'TOILETTES SUSPENDUESLATRINES SUSPENDUES',
                          'Water flush connected to latrines' =  'CHASSE D?EAU  RELIE AUX LATRINES',
                          'Water flush connected to free air' = "CHASSE D?EAU  RELIE A L'AIR LIBRE"),
         fs9 = factor(fs9, levels = c("Nature", "Open pit", 
                                      "non washable slab", "washable slab",
                                      "Other", "Suspended toilets", "Water flush connected to latrines",
                                      "Water flush connected to free air")),
         uf4 = ifelse(uf4 == "NON REPONSE", NA_character_, as.character(uf4)),
         uf4 = fct_recode(uf4,
                          "Construction wood" = "BOIS DE CONSTRUCTION",
                           "Other" = "AUTRE",
                          "Pirogue Construction" = "CONSTRUCTION DE PIROGUE"),
         uf4 = factor(uf4, levels = c("Construction wood", "Other", "Pirogue Construction")),
         uf5 = ifelse(uf5 == "NON REPONSE", NA_character_, as.character(uf5)),
         uf5 = fct_recode(uf5,
                          "Smoking the bees" = "EN ENFUMANT LES ABEILLES",
                          "Harvesting from trees" = "EN RECOLTANT SIMPLEMENT DANS L'ARBRE"),
         uf5 = factor(uf5, levels = c("Smoking the Bees", "Harvesting from trees")),
         uf7 = ifelse(uf7 == "Non Reponse", NA_character_, as.character(uf7)),
         uf7 = fct_recode(uf7, 
                          "Construction Wood" = "BOIS DE CONSTRUCTION",
                          "Charcoal"= "CHARBON DE BOIS",
                          "Firewood" = "BOIS DE CHAUFFAGE",
                          "Pirogue construction" = "CONSTRUCTION DE PIROGUE"),
         uf7 = factor(uf7, levels = c("Construction Wood", "Charcoal", "Firewood", "Pirogue construction")),
         uf8 = ifelse(uf8 == "Non Reponse", NA_character_, as.character(uf8)),
         uf8 = fct_recode(uf8,
                          "Smoking the bees" = "EN ENFUMANT LES ABEILLES",
                          "Harvesting from trees" = "EN RECOLTANT SIMPLEMENT DANS L'ARBRE"),
         uf8 = factor(uf8,levels = c("Smoking the bees", "Harvesting from trees")),
         uf11 = ifelse(uf11 == "NE SAIT PAS", NA_character_, as.character(uf11)),
         uf11 = fct_recode(uf11,
                           "Parcel II" = "PARCELLE II LITTORALE DE LA RESERVE SPECIALE  STRICTE",
                           "Parcel I" = "PARCELLE I DE LA RESERVE SPECIALE  STRICTE",
                           "Special Reserve"= "FORET CLASSEE"),
         uf11 = factor(uf11, levels = c("Parcel II", "Parcel I", "Special Reserve")))

sapply(menage, class)
####################################################################################
# Categorical several columns ######################################################
####################################################################################
#Columns need to be boolean (TRUE, FALSE, NA)
#NA means no response

#List with categorical questions that are in more than 1 column
#The second column indicates to which group of question they belong
cat_question_1c = c("hc1", "Main Material of Floor", "Household Characteristics",
                    "hc2", "Main Material of Roof", "Household Characteristics",
                    "hc3", "Main Material of Floor", "Household Characteristics",
                    "hc4a", "Main stove for cooking", "Household Characteristics",
                    "hc4b", "Type of energy for cooker", "Household Characteristics",
                    "hc5", "Cooking Location", "Household Characteristics",
                    "hc9", "Land title", "Household Characteristics",
                    "hc28", "Main income", "Household Characteristics",
                    "ws1", "Main source of drinking water", "Water and Sanitation",
                    "ws7", "Toilet Type", "Water and Sanitation",
                    "fs9", "Assets sold", "Food Supply",
                    "uf4", "Main reason for cut", "Food supply",
                    "uf5", "Main Honey collection method", "Food supply",
                    "uf7", "Other reasons for cut", "Food supply",
                    "uf8", "Other Honey collecetion method", "Food supply",
                    "uf11", "Closest foreste", "Forest use"
                    )

menage_check = menage %>% select(c(hc1, hc2, hc3, hc4a, hc4b, hc5, hc9, hc28, ws1,
                                   ws7, fs9, uf4, uf5, uf7, uf8, uf11, village_code,
                                   completeness))

menage_check = na.omit(menage_check)

cat_question_1c = matrix(cat_question_1c , ncol = 3, byrow = TRUE)

#Function to create the dataframe
cat_1c_type <- function(df, col){
  #Computes the sum and total per village and category
  df_one <- df %>% filter(completeness == "Complete") %>%
    select(village_code, {{col}}) %>%
    filter_all(all_vars(!is.na(.))) %>%
    group_by(village_code) %>%
    mutate(total = n()) %>%
    group_by_all() %>%
    summarise(sum = n(),
              total = mean(total)) %>%
    rename(categories = {{col}})
  
  #Firstly, creates a dataframe with all the combinations (village, category)
  #and then it does the join with df_one
  df <- crossing(df["village_code"], categories = levels(df[,col])) %>%
    left_join(unique(df_one[,c("village_code", "total")], by = c("village_code")))  %>%
    left_join(df_one[,c("village_code", "categories", "sum")],
              by = c("village_code", "categories")) %>%
    mutate(sum = ifelse(!is.na(sum), sum, 0),
           total = ifelse(!is.na(total), total, 0))
  
  return(df)
}

#Apply the function cat_1c_type to each column
cat_1c_df <- apply(cat_question_1c, 1, function(x){
  cat_1c_type(menage, x[1]) %>%
    mutate(question = x[2],
           topic = x[3])
})
#rlang::last_error()
#From listo to dataframe
cat_1c_df <- bind_rows(cat_1c_df) %>%
  select(village_code, topic, question, categories, sum, total)
###################################################################################
# Final changes ###################################################################
###################################################################################
#rbind both categorical dataframes
df_cat <- rbind(cat_1c_df, cat_xc_df)
#Save it
saveRDS(cat_1c_df, file = here("Baseline_Survey/preprocessing/menage_cat.rds"))

#rbind boolean df and average df
df_num <- rbind(bool_df, avg_df)
df_num[1] = flapply(df_num[1], as.numeric)
#Save it
saveRDS(df_num, file = here("Baseline_Survey/preprocessing/menage_num.rds"))
