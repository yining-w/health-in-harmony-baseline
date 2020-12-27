library(tidyverse)
library(dplyr)

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
                              -finhh
                              ))

## Remove "Specify other" / aggregate of 'check all' questions
menage <- menage %>% select(-contains("autre"))
menage <- menage %>% select(-contains("nr"))
menage <- menage %>% select(-c(ws6, st4, uf3))
                         
  
##Rename Administrative info
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
menage <- menage %>% mutate(completeness = ifelse(completeness == "Completé", 1, 0))
menage <- menage %>% mutate(completeness = ifelse(permission == "OUI", 1, 0))

##Rename vars
menage <- menage %>% rename(cat_material_floor = hc1,
                            cat_material_roof = hc2,
                            cat_material_wall = hc3,
                            cat_material_stove = hc4a,
                            cat_material_stove_fuel = hc4b,
                            cat_cook_location = hc5,
                            bool_owns_cellphone = hc6a,
                            bool_owns_show = hc6b,
                            bool_owns_bicycle = hc6c,
                            bool_owns_motorcycle = hc6d,
                            bool_owns_boat = hc6e,
                            bool_owns_net = hc6f,
                            bool_owns_animal = hc6g,
                            bool_owns_truck = hc6h,
                            bool_owns_motorboat = hc6i,
                            bool_owns_plow = hc6j,
                            bool_owns_sewingmachine =hc6k,
                            bool_owns_tractor = hc6l,
                            bool_has_electricity = hc7,
                            bool_owns_land = hc8,
                            cat_titled_land = hc9,
                            num_hectare_owned = hc10,
                            num_pct_agr = hc11,
                            bool_grows_irrigated_rice = hc12a,
                            bool_grows_rainfed_rice = hc12b,
                            bool_grows_cassava_rice = hc12c,
                            bool_grows_potato_rice = hc12d,
                            bool_grows_cashcrop = hc12e,
                            bool_grows_other = hc12x,
                            bool_grows_traditional_rice = hc13,
                            bool_grows_traditional_irrigated = hc14a,
                            bool_grows_traditional_rainfed = hc14b,
                            num_months_production_irrigated = hc15a,
                            num_months_production_rainfed = hc15b,
                            bool_slash = hc16,
                            num_cows = hc20a,
                            num_zebus = hc20b,
                            num_goats = hc20c,
                            num_sheep = hc20d,
                            num_chickens = hc20e,
                            num_pigs = hc20f,
                            num_poultry = hc20g,
                            num_other = hc20x,
                            bool_owns_shoes = hc21,
                            bool_owns_landline = hc22a,
                            bool_owns_radio = hc22b,
                            bool_owns_table = hc22c,
                            bool_owns_chair = hc22d,
                            bool_owns_reads = hc22e,
                            bool_owns_sofa = hc22f,
                            bool_owns_tv = hc22g,
                            bool_owns_cdplayer = hc22h,
                            num_rooms = hc23,
                            bool_owns_tablet = hc24,
                            bool_has_internet = hc26,
                            bool_has_bankaccount = hc27,
                            cat_main_income = hc28,
                            num_total_income = hc29,
                            cat_water_drinking = ws1,
                            cat_water_other = ws2,
                            cat_water_location = ws3,
                            num_water_duration = ws4,
                            cat_water_safe = ws5,
                            bool_water_boil = ws6a,
                            bool_water_bleach = ws6b,
                            bool_water_cloth = ws6c,
                            bool_water_filter = ws6d,
                            bool_water_solar = ws6e,
                            bool_water_disinfection = ws6f,
                            bool_water_other = ws6x,
                            cat_toilet_type = ws7,
                            bool_mosquito_nets = tn1,
                            num_mosquito_nets = tn2,
                            bool_mosquito_nets_scare = tn17a,
                            bool_mosquito_nets_fish = tn17b,
                            bool_mosquito_nets_poultry = tn17c,
                            bool_mosquito_nets_other = tn17x) 
##(not finished renaming)

#change all the uppercase
menage <- mutate_all(menage, .funs=toupper)

# change all Oui/Non #############################################################
menage = menage %>% mutate_at(.vars=vars(starts_with("bool_grows_traditional")),
                              ~ifelse(grepl('TRADITIONNEL', .), 0, 1))



##still need to finish::
###finish renaming all vars
##change categorical variables
##aggregate 
##create survey var
#create topic var
#create question var
#reshape long

#enfant[is.na(enfant)] <- 0


#obs_enfant = enfant %>% group_by(village_code) %>% tally()
#summary_enfant = enfant %>% group_by(reserve_section, village_code) %>% 
#  summarize_if(is.numeric, sum, na.rm=TRUE)

#summary_enfant = merge(summary_enfant, obs_enfant, by = "village_code")

##COME BACK TO THIS
#test= 
#  for (i in 3:58) {
#  summary_enfant[i]/summary_enfant$n
#}

#write.csv(summary_efnant,)
