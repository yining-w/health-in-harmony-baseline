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
                            bool_mosq_net = tn1,
                            numeric_mosq_net_number = tn2,
                            bool_mosq_net_reason_scared = tn17a,
                            bool_mosq_net_reason_fish = tn17b,
                            bool_mosq_net_reason_poultry = tn17c,
                            bool_mosq_net_reason_other = tn17x,
                            bool_mosq_otherway_spray = tn18ba,
                            bool_mosq_otherway_puddles = tn18bb,
                            bool_mosq_otherway_bushes = tnbc,
                            bool_mosq_otherway_repell = tn18bd,
                            bool_mosq_otherway_other = tn18bx,
                            bool_health_diarrhea = st1,
                            bool_health_fever = st2,
                            bool_health_cough = st3,
                            bool_health_hospital = st4a,
                            bool_health_center = st4b,
                            bool_health_post = st4c,
                            bool_health_comm = st4d,
                            bool_health_clinic = st4e,
                            bool_health_public_other = st4f,
                            bool_health_private_clinic = st4g,
                            bool_health_private_dr = st4h,
                            bool_health_pharmacy = st4i,
                            bool_health_officer = st4j,
                            bool_health_mobile = st4k,
                            bool_health_private_other = st4l,
                            bool_health_family = st4m,
                            bool_health_shop = st4n,
                            bool_health_traditional = st4o,
                            bool_health_other = st4x,
                            bool_health_treat = st5,
                            bool_health_cost_transport = st6a,
                            bool_health_cost_medicine = st6b,
                            bool_health_cost_service = st6c,
                            bool_health_cost_afford = st7,
                            bool_health_cost_main_transport = st8a,
                            bool_health_cost_main_medicine = st8b,
                            bool_health_cost_main_service = st8c,
                            bool_health_cost_main_other = st8x,
                            bool_food_supply = fs1,
                            bool_food_runout = fs2,
                            bool_food_hungry = fs3,
                            bool_food_skipmeals = fs4,
                            bool_food_norice = fs5a,
                            bool_food_january = fs6aa,
                            bool_food_february = fs6ab,
                            bool_food_march = fs6ac,
                            bool_food_april = fs6ad,
                            bool_food_may = fs6ae,
                            bool_food_june = fs6af,
                            bool_food_july = fs6ag,
                            bool_food_august = fs6ah,
                            bool_food_september = fs6ai,
                            bool_food_october = fs6aj,
                            bool_food_november = fs6ak,
                            bool_food_december = fs6al,
                            bool_food_dontknow = fs6ay,
                            bool_food_notconcerned = fs6az,
                            numeric_food_months = fs6b,
                            bool_food_eatpotatos = fs7a,
                            bool_food_eatvia = fs7b,
                            bool_food_eatother = fs7x,
                            bool_food_soldassets = fs8,
                            bool_food_sold_specify =fs9,
                            bool_food_care_or_eat = fs10,
                            bool_food_care_or_eat_choice = fs11,
                            bool_forest_entered = uf1,
                            bool_forest_coastalplottwo = uf2a,
                            bool_forest_plotone = uf2b,
                            bool_forest_classified = uf2c,
                            bool_forest_reason_rice = uf3a,
                            bool_forest_reason_mahampy = uf3b,
                            bool_forest_reason_travel = uf3c,
                            bool_forest_reason_hunting = uf3d,
                            bool_forest_reason_cuttree = uf3e,
                            bool_forest_reason_collecttree = uf3f,
                            bool_forest_reason_fruitgathering = uf3g,
                            bool_forest_reason_catchbird = uf3h,
                            bool_forest_reason_harvestdonkey = uf3i,
                            bool_forest_reason_digtavolo = uf3j,
                            bool_forest_reason_other = uf3x,
                            cat_forest_cut = uf4,
                            cat_forest_honey_method = uf5,
                            bool_forest_reason_other_rice = uf6a,
                            bool_forest_reason_other_mahampy = uf6b,
                            bool_forest_reason_other_travel = uf6c,
                            bool_forest_reason_other_hunting = uf6d,
                            bool_forest_reason_other_cuttree = uf6e,
                            bool_forest_reason_other_collecttree = uf6f,
                            bool_forest_reason_other_fruitgathering = uf6g,
                            bool_forest_reason_other_catchbird = uf6h,
                            bool_forest_reason_other_harvestdonkey = uf6i,
                            bool_forest_reason_other_digtavolo = uf6j,
                            bool_forest_reason_other_other = uf6x,
                            cat_forest_other_cut = uf7,
                            cat_forest_other_honey_method = uf8,
                            bool_forest_notentered_noneed = uf9a,
                            bool_forest_notentered_notauthorized = uf9b,
                            bool_forest_notentered_fear = uf9c,
                            bool_forest_notentered_opportunity = uf9d,
                            bool_forest_notentered_far = uf9e,
                            bool_forest_notentered_other = uf9f,
                            bool_forest_pay_healthcare = uf10,
                            cat_forest_closest = uf11,
                            bool_forest_rules = uf12,
                            bool_forest_advantages = uf13,
                            bool_forest_advantages_water = uf14a,
                            bool_forest_advantages_protein = uf14b,
                            bool_forest_advantages_construction = uf14c,
                            bool_forest_advantages_firewood = uf14d,
                            bool_forest_advantages_fruit = uf14e,
                            bool_forest_advantages_seeds = uf14f,
                            bool_forest_advantages_medicine = uf14g,
                            bool_forest_advantages_cleanair = uf14h,
                            bool_forest_advantages_rain = uf14i,
                            bool_forest_advantages_shade = uf14j,
                            bool_forest_advantages_tourism = uf14k,
                            bool_forest_advantages_cultural = uf14l,
                            
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
