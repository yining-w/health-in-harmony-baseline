library(tidyverse)
library(here)
rm(list=ls())
load(here("Baseline_Survey/data/MOUSTIQUAIRE.RData"))

# Administrative data #############################################################
moustiquaire <- moustiquaire %>% select(-contains(c("autre", "nr")), -tn18bx)

moustiquaire <- moustiquaire %>% rename(interviewer = hh3,
                          village_code = hh1, 
                          household_number = hh2,
                          reserve_section = hhstrate,
                          bool_mosq_net=tn1,
                          bool_mosq_net_reason_scared = tn17a,
                          bool_mosq_net_reason_fish = tn17b,
                          bool_mosq_net_reason_poultry = tn17c,
                          bool_mosq_net_reason_other = tn17x,
                          bool_mosq_otherway = tn18a,
                          numeric_mosq_net_number = tn2,
                          alpha_bool_mosq_otherway_spray = tna8ba,
                          alpha_bool_mosq_otherway_puddles = tn18bb,
                          alpha_bool_mosq_otherway_bushes = tn18bc,
                          alpha_bool_mosq_otherway_repell = tn18bd,
                          cat_mosq_net_months = tn4,
                          cat_mosq_net_type = tn5,
                          cat_mosq_net_source = tn10,
                          cat_mosq_net_source_other = tn12,
                          bool_mosq_net_sleep = tn13) %>%
        select(-hh1a, -hh6, -hh6a,
               -hh7, -hh7a, -hh8,
               -hh8a, -hh9, -hh9a,
               -tn11, -tn18b, -tn3, -tn14,
               -tnln) %>% 
        mutate(cat_mosq_net_type = as.character(cat_mosq_net_type),
               cat_mosq_net_type = case_when(cat_mosq_net_type == 'NON REPONSE' ~ "Any type of mosquito net",
                                              cat_mosq_net_type == 'NSP MARQUE' ~ "Any type of mosquito net",
                                              cat_mosq_net_type == 'AUTRE TYPE' ~ "Insecticide-treated mosquito net (ITN)",
                                              cat_mosq_net_type == 'NSP MARQUE /TYPE' ~ "Insecticide-treated mosquito net (ITN)",
                                              !is.na(cat_mosq_net_type) ~ "Insecticide-treated mosquito net (ITN)"),
               cat_mosq_net_type = factor(cat_mosq_net_type, 
                                              levels=c("Insecticide-treated mosquito net (ITN)",
                                                       "Any type of mosquito net")),
               cat_mosq_net_source = as.character(cat_mosq_net_source),
               cat_mosq_net_source_other = as.character(cat_mosq_net_source_other),
               cat_mosq_net_source = case_when(cat_mosq_net_source == "NON" ~ cat_mosq_net_source_other,
                                               cat_mosq_net_source == "NSP" ~ cat_mosq_net_source_other,
                                               TRUE ~ cat_mosq_net_source),
               cat_mosq_net_source = case_when(cat_mosq_net_source == 'OUI, CAMPAGNE MID' ~ 'Mass distribution campaign',
                                               cat_mosq_net_source == 'OUI, CPN' ~ 'Prenatal care visit',
                                               cat_mosq_net_source == 'OUI, VACCINATION' ~ 'Vaccination visit',
                                               cat_mosq_net_source == 'CENTRE DE SANTE PUBLIC' ~ 'Health facility-Government',
                                               cat_mosq_net_source == 'CENTRE DE SANTE PRIVE' ~ 'Health facility-Private',
                                               cat_mosq_net_source == 'BOUTIQUE / MARCHE / RUE' ~ 'Shop / Market / Street',
                                               cat_mosq_net_source == 'AGENT DE SANTE COMMUNAUTAIRE' ~ 'Community health worker',
                                               cat_mosq_net_source == 'AUTRE' ~ 'Other',
                                               cat_mosq_net_source == 'NSP' ~ 'DK / Missing'),
               cat_mosq_net_source = factor(cat_mosq_net_source,
                                            levels = c('Mass distribution campaign',
                                                       'Prenatal care visit',
                                                       'Vaccination visit',
                                                       'Health facility-Government',
                                                       'Health facility-Private',
                                                       'Shop / Market / Street',
                                                       'Community health worker',
                                                       'Other',
                                                       'DK / Missing')) 
              ) %>%
        select(-cat_mosq_net_source_other)


moustiquaire = moustiquaire %>% mutate_at(.vars=vars(starts_with("bool_")),
                                          ~ifelse(grepl('NON', ., ignore.case=TRUE), 0, 1))

moustiquaire = moustiquaire %>% mutate_at(.vars=vars(starts_with("alpha_")),
                                          ~ifelse(grepl('', .), 0, 1))

moustiquaire$numeric_mosq_net_number <- as.numeric(as.character(moustiquaire$numeric_mosq_net_number))

#Adding info from other tables #####################################################

#Adding the number of members of the household
load(here("Baseline_Survey/data/MENAGE.RData"))
menage <- menage %>% select(village_code = hh1,
                            household_number = hh2,
                            household_members = hh48)
moustiquaire = moustiquaire %>% left_join(menage, 
                                          by = c("village_code",
                                                 "household_number"))

#Adding household education
load(here("Baseline_Survey/data/TABLEAU_DE_MENAGE.RData"))
tableau_de_menage <- tableau_de_menage %>% filter(hl1 == 1) %>%
        select(village_code = hh1,
               household_number = hh2,
               head_education = hl10,
               head_education_level = hl11a) %>%
        mutate(head_education_level = case_when(
                head_education == "Non" ~ "Preschool or uneducated",
                head_education == "Non reponse" ~ "DK / Missing",
                head_education == "Oui" & head_education_level == "Pré-scolaire" ~ "Preschool or uneducated",
                head_education == "Oui" & head_education_level == "Primaire" ~ "Primary",
                head_education == "Oui" & head_education_level == "Secondaire 1" ~ "Secondary +",
                head_education == "Oui" & head_education_level == "Secondaire 2" ~ "Secondary +",
                head_education == "Oui" & head_education_level == "Superieur" ~ "Secondary +",
                TRUE ~ "DK / Missing")
               ) %>%
        select(-head_education)

moustiquaire = moustiquaire %>% left_join(tableau_de_menage, 
                                          by = c("village_code",
                                                 "household_number"))


#Creating table who splet under #####################################################

#Adding who slept under
load(here("Baseline_Survey/data/TABLEAU_DE_MENAGE.RData"))
sleep_with_net = tableau_de_menage %>%
        select(village_code = hh1,
               household_number = hh2,
               line_number = hl1,
               sex = hl4,
               age = hl6) %>%
        mutate(sex = fct_recode(sex,
                                 "Male" = "Masculin",
                                 "Female" = "Féminin"),
               age = case_when(
                       age <= 4 ~ "0-4",
                       age <= 14 ~ "5-14",
                       age <= 34 ~ "15-34",
                       age <= 49 ~ "35-49",
                       age > 50 ~ "50+"),
               age = factor(age, 
                            levels=c("0-4",
                                     "5-14",
                                     "15-34",
                                     "35-49",
                                     "50+")
                            )
               )


aux_moustiquaire <- moustiquaire %>%
        select(village_code,
               household_number,
               bool_mosq_net,
               cat_mosq_net_type,
               contains("tn15_")) %>%
        gather(key = "aux",
               value = "line_number",
               tn15_1:tn15_8) %>%
        select(-"aux")


sleep_with_net = sleep_with_net %>% left_join(aux_moustiquaire,
                                              by = c("village_code",
                                                     "household_number",
                                                     "line_number"))

moustiquaire <- moustiquaire %>% select(-contains("tn15_"))

rm(list = c("aux_moustiquaire","tableau_de_menage", "menage"))

##### Creating tables ##########################################################
temp <- moustiquaire %>% group_by(village_code, 
                                  household_number, 
                                  reserve_section,
                                  head_education_level,
                                  cat_mosq_net_type) %>% 
        summarise(nets = n(),
                  household_members = mean(household_members)) %>%
        spread(key = cat_mosq_net_type, 
               value = nets,
               fill = 0) %>%
        mutate(nets = sum(`Insecticide-treated mosquito net (ITN)`,
                          `Any type of mosquito net`,
                          na.rm = TRUE),
               ITN_2_pp = ifelse(household_members/`Insecticide-treated mosquito net (ITN)`>2,
                                 FALSE,
                                 TRUE),
               nets_2_pp = ifelse(household_members/nets>2,
                                  FALSE,
                                  TRUE))

saveRDS(temp, file = here("Baseline_Survey/data/mosquito_nets.rds"))


#change the cats ################################################################
moustiquaire_cats = moustiquaire %>% count(reserve_section, village_code, cat_mosq_net_source, sort = TRUE)
moustiquaire_cats = moustiquaire_cats %>% rename(mosq_net_source = n)
moustiquaire_cats = reshape(moustiquaire_cats, idvar = c("reserve_section", "village_code"), timevar="cat_mosq_net_source", direction = "wide")

moustiquaire_cats_two = moustiquaire %>% count(reserve_section, village_code, cat_mosq_net_type, sort = TRUE)
moustiquaire_cats_two = moustiquaire_cats_two %>% rename(mosq_net_type = n)
moustiquaire_cats_two = reshape(moustiquaire_cats_two, idvar = c("reserve_section", "village_code"), timevar="cat_mosq_net_type", direction = "wide")

moustiquaire_cats = merge(moustiquaire_cats, moustiquaire_cats_two, by = c("village_code", "reserve_section"))

rm("moustiquaire_cats_two")

##aggregate bools and nums
moustiquaire_bools = moustiquaire %>% select(c(contains("bool")), "village_code", "reserve_section")
summary_moustiquaire = moustiquaire_bools %>% group_by(reserve_section, village_code) %>%
        summarise_all(sum, na.rm=TRUE)

moustiquaire_num = moustiquaire %>% select(c(contains("numeric")), "village_code", "reserve_section")
moustiquaire_num = moustiquaire_num %>% group_by(reserve_section, village_code) %>%
        summarize_if(is.numeric, mean, na.rm=TRUE)

summary_moustiquaire = merge(summary_moustiquaire, moustiquaire_num, by = c("village_code", "reserve_section"))
summary_moustiquaire = merge(summary_moustiquaire, moustiquaire_cats, by= c("village_code", "reserve_section"))

#tally observations
obs_moustiquaire = moustiquaire %>% group_by(village_code) %>% tally()
summary_moustiquaire = merge(summary_moustiquaire, obs_moustiquaire, by='village_code')

## reshape to long
summary_moustiquaire <- pivot_longer(summary_moustiquaire, cols=3:26, names_to = "Question", values_to = "Value")

#Create dummy columns
summary_moustiquaire <- summary_moustiquaire %>% mutate(survey = "test")
summary_moustiquaire <- summary_moustiquaire %>% mutate(topic = case_when(grepl("mosq", Question) ~ "Mosquito Nets",
                                                                          grepl("placeholder", Question) ~"Other"))


##Rename the questions
summary_moustiquaire = summary_moustiquaire %>%
        mutate(Question = fct_recode(Question,
                                       'Do you have a mosquito net?' = 'bool_mosq_net',
                                       'Do you own a net to scare mosquitos?' = 'bool_mosq_net_reason_scared',
                                       'Do you own a net to fish?' = 'bool_mosq_net_reason_fish',
                                       'Do you own a net to catch poultry?' = 'bool_mosq_net_reason_poultry',
                                       'Do you own a net for other reasons?' = 'bool_mosq_net_reason_other',
                                       'Do you avoid mosquitos with other ways?' = 'bool_mosq_otherway',
                                        'Do you use spray to avoid mosquitos?' = 'alpha_bool_mosq_otherway_spray',
                                        'Do you remove puddles to avoid mosquitos?' = 'alpha_bool_mosq_otherway_puddles',
                                     'Do you remove mosquito bushes to avoid mosquitos?'='alpha_bool_mosq_otherway_bushes',
                                     'Do you use repellent to avoid mosquitos?' = 'alpha_bool_mosq_otherway_repell',
                                     'Do you use a mosquito net when you sleep?' = 'bool_mosq_net_sleep',
                                     'How many mosquito nets do you have?' = 'numeric_mosq_net_number',
                                     'People who have not responded to where they got their nets' = 'mosq_net_source_other.NA',
                                     'People who got their nets from a street/shop/market' = 'mosq_net_source_other.Shop_Market_Street',
                                     'People who got their net from a community officer' = 'mosq_net_source_other.Community_officer',
                                     'People who dont know where they got their nets' = 'mosq_net_source_other.Dont Know',
                                     'People who got their net from a public health center' = 'mosq_net_source_other.Public_health',
                                     'People who got their net from other places' = 'mosq_net_source_other.Other',
                                     'People who got their net from a private health center' = 'mosq_net_source_other.Private_center',
                                     'People who got their net through an MID campaign' = 'mosq_net_source.MID',
                                     'People who did not get a net' = 'mosq_net_source.No',
                                     'People who got a net through CPN' = 'mosq_net_source.CPN',
                                     'People who got a net through a vaccination visit' = 'mosq_net_source.vaccination',
                                     'People who dont know where they got their net' = 'mosq_net_source.Dont_Know',
                                     'People who use Permanet brand' = 'mosq_net_brand.Permanet',
                                     'People who use other brands' = 'mosq_net_brand.Other',
                                     'People who use Yorkool' = 'mosq_net_brand.YORKOOL',
                                     'People who use Super Moustiquaire' = 'mosq_net_brand.SuperMoustiquaire',
                                     'People who use Tsaralay' = 'mosq_net_brand.TSARALAY',
                                     'People who dont know what brand they use' = 'mosq_net_brand.Dont_Know',
                                     'People who use Royal Sentry' = 'mosq_net_brand.RoyalSentry',
                                     'People who use Olyset' = 'mosq_net_brand.OLYSET',
                                     'People who use Bestnet' = 'mosq_net_brand.BESTNET/NETPROTECT',
                                     'People who use interceptor' = 'mosq_net_brand.INTERCEPTOR'))

write.csv(summary_moustiquaire, 'moustiquaire_survey.csv')

#TRY AGAIN LATER:::: for (i in moustiquaire) {
#        if (i(vars(starts_with("cat")))) {
#        new = count(reserve_section, village_code, cat_mosq_net_source_other, sort = TRUE)        
#        }
#        return(new)
#} 