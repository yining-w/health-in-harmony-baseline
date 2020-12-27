library(tidyverse)
library(here)
rm(list=ls())
load(here("Baseline_Survey/data/MOUSTIQUAIRE.RData"))

moutiquaire_copy = moustiquaire
moustiquaire = moutiquaire_copy
# Administrative data #############################################################
moustiquaire <- moustiquaire %>% select(-contains(c("autre", "nr", "tn15_")))

moustiquaire <- mutate_all(moustiquaire, .funs=toupper)
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
                          cat_mosq_net_brand = tn5,
                          cat_mosq_net_source = tn10,
                          cat_mosq_net_source_other = tn12,
                          bool_mosq_net_sleep = tn13) %>%
        select(-hh1a, -hh6, -hh6a,
               -hh7, -hh7a, -hh8,
               -hh8a, -hh9, -hh9a,
               -tn11, -tn18b, -tn3, -tn14,
               -tnln) %>% 
        mutate(cat_mosq_net_brand = fct_recode(cat_mosq_net_brand,
                                           'Other' = 'AUTRES MARQUES',
                                           'No_response' = 'NON REPONSE',
                                           'Dont_Know' = 'NSP MARQUE',
                                           'Other' = 'AUTRE TYPE',
                                           'Dont_Know' = 'NSP MARQUE /TYPE',
                                           'Permanet' = 'PERMANET',
                                           'SuperMoustiquaire' = 'SUPER MOUSTIQUAIRE',
                                           'RoyalSentry' = 'ROYAL SENTRY'),
               cat_mosq_net_source = fct_recode(cat_mosq_net_source,
                                            'MID' = "OUI, CAMPAGNE MID",
                                            'CPN' = 'OUI, CPN',
                                            'vaccination' = 'OUI, VACCINATION',
                                            'No' = 'NON',
                                            'Dont_Know' = 'NSP',
                                            'No_response' = 'NON RESPONSE'),
               cat_mosq_net_source_other = fct_recode(cat_mosq_net_source_other,
                                                  'Public_health' = "CENTRE DE SANTE PUBLIC",
                                                  'Private_center' = 'CENTRE DE SANTE PRIVE',
                                                  'Pharmacy' = 'PHARMACIE',
                                                  'Shop_Market_Street' = 'BOUTIQUE / MARCHE / RUE',
                                                  'Community_officer' = 'AGENT DE SANTE COMMUNAUTAIRE',
                                                  'Religious' = 'INSTITUTION RELIGIEUSE',
                                                  'School' = 'ECOLE',
                                                  'Other' = 'AUTRE',
                                                  'Dont Know' = 'NSP',
                                                  'No Response' = 'NON REPONSE'
               ))
        

moustiquaire = moustiquaire %>% mutate_at(.vars=vars(starts_with("bool_")),
                                          ~ifelse(grepl('NON', .), 0, 1))

moustiquaire = moustiquaire %>% mutate_at(.vars=vars(starts_with("alpha_")),
                                          ~ifelse(grepl('', .), 0, 1))

moustiquaire$numeric_mosq_net_number <- as.numeric(as.character(moustiquaire$numeric_mosq_net_number))

#change the cats
moustiquaire_cats = moustiquaire %>% count(reserve_section, village_code, cat_mosq_net_source_other, sort = TRUE)
moustiquaire_cats = moustiquaire_cats %>% rename(mosq_net_source_other = n)
moustiquaire_cats = reshape(moustiquaire_cats, idvar = c("reserve_section", "village_code"), timevar="cat_mosq_net_source_other", direction = "wide")

moustiquaire_cats_two = moustiquaire %>% count(reserve_section, village_code, cat_mosq_net_source, sort = TRUE)
moustiquaire_cats_two = moustiquaire_cats_two %>% rename(mosq_net_source = n)
moustiquaire_cats_two = reshape(moustiquaire_cats_two, idvar = c("reserve_section", "village_code"), timevar="cat_mosq_net_source", direction = "wide")

moustiquaire_cats_three = moustiquaire %>% count(reserve_section, village_code, cat_mosq_net_brand, sort = TRUE)
moustiquaire_cats_three = moustiquaire_cats_three %>% rename(mosq_net_brand = n)
moustiquaire_cats_three = reshape(moustiquaire_cats_three, idvar = c("reserve_section", "village_code"), timevar="cat_mosq_net_brand", direction = "wide")

moustiquaire_cats = merge(moustiquaire_cats, moustiquaire_cats_two, by = c("village_code", "reserve_section"))
moustiquaire_cats = merge(moustiquaire_cats, moustiquaire_cats_three, by = c("village_code", "reserve_section"))

##aggregate bools and nums
moustiquaire_bools = moustiquaire %>% select(c(contains("bool")), "village_code", "reserve_section")
summary_moustiquaire = moustiquaire_bools %>% group_by(reserve_section, village_code) %>% 
        summarize_if(is.numeric, sum, na.rm=TRUE)

moustiquaire_num = moustiquaire %>% select(c(contains("numeric")), "village_code", "reserve_section")
moustiquaire_num = moustiquaire_num %>% group_by(reserve_section, village_code) %>% 
        summarize_if(is.numeric, mean, na.rm=TRUE)

summary_moustiquaire = merge(summary_moustiquaire, moustiquaire_num, by = c("village_code", "reserve_section"))
summary_moustiquaire = merge(summary_moustiquaire, moustiquaire_cats, by= c("village_code", "reserve_section"))

#tally observations
obs_moustiquaire = moustiquaire %>% group_by(village_code) %>% tally()
summary_moustiquaire = merge(summary_moustiquaire, obs_moustiquaire, by='village_code')

## reshape to long
summary_moustiquaire <- pivot_longer(summary_moustiquaire, cols=3:36, names_to = "Question", values_to = "Value")

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