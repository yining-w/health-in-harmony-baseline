library(tidyverse)
library(here)

load(here("Baseline_Survey/data/MOUSTIQUAIRE.RData"))


# Administrative data #############################################################

moustiquaire <- moustiquaire %>% rename(intervier = hh3,
                          village_code = hh1, 
                          household_number = hh2,
                          reserve_section = hhstrate,
                          mosq_net=tn1,
                          mosq_net_reason_scared = tn17a,
                          mosq_net_reason_fish = tn17b,
                          mosq_net_reason_poultry = tn17c,
                          mosq_otherway = tn18a,
                          mosq_net_number = tn2,
                          mosq_net_reason_other = tn17x_autre,
                          mosq_otherway_spray = tna8ba,
                          mosq_otherway_puddles = tn18bb,
                          mosq_otherway_bushes = tn18bc,
                          mosq_otherway_repell = tn18bd) %>%
        select(-hh1a, -hh6, -hh6a,
               -hh7, -hh7a, -hh8,
               -hh8a, -hh9, -hh9a) %>%
        mutate_at(.vars = c("mosq_net",
                            "mosq_net_reason_scared",
                            "mosq_net_reason_fish",
                            "mosq_net_reason_poultry",
                            "mosq_otherway"),
                  funs(case_when(
                          tolower(.) == 'oui' ~ TRUE,
                          tolower(.) == 'non' ~ FALSE,
                          TRUE ~ NA)
                       )
                  ) %>%
        mutate_at(.vars=vars(starts_with("mosq_otherway_")),
                  ~ifelse(grepl('[A-Z]', .), TRUE, NA)
        ) %>%
        rename(mosq_otherway_other = tn18b_autre,
               mosq_net_months = tn4,
               mosq_net_brand = tn5,
               mosq_net_brand_other = tn5_autre,
               mosq_net_source = tn10,
               mosq_net_source_other = tn12,
               mosq_net_sleep = tn13) %>%
        mutate(mosq_net_brand = fct_recode(mosq_net_brand,
                                           'Other brands' = 'AUTRES MARQUES',
                                           'No response' = 'NON REPONSE',
                                           'Does not know brand' = 'NSP MARQUE',
                                           'Other type' = 'AUTRE TYPE',
                                           'Does not know brand / type' = 'NSP MARQUE /TYPE'),
               mosq_net_source = fct_recode(mosq_net_source,
                                         'Yes, MID campaing' = "OUI, CAMPAGNE MID",
                                         'Yes, CPN' = 'OUI, CPN',
                                         'Yes, vaccination' = 'OUI, VACCINATION',
                                         'No' = 'NON',
                                         'Does not know' = 'NSP',
                                         'No response' = 'NON REPONSE'),
               mosq_net_source_other = fct_recode(mosq_net_source_other,
                                            'Public health' = "CENTRE DE SANTE PUBLIC",
                                            'Centerprivate health center' = 'CENTRE DE SANTE PRIVE',
                                            'Pharmacy' = 'PHARMACIE',
                                            'Shop / market / street' = 'BOUTIQUE / MARCHE / RUE',
                                            'Community health officer' = 'AGENT DE SANTE COMMUNAUTAIRE',
                                            'Religious institution' = 'INSTITUTION RELIGIEUSE',
                                            'School' = 'ECOLE',
                                            'Other' = 'AUTRE',
                                            'Does not know' = 'NSP',
                                            'No response' = 'NON REPONSE'
                                            ),
               mosq_net_sleep = fct_recode(mosq_net_sleep,
                                            'Yes' = "OUI",
                                            'No' = 'NON',
                                            'Does not know' = 'NSP / PAS SUR',
                                            'No response' = 'NON REPONSE'),
               mosq_net_sleep_people = as.numeric(!is.na(tn15_1)) + as.numeric(!is.na(tn15_2)) +
                       as.numeric(!is.na(tn15_3)) + as.numeric(!is.na(tn15_4)) +
                       as.numeric(!is.na(tn15_5)) + as.numeric(!is.na(tn15_6)) +
                       as.numeric(!is.na(tn15_7)) + as.numeric(!is.na(tn15_8))
               ) %>%
        select(-tn15_1, -tn15_2, -tn15_3, -tn15_4,
               -tn15_5, -tn15_6, -tn15_7, -tn15_8)
