library(tidyverse)
library(here)

load(here("Baseline_Survey/data/femme.RData"))

# Administrative data #############################################################

#Changing the name of administrative columns
femme <- femme %>% rename(intervier = wm5,
                          village_code = wm1, 
                          household_number = wm2,
                          woman_line = wm3,
                          woman_name = wm3a,
                          reserve_section = wmstrate,
                          team_leader = wm4,
                          permission = wm8,
                          completeness = wm16) %>%
        #Deleting columns about time and others with no info
        select(-wm6jj, -wm6mm, -wm6aa,
               -wm7hh, -wm7mn, -wm10hh,
               -wm10mn, -wm16a, -finwm) %>%
        #Translating
        mutate(permission = case_when(
                                permission == "Oui" ~ TRUE,
                                TRUE ~ FALSE
                                ),
               completeness = case_when(
                                completeness == "Completé" ~ "Complete",
                                completeness == "Pas à la maison" ~ "Not at home",
                                completeness == "Refusée" ~ "Refusal",
                                completeness == "Partiellement completé" ~ "Partially completed",
                                completeness == "En incapacité" ~ "Incapacitated",
                                completeness == "Autre" ~ "Other"
                                ),
               completeness = factor(completeness, 
                                     levels = c("Complete",
                                                "Not at home",
                                                "Refusal",
                                                "Partially completed",
                                                "Incapacitated",
                                                "Other"))
               )
        
# children data #############################################################

femme <- femme %>% rename(birth = cm1,
                          children_household = cm2,
                          sons_household = cm3,
                          daughters_household = cm4,
                          children_not_household = cm5,
                          sons_not_household = cm6,
                          daughters_not_household = cm7,
                          children_died = cm8,
                          sons_died = cm9,
                          daughters_died = cm10) %>%
        #Deleting columns with no useful info and duplicated info
        select(-cm12, -cm11, -cm14, -cm15,
               -cm17, -cm18, -bh12) %>%
        #Translating
        mutate(birth = case_when(
                                birth == 'Oui' ~ TRUE,
                                birth == 'Non' ~ FALSE
                                ),
               children_household = case_when(
                                children_household == 'Oui' ~ TRUE,
                                children_household == 'Non' ~ FALSE
                                ),
               children_not_household = case_when(
                                children_not_household == 'Oui' ~ TRUE,
                                children_not_household == 'Non' ~ FALSE
                                ),
               children_died = case_when(
                       children_died == 'Oui' ~ TRUE,
                       children_died == 'Non' ~ FALSE,
                       !is.na(children_died) ~ NA #The NA is because there was a non respond
               ),
               )

# Last birth place #############################################################

femme <- femme %>% rename(birth_2y = mn1,
                          birth_place = mn2,
                          birth_place_other = mn2_autre,
                          birth_assist_med = mn3a,
                          birth_assist_nurse = mn3b,
                          birth_assist_assist = mn3c,
                          birth_assist_trad = mn3d,
                          birth_assist_comm = mn3e,
                          birth_assist_close = mn3f,
                          birth_assist_other = mn3x,
                          birth_assist_noone = mn3y
                          ) %>%
        select(-mn3, -mn3nr, -mn3_autre) %>%
        mutate(birth_2y = case_when(
                                birth_2y == 'Oui' ~ TRUE,
                                TRUE ~ FALSE
                                ),
               birth_place = fct_recode(birth_place,
                                        'Respondent domicile' = 'DOMICILE DE L?ENQUETEE',
                                        'Other domicile' = 'AUTRE DOMICILE',
                                        'Public hospital' = 'HOPITAL PUBLIC',
                                        'Clinic / Public health center' = 'CLINIQUE / CENTRE DE SANTE PUBLIC',
                                        'Public health post' = 'POSTE DE SANTE PUBLIC',
                                        'Other public medical sector' = 'AUTRE SECTEUR MEDICAL PUBLIC',
                                        'Private hospital' = 'HOPITAL PRIVE',
                                        'Private clinic' = 'CLINIQUE PRIVEE',
                                        'Private maternity' = 'MATERNITE PRIVEE',
                                        'Other medical private' = 'AUTRE MEDICAL PRIVE',
                                        'Other' = 'AUTRE',
                                        'No response' = 'NON REPONSE'),
               birth_place_type = case_when(
                                        as.numeric(birth_place) <= 2 ~ "Domicile",
                                        as.numeric(birth_place) <= 6 ~ "Public sector",
                                        as.numeric(birth_place) <= 10 ~ "Private sector",
                                        as.numeric(birth_place) == 10 ~ "Other"
                                        ),
               birth_place_type = factor(birth_place_type, 
                                         levels= c("Domicile","Public sector",
                                                   "Private sector","Other"))
               ) %>%
        #When there is a letter TRUE else NA
        mutate_at(.vars = c("birth_assist_med",
                            "birth_assist_nurse",
                            "birth_assist_assist",
                            "birth_assist_trad",
                            "birth_assist_comm",
                            "birth_assist_close",
                            "birth_assist_other",
                            "birth_assist_noone"),
                  ~ifelse(grepl('[A-Z]', .), TRUE, NA)) %>%
        mutate(birth_assist_type = case_when(
                birth_assist_med | birth_assist_nurse | birth_assist_assist ~ "Health professional",
                birth_assist_trad | birth_assist_comm | birth_assist_close ~ "Other person",
                birth_assist_other ~ "Other",
                birth_assist_noone ~ "No one"
                ),
               birth_assist_type = factor(birth_assist_type, 
                                          levels = c("Health professional",
                                                     "Other person",
                                                     "Other",
                                                     "No one"))
               )


# Sterilization P1 #############################################################

femme <- femme %>% rename(ster_f_ster = cp1a,
                          ster_m_ster = cp1b,
                          ster_iud = cp1c,
                          ster_inje = cp1d,
                          ster_impl = cp1e,
                          ster_pill = cp1f,
                          ster_m_cond = cp1g,
                          ster_f_cond = cp1h,
                          ster_morn = cp1i,
                          ster_fixe = cp1j,
                          ster_amen = cp1k,
                          ster_rhyt = cp1l,
                          ster_with = cp1m,
                          ) %>%
        select(-cp1x, -cp2) %>%
        mutate_at(.vars=vars(starts_with("ster")),
                  ~as.character(.)
        ) %>%
        mutate_at(.vars=vars(starts_with("ster")),
                  funs(case_when(
                          . == "Oui, spontané" ~ "Yes, spontaneous",
                          . == "Oui, après explication" ~ "Yes, after explanation",
                          . == "Non" ~ "No",
                          . == "Non reponse" ~ "No response"
                          )
                       )
                  ) %>%
        mutate_at(.vars=vars(starts_with("ster")),
                  ~factor(., levels = c("Yes, spontaneous",
                                        "Yes, after explanation",
                                        "No",
                                        "No response"))
                  )%>%
        rename(ster_other = cp1_autre)

# Sterilization P2 #############################################################

femme <- femme %>% rename()
