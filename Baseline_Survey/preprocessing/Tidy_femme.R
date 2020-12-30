library(tidyverse)
library(here)
remove(list=ls())
load(here("Baseline_Survey/data/femme.RData"))

# Administrative data #############################################################
femme <- femme %>% select(-contains(c("autre", "nr")))

#Changing the name of administrative columns
femme <- femme %>% rename(hh_interviewer = wm5,
                          hh_village_code = wm1, 
                          hh_household_number = wm2,
                          hh_woman_line = wm3,
                          hh_woman_name = wm3a,
                          hh_reserve_section = wmstrate,
                          hh_team_leader = wm4,
                          hh_permission = wm8,
                          hh_completeness = wm16) %>%
        #Deleting columns about time and others with no info
        select(-wm6jj, -wm6mm, -wm6aa,
               -wm7hh, -wm7mn, -wm10hh,
               -wm10mn, -wm16a, -finwm) %>%
        #Translating
        mutate(hh_permission = case_when(
                                hh_permission == "Oui" ~ TRUE,
                                TRUE ~ FALSE
                                ),
               hh_completeness = case_when(
                                hh_completeness == "Completé" ~ "Complete",
                                hh_completeness == "Pas à la maison" ~ "Not at home",
                                hh_completeness == "Refusée" ~ "Refusal",
                                hh_completeness == "Partiellement completé" ~ "Partially completed",
                                hh_completeness == "En incapacité" ~ "Incapacitated",
                                hh_completeness == "Autre" ~ "Other"
                                ),
               hh_completeness = factor(hh_completeness, 
                                     levels = c("Complete",
                                                "Not at home",
                                                "Refusal",
                                                "Partially completed",
                                                "Incapacitated",
                                                "Other"))
               )
        
# children data #############################################################

femme <- femme %>% rename(bool_hh_birth = cm1,
                          bool_hh_children_household = cm2,
                          numeric_hh_sons_household = cm3,
                          numeric_hh_daughters_household = cm4,
                          bool_hh_children_not_household = cm5,
                          numeric_hh_sons_not_household = cm6,
                          numeric_hh_daughters_not_household = cm7,
                          bool_hh_children_died = cm8,
                          numeric_hh_sons_died = cm9,
                          numeric_hh_daughters_died = cm10) %>%
        #Deleting columns with no useful info and duplicated info
        select(-cm12, -cm11, -cm14, -cm15,
               -cm17, -cm18, -bh12) %>%
        #Translating
        mutate(bool_hh_birth = case_when(
                                bool_hh_birth == 'Oui' ~ TRUE,
                                bool_hh_birth == 'Non' ~ FALSE
                                ),
               bool_hh_children_household = case_when(
                                bool_hh_children_household == 'Oui' ~ TRUE,
                                bool_hh_children_household == 'Non' ~ FALSE
                                ),
               bool_hh_children_not_household = case_when(
                                bool_hh_children_not_household == 'Oui' ~ TRUE,
                                bool_hh_children_not_household == 'Non' ~ FALSE
                                ),
               bool_hh_children_died = case_when(
                       bool_hh_children_died == 'Oui' ~ TRUE,
                       bool_hh_children_died == 'Non' ~ FALSE,
                       !is.na(bool_hh_children_died) ~ NA #The NA is because there was a non respond
               ),
               )

# Last birth place #############################################################

femme <- femme %>% rename(birth_2y = mn1,
                          birth_place = mn2,
                          birth_assist_med = mn3a,
                          birth_assist_nurse = mn3b,
                          birth_assist_assist = mn3c,
                          birth_assist_trad = mn3d,
                          birth_assist_comm = mn3e,
                          birth_assist_close = mn3f,
                          birth_assist_other = mn3x,
                          birth_assist_noone = mn3y
                          ) %>%
        select(-mn3) %>%
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


# Sterilization knowledge #############################################################

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
                  )

# Sterilization where knowledge #################################################

femme <- femme %>% rename(ster_place_hosp_pu = cp4a,
                          ster_place_cent_pu = cp4b,
                          ster_place_clincpf_pu = cp4c,
                          ster_place_clinmo_pu = cp4d,
                          ster_place_agent_pu = cp4e,
                          ster_place_hosp_pr = cp4g,
                          ster_place_clinc_pr = cp4h,
                          ster_place_pharm_pr = cp4i,
                          ster_place_med_pr = cp4j,
                          ster_place_clinmo_pr = cp4k,
                          ster_place_agent_pr = cp4l,
                          ster_place_bout = cp4n,
                          ster_place_reli = cp4o,
                          ster_place_close = cp4p
                          ) %>%
        select(-cp4, -cp4f, -cp4m, -cp4x) %>%
        mutate_at(.vars=vars(starts_with("ster_place")),
                  ~ifelse(grepl('[A-Z]', .), TRUE, NA)
        ) %>%
        rename(ster_place_know = cp3,
               ) %>%
        mutate(ster_place_know = case_when(
                ster_place_know == 'Oui' ~ TRUE,
                ster_place_know == 'Non' ~ FALSE,
                TRUE ~ NA
                )
        )


# Sterilization use #############################################################

femme <- femme %>% rename(ster_used_f_ster = cp6a,
                          ster_used_m_ster = cp6b,
                          ster_used_iud = cp6c,
                          ster_used_inje = cp6d,
                          ster_used_impl = cp6e,
                          ster_used_pill = cp6f,
                          ster_used_m_cond = cp6g,
                          ster_used_f_cond = cp6h,
                          ster_used_morn = cp6i,
                          ster_used_fixe = cp6j,
                          ster_used_amen = cp6k,
                          ster_used_rhyt = cp6l,
                          ster_used_with = cp6m) %>%
        select(-cp6, -cp6x) %>%
        mutate_at(.vars=vars(starts_with("ster_used")),
                  ~ifelse(grepl('[A-Z]', .), TRUE, NA)
                ) %>%
        rename(ster_used = cp5) %>%
        mutate(ster_used = case_when(
                ster_used == 'Oui' ~ TRUE,
                ster_used == 'Non' ~ FALSE,
                TRUE ~ NA
                )
               )

# Sterilization using #############################################################

femme <- femme %>% rename(ster_using_f_ster = cp10a,
                          ster_using_m_ster = cp10b,
                          ster_using_iud = cp10c,
                          ster_using_inje = cp10d,
                          ster_using_impl = cp10e,
                          ster_using_pill = cp10f,
                          ster_using_m_cond = cp10g,
                          ster_using_f_cond = cp10h,
                          ster_using_morn = cp10i,
                          ster_using_fixe = cp10j,
                          ster_using_amen = cp10k,
                          ster_using_rhyt = cp10l,
                          ster_using_with = cp10m) %>%
        select(-cp10, -cp10x) %>%
        mutate_at(.vars=vars(starts_with("ster_using")),
                  ~ifelse(grepl('[A-Z]', .), TRUE, NA)
        ) %>%
        rename(ster_using = cp9) %>%
        mutate(ster_used = case_when(
                ster_used == 'Oui' ~ TRUE,
                ster_used == 'Non' ~ FALSE,
                TRUE ~ NA
        )
        ) %>%
        rename(ster_child_num = cp7,
               ster_preg = cp8) %>%
        mutate(ster_preg = case_when(
                ster_preg == 'Oui' ~ TRUE,
                ster_preg == 'Non' ~ FALSE,
                TRUE ~ NA
        ))

# Sterilization why not ########################################################

femme <-femme %>% rename(ster_whynot = cp11) %>%
        mutate(
                ster_whynot = fct_recode(ster_whynot,
                                 'Non married' = 'NON MARIÉE',
                                 'No rap. sexual / rap. sexual few frequent' = 'PAS DE RAP. SEXUELS/RAP. SEXUELS PEU FRÉQUENTS',
                                 'Menopause / hysterectomy' = 'MÉNOPAUSE/HYSTÉRECTOMIE',
                                 'Under-fertile / sterile' = 'SOUS-FÉCONDE/STÉRILE',
                                 'Want so many children as possible'= "VEUT AUTANT D'ENFANTS QUE POSSIBLE",
                                 'Respondent opposite' = 'ENQUÊTÉE OPPOSÉE',
                                 'Husband / opposite partner' = 'MARI/PARTENAIRE OPPOSÉ',
                                 'Other opposing people' = 'AUTRES PERSONNES OPPOSÉES',
                                 'Religious prohibitions' = 'INTERDITS RELIGIEUX',
                                 "Doesn't know any method" = 'NE CONNAÎT AUCUNE MÉTHODE',
                                 'Knows no source' = 'NE CONNAÎT AUCUNE SOURCE',
                                 'Health problems' = 'PROBLÈMES DE SANTÉ',
                                 'Fear of side effects' = 'PEUR DES EFFETS SECONDAIRES',
                                 'Not accessible / too far' = 'PAS ACCESSIBLE/TROP LOIN',
                                 'Too expensive' = 'TROP CHER',
                                 'Not practical to use' = 'PAS PRATIQUE À UTILISER',
                                 'Interferes with normal body functions' = 'INTERFÈRE AVEC FONCTIONS NORMALES DU CORPS',
                                 'Other' = 'AUTRE',
                                 "Don't know" = 'NE SAIT PAS',
                                 'No response' = 'NON REPONSE'),
                ster_whynot_type = case_when(
                        as.numeric(ster_whynot) <= 1 ~ "Non married",
                        as.numeric(ster_whynot) <= 5 ~ "Reasons related to fertility",
                        as.numeric(ster_whynot) <= 9 ~ "Opposition to use",
                        as.numeric(ster_whynot) <= 11 ~ "Lack of knowledge",
                        as.numeric(ster_whynot) <= 17 ~ "Reasons related to methods",
                        as.numeric(ster_whynot) == 18 ~ "Other",
                        as.numeric(ster_whynot) == 19 ~ "Don't know"
                        ),
                ster_whynot_type = factor(ster_whynot_type, 
                          levels= c("Non married","Reasons related to fertility",
                                    "Opposition to use","Lack of knowledge",
                                    "Reasons related to methods","Other",
                                    "Don't know"))
)

# Sterilization where get ########################################################

femme <- femme %>% rename(ster_modern_using = cp12,
                          ster_get_hosp_pu = cp13a,
                          ster_get_cent_pu = cp13b,
                          ster_get_clincpf_pu = cp13c,
                          ster_get_clinmo_pu = cp13d,
                          ster_get_agent_pu = cp13e,
                          ster_get_hosp_pr = cp13g,
                          ster_get_clinc_pr = cp13h,
                          ster_get_pharm_pr = cp13i,
                          ster_get_med_pr = cp13j,
                          ster_get_clinmo_pr = cp13k,
                          ster_get_agent_pr = cp13l,
                          ster_get_bout = cp13n,
                          ster_get_reli = cp13o,
                          ster_get_close = cp13p) %>%
        mutate(ster_modern_using = case_when(
                ster_modern_using == 'Oui' ~ TRUE,
                ster_modern_using == 'Non' ~ FALSE,
                TRUE ~ NA
                )
        ) %>% 
        select(-cp13, -cp13f, -cp13m, -cp13x) %>%
        mutate_at(.vars=vars(starts_with("ster_get")),
                  ~ifelse(grepl('[A-Z]', .), TRUE, NA)
        ) 


# Last questions ########################################################

femme <- femme %>% rename(ster_npreg_nster = cp14,
                          ster_future = cp15,
                          ster_whynot_future = cp16,
                          ster_wanted_childs = un1) %>%
        mutate(ster_npreg_nster = case_when(
                ster_npreg_nster == 'Oui' ~ TRUE,
                ster_npreg_nster == 'Non' ~ FALSE,
                TRUE ~ NA),
               ster_future = fct_recode(ster_future,
                                        'Yes' = "Oui",
                                        'No' = 'Non',
                                        'Not sure / Not decided' = 'Pas sur / Pas décidée',
                                        'No reponse' = 'Non reponse'),
               ster_wanted_childs_other = case_when(
                       ster_wanted_childs == 94 ~ "I cannot give the numbers",
                       ster_wanted_childs == 95 ~ "What God gives us"
                               ),
               ster_wanted_childs = ifelse(ster_wanted_childs > 90, NA, ster_wanted_childs)
               )
        
        