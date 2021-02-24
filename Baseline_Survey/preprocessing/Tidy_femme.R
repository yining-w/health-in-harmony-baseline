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
                  #~ifelse(grepl('[A-Z]', .), TRUE, NA)) %>%
                  ~ case_when(
                          grepl('[A-Z]', .) ~ TRUE,
                          !is.na(.) ~ FALSE,
                          TRUE ~ NA,
                  )) %>%
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
        select(-cp1x) %>%
        mutate_at(.vars=vars(starts_with("ster")),
                  ~as.character(.)
        ) %>%
        mutate_at(.vars=vars(starts_with("ster")),
                  #funs(case_when(
                  #        . == "Oui, spontané" ~ "Yes, spontaneous",
                  #        . == "Oui, après explication" ~ "Yes, after explanation",
                  #        . == "Non" ~ "No",
                  #        . == "Non reponse" ~ "No response"
                  #)
                  #)
                  ~case_when(
                          . == "Oui, spontané" ~ TRUE,
                          . == "Oui, après explication" ~ TRUE,
                          . == "Non" ~ FALSE,
                          TRUE ~ NA
                          )
                  ) %>%
        #mutate_at(.vars=vars(starts_with("ster")),
        #          ~factor(., levels = c("Yes, spontaneous",
        #                                "Yes, after explanation",
        #                                "No",
        #                                "No response"))
        #)%>%
        rename(ster_other = cp1_autre) %>%
        mutate(ster_other = case_when(
                grepl('[A-Z]', ster_other) ~ TRUE,
                !is.na(ster_other) ~ FALSE ,
                TRUE ~ NA
        )) %>%
        rename(ster_know = cp2) %>%
        mutate(ster_know = case_when(
                ster_know == 'Oui' ~ TRUE,
                ster_know == 'Non' ~ FALSE,
                TRUE ~ NA))
        

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
        select(-cp4, -cp4f, -cp4m, -cp4x, -cp4nr) %>%
        mutate_at(.vars=vars(starts_with("ster_place")),
                  #~ifelse(grepl('[A-Z]', .), TRUE, NA)
                  ~ case_when(
                          grepl('[A-Z]', .) ~ TRUE,
                          !is.na(.) ~ FALSE,
                          TRUE ~ NA,
                  )) %>%
        rename(ster_place_know = cp3,
               ster_place_other_pu = cp4f_autre,
               ster_place_other_pr = cp4m_autre,
               ster_place_other = cp4_autre
        ) %>% 
        mutate_at(.vars = c("ster_place_other_pu", "ster_place_other_pr", "ster_place_other"),
                  ~case_when(
                          grepl('[A-Z]', .) ~ TRUE,
                          !is.na(.) ~ FALSE ,
                          TRUE ~ NA
                  )) %>%
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
                          ster_used_with = cp6m,
                          ster_used_other = cp6_autre) %>%
        select(-cp6, -cp6x, -cp6nr) %>%
        mutate_at(.vars=vars(starts_with("ster_used")),
                  ~ case_when(
                          grepl('[A-Z]', .) ~ TRUE,
                          !is.na(.) ~ FALSE,
                          TRUE ~ NA,
                  )) %>%
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
                          ster_using_with = cp10m,
                          ster_using_other = cp10_autre) %>%
        select(-cp10, -cp10x, -cp10nr) %>%
        mutate_at(.vars=vars(starts_with("ster_using")),
                  ~case_when(
                          grepl('[A-Z]', .) ~ TRUE,
                          !is.na(.) ~ FALSE ,
                          TRUE ~ NA
                  )) %>%
        rename(ster_using = cp9) %>%
        mutate(ster_using = case_when(
                ster_using == 'Oui' ~ TRUE,
                ster_using == 'Non' ~ FALSE,
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

femme <-femme %>% rename(ster_whynot = cp11,
                         ster_whynot_other = cp11_autre) %>%
        mutate(
                ster_whynot = fct_recode(ster_whynot,
                                         'Non married' = 'NON MARIÉE',
                                         'Not sexually active' = 'PAS DE RAP. SEXUELS/RAP. SEXUELS PEU FRÉQUENTS',
                                         'Menopause / hysterectomy' = 'MÉNOPAUSE/HYSTÉRECTOMIE',
                                         'Under-fertile / sterile' = 'SOUS-FÉCONDE/STÉRILE',
                                         'Want as many children as possible'= "VEUT AUTANT D'ENFANTS QUE POSSIBLE",
                                         'Oppose it' = 'ENQUÊTÉE OPPOSÉE',
                                         'Husband / partner' = 'MARI/PARTENAIRE OPPOSÉ',
                                         'Other people oppose' = 'AUTRES PERSONNES OPPOSÉES',
                                         'Religious prohibitions' = 'INTERDITS RELIGIEUX',
                                         "Don't know any method" = 'NE CONNAÎT AUCUNE MÉTHODE',
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
                          ster_get_close = cp13p,
                          ster_get_other_pu = cp13f_autre,
                          ster_get_other_pi = cp13m_autre,
                          ster_get_other = cp13_autre) %>%
        mutate(ster_modern_using = case_when(
                ster_modern_using == 'Oui' ~ TRUE,
                ster_modern_using == 'Non' ~ FALSE,
                TRUE ~ NA
        )
        ) %>% 
        select(-cp13, -cp13f, -cp13m, -cp13x, -cp13nr) %>%
        mutate_at(.vars=vars(starts_with("ster_get")),
                  ~case_when(
                          grepl('[A-Z]', .) ~ TRUE,
                          !is.na(.) ~ FALSE ,
                          TRUE ~ NA
                  ))


# Last questions ########################################################

femme <- femme %>% rename(ster_npreg_nster = cp14,
                          ster_future = cp15,
                          ster_whynot_future = cp16,
                          ster_whynot_future_other = cp16_autre,
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
        ) %>%
        mutate(
                ster_whynot_future = fct_recode(ster_whynot_future,
                                         'Non married' = 'NON MARIÉE',
                                         'Not sexually active' = 'PAS DE RAP. SEXUELS/RAP. SEXUELS PEU FRÉQUENTS',
                                         'Menopause / hysterectomy' = 'MÉNOPAUSE/HYSTÉRECTOMIE',
                                         'Under-fertile / sterile' = 'SOUS-FÉCONDE/STÉRILE',
                                         'Want as many children as possible'= "VEUT AUTANT D'ENFANTS QUE POSSIBLE",
                                         'Oppose it' = 'ENQUÊTÉE OPPOSÉE',
                                         'Husband / opposite partner' = 'MARI/PARTENAIRE OPPOSÉ',
                                         'Other people oppose' = 'AUTRES PERSONNES OPPOSÉES',
                                         'Religious prohibitions' = 'INTERDITS RELIGIEUX',
                                         "Don't know any method" = 'NE CONNAÎT AUCUNE MÉTHODE',
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
                ster_whynot_future_type = case_when(
                        as.numeric(ster_whynot_future) <= 1 ~ "Non married",
                        as.numeric(ster_whynot_future) <= 5 ~ "Reasons related to fertility",
                        as.numeric(ster_whynot_future) <= 9 ~ "Opposition to use",
                        as.numeric(ster_whynot_future) <= 11 ~ "Lack of knowledge",
                        as.numeric(ster_whynot_future) <= 17 ~ "Reasons related to methods",
                        as.numeric(ster_whynot_future) == 18 ~ "Other",
                        as.numeric(ster_whynot_future) == 19 ~ "Don't know"
                ),
                ster_whynot_future_type = factor(ster_whynot_future_type, 
                                          levels= c("Non married","Reasons related to fertility",
                                                    "Opposition to use","Lack of knowledge",
                                                    "Reasons related to methods","Other",
                                                    "Don't know"))
        )

#####################################################################
# Boolean ###########################################################
#####################################################################
#Columns need to be boolean (TRUE, FALSE, NA) only
#NA means no response
#Changing one with three possible answers to only two
femme <- femme %>% mutate(ster_future = case_when(
        ster_future == 'Yes' ~ TRUE,
        ster_future == 'No' ~ FALSE,
        ster_future == 'Not sure / Not decided' ~ FALSE,
        TRUE ~ NA))

#Matrix with the questions/KPIs. The second column indicates the divisor
bool_questions = c("birth", "total", "Percentage of women who gave birth", "Fertility",
                   "children_household", "birth", "Percentage of women with sons or daughters at home", "Fertility",
                   "children_not_household", "birth", "Percentage of women with sons or daughters not at home", "Fertility",
                   "children_died", "birth", "Percentage of women whose son or daughter died after birth", "Child Mortality",
                   "birth_2y", "birth", "Percentage of women who gave birth in the last 2 years", "Fertility",
                   "ster_know", "total", "Percentage of women who know about at least one sterilization mean or method", "Contraception",
                   "ster_place_know", "total", "Percentage of women who know at least one place to get a family planning method", "Contraception",
                   "ster_used", "total", "Percentage of women who has used a family planning method", "Contraception",
                   "ster_preg", "total", "Percentage of women who are currently pregnant", "Fertility",
                   "ster_using", "ster_preg", "Percentage of not pregnant women who are using a family planning method", "Contraception",
                   "ster_future", "total", "Percentage of women who plan to use a family planning method in the future", "Contraception")
bool_questions = matrix(bool_questions, ncol = 4, byrow = TRUE)

#Function to compute the sum and total for each question in each village
bool_type <- function(df, col, divisor){
        #Only surveys with status complete
        df <- df %>% filter(completeness == "Complete")
        
        #Depending on the divisor, filter the dataframe
        if (divisor == 'birth'){
                df <- df %>% filter(birth == TRUE)
        }
        else if (divisor == 'ster_preg'){
                df <- df %>% filter(ster_preg == FALSE)
        }
        
        #Creating the new dataframe
        df <- df %>% select(village_code, all_of(col)) %>%
                group_by(village_code) %>%
                summarise_all(.funs = list(value = ~ sum(., na.rm = TRUE),
                                           total = ~ sum(!is.na(.))))
        
        return(df)
        
}

#Apply the bool_type function to each column in bool_questions        
bool_df <- apply(bool_questions, 1, function(x){
        bool_type(femme, x[1], x[2]) %>%
                mutate(question = x[3],
                       topic = x[4])
})

#From listo to dataframe
bool_df <- bind_rows(bool_df) %>%
        select(village_code, topic, question, value, total) %>%
        mutate(type = "Percentage")

############################################################################
# Average ##################################################################
############################################################################
#COlumns need to be numerical
#NA means no response (be careful because some NA need to be change to 0)
#Fill some NAs. If there is a birth, some values are not NA, there are 0.
femme <- femme %>% 
        mutate(
                sons_household = case_when(
                        birth == TRUE & !is.na(sons_household) ~ as.integer(sons_household),
                        birth == TRUE & is.na(sons_household) ~ 0L,
                        birth == FALSE ~ NA_integer_,
                        TRUE ~ NA_integer_),
                daughters_household = case_when(
                        birth == TRUE & !is.na(daughters_household) ~ as.integer(daughters_household),
                        birth == TRUE & is.na(daughters_household) ~ 0L,
                        birth == FALSE ~ NA_integer_,
                        TRUE ~ NA_integer_),
                sons_not_household = case_when(
                        birth == TRUE & !is.na(sons_not_household) ~ as.integer(sons_not_household),
                        birth == TRUE & is.na(sons_not_household) ~ 0L,
                        birth == FALSE ~ NA_integer_,
                        TRUE ~ NA_integer_),
                daughters_not_household = case_when(
                        birth == TRUE & !is.na(daughters_not_household) ~ as.integer(daughters_not_household),
                        birth == TRUE & is.na(daughters_not_household) ~ 0L,
                        birth == FALSE ~ NA_integer_,
                        TRUE ~ NA_integer_),
                sons_died = case_when(
                        birth == TRUE & !is.na(sons_died) ~ as.integer(sons_died),
                        birth == TRUE & is.na(sons_died) ~ 0L,
                        birth == FALSE ~ NA_integer_,
                        TRUE ~ NA_integer_),
                daughters_died = case_when(
                        birth == TRUE & !is.na(daughters_died) ~ as.integer(daughters_died),
                        birth == TRUE & is.na(daughters_died) ~ 0L,
                        birth == FALSE ~ NA_integer_,
                        TRUE ~ NA_integer_)
        )

#Matrix with the numeric questions
avg_questions = c("sons_household", "Average number of sons in the household", "Fertility",
                  "daughters_household", "Average number of daughters in the household", "Fertility",
                  "sons_not_household", "Average number of sons not in the household", "Fertility",
                  "daughters_not_household", "Average number of daughters not in the household", "Fertility",
                  "sons_died", "Average number of sons who died", "Child Mortality",
                  "daughters_died", "Average number of sons who died", "Child Mortality",
                  "ster_child_num", "Average number of children respondent wants" , "Contraception")
avg_questions = matrix(avg_questions, ncol = 3, byrow = TRUE)
#Transform the matrix to dataframe
avg_questions = as.data.frame(avg_questions)
names(avg_questions) <- c("variable", "question", "topic")

#Calculating the total column
avg_df_total <- femme %>% filter(completeness == "Complete") %>% 
        select(village_code, avg_questions[,1]) %>%
        group_by(village_code) %>%
        summarise_all(~sum(!is.na(.))) %>%
        gather(key = "variable", value = "total", -village_code)

#Calculating the average column
avg_df_avg <- femme %>% filter(completeness == "Complete") %>%
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
cat_question_1c = c("birth_place", "Birth place of last child", "Fertility",
                    "birth_place_type",  "Birth place type of last child", "Fertility",
                    "birth_assist_type", "Last type of birth assistance recieved", "Fertility",
                    "ster_whynot", "Main reason to not use family planning method", "Contraception",
                    "ster_whynot_type", "Type of reason to not use family planning method", "Contraception",
                    "ster_whynot_future", "Main reason to not use family planning method in the future", "Contraception",
                    "ster_whynot_future_type", "Type of reason to not use family planning method in the future", "Contraception")
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
        cat_1c_type(femme, x[1]) %>%
                mutate(question = x[2],
                       topic = x[3])
})

#From listo to dataframe
cat_1c_df <- bind_rows(cat_1c_df) %>%
        select(village_code, topic, question, categories, sum, total)

####################################################################################
# Categorical several columns ######################################################
####################################################################################
#Columns need to be boolean (TRUE, FALSE, NA)
#NA means no response

#List with categorical questions that are in more than 1 column
#The second column indicates to which group of question they belong
cat_questions_xc = c("birth_assist_med", 1, "Doctor", "Assistance during birth" ,"Fertility",
                  "birth_assist_nurse", 1, "Nurse / Midwife", "Assistance during birth" ,"Fertility",
                  "birth_assist_assist", 1, "Medical assistant", "Assistance during birth" ,"Fertility",
                  "birth_assist_trad", 1, "Traditional delivery", "Assistance during birth" ,"Fertility",
                  "birth_assist_comm", 1, "Community health officer", "Assistance during birth" ,"Fertility",
                  "birth_assist_close", 1, "Relative / Friend", "Assistance during birth" ,"Fertility",
                  "birth_assist_other", 1, "Other", "Assistance during birth" ,"Fertility",
                  "birth_assist_noone", 1, "Nobody", "Assistance during birth" ,"Fertility",
                  #
                  "ster_f_ster", 2, "Female sterilization", "Familiarity with family planning methods", "Contraception",
                  "ster_m_ster", 2, "Male sterilization", "Familiarity with family planning methods", "Contraception",
                  "ster_iud", 2, "IUD", "Familiarity with family planning methods", "Contraception",
                  "ster_inje", 2, "Injectables", "Familiarity with family planning methods", "Contraception",
                  "ster_impl", 2, "Implants", "Familiarity with family planning methods", "Contraception",
                  "ster_pill", 2, "Pills", "Familiarity with family planning methods", "Contraception",
                  "ster_m_cond", 2, "Condoms", "Familiarity with family planning methods", "Contraception",
                  "ster_f_cond", 2, "Female condoms", "Familiarity with family planning methods", "Contraception",
                  "ster_morn", 2, "Morning after pill", "Familiarity with family planning methods", "Contraception",
                  "ster_fixe", 2, "Fixed day method / collar", "Familiarity with family planning methods", "Contraception",
                  "ster_amen", 2, "Amenorrhea and breastfeeding method", "Familiarity with family planning methods", "Contraception",
                  "ster_rhyt", 2, "Rhythm", "Familiarity with family planning methods", "Contraception",
                  "ster_with", 2, "Withdrawal", "Familiarity with family planning methods", "Contraception",
                  "ster_other", 2, "Other", "Familiarity with family planning methods", "Contraception",
                  #
                  "ster_place_hosp_pu", 3, "Public hospital", "Place to get information about family planning", "Contraception",
                  "ster_place_cent_pu", 3, "Public health center", "Place to get information about family planning", "Contraception",
                  "ster_place_clincpf_pu", 3, "Public clinic", "Place to get information about family planning", "Contraception",
                  "ster_place_clinmo_pu", 3, "Public mobile clinic", "Place to get information about family planning", "Contraception",
                  "ster_place_agent_pu", 3, "Public health agent", "Place to get information about family planning", "Contraception",
                  "ster_place_hosp_pr", 3, "Private hospital", "Place to get information about family planning", "Contraception",
                  "ster_place_clinc_pr", 3, "Private clinic", "Place to get information about family planning", "Contraception",
                  "ster_place_pharm_pr", 3, "Pharmacie", "Place to get information about family planning", "Contraception",
                  "ster_place_med_pr", 3, "Private doctor", "Place to get information about family planning", "Contraception",
                  "ster_place_clinmo_pr", 3, "Private mobile clinic", "Place to get information about family planning", "Contraception",
                  "ster_place_agent_pr", 3, "Private health agent", "Place to get information about family planning", "Contraception",
                  "ster_place_bout", 3, "Boutique", "Place to get information about family planning", "Contraception",
                  "ster_place_reli", 3, "Religious institution", "Place to get information about family planning", "Contraception",
                  "ster_place_close", 3, "Relative / Friend", "Place to get information about family planning", "Contraception",
                  "ster_place_other_pu", 3, "Other public place", "Place to get information about family planning", "Contraception",
                  "ster_place_other_pr", 3, "Other private place", "Place to get information about family planning", "Contraception",
                  "ster_place_other", 3, "Other", "Place to get information about family planning", "Contraception",
                  #
                  "ster_used_f_ster", 4, "Female sterilization", "Family planning methods used", "Contraception",
                  "ster_used_m_ster", 4, "Male sterilization", "Family planning methods used", "Contraception",  
                  "ster_used_iud", 4, "IUD", "Family planning methods used", "Contraception",
                  "ster_used_inje", 4, "Injectables", "Family planning methods used", "Contraception",
                  "ster_used_impl", 4, "Implants", "Family planning methods used", "Contraception",
                  "ster_used_pill", 4, "Pills", "Family planning methods used", "Contraception",
                  "ster_used_m_cond", 4, "Condoms", "Family planning methods used", "Contraception",
                  "ster_used_f_cond", 4, "Female condoms", "Family planning methods used", "Contraception",
                  "ster_used_morn", 4, "Morning after pill", "Family planning methods used", "Contraception",
                  "ster_used_fixe", 4, "Fixed day method / collar", "Family planning methods used", "Contraception",
                  "ster_used_amen", 4, "Amenorrhea and breastfeeding method", "Family planning methods used", "Contraception",         
                  "ster_used_rhyt", 4, "Rhythm", "Family planning methods used", "Contraception",
                  "ster_used_with", 4, "Withdrawal", "Family planning methods used", "Contraception",
                  "ster_used_other", 4, "Other", "Family planning methods used", "Contraception",
                  #
                  "ster_using_f_ster", 5, "Female sterilization", "Current family planning method", "Contraception",
                  "ster_using_m_ster", 5, "Male sterilization", "Current family planning method", "Contraception",  
                  "ster_using_iud", 5, "IUD", "Current family planning method", "Contraception",
                  "ster_using_inje", 5, "Injectables", "Current family planning method", "Contraception",
                  "ster_using_impl", 5, "Implants", "Current family planning method", "Contraception",
                  "ster_using_pill", 5, "Pills", "Current family planning method", "Contraception",
                  "ster_using_m_cond", 5, "Condoms", "Current family planning method", "Contraception",
                  "ster_using_f_cond", 5, "Female condoms", "Current family planning method", "Contraception",
                  "ster_using_morn", 5, "Morning after pill", "Current family planning method", "Contraception",
                  "ster_using_fixe", 5, "Fixed day method / collar", "Current family planning method", "Contraception",
                  "ster_using_amen", 5, "Amenorrhea and breastfeeding", "Current family planning method used", "Contraception",         
                  "ster_using_rhyt", 5, "Rhythm", "Current family planning method", "Contraception",
                  "ster_using_with", 5, "Withdrawal", "Current family planning method", "Contraception",
                  "ster_using_other", 5, "Other", "Current family planning method", "Contraception",
                  #
                  "ster_get_hosp_pu", 6, "Public hospital", "Current place used to get family planning method", "Contraception",
                  "ster_get_cent_pu", 6, "Public health center", "Current place used to get family planning method", "Contraception",
                  "ster_get_clincpf_pu", 6, "Public clinic", "Current place used to get family planning method", "Contraception",
                  "ster_get_clinmo_pu", 6, "Public mobile clinic", "Current place used to get family planning method", "Contraception",
                  "ster_get_agent_pu", 6, "Public health agent", "Current place used to get family planning method", "Contraception",
                  "ster_get_hosp_pr", 6, "Private hospital", "Current place used to get family planning method", "Contraception",
                  "ster_get_clinc_pr", 6, "Private clinic", "Current place used to get family planning method", "Contraception",       
                  "ster_get_pharm_pr", 6, "Pharmacie", "Current place used to get family planning method", "Contraception",
                  "ster_get_med_pr", 6, "Private doctor", "Current place used to get family planning method", "Contraception",
                  "ster_get_clinmo_pr", 6, "Private mobile clinic", "Current place used to get family planning method", "Contraception",     
                  "ster_get_agent_pr", 6, "Private health agent", "Current place used to get family planning method", "Contraception",
                  "ster_get_bout", 6, "Boutique", "Current place used to get family planning method", "Contraception",
                  "ster_get_reli", 6, "Religious institution", "Current place used to get family planning method", "Contraception",
                  "ster_get_close", 6, "Relative / Friend", "Current place used to get family planning method", "Contraception",
                  "ster_get_other_pu", 6, "Other public place", "Current place used to get family planning method", "Contraception",
                  "ster_get_other_pi", 6, "Other private place", "Current place used to get family planning method", "Contraception",      
                  "ster_get_other", 6, "Other", "Current place used to get family planning method", "Contraception"
)
cat_questions_xc = matrix(cat_questions_xc , ncol = 5, byrow = TRUE)

#Empty list to store the results per group
cat_xc_df <- list()

#Iteration in the groups
for (i in 1:max(cat_questions_xc[,2])){
        #Vector of variables names of this group
        variables = cat_questions_xc[which(cat_questions_xc[,2] == as.character(i)),1]
        #Vector of variables names, not including the "other" option
        #"other" columns sometimes does not have NA where is needed 
        variables_xc_other = grep("other", cat_questions_xc[which(cat_questions_xc[,2] == as.character(i)),1])
        variables_xc_other = variables[-variables_xc_other]
        
        #Computing the dataframe for the group
        df <- femme %>% filter(completeness == "Complete") %>%
                select(village_code, {{variables}}) %>%
                mutate(aux = rowSums(!is.na(.[,variables_xc_other]))) %>%
                filter(aux > 0) %>%
                select(-aux) %>%
                gather(key = "variable",
                       value = "answer",
                       na.rm = FALSE,
                       -village_code) %>%
                mutate(answer = ifelse(!is.na(answer), answer, FALSE)) %>%
                group_by(village_code, variable) %>%
                summarise(sum = sum(answer),
                          total = sum(!is.na(answer)))
        
        #Store the dataframe in the list
        cat_xc_df[[i]]<-df
}

#From list to dataframe
cat_xc_df <- bind_rows(cat_xc_df)

#Change the question matrix to dataframe
cat_questions_xc = as.data.frame(cat_questions_xc)
names(cat_questions_xc) <- c("variable","id","categories","question","topic")

#Add info of the columns to the main dataframe
cat_xc_df <- cat_xc_df %>% left_join(cat_questions_xc, by = "variable") %>%
        select(-id, -variable) %>%
        select(village_code, topic, question, categories, sum, total)


###################################################################################
# Final changes ###################################################################
###################################################################################
#rbind both categorical dataframes
df_cat <- rbind(cat_1c_df, cat_xc_df)
#Save it
saveRDS(df_cat, file = here("Baseline_Survey/preprocessing/femme_cat.rds"))

#rbind boolean df and average df
df_num <- rbind(bool_df, avg_df)
#Save it
saveRDS(df_num, file = here("Baseline_Survey/preprocessing/femme_num.rds"))
