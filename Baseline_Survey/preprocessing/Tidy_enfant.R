library(tidyverse)
library(here)
rm(list=ls())
load(here("Baseline_Survey/data/enfant.RData"))

# Remote White Space ########################
dfList <- list(enfant)

dfList <- lapply(dfList, function(x) {
  cols = names(x)[vapply(x, is.character, logical(1))]
  x[,cols] <- lapply(x[,cols], trimws)
  x
} )

enfant = dfList[[1]]
# Administrative data #############################################################

#Changing the name of administrative columns
enfant <- enfant%>% rename(interviewer = ch5,
                          village_code = ch1, 
                          household_number = ch2,
                          child_line = ch3,
                          child_name = ch3a,
                          mother_line = ch4,
                          mother_name = ch4a,
                          team_leader = ch6,
                          interview_day = ch7jj,
                          interview_month = ch7mm,
                          interview_hour_s = ch8hh,
                          interview_min = ch8mn,
                          reserve_section = chstrate,
                          permission = ch9,
                          completeness= ch10,
                          interview_hour_e = ch11hh,
                          interview_min_e = ch11mn,
                          se_had_diarrhea = se1,
                          se_diarrhea_treated = se2,
                          se3_all = se3,
                          se_fever = se4,
                          se_fever_treatment = se5,
                          se6_all = se6,
                          se_cough = se7,
                          se_cough_treatment = se8) %>%
  #Deleting columns about time and others with no info
  select(-ch7aa, -ch10a, -finch,
         -se3nr, -se3f_autre, -se3l_autre,
         -se6nr, -se6f_autre, -se6l_autre, 
         -se9nr, -se9f_autre, -se9l_autre,
         -se9, -23, -46) %>%
  #Translating
  mutate(permission = case_when(
    permission == "Oui" ~ TRUE,
    TRUE ~ FALSE
  ),
  se_had_diarrhea = case_when(
    se_had_diarrhea == "Oui" ~ TRUE,
    TRUE ~ FALSE),
  se_diarrhea_treated = case_when(
    se_diarrhea_treated == "Oui" ~ TRUE,
    TRUE ~ FALSE),
  se_fever = case_when(
    se_fever == "Oui" ~ TRUE,
    TRUE ~ FALSE),
  se_fever_treatment = case_when(
    se_fever_treatment == "Oui" ~ TRUE,
    TRUE ~ FALSE),
  se_cough = case_when(
    se_cough == "Oui" ~ TRUE,
    TRUE ~ FALSE),
  se_cough_treatment = case_when(
    se_cough_treatment == "Oui" ~ TRUE,
    TRUE ~ FALSE),
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

# seek care data #############################################################

enfant <- enfant %>% rename(care_cough_hospital = se9a,
                          care_cough_center = se9b,
                          care_cough_post = se9c,
                          care_cough_comm = se9d,
                          care_cough_clinic = se9e,
                          care_cough_public_other = se9f,
                          care_cough_private_clinic = se9g,
                          care_cough_private_dr = se9h,
                          care_cough_pharmacy = se9i,
                          care_cough_officer = se9j,
                          care_cough_mobile = se9k,
                          care_cough_private_other = se9l,
                          care_cough_family = se9m,
                          care_cough_shop = se9n,
                          care_cough_traditional = se9o,
                          care_cough_other = se9x,
                          care_diarrhea_hospital = se3a,
                          care_diarrhea_center = se3b,
                          care_diarrhea_post = se3c,
                          care_diarrhea_comm = se3d,
                          care_diarrhea_clinic = se3e,
                          care_diarrhea_public_other = se3f,
                          care_diarrhea_private_clinic = se3g,
                          care_diarrhea_private_dr = se3h,
                          care_diarrhea_pharmacy = se3i,
                          care_diarrhea_officer = se3j,
                          care_diarrhea_mobile = se3k,
                          care_diarrhea_private_other = se3l,
                          care_diarrhea_family = se3m,
                          care_diarrhea_shop = se3n,
                          care_diarrhea_traditional = se3o,
                          care_diarrhea_other = se3x,
                          care_fever_hospital = se6a,
                          care_fever_center = se6b,
                          care_fever_post = se6c,
                          care_fever_comm = se6d,
                          care_fever_clinic = se6e,
                          care_fever_public_other = se6f,
                          care_fever_private_clinic = se6g,
                          care_fever_private_dr = se6h,
                          care_fever_pharmacy = se6i,
                          care_fever_officer = se6j,
                          care_fever_mobile = se6k,
                          care_fever_private_other= se6l,
                          care_fever_family = se6m,
                          care_fever_shop = se6n,
                          care_fever_traditional = se6o,
                          care_fever_other = se6x) 

###Change all to True or False
v=c(names(enfant[20:35]),names(enfant[39:54]),names(enfant[58:73]))

enfant[v] <- lapply(enfant[v],function(x){ifelse(x=='', FALSE, TRUE)})

