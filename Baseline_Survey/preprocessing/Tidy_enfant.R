library(tidyverse)
library(here)
rm(list=ls())
load(here("GitHub/health-in-harmony-baseline-/Baseline_Survey/data/enfant.RData"))
# Remote White Space ########################
dfList <- list(enfant)

dfList <- lapply(dfList, function(x) {
  cols = names(x)[vapply(x, is.character, logical(1))]
  x[,cols] <- lapply(x[,cols], trimws)
  x
} )

enfant = dfList[[1]]
# Administrative data #############################################################
enfant <- enfant %>% select(-contains(c("autre", "nr")))
#Changing the name of administrative columns
enfant <- enfant %>% rename(hh_interviewer = ch5,
                          hh_village_code = ch1, 
                          hh_household_number = ch2,
                          hh_child_line = ch3,
                          hh_child_name = ch3a,
                          hh_mother_line = ch4,
                          hh_mother_name = ch4a,
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
                          se_fever = se4,
                          se_fever_treatment = se5,
                          se_cough = se7,
                          se_cough_treatment = se8) %>%
  #Deleting columns about time and others with no info
  select(-ch7aa, -ch10a, -finch, -se9, -se6, -se3) 

enfant$completeness = case_when(
    enfant$completeness == "Completé" ~ TRUE,
    enfant$completeness == "Pas à la maison" ~ FALSE
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

## Make numeric
enfant[] <- lapply(enfant, type.convert, as.is = TRUE)

enfant = enfant %>% mutate_at(.vars=vars(starts_with("care_")),
                              ~ifelse(grepl('OUI', ., ignore.case=TRUE), TRUE, FALSE))


v=c(names(enfant[20:35]),names(enfant[39:54]),names(enfant[58:73])) ## check all that apply

enfant[v] <- lapply(enfant[v],function(x){ifelse(x=='', FALSE, TRUE)})

a=c(names(enfant[14]), names(enfant[18:19]), names(enfant[37:38]), names(enfant[56:57])) ##yes or no
enfant[a] <- lapply(enfant[a],function(x){ifelse(x=='Non', 0, 1)})

enfant[is.na(enfant)] <- 0

##Remove unused variables
enfant =
  enfant %>% select(c(-household_number, -child_line, -mother_line, -team_leader, -interview_day,
                    -interview_hour_s, -interview_min, -interview_hour_e, -interview_min_e))

obs_enfant = enfant %>% group_by(village_code) %>% tally()
summary_enfant = enfant %>% group_by(reserve_section, village_code) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE)

summary_enfant = merge(summary_enfant, obs_enfant, by = "village_code")

##COME BACK TO THIS
#test= 
#  for (i in 3:58) {
#  summary_enfant[i]/summary_enfant$n
#}

#write.csv(summary_efnant,)
