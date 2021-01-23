library(tidyverse)
library(here)
rm(list=ls())
load(here("Baseline_Survey/data/enfant.RData"))


#Changing the name of administrative columns
enfant <- enfant %>% rename(interviewer = ch5,
                          village_code = ch1, 
                          household_number = ch2,
                          child_line = ch3,
                          mother_line = ch4,
                          team_leader = ch6,
                          reserve_section = chstrate,
                          permission = ch9,
                          completeness= ch10) %>% 
  filter(completeness == "Completé")

#####################################################################
# Boolean ###########################################################
#####################################################################
enfant <-enfant %>% 
        mutate_at(.vars = c("se1", "se2", "se4", 'se5', 'se7', 'se8'),
            ~case_when(
              . == "Oui" ~ TRUE,
              . == "Non" ~ FALSE,
              TRUE ~ NA
            ))

bool_questions = c('se1', "" , "Percentage of children who have had diarrhea in the last two weeks", "Child Health",
                   'se2', "" , "Percentage of children who had diarrhea and for whom they sought treatment", "Child Health",
                   'se4', "" , "Percentage of children who have been ill with fever in the last two weeks", "Child Health",
                   'se5', "" , "Percentage of children who had fever and for whom they sought treatment", "Child Health",
                   'se7', "" , "Percentage of children who have been ill with cough in the last two weeks", "Child Health",
                   'se8', "" , "Percentage of children who had cough and from whom they sought treatment", "Child Health") 

bool_questions = matrix(bool_questions, ncol = 4, byrow = TRUE)

#Function to compute the sum and total for each question in each village
bool_type <- function(df, col){
        #Creating the new dataframe
        df <- df %>% select(village_code, all_of(col)) %>%
                group_by(village_code) %>%
                summarise_all(.funs = list(value = ~ sum(., na.rm = TRUE),
                                           total = ~ sum(!is.na(.))))
        
        return(df)
}

#Apply the bool_type function to each column in bool_questions        
bool_df <- apply(bool_questions, 1, function(x){
        bool_type(enfant, x[1]) %>%
                mutate(question = x[3],
                       topic = x[4])
})

#From listo to dataframe
bool_df <- bind_rows(bool_df) %>%
        select(village_code, topic, question, value, total) %>%
        mutate(type = "Percentage")

####################################################################################
# Categorical several columns ######################################################
####################################################################################
enfant <- enfant %>%
        select(-se3) %>%
        mutate_at(.vars=vars(starts_with("se3")),
          ~case_when(
                  grepl('[A-Z]', .) ~ TRUE,
                  se2 == TRUE ~ FALSE ,
                  TRUE ~ NA
          )) %>%
        select(-se6) %>%
        mutate_at(.vars=vars(starts_with("se6")),
                  ~case_when(
                          grepl('[A-Z]', .) ~ TRUE,
                          se5 == TRUE ~ FALSE ,
                          TRUE ~ NA
                  )) %>%
        select(-se9) %>%
        mutate_at(.vars=vars(starts_with("se9")),
                  ~case_when(
                          grepl('[A-Z]', .) ~ TRUE,
                          se8 == TRUE ~ FALSE ,
                          TRUE ~ NA
                  ))

enfant <- enfant %>%
        mutate(se3_public = case_when(
                se3a == TRUE ~ TRUE,
                se3b == TRUE ~ TRUE,
                se3c == TRUE ~ TRUE,
                se3d == TRUE ~ TRUE,
                se3e == TRUE ~ TRUE,
                se3f == TRUE ~ TRUE,
                se2 == TRUE ~ FALSE,
                TRUE ~ NA),
               se3_private = case_when(
                       se3h == TRUE ~ TRUE,
                       se3i == TRUE ~ TRUE,
                       se3j == TRUE ~ TRUE,
                       se3k == TRUE ~ TRUE,
                       se3l == TRUE ~ TRUE,
                       se2 == TRUE ~ FALSE,
                       TRUE ~ NA
               ),
               se3_other = case_when(
                       se3m == TRUE ~ TRUE,
                       se3n == TRUE ~ TRUE,
                       se3o == TRUE ~ TRUE,
                       se3x == TRUE ~ TRUE,
                       se2 == TRUE ~ FALSE,
                       TRUE ~ NA
               ),
               se6_public = case_when(
                       se6a == TRUE ~ TRUE,
                       se6b == TRUE ~ TRUE,
                       se6c == TRUE ~ TRUE,
                       se6d == TRUE ~ TRUE,
                       se6e == TRUE ~ TRUE,
                       se6f == TRUE ~ TRUE,
                       se5 == TRUE ~ FALSE,
                       TRUE ~ NA),
               se6_private = case_when(
                       se6h == TRUE ~ TRUE,
                       se6i == TRUE ~ TRUE,
                       se6j == TRUE ~ TRUE,
                       se6k == TRUE ~ TRUE,
                       se6l == TRUE ~ TRUE,
                       se5 == TRUE ~ FALSE,
                       TRUE ~ NA
               ),
               se6_other = case_when(
                       se6m == TRUE ~ TRUE,
                       se6n == TRUE ~ TRUE,
                       se6o == TRUE ~ TRUE,
                       se6x == TRUE ~ TRUE,
                       se5 == TRUE ~ FALSE,
                       TRUE ~ NA
               ),
               se9_public = case_when(
                       se9a == TRUE ~ TRUE,
                       se9b == TRUE ~ TRUE,
                       se9c == TRUE ~ TRUE,
                       se9d == TRUE ~ TRUE,
                       se9e == TRUE ~ TRUE,
                       se9f == TRUE ~ TRUE,
                       se8 == TRUE ~ FALSE,
                       TRUE ~ NA),
               se9_private = case_when(
                       se9h == TRUE ~ TRUE,
                       se9i == TRUE ~ TRUE,
                       se9j == TRUE ~ TRUE,
                       se9k == TRUE ~ TRUE,
                       se9l == TRUE ~ TRUE,
                       se8 == TRUE ~ FALSE,
                       TRUE ~ NA
               ),
               se9_other = case_when(
                       se9m == TRUE ~ TRUE,
                       se9n == TRUE ~ TRUE,
                       se9o == TRUE ~ TRUE,
                       se9x == TRUE ~ TRUE,
                       se8 == TRUE ~ FALSE,
                       TRUE ~ NA
               )) 

cat_questions_xc = c("se3a", 1, "Government hospital", "Diarrhea treatment source", "Child Health",
                     "se3b", 1, "Government health center", "Diarrhea treatment source", "Child Health",
                     "se3c", 1, "Government health post", "Diarrhea treatment source", "Child Health",
                     "se3d", 1, "Community health worker (public)", "Diarrhea treatment source", "Child Health",
                     "se3e", 1, "Mobile Clinic/Advanced Strategy (public)", "Diarrhea treatment source", "Child Health",
                     "se3f", 1, "Other public institution", "Diarrhea treatment source", "Child Health",
                     "se3g", 1, "Private hospital/clinic", "Diarrhea treatment source", "Child Health",       
                     "se3h", 1, "Private doctor", "Diarrhea treatment source", "Child Health",
                     "se3i", 1, "Pharmacy", "Diarrhea treatment source", "Child Health",
                     "se3j", 1, "Community health worker (private)", "Diarrhea treatment source", "Child Health",     
                     "se3k", 1, "Mobile clinic (private)", "Diarrhea treatment source", "Child Health",
                     "se3l", 1, "Other private institution", "Diarrhea treatment sourced", "Child Health",
                     "se3m", 1, "Family/Friends", "Diarrhea treatment source", "Child Health",
                     "se3n", 1, "Shop/Market", "Diarrhea treatment source", "Child Health",
                     "se3o", 1, "Traditional practitioner", "Diarrhea treatment source", "Child Health",
                     "se3x", 1, "Other", "Diarrhea treatment source", "Child Health",
                     
                     "se3_private", 2, "Public treatment source", "Diarrhea treatment source type", "Child Health",
                     "se3_public", 2, "Private treatment source", "Diarrhea treatment source type", "Child Health",
                     "se3_other", 2, "Other treatment source", "Diarrhea treatment source type", "Child Health",
                     
                     "se6a", 3, "Government hospital", "Fever treatment source", "Child Health",
                     "se6b", 3, "Government health center", "Fever treatment source", "Child Health",
                     "se6c", 3, "Government health post", "Fever treatment source", "Child Health",
                     "se6d", 3, "Community health worker (public)", "Fever treatment source", "Child Health",
                     "se6e", 3, "Mobile Clinic/Advanced Strategy (public)", "Fever treatment source", "Child Health",
                     "se6f", 3, "Other public institution", "Fever treatment source", "Child Health",
                     "se6g", 3, "Private hospital/clinic", "Fever treatment source", "Child Health",       
                     "se6h", 3, "Private doctor", "Fever treatment source", "Child Health",
                     "se6i", 3, "Pharmacy", "Fever treatment source", "Child Health",
                     "se6j", 3, "Community health worker (private)", "Fever treatment source", "Child Health",     
                     "se6k", 3, "Mobile clinic (private)", "Fever treatment source", "Child Health",
                     "se6l", 3, "Other private institution", "Fever treatment source", "Child Health",
                     "se6m", 3, "Family/Friends", "Fever treatment source", "Child Health",
                     "se6n", 3, "Shop/Market", "Fever treatment source", "Child Health",
                     "se6o", 3, "Traditional practitioner", "Fever treatment source", "Child Health",
                     "se6x", 3, "Other", "Fever treatment source", "Child Health",
                     
                     "se6_private", 4, "Public treatment source", "Fever treatment source type", "Child Health",
                     "se6_public", 4, "Private treatment source", "Fever treatment source type", "Child Health",
                     "se6_other", 4, "Other treatment source", "Fever treatment source type", "Child Health",
                     
                     "se9a", 5, "Government hospital", "Cough treatment source", "Child Health",
                     "se9b", 5, "Government health center", "Cough treatment source", "Child Health",
                     "se9c", 5, "Government health post", "Cough treatment source", "Child Health",
                     "se9d", 5, "Community health worker (public)", "Cough treatment source", "Child Health",
                     "se9e", 5, "Mobile Clinic/Advanced Strategy (public)", "Cough treatment source", "Child Health",
                     "se9f", 5, "Other public institution", "Cough treatment source", "Child Health",
                     "se9g", 5, "Private hospital/clinic", "Cough treatment source", "Child Health",       
                     "se9h", 5, "Private doctor", "Cough treatment source", "Child Health",
                     "se9i", 5, "Pharmacy", "Cough treatment source", "Child Health",
                     "se9j", 5, "Community health worker (private)", "Cough treatment source", "Child Health",     
                     "se9k", 5, "Mobile clinic (private)", "Cough treatment source", "Child Health",
                     "se9l", 5, "Other private institution", "Cough treatment source", "Child Health",
                     "se9m", 5, "Family/Friends", "Cough treatment source", "Child Health",
                     "se9n", 5, "Shop/Market", "Cough treatment source", "Child Health",
                     "se9o", 5, "Traditional practitioner", "Cough treatment source", "Child Health",
                     "se9x", 5, "Other", "Cough treatment source", "Child Health",
                     
                     "se9_private", 6, "Public treatment source", "Cough treatment source type", "Child Health",
                     "se9_public", 6, "Private treatment source", "Cough treatment source type", "Child Health",
                     "se9_other", 6, "Other treatment source", "Cough treatment source type", "Child Health"
)

cat_questions_xc = matrix(cat_questions_xc , ncol = 5, byrow = TRUE)

#Empty list to store the results per group
cat_xc_df <- list()

#Iteration in the groups
for (i in 1:max(cat_questions_xc[,2])){
        #Vector of variables names of this group
        variables = cat_questions_xc[which(cat_questions_xc[,2] == as.character(i)),1]

        #Computing the dataframe for the group
        df <- enfant %>% 
                select(village_code, {{variables}}) %>%
                mutate(aux = rowSums(!is.na(.[,variables]))) %>%
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

#Bolean 
#Save it
saveRDS(bool_df, file = here("Baseline_Survey/preprocessing/enfant_num.rds"))

#Categorical
#Save it
saveRDS(cat_xc_df, file = here("Baseline_Survey/preprocessing/enfant_cat.rds"))
