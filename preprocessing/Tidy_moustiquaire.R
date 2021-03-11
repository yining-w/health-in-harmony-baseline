library(tidyverse)
library(fastDummies)
library(here)
rm(list=ls())
load("../data/MOUSTIQUAIRE.RData")

# Administrative data #############################################################
moustiquaire <- moustiquaire %>% select(-contains(c("autre", "nr")))

moustiquaire <- moustiquaire %>% select(village_code = hh1, 
                                        household_number = hh2,
                                        type = tn5,
                                        source = tn10,
                                        source_other = tn12,
                                        contains("tn15_"),
                                        contains("tn17"),
                                        tna8ba,
                                        contains("tn18"),
                                        tn4) %>%
        mutate(type = as.character(type),
               type = case_when(type == 'NON REPONSE' ~ "Other",
                                type == 'NSP MARQUE' ~ "Other",
                                type == 'AUTRE TYPE' ~ "ITN",
                                type == 'NSP MARQUE /TYPE' ~ "ITN",
                                !is.na(type) ~ "ITN"),
               type = factor(type, 
                             levels=c("ITN",
                                      "Other")),
               source = as.character(source),
               source_other = as.character(source_other),
               source = case_when(source == "NON" ~ source_other,
                                  source == "NSP" ~ source_other,
                                  TRUE ~ source),
               source = case_when(source == 'OUI, CAMPAGNE MID' ~ 'Mass distribution campaign',
                                  source == 'OUI, CPN' ~ 'Prenatal care visit',
                                  source == 'OUI, VACCINATION' ~ 'Vaccination visit',
                                  source == 'CENTRE DE SANTE PUBLIC' ~ 'Health facility-Government',
                                  source == 'CENTRE DE SANTE PRIVE' ~ 'Health facility-Private',
                                  source == 'BOUTIQUE / MARCHE / RUE' ~ 'Shop / Market / Street',
                                  source == 'AGENT DE SANTE COMMUNAUTAIRE' ~ 'Community health worker',
                                  source == 'AUTRE' ~ 'Other',
                                  source == 'NSP' ~ 'DK / Missing'),
               source = factor(source,
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
        select(-source_other) %>%
        mutate_at(.vars = c("tn17a", "tn17b", "tn17c", 'tn17x', "tn18a"),
                  ~case_when(
                          . == "Oui" ~ TRUE,
                          . == "Non" ~ FALSE,
                          TRUE ~ NA
                  )) %>%
        mutate_at(.vars = c("tna8ba", "tn18bb", "tn18bc", 'tn18bd', "tn18bx"),
                  ~case_when(
                          grepl('[A-Z]', .) ~ TRUE,
                          tn18a == TRUE ~ FALSE ,
                          TRUE ~ NA
                          )) %>%
        select(-tn18b)

# Creating table per household ##################################################
#Reading menage
load(here("Baseline_Survey/data/MENAGE.RData"))
menage <- menage %>% filter(hh46 == "Completé") %>%
        select(village_code = hh1,
               household_number = hh2,
               household_members = hh48)

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

#Adding education to menage
menage <- menage %>% left_join(tableau_de_menage, 
                               by = c("village_code",
                                      "household_number"))


moustiquaire_household <- moustiquaire %>% select(-contains("tn15_"),
                                                  -source, -tn4)

moustiquaire_household <- dummy_cols(moustiquaire_household,
                                     select_columns = "type",
                                     remove_selected_columns = TRUE) %>%
        group_by(village_code, household_number, tn17a,
                 tn17b, tn17c, tn17x, tn18a, tna8ba, 
                 tn18bb, tn18bc, tn18bd, tn18bx) %>%
        summarise_all(sum)

moustiquaire_household <- menage %>% left_join(moustiquaire_household,
                                               by = c("village_code",
                                                      "household_number")) %>%
        replace_na(list(type_ITN = 0, type_Other = 0)) %>%
        mutate(net_count = type_ITN + type_Other,
               have_net = ifelse(net_count > 0, TRUE, FALSE))


# Creating table per net ########################################################

moustiquaire_net <- moustiquaire %>% 
        left_join(menage, by = c("village_code",
                                 "household_number")) %>%
        mutate(count = rowSums(!is.na(select(.,tn15_1:tn15_8))),
               used = ifelse(count>0, TRUE, FALSE))

#Creating table who splet under #####################################################

#Adding who slept under
load(here("Baseline_Survey/data/TABLEAU_DE_MENAGE.RData"))
tableau_de_menage = tableau_de_menage %>%
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
        ) %>%
        left_join(menage, by = c("village_code",
                                 "household_number"))

aux_moustiquaire <- moustiquaire_net %>%
        select(village_code,
               household_number,
               type,
               contains("tn15_")) %>%
        gather(key = "aux",
               value = "line_number",
               tn15_1:tn15_8) %>%
        select(-"aux")

moustiquaire_person <- tableau_de_menage %>% left_join(aux_moustiquaire,
                                                       by = c("village_code",
                                                              "household_number",
                                                              "line_number")) %>%
        left_join(moustiquaire_household[,c("village_code","household_number","type_ITN")], 
                  by = c("village_code",
                         "household_number")) %>%
        filter(type_ITN > 0) %>%
        select(-type_ITN)


rm(list = c("aux_moustiquaire","tableau_de_menage", "menage", "moustiquaire"))

##### Cleaning Tables ##########################################################

#####################################################################
# Boolean ###########################################################
#####################################################################

moustiquaire_net <- moustiquaire_net %>% 
        mutate(is_ITN = case_when(
                type == "ITN" ~ TRUE,
                !is.na(type) ~ FALSE,
                TRUE ~ NA
        ))

moustiquaire_person <- moustiquaire_person %>%
        mutate(under_net = case_when(
                !is.na(type) ~ TRUE,
                TRUE ~ FALSE),
               under_ITN = case_when(
                       type == "ITN" ~ TRUE,
                       TRUE ~ FALSE
               ))

bool_questions = c("have_net", "moustiquaire_household", "Percentage of households with mosquitoes net", "Possession and use of mosquito nets",
                   "tn18a", "moustiquaire_household", "Percentage households with other ways to avoid mosquitoes", "Possession and use of mosquito nets",
                   "is_ITN", "moustiquaire_net", "Percentage of ITN mosquito nets", "Possession and use of mosquito nets",
                   "used", "moustiquaire_net", "Percentage of mosquito nets used during sleep", "Possession and use of mosquito nets",
                   "under_net", "moustiquaire_person", "Percentage of people who slept under a mosquito net last night", "Possession and use of mosquito nets",
                   "under_ITN", "moustiquaire_person", "Percentage of people who slept under a mosquito net (ITN) last night", "Possession and use of mosquito nets")
bool_questions = matrix(bool_questions, ncol = 4, byrow = TRUE)


#Function to compute the sum and total for each question in each village
bool_type <- function(col, df){
        
        if (df == "moustiquaire_household"){df <- moustiquaire_household}
        else if (df == "moustiquaire_net"){df <- moustiquaire_net}
        else if (df == "moustiquaire_person"){df <- moustiquaire_person}
        
        #Creating the new dataframe
        df <- df %>% select(village_code, all_of(col)) %>%
                group_by(village_code) %>%
                summarise_all(.funs = list(value = ~ sum(., na.rm = TRUE),
                                           total = ~ sum(!is.na(.))))
        
        return(df)
        
}

#Apply the bool_type function to each column in bool_questions        
bool_df <- apply(bool_questions, 1, function(x){
        bool_type(x[1], x[2]) %>%
                mutate(question = x[3],
                       topic = x[4])
})

#From listo to dataframe
bool_df <- bind_rows(bool_df) %>%
        select(village_code, topic, question, value, total) %>%
        mutate(type = "Percentage")


#####################################################################
# Average ###########################################################
#####################################################################

moustiquaire_net <- moustiquaire_net %>%
        mutate(tn4 = ifelse(tn4 > 90, NA_integer_, tn4))

avg_questions = c("net_count", "moustiquaire_household", "Average number of mosquito net per household", "Possession and use of mosquito nets",
                  "tn4", "moustiquaire_net", "Average age of the mosquito nets", "Possession and use of mosquito nets")

avg_questions = matrix(avg_questions, ncol = 4, byrow = TRUE)

avg_type <- function(df, col){
        
        if (df == "moustiquaire_household"){df <- moustiquaire_household}
        else if (df == "moustiquaire_net"){df <- moustiquaire_net}
        else if (df == "moustiquaire_person"){df <- moustiquaire_person}
        
        df <- df %>% select(village_code, all_of(col)) %>%
                group_by(village_code) %>%
                summarise_all(.funs = list(value = ~ mean(., na.rm = TRUE),
                                           total = ~ sum(!is.na(.))))

}

#Apply the bool_type function to each column in bool_questions        
avg_df <- apply(avg_questions, 1, function(x){
        avg_type(x[2], x[1]) %>%
                mutate(question = x[3],
                       topic = x[4],
                       type = "Average")
})

avg_df <- bind_rows(avg_df) %>%
        select(village_code, topic, question, value, total, type)


#####################################################################
# Cat 1 col #########################################################
#####################################################################

cat_question_1c = c("source", "moustiquaire_net", "Source of mosquito nets", "Possession and use of mosquito nets")
cat_question_1c = matrix(cat_question_1c , ncol = 4, byrow = TRUE)

#Function to create the dataframe
cat_1c_type <- function(df, col){
        
        if (df == "moustiquaire_household"){df <- moustiquaire_household}
        else if (df == "moustiquaire_net"){df <- moustiquaire_net}
        else if (df == "moustiquaire_person"){df <- moustiquaire_person}
        
        #Computes the sum and total per village and category
        df_one <- df %>%
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
        cat_1c_type(x[2], x[1]) %>%
                mutate(question = x[3],
                       topic = x[4])
})

#From listo to dataframe
cat_1c_df <- bind_rows(cat_1c_df) %>%
        select(village_code, topic, question, categories, sum, total)

####################################################################################
# Categorical several columns ######################################################
####################################################################################

cat_questions_xc = c("tn17a", 1, "Scare away mosquitoes when sleeping", "Reason to have mosquito net" ,"Possession and use of mosquito nets",
                     "tn17b", 1, "Fishing", "Reason to have mosquito net" ,"Possession and use of mosquito nets",
                     "tn17c", 1, "Prevent poultry from going into the garden", "Reason to have mosquito net" ,"Possession and use of mosquito nets",
                     "tn17x", 1, "Other", "Reason to have mosquito net", "Possession and use of mosquito nets",
                     #
                     "tna8ba", 2, "Spray", "Other ways to avoid mosquitoes", "Possession and use of mosquito nets",
                     "tn18bb", 2, "Remove puddles of water", "Other ways to avoid mosquitoes", "Possession and use of mosquito nets",
                     "tn18bc", 2, "Remove bushes", "Other ways to avoid mosquitoes", "Possession and use of mosquito nets",
                     "tn18bd", 2, "Dissemination of mosquito repellents", "Other ways to avoid mosquitoes", "Possession and use of mosquito nets",
                     "tn18bx", 2, "Other", "Other ways to avoid mosquitoes", "Possession and use of mosquito nets"
)


cat_questions_xc = matrix(cat_questions_xc , ncol = 5, byrow = TRUE)

#Empty list to store the results per group
cat_xc_df <- list()

#Iteration in the groups
for (i in 1:max(cat_questions_xc[,2])){
        #Vector of variables names of this group
        variables = cat_questions_xc[which(cat_questions_xc[,2] == as.character(i)),1]
        
        #Computing the dataframe for the group
        df <- moustiquaire_household %>% 
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
#rbind both categorical dataframes
df_cat <- rbind(cat_1c_df, cat_xc_df)
#Save it
saveRDS(df_cat, file = "../madagascar_app/data/moustiquaire_cat.rds")

#rbind boolean df and average df
df_num <- rbind(bool_df, avg_df)
#Save it
saveRDS(df_num, file = "../madagascar_app/data/moustiquaire_num.rds")