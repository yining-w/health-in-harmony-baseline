library(tidyverse)
library(here)

load(here("Baseline_Survey/data/TABLEAU_DE_MENAGE.RData"))

tableau_de_menage <- tableau_de_menage %>% 
                        rename(intervier = hh3,
                          village_code = hh1, 
                          household_number = hh2,
                          reserve_section = hhstrate,
                          team_leader = hh4)

#####################################################################
# Boolean ###########################################################
#####################################################################

tableau_de_menage <-tableau_de_menage %>% 
        mutate(hl8 = ifelse(!is.na(hl8), TRUE, FALSE),
               hl7 = case_when(
                       hl4 == "Féminin" & !is.na(hl7) ~ TRUE,
                       hl4 == "Féminin" & is.na(hl7) ~ FALSE,
                       TRUE ~ NA
               )) %>%
        mutate_at(.vars = c("hl10", "hl14", "hl15"),
                  ~case_when(
                          . == "Oui" ~ TRUE,
                          . == "Non" ~ FALSE,
                          TRUE ~ NA
                  ))
        
#Matrix with the questions/KPIs. The second column indicates the divisor
bool_questions = c("hl7" , "women", "Percentage of women aged 15-49", "Socio-demographic characteristics of the  interviewees",
                   "hl8", "total", "Percentage of people under 5 years", "Socio-demographic characteristics of the  interviewees",
                   "hl10", "total", "Percentage of people over 5 who attended school or an early program education childhood", "Socio-demographic characteristics of the  interviewees",
                   "hl14", "active", "Percentage of people with activity contribuing to the household income", "Socio-demographic characteristics of the  interviewees",
                   "hl15", "working", "Percentage of people over 5 that entered the forest in the last 4 weeks", "Forest use")

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
        bool_type(tableau_de_menage, x[1]) %>%
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

avg_questions = c("hl6", "Average age", "Socio-demographic characteristics of the  interviewees")
avg_questions = matrix(avg_questions, ncol = 3, byrow = TRUE)

#Transform the matrix to dataframe
avg_questions = as.data.frame(avg_questions)
names(avg_questions) <- c("variable", "question", "topic")

#Calculating the total column
avg_df_total <- tableau_de_menage %>% 
        select(village_code, avg_questions[,1]) %>%
        group_by(village_code) %>%
        summarise_all(~sum(!is.na(.))) %>%
        gather(key = "variable", value = "total", -village_code)

#Calculating the average column
avg_df_avg <- tableau_de_menage %>% 
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

tableau_de_menage$hl6_cat <- cut2(tableau_de_menage$hl6, seq(from = 5, to = 90, by = 5))

tableau_de_menage <- tableau_de_menage %>%
        mutate(hl4 = fct_recode(hl4,
                                "Male" = "Masculin",
                                "Female" = "Féminin"),
               hl11a = ifelse(hl11a %in% c("NSP", "Non reponse"), NA_character_, as.character(hl11a)),
               hl11a = fct_recode(hl11a,
                                  "Pre-school" = "Pré-scolaire",
                                  "Primary" = "Primaire",
                                  "Secondary 1" = "Secondaire 1",
                                  "Secondary 2" = "Secondaire 2",
                                  "Superior" = "Superieur"),
               hl11a = factor(hl11a, levels = c("Pre-school",
                                                "Primary",
                                                "Secondary 1",
                                                "Secondary 2",
                                                "Superior")),
               hl12 = ifelse(hl12 == "NON REPONSE", NA_character_, as.character(hl12)),
               hl12 = fct_recode(hl12,
                                  "Farmer" = "FERMIER/AGRICULTEUR",
                                  "Fisher" = "PÊCHEUR",
                                  "Business owner" = "PROPRIÉTAIRE D?UNE ENTREPRISE",
                                  "Teacher" = "ENSEIGNANT",
                                  "Childcare or housewife" = "GARDE D'ENFANTS OU MÉNAGÈRE",
                                 "Home production" = "FABRICATION À DOMICILE",
                                 "Charcoal maker" = "FABRICANT DE CHARBON DE BOIS",
                                 "Trader" = "COMMERÇANT",
                                 "Student" = "ETUDIANT",
                                 "Unemployed" = "CHOMEUR",
                                 "Retirement" = "RETRAITE",
                                 "Never/not professionally active" = "JAMAIS ACTIF SUR LE PLAN PROFESSIONNEL/PAS DE TRAVAIL",
                                 "Other" = "AUTRE"),
               hl12 = factor(hl12, levels = c("Farmer",
                                              "Fisher",
                                              "Business owner",
                                              "Teacher",
                                              "Childcare or housewife",
                                              "Home production",
                                              "Charcoal maker",
                                              "Trader",
                                              "Student",
                                              "Unemployed",
                                              "Retirement",
                                              "Never/not professionally active",
                                              "Other")),
               hl13 = ifelse(hl13 == "NON REPONSE", NA_character_, as.character(hl13)),
               hl13 = fct_recode(hl13,
                                 "Farmer" = "FERMIER/AGRICULTEUR",
                                 "Fisher" = "PÊCHEUR",
                                 "Business owner" = "PROPRIÉTAIRE D?UNE ENTREPRISE",
                                 "Teacher" = "ENSEIGNANT",
                                 "Childcare or housewife" = "GARDE D'ENFANTS OU MÉNAGÈRE",
                                 "Home production" = "FABRICATION À DOMICILE",
                                 "Charcoal maker" = "FABRICANT DE CHARBON DE BOIS",
                                 "Trader" = "COMMERÇANT",
                                 "No secondary activity" = "PAS D'ACTIVITE SECONDAIRE",
                                 "Other" = "AUTRE"),
               hl13 = factor(hl13, levels = c("Farmer",
                                              "Fisher",
                                              "Business owner",
                                              "Teacher",
                                              "Childcare or housewife",
                                              "Home production",
                                              "Charcoal maker",
                                              "Trader",
                                              "No secondary activity",
                                              "Other")),
               hl16 = fct_recode(hl16,
                                 "Rice growing in swampy areas" = "RIZICULTURE DANS LES ZONES MARECAGEUSES",
                                 "Gathering 'mahampy' for weaving" = "RASSEMBLER MAHAMPY POUR LE TISSAGE",
                                 "Cross (go from A to B)" = "TRAVERSER (ALLER DE A à B)",
                                 "Hunting / Trapping" = "CHASSE / PIEGEAGE",
                                 "Cut down trees" = "COUPER DES ARBRES",
                                 "Collection of firewood from dead trees" = "COLLECTE DE BOIS DE CHAUFFAGE D'ARBRES MORTS",
                                 "Fruit picking" = "CUEILLETTE DE FRUITS",
                                 "Catch birds" = "ATTRAPER DES OISEAUX",
                                 "Honey harvest" = "RECOLTE DE MIEL",
                                 "Creating wild potatoes (tavolo)" = "CREUSER DES POMMES DE TERRE SAUVAGES (TAVOLO/OVIALA)",
                                 "Other" = "AUTRE",
                                 NULL = "NON REPONSE"),
               hl16a = fct_recode(hl16a,
                                  "Charcoal" = "CHARBON DE BOIS",
                                  "Construction wood" = "BOIS DE CONSTRUCTION",
                                  "Canoe construction" = "CONSTRUCTION DE PIROGUE",
                                  "Firewood" = "BOIS DE CHAUFFAGE",
                                  "Other" = "AUTRE",
                                  NULL = "NON REPONSE"),
               hl16b = fct_recode(hl16b,
                                  "Smoking the bees" = "EN ENFUMANT LES ABEILLES",
                                  "By simply harvesting in the tree" = "EN RECOLTANT SIMPLEMENT DANS L'ARBRE",
                                  "Other" = "AUTRE",
                                  NULL = "NON REPONSE")
               
        )


cat_question_1c = c("hl4", "Sex distribution", "Socio-demographic characteristics of the  interviewees",
                    "hl6_cat",  "Age distribution", "Socio-demographic characteristics of the  interviewees",
                    "hl11a", "Level of education per person", "Socio-demographic characteristics of the  interviewees",
                    "hl12", "Primary activity per person", "Socio-demographic characteristics of the  interviewees",
                    "hl13", "Secondary activity per person", "Socio-demographic characteristics of the  interviewees",
                    "hl16", "Main reason to enter the forest in the last 4 weeks per person", "Forest use",
                    "hl16a", "Main reason to cut trees per person", "Forest use",
                    "hl16b", "Main method used in honey harvest per person", "Forest use")
cat_question_1c = matrix(cat_question_1c , ncol = 3, byrow = TRUE)

#Function to create the dataframe
cat_1c_type <- function(df, col){
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
        cat_1c_type(tableau_de_menage, x[1]) %>%
                mutate(question = x[2],
                       topic = x[3])
})

#From listo to dataframe
cat_1c_df <- bind_rows(cat_1c_df) %>%
        select(village_code, topic, question, categories, sum, total)

###################################################################################
# Final changes ###################################################################
###################################################################################
#Save categorical
saveRDS(cat_1c_df, file = here("Baseline_Survey/preprocessing/tableau_de_menage_cat.rds"))

#rbind boolean df and average df
df_num <- rbind(bool_df, avg_df)
#Save it
saveRDS(df_num, file = here("Baseline_Survey/preprocessing/tableau_de_menage_num.rds"))


        