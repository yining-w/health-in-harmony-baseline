library(tidyverse)
library(lubridate)
library(here)

load(here("Baseline_Survey/data/HISTORIQUE_NAISSANCE.RData"))

historique_naissance <- historique_naissance %>% 
        rename(intervier = wm5,
               village_code = wm1, 
               household_number = wm2,
               woman_number = wm3,
               reserve_section = wmstrate,
               team_leader = wm4)

#####################################################################
# Boolean ###########################################################
#####################################################################

historique_naissance <- historique_naissance %>% 
        mutate(bh2 = case_when(
                          bh2 == "Jumeaux" ~ TRUE,
                          bh2 == "Simple" ~ FALSE,
                          TRUE ~ NA
                  )) %>%
        mutate_at(.vars = c("bh5", "bh7"),
                  ~case_when(
                          . == "Oui" ~ TRUE,
                          . == "Non" ~ FALSE,
                          TRUE ~ NA
                  ))

bool_questions = c("bh2", "total", "Percentage of twins", "Fertility",
                   "bh5", "total", "Percentage of children alive", "Child mortality",
                   "bh7", "alive", "Percentage of children living at home", "Fertility")


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
        bool_type(historique_naissance, x[1]) %>%
                mutate(question = x[3],
                       topic = x[4])
})

#From list to dataframe
bool_df <- bind_rows(bool_df) %>%
        select(village_code, topic, question, value, total) %>%
        mutate(type = "Percentage")

############################################################################
# Average ##################################################################
############################################################################
#COlumns need to be numerical
#NA means no response (be careful because some NA need to be change to 0)

historique_naissance <- historique_naissance %>%
        mutate_at(.vars = c("bh4m", "bh9m"),
                  ~fct_recode(.,
                          "January" = "JANVIER",
                          "February" = "FÉVRIER",
                          "March" = "MARS",
                          "April" = "AVRIL",
                          "May" = "MAI",
                          "June" = "JUIN",
                          "July" = "JUILLET",
                          "August" = "AOUT",
                          "September" = "SEPTEMBRE",
                          "October" = "OCTOBRE",
                          "November" = "NOVEMBRE",
                          "December" = "DECEMBRE",
                          NULL = "NSP",
                          NULL = "NON REPONSE"
                  )) %>%
        mutate(bh4m_temp = ifelse(!is.na(bh4y) & is.na(bh4m), 1, bh4m),
               bh9m_temp = ifelse(!is.na(bh9a) & is.na(bh9m), 1, bh9m),
               birth_date =  ymd(paste(bh4y, bh4m_temp, bh4y/bh4y, sep= '-')),
               death_date =  ymd(paste(bh9a, bh9m_temp, bh9a/bh9a, sep= '-')),
               death_age = interval(birth_date, death_date) / years(1)
        ) %>%
        select(-bh4m_temp, -bh9m_temp)
                  


avg_questions = c("bh6", "Average children age", "Fertility", #Alive
                  "death_age", "Average age children die", "Child mortality") #Dea

avg_questions = matrix(avg_questions, ncol = 3, byrow = TRUE)

#Transform the matrix to dataframe
avg_questions = as.data.frame(avg_questions)
names(avg_questions) <- c("variable", "question", "topic")

#Calculating the total column
avg_df_total <- historique_naissance %>% 
        select(village_code, avg_questions[,1]) %>%
        group_by(village_code) %>%
        summarise_all(~sum(!is.na(.))) %>%
        gather(key = "variable", value = "total", -village_code)

#Calculating the average column
avg_df_avg <- historique_naissance %>% 
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

historique_naissance <- historique_naissance %>%
        mutate(bh3 = fct_recode(bh3,
                                "Male" = "Garçon",
                                "Female" = "Fille"),
               bh10 = fct_recode(bh10,
                                 "Fever" = "Fièvre",
                                 "Malaria" = "Paludisme",
                                 "Diarrhea" = "Diarrhée",
                                 "ARI" = "IRA",    
                                 "Convulsion" = "Convulsion",
                                 "Accident" = "Accident",
                                 "Dehydration" = "Déshydratation",
                                 "Failure to eat" = "Défaut de manger", 
                                 "Witchcraft" = "La sorecellerie",
                                 "God's will" = "La volonté de Dieu", 
                                 "Other" = "Autres",
                                 NULL = "NSP",     
                                 NULL = "Non reponse")
        )

cat_question_1c = c("bh3", "Children sex", "Fertility",
                    "bh4m",  "Birth month of children", "Fertility",
                    #"bh4y",  "Birth year of children", "Fertility",
                    "bh9m", "Month of children death", "Child mortality",
                    #"bh9a", "Year of children death", "Child mortality",
                    "bh10", "Cause of children death", "Child mortality")

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
        cat_1c_type(historique_naissance, x[1]) %>%
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
saveRDS(cat_1c_df, file = here("Baseline_Survey/preprocessing/historique_naissance_cat.rds"))

#rbind boolean df and average df
df_num <- rbind(bool_df, avg_df)
#Save it
saveRDS(df_num, file = here("Baseline_Survey/preprocessing/historique_naissance_num.rds"))
