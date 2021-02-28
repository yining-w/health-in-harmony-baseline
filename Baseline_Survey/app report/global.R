library(shiny)
library(shinydashboard)
library(callr)
library(tidyverse)
library(leaflet)
library(tidyverse)
library(leaflet.extras)
library(viridis)
library(shinyWidgets)
library(shinythemes)
library(rhandsontable)
library(RColorBrewer)
library(sf)
library(shinycssloaders)

# Loading coordinates ---------------------------------------------------------------------
load("gps.RData")

#Transform GPS data into vector data
#lat = gps$gpslat
#lon = gps$gpslon

gps = gps %>% rename(village_code = gps1,
               village_name = gps1a,
               reserve_section = gpstrate)
#gps<-st_as_sf(x=gps,
#              crs="+proj=longlat +datum=WGS84 +no_defs",
#              coords=c("gpslon", "gpslat")) %>%
#        rename(village_code = gps1,
#               village_name = gps1a,
#               reserve_section = gpstrate)

gps = gps %>% rename(
        hh_village_code = village_code
)

# Adding population (size of the circle) --------------------------------------------------
load("MENAGE.RData")
population <- menage %>% select(village_code = hh1, 
                                reserve_section = hhstrate, 
                                household_members = hh48) %>%
        group_by(village_code, reserve_section) %>%
        summarise(population = sum(household_members, na.rm = TRUE)) %>%
        select(village_code, population) 

gps <- gps %>% left_join(population, by = c("hh_village_code" = "village_code"))

# Reading Survey Data ---------------------------------------------------------------------
num_df <- list.files(pattern = "*num.rds$",
                     full.names = TRUE) %>%
        map_dfr(readRDS) %>% ungroup()

cat_df <- list.files(pattern = "*cat.rds$",
                     full.names = TRUE) %>%
        map_dfr(readRDS) %>% ungroup()


# Setting question options ---------------------------------------------------------------
questions <- rbind((num_df %>% select(topic, question) %>% mutate(type = "numeric")),
                   (cat_df %>% select(topic, question) %>% mutate(type = "categorical"))) %>%
        unique()


topic_options <- questions %>% pull(topic)

type_options <- ""

question_options <- ""

# Function to update options --------------------------------------------------------------
get_type_options <- function(df, topic_input){
        return(df %>% filter(df$topic == topic_input) %>% pull(type))
        
}

get_question_options <- function(df, topic_input, type_input){
        return(df %>% filter(df$topic == topic_input, df$type == type_input) %>% pull(question))
}

# Dataframes ------------------------------------------------------------------------------
create_num_dataframe <-function(df, gps_data, topic_input, type_input, question_input){
        if(question_input != "" & type_input == "numeric"){
                df <- df %>% filter(topic == topic_input,
                                    question == question_input,
                                    village_code %in% gps_data$hh_village_code)
                gps_data %>% left_join(df, by = c("hh_village_code" = "village_code")) %>%
                        mutate(kpi = case_when(
                                type == "Percentage" ~ value/total,
                                type == "Average" ~ value),
                               kpi = ifelse(is.nan(kpi), NA, kpi))
        }
        else{

        }
}

create_cat_dataframe <-function(df, gps_data, topic_input, type_input, question_input){
        if(question_input != "" & type_input == "categorical"){
                df <- df %>% filter(topic == topic_input,
                                    question == question_input,
                                    village_code %in% gps_data$hh_village_code)
                df
        }
        else{

        }
}

# Map -------------------------------------------------------------------------------------
draw_base_map <-function(map, session){
        leafletProxy(mapId = map) %>%
                clearMarkers() %>%
                clearControls()
        
        leaflet(
                options = leafletOptions(minZoom = 12, maxZoom = 14)
        ) %>%
                setMaxBounds(lng1 = 47.393221, lat1 = -23.185126, lng2 = 47.822476, lat2 = -22.870085) %>%
                addProviderTiles('CartoDB.Positron') %>%
                addResetMapButton()
}

update_no_color_map <- function(map, session, gps_data){
        leafletProxy(mapId = map, 
                     session = session) %>%
                clearMarkers() %>%
                clearControls() %>%
                addCircleMarkers(data = gps_data,
                                 radius = ~population/25 + 5,
                                 stroke = FALSE,
                                 fillOpacity = 0.5,
                                 label = ~village_name,
                                 lng = ~gpslng, 
                                 lat = ~gpslat,
                                 layerId = ~hh_village_code,
                                 labelOptions = labelOptions(noHide = T,
                                                             textOnly = TRUE,
                                                             direction = 'auto')
                                 )
}

update_color_map <- function(map, session, num_df){
        pal <- colorNumeric(palette = "YlGnBu",
                            domain = num_df$kpi)
        leafletProxy(mapId = map, 
                     session = session) %>%
                clearMarkers() %>%
                clearControls() %>%
                addCircleMarkers(data = num_df,
                                 radius = ~population/25 + 5,
                                 stroke = FALSE,
                                 color = ~pal(kpi),
                                 fillOpacity = 0.5,
                                 label = ~village_name,
                                 layerId = ~hh_village_code,
                                 labelOptions = labelOptions(noHide = T,
                                                             textOnly = TRUE,
                                                             direction = 'auto')
                ) %>%
                addLegend(position = "bottomleft", 
                          pal = pal, 
                          values = num_df$kpi,
                          title = "KPI",
                          opacity = 0.5)
}

# Plots -----------------------------------------------------------------------------------
create_num_histogram <- function(num_df, id){
        id <- as.numeric(id)
        type <- as.character(num_df[num_df$hh_village_code == id,'type'] %>% st_drop_geometry())
        kpi_value <- as.numeric(num_df[num_df$hh_village_code == id,'kpi'] %>% st_drop_geometry())
        
        if (type == "Percentage"){
                total_value <- num_df %>% st_drop_geometry() %>% 
                        group_by(question) %>% summarise(value = sum(value),
                                                         total = sum(total)) %>%
                        mutate(total_value = value/total) %>% pull(total_value)
                
                num_df %>% ggplot(aes(x=kpi)) +
                        geom_histogram(bins = round(nrow(num_df)/3)) +
                        geom_vline(xintercept = kpi_value, color = '#E69F00', size = 1.5, alpha = 0.5)+
                        geom_vline(xintercept = total_value, color = '#56B4E9', size = 1.5, alpha = 0.5)+
                        scale_x_continuous(labels = function(x) paste0(x*100, "%"))
        }
        else if (type == "Average"){
                total_value <- num_df %>% st_drop_geometry() %>%
                        mutate(value = value * total) %>% group_by(question) %>%
                        summarise(value = sum(value),
                                  total = sum(total)) %>%
                        mutate(total_value = value/total) %>% pull(total_value)
                
                num_df %>% ggplot(aes(x=kpi)) +
                        geom_histogram(bins = round(nrow(num_df)/3)) +
                        geom_vline(xintercept = kpi_value, color = "#E69F00", size = 1.5, alpha = 0.5)+
                        geom_vline(xintercept = total_value, color = "#56B4E9", size = 1.5, alpha = 0.5)+
                        scale_x_continuous(labels = function(x) round(x,1))
        }
}

create_cat_plot <- function(cat_df, id){
        id <- as.numeric(id)
        
        df_village <- cat_df %>% mutate(percentage = sum/total) %>% 
                filter(village_code == id) %>% arrange(percentage)
        
        df_total <- cat_df %>% group_by(categories) %>% 
                summarise(sum = sum(sum),
                          total = sum(total)) %>% 
                mutate(percentage_total = sum/total) %>% ungroup()
        
        df <- df_village %>% left_join(df_total, by="categories") %>% 
                gather(type, percentage, c(percentage, percentage_total))
        
        df %>% ggplot(aes(y=reorder(categories, percentage), x=percentage, fill = type)) + geom_col(position = 'dodge') +
                ylab(NULL) + 
                scale_fill_manual(labels = c("Village value", "Area value"), values = c("#E69F00", "#56B4E9")) +
                theme(legend.position="top", 
                      legend.title = element_blank(),
                ) +
                scale_x_continuous(labels = function(x) paste0(x*100, "%"))
}

# a <- cat_df %>% filter(question == "Diarrhea treatment source") %>% mutate(percentage = sum/total) %>% 
#         filter(village_code == 1) %>% arrange(percentage)
# 
# b <- cat_df %>% filter(question == "Diarrhea treatment source") %>% group_by(categories) %>% 
#         summarise(sum = sum(sum),
#                   total = sum(total)) %>% mutate(percentage_total = sum/total) %>% ungroup()
# 
# c <- a %>% left_join(b, by="categories") %>% gather(type, percentage, c(percentage, percentage_total))
# 
# c %>% ggplot(aes(y=reorder(categories, percentage), x=percentage, fill = type)) + geom_col(position = 'dodge') +
#         ylab(NULL) + 
#         scale_fill_manual(labels = c("Village value", "Area value"), values = c("#E69F00", "#56B4E9")) +
#         theme(legend.position="top", 
#                            legend.title = element_blank(),
#                            ) +
#         scale_x_continuous(labels = function(x) paste0(x*100, "%"))



# tables ----------------------------------------------------------------------------------
draw_num_table <- function(num_df, id){
        id <- as.numeric(id)
        village_name <- as.character(num_df[num_df$hh_village_code == id,'village_name'] %>% st_drop_geometry())
        kpi_value <- as.numeric(num_df[num_df$hh_village_code == id,'kpi'] %>% st_drop_geometry())
        type <- as.character(num_df[num_df$hh_village_code == id,'type'] %>% st_drop_geometry())
        
        total_value <- if (type=="Percentage"){
                num_df %>% st_drop_geometry() %>% 
                        group_by(question) %>% summarise(value = sum(value),
                                                     total = sum(total)) %>%
                        mutate(total_value = value/total) %>% pull(total_value)
        }
        else if (type == "Average"){
                num_df %>% st_drop_geometry() %>%
                        mutate(value = value * total) %>% group_by(question) %>%
                        summarise(value = sum(value),
                                  total = sum(total)) %>%
                        mutate(total_value = value/total) %>% pull(total_value)
        }
        
        
        print(total_value)
        
        if (type == "Percentage"){
                table<- data.frame(col_name = c('Village name', "Area value", "Village value"),
                                   values = c(village_name,
                                              paste0(round(total_value*100,2),"%"),
                                              paste0(round(kpi_value*100,2),"%")))
                table
        }
        else if (type == "Average"){
                table<- data.frame(col_name = c('Village name', "Area value", "Village value"),
                                   values = c(village_name,
                                              round(total_value,2),
                                              round(kpi_value,2)))
                table
                
        }
        
}

draw_cat_table <- function(cat_df, id, gps_data){
        id <- as.numeric(id)
        village_name <- as.character(gps_data[gps_data$hh_village_code == id,'village_name'] %>% st_drop_geometry())
        
        table<- data.frame(col_name = c('Village name'),
                           values = c(village_name))
        table
}

