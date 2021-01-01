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
#library(here)
remove(list=ls())

#Loading coordinates #############################################################
#load(here("Baseline_Survey/data/gps.RData"))
#load(here("Baseline_Survey/preprocessing/menage_survey.csv"))
#load(here("Baseline_Survey/preprocessing/moustiquaire_survey.csv"))
setwd("~/GitHub/health-in-harmony-baseline-/Baseline_Survey/preprocessing")
data_folder = "~/GitHub/health-in-harmony-baseline-/Baseline_Survey/data"
css_folder = "~/GitHub/health-in-harmony-baseline-/Baseline_Survey/"
    
load(paste0(data_folder, "/gps.RData"))
moustiquaire = read.csv("moustiquaire_survey.csv")
moustiquaire = moustiquaire %>% select(-n)
menage = read.csv("menage_survey.csv")

survey = rbind(menage, moustiquaire)


gps = gps %>% rename(
    hh_village_code = gps1,
    village_name = gps1a,
    reserve_section = gpstrate,
    lat = gpslat,
    long = gpslon)


###merging lat long with survey information
merge = survey %>% left_join(gps)

#Adding population
#load(here("Baseline_Survey/data/MENAGE.RData"))
#population <- menage %>% select(village_code = hh1, 
#                         reserve_section = hhstrate, 
#                         household_members = hh48) %>%
#    group_by(village_code, reserve_section) %>%
#    summarise(population = sum(household_members, na.rm = TRUE)) %>%
#    select(village_code, population) 

#gps <- gps %>% left_join(population)

#We also need to add all the variables that are going to be on the map

#Reading Survey #################################################################
#Here we need to add all the tables that we are going to use on the plots
#mosquito_1 <- readRDS(here("Baseline_Survey/data/mosquito_nets.rds"))

mosquito_1 <- readRDS(paste0(data_folder, "/mosquito_nets.rds"))

#This is only temp 
topic <- c("HOUSING CHARACTERISTICS",
           "SOCIO-DEMOGRAPHIC CHARACTERISTICS",
           "HOUSING CHARACTERISTICS",
           "POSSESSION AND USE OF MOSQUITO NETS",
           "AGRICULTURE",
           "FOREST USE",
           "FOOD SECURITY",
           "FERTILITY",
           "CONTRACEPTION",
           "CHILD MORTALITY",
           "CHILD HEALTH",
           "HEALTH CARE AND TREATMENT")

##### Map Set up ######
wardpal <- colorFactor(palette ='PuBuGn', domain=gps$gpstrate)

# Define UI ####################################################################
ui <-navbarPage(
            theme=shinytheme("journal"),
            title = div(img(src="https://pbs.twimg.com/profile_images/1319346219714424833/9J3Uetn6_400x400.jpg", style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
            id = "main",
            
            #TAB ABOUT US
            tabPanel("About Us",
                     div(class = "outer",
                         style = "display::inline-block;vertical-align:top;",
                         br(),
                         h4("MISSION"),
                         p("To Reverse Tropical Rainforest Deforestation for planetary health."),
                         h4("VISION"),
                         p("Healthy people. Healthy forests. Healthy planet"),
                         h4("MANOBO"),
                         p("In 2019, with rainforest communities, local nonprofits, and renowned conservation organizations,
                         we launched programs in the Manombo Special Reserve, a 14,300-acre protected area in southeast Madagascar. 
                         The reserve is home to nine species of lemur, all of which are endemic to Madagascar and threatened by extinction. 
                         We address the challenges of access to high-quality healthcare, resilient and adaptable farming techniques, 
                           and, most urgently, hunger, allowing communities to live in balance with this precious rainforest."
                           ),
                         
                         #Youtube video
                         absolutePanel(right = 100,
                                       top = 70,
                                       column(7, 
                                              box(
                                                  width = NULL,
                                                  title = "Saving Lives by Saving Trees: Kinari Webb | TEDxRainier", 
                                                  HTML('<iframe width="560" height="315" src="https://www.youtube.com/watch?reload=9&v=tJkeZ_4wuYg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                  )
                                              )
                                       ),
                         # Logo on the bottom
                         absolutePanel(id = "logo", 
                                       class = "card", 
                                       bottom = 30, 
                                       right = 50, 
                                       width= 20, 
                                       fixed=TRUE, 
                                       draggable = FALSE, 
                                       height = 20,
                                       tags$a(href='https://www.healthinharmony.org', 
                                              img(src='https://pbs.twimg.com/profile_images/1319346219714424833/9J3Uetn6_400x400.jpg',
                                                  height='40',
                                                  width='40')
                                              )
                                       ),
                         )
                     ),
            
            #MAP TAB
            tabPanel("Baseline Survey Results", 
                     div(class = "outer",
                         tags$head(
                             includeCSS(paste0(css_folder, "/styles.css"))
                         ),
                         
                     ##Leaflet map
                     leafletOutput("map", height = "750px"),    
                         
                         #Filter panel
                         absolutePanel(
                             id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 70, left = "8%", #125 
                             right = "auto", bottom = "auto",
                             width = 0, height = 0,
                             dropdownButton(
                                 label = "",
                                 icon = icon("gear"),
                                 status = "primary",
                                 circle = TRUE,
                                 width = 250,
                                 
                                 h4("Geography"),
                                 
                                 checkboxGroupInput(inputId="stratum",
                                             label="Stratum",
                                             choices=levels(merge$reserve_section),
                                             selected=c("PARCELLE I",
                                                        "FORET CLASSEE",
                                                        "PARCELLE II/LITTORALE")
                                             ),
                                 
                                 hr(),
                                 
                                 h4("Survey"),
                                 
                                 selectInput(inputId="survey",
                                             label="Survey",
                                             choices=c("Select" = "", unique(merge$survey))),
                                 conditionalPanel("input.survey",
                                                  selectInput(inputId="topic",
                                                label="topic",
                                                choices=c("Select" = "", unique(merge$topic)))),
                                 conditionalPanel("input.survey",
                                                  selectInput(inputId="question",
                                                         label="question",
                                                         choices=c("Select" = "", unique(merge$Question))))
                         )),
                         
                         #Plot panel
                         absolutePanel(
                             id = "hist_panel", 
                             class = "panel panel-default",
                             fixed = TRUE, 
                             draggable = TRUE,
                             top = 70, 
                             left = "auto", 
                             right = 0,
                             bottom = "auto",
                             width = "27%", height = "auto", #430
                         
                             h3("Use the gear icon to select map parameters"),
                             h4(tags$em("Click on a village for further details")),
                             uiOutput("clear_district", align = "center"),
                             uiOutput("district_result"),
                             htmlOutput("all_india_text"),
                             h5(strong("Given Parameters, Distribution of Districts with Respect to:"))
                             )
                        #     plotOutput("myhist", height = 120) #%>% 
                             #    withSpinner(type = spinner_type, color = spinner_color),
                             #h5(strong("Trend over Time for the Same Parameters:")),
                             #plotOutput("line_plot", height = plot_height) %>% 
                             #    withSpinner(type = spinner_type, color = spinner_color)
                         )),
                         
                
            tabPanel("Get Involved", 
                     div(class = "Outer",
                         br(),
                         h4(a("Join us", 
                              href = "https://healthinharmony.org/get-involved/"),
                            "to protect forests, people, and the planet today.  Everyone has something to give to create a healthy future for all."
                            ),
                         br(),
                         p("For general inquiries, email info@healthinharmony.org, or call 503 688-5579")
                         )
                
                )
            )
        
        
    
    
    
# Define server ############################################################
server <- function(input, output, session)  { 
    
    #Transforming the layer to reactive
    gps_reactive <- reactive({
        merge %>% filter(reserve_section %in% input$stratum)
    })
    
    output$map <- renderLeaflet({
            ##Base map
            leaflet() %>%
            addProviderTiles('CartoDB.Positron') %>%
  
        ##add points 
            addCircleMarkers(data = merge,
                             #radius = ~population/25 + 5,
                             lng = ~long,
                             lat = ~lat,
                             color=~wardpal(reserve_section),
                             #stroke = FALSE,
                             label = ~village_name
            ) 
    })

    # Update drop down selection

 #   observe({
 #       survey <- if (is.null(input$survey)) character(0) else {
 #           filter(merge, survey %in% input$survey) %>%
 #               `$`('Topic') %>%
 #               unique() %>%
 #               sort()
 #       }
 #       stillSelected <- isolate(input$topic[input$topic %in% topic])
 #       updateSelectizeInput(session, "topic", choices = topic,
 #                            selected = stillSelected, server = TRUE) 
 #   })
 #   observe({
 #       question <- if (is.null(input$survey)) character(0) else {
 #           merge %>%
 #               filter(survey %in% input$survey,
 #                      is.null(input$topic) | topic %in% input$topic) %>%
 #               `$`('topic') %>%
 #               unique() %>%
 #               sort()
 #       }
  #  stillSelected <- isolate(input$Question[input$Question %in% question])
  #      updateSelectizeInput(session, "question", choices = question,
   #                          selected = stillSelected, server = TRUE)
    #})
    }

# Run the application 
shinyApp(ui = ui, server = server)