### Libraries ######################################################################
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
library(here)
library(DT)
source(here("Baseline_Survey/app report/global.R"))
remove(list=ls())

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
                             includeCSS(here("Baseline_Survey/app report/styles.css"))
                         ),
                         
                     ##Leaflet map
                     leafletOutput("mymap", height = "750px"),    
                         
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
                                             choices=levels(gps$reserve_section),
                                             selected=c("PARCELLE I",
                                                        "FORET CLASSEE",
                                                        "PARCELLE II/LITTORALE")
                                             ),
                                 
                                 hr(),
                                 #Dropdown lists
                                 h4("Survey"),
                                 selectizeInput('topic', 
                                                'Select Topic', 
                                                choices = c("select" = "", 
                                                            topic_options)),
                                 selectizeInput('type', 
                                                'Select Type', 
                                                choices = c("select" = "", type_options)),
                                 selectizeInput('question', 
                                                'Select Question', 
                                                choices = c("select" = "", question_options))
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
                             
                             textOutput("kpi_name"),
                             DTOutput("summary_table"),
                             
                             h5(strong("Given Parameters, Distribution of Districts with Respect to:")),
                             plotOutput("myhist", height = 120),
                             plotOutput("cat_plot", height = 300)
                         )#%>% 
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


#This function filter a dataframe based on a list of filters.
#Does not matter if one of the filters is NULL
# filter_dataframe <- function(df, filters){
#         for (i in 1:length(filters)){
#                 if (!is.null(filters[i])){
#                         df <- df %>% filter(.data[[names(filters[i])]] == as.character(filters[i]))
#                 } 
#         }
#         return(df)
# }

server <- function(input, output, session)  { 
# Dropdown lists ----------------------------------------------------------------------------
        observeEvent(input$topic,{
                type_choices <- get_type_options(questions, input$topic)
                updateSelectInput(session, "type",
                                  choices = c("select" = "", type_choices)
                                  )
                updateSelectInput(session, "question",
                                  choices = c("select" = "")
                )
        })
        
        observeEvent(input$type,{
                question_choices <- get_question_options(questions, input$topic, input$type)
                updateSelectInput(session, "question",
                                  choices = c("select" = "", question_choices)
                )
        })
        
# Dataframes --------------------------------------------------------------------------------        
        # Reactive df, it only changes when stratum change
        gps_reactive <- reactive({
                gps %>% filter(reserve_section %in% input$stratum)
        })
        
        #Reactive numeric df, changes when question is not null
        num_df_reactive <- reactive({
                create_num_dataframe(num_df, gps_reactive(), input$topic, input$type, input$question)
        })
        
        #Reactive numeric df, changes when question is not null
        cat_df_reactive <- reactive({
                create_cat_dataframe(cat_df, gps_reactive(), input$topic, input$type, input$question)
        })
                
# Map ---------------------------------------------------------------------------------------        
        output$mymap <- renderLeaflet({
                draw_base_map(gps_reactive())
        })
        
        toListen <- reactive({
                list(input$question,input$stratum)    
        })
        
        observeEvent(
                eventExpr = toListen(), {
                        if(input$question != "" & input$type == "numeric"){
                                update_color_map("mymap", session, num_df_reactive())
                                print("color")
                        }
                        else {
                                update_no_color_map("mymap", session, gps_reactive())
                                print("nocolor")
                        }
                }
        )
        
# Map click ---------------------------------------------------------------------------------
        observeEvent(
                eventExpr = input$mymap_marker_click, {
                        if(input$question != "" & input$type == "numeric"){
                                village_id <- input$mymap_marker_click$id
                                
                                output$kpi_name <- renderText({input$question})
                                
                                output$summary_table <-renderDT(
                                        DT::datatable(draw_num_table(num_df_reactive(),
                                                                               village_id),
                                        rownames = FALSE, colnames = c("",""), filter = "none",
                                        style = "bootstrap",
                                        class = "compact",
                                        options = list(
                                                dom = 'b', ordering = FALSE)) %>% 
                                                formatStyle(
                                                        'col_name',
                                                        target = "row",
                                                        color = styleEqual(
                                                                c("Area value", 
                                                                  "Village value"), 
                                                                c("#56B4E9", 
                                                                  "#E69F00")
                                                                )
                                                        )
                                )
                                
                                
                                output$myhist <- renderPlot({
                                        create_num_histogram(num_df_reactive(),village_id)
                                })
                        }
                        else if(input$question != "" & input$type == "categorical"){
                                village_id <- input$mymap_marker_click$id
                                output$kpi_name <- renderText({input$question})
                                
                                output$cat_plot<-renderPlot({
                                        create_cat_plot(cat_df_reactive(),village_id)
                                })
                                
                                output$summary_table<-NULL
                                
                        }
                        else{
                                output$myhist<-NULL
                                output$summary_table<-NULL
                        }

                }
        )
        
        # observeEvent(
        #         eventExpr = input$question,
        #         {
        #                 update_no_color_map("mymap", session, gps_reactive())
        #                 print("nocolor") 
        #         }
        # )
  
  
        # #Transforming  gps to reactive
        # gps_reactive <- reactive({
        #         gps %>% filter(reserve_section %in% input$stratum)
        # })
        # 
        # 
        # #Plot by default
        # output$map <- renderLeaflet({
        #         leaflet(gps_reactive()) %>%
        #                 addProviderTiles('CartoDB.Positron') %>%
        #                 addCircleMarkers(radius = ~population/25 + 5,
        #                 stroke = FALSE,
        #                 label = ~village_name,
        #                 labelOptions = labelOptions(noHide = T, 
        #                                             textOnly = TRUE,
        #                                             direction = 'auto')
        #                 )
        # })
        # 
        # #Update KPI df
        # num_df_reactive <- reactive({
        #         filter_dataframe(num_df,
        #                          list(topic = input$topic,
        #                               question = input$question)) %>%
        #                 filter(village_code %in% gps_reactive()$hh_village_code)
        # })
        # 
        # #Update categorical df
        # cat_df_reactive <- reactive({
        #         filter_dataframe(cat_df,
        #                          list(topic = input$topic,
        #                               question = input$question)) %>%
        #                 filter(village_code %in% gps_reactive()$hh_village_code)
        # })
        # 
        # #Change Map based on KPI and create an aggregate plot
        # observeEvent(
        #         eventExpr = input$question, {
        #                 #If the type is numeric and there is a question:
        #                 if (input$type == "numeric" & !(input$question == "")){
        #                         #Create df with the value and village_code
        #                         df <- gps_reactive() %>% 
        #                                 left_join(num_df_reactive(),
        #                                           by = c("hh_village_code" = "village_code")) %>%
        #                                 mutate(kpi = case_when(
        #                                         type == "Percentage" ~ value/total,
        #                                         type == "Average" ~ value),
        #                                        kpi = ifelse(is.nan(kpi), NA, kpi)
        #                                        )
        #                         
        #                         print(min(df$kpi))
        #                         print(max(df$kpi))
        #                         
        #                         pal <- colorNumeric(palette = "YlGnBu",
        #                                             domain = df$kpi)
        #                         #Render the map with KPI//////////////////////////////////////
        #                         leafletProxy(
        #                                 mapId = "map",
        #                                 session = session
        #                                 )%>% clearMarkers() %>%
        #                                 clearControls() %>%
        #                                 addCircleMarkers(data = df,
        #                                                  radius = ~population/25 + 5,
        #                                                  stroke = FALSE,
        #                                                  color = ~pal(kpi),
        #                                                  fillOpacity = 1,
        #                                                  label = ~village_name,
        #                                                  labelOptions = labelOptions(noHide = T, 
        #                                                                              textOnly = TRUE,
        #                                                                              direction = 'auto')
        #                                 ) %>%
        #                                 addLegend(position = "bottomleft", 
        #                                         pal = pal, 
        #                                         values = df$kpi,
        #                                         title = "KPI",
        #                                         opacity = 1)
        #                           
        #                         #Create aggregate plot ////////////////////////////////////////////
        #                         # df_total <- gps_reactive() %>% 
        #                         #         left_join(num_df_reactive(),
        #                         #                   by = c("hh_village_code" = "village_code")) %>%
        #                         #         group_by(hh_village_code, reserve_section,)
        #                 }
        #                 #If the type is categorical and there is a question:
        #                 else if (input$type == "categorical" & !(input$question == "")){
        #                         #Render the default map /////////////////////////////////////
        #                         leafletProxy(
        #                                 mapId = "map",
        #                                 session = session
        #                         )%>% clearMarkers() %>%
        #                                 addCircleMarkers(data = gps_reactive(),
        #                                                  radius = ~population/25 + 5,
        #                                                  stroke = FALSE,
        #                                                  label = ~village_name,
        #                                                  labelOptions = labelOptions(noHide = T, 
        #                                                                              textOnly = TRUE,
        #                                                                              direction = 'auto')
        #                                 )
        #                         #Create aggregate plot ///////////////////////////////////////////
        #                         
        #                 }
        #                 
        #                 
        #         }
        # )
        # 
        # #Clic event on village:
        # #Create the plots /////////////////////////////////////////////////////////////
        # observeEvent(
        #         eventExpr = input$map_shape_click, {
        #                 
        #         }
        # )
    
    }

# Run the application 
shinyApp(ui = ui, server = server)