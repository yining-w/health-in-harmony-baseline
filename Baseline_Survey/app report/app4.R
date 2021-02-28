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
##source(here("Baseline_Survey/app report/global.R"))
source("global.R")/

#remove(list=ls())

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
                             includeCSS("styles.css")
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
                             uiOutput("table"),
                             
                             uiOutput("plot")
                         )
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
    #Render base map without circles
    output$mymap <- renderLeaflet({
        draw_base_map("mymap", session)
    })
    
    #Creating reacting variable to observe
    toListen <- reactive({
        list(input$question,input$stratum)    
    })
    
    #If the question or stratum changes, update the map
    observeEvent(
        eventExpr = toListen(), {
            if(is.null(input$stratum)){
                draw_base_map("mymap", session)
            }
            else if(input$question != "" & input$type == "numeric"){
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
    #Creating reactive variable to store click
    rv <- reactiveValues(click = NULL)
    
    #Store click object when changed
    observeEvent(input$mymap_marker_click, {
        rv$click <- input$mymap_marker_click
    })
    
    #If type change click is NULL
    observeEvent(input$type, {
        rv$click <- NULL
    })
    
    #If topic change click is NULL
    observeEvent(input$topic, {
        rv$click <- NULL
    })
    
    #Observe event to change left panel
    observe({
        #If click is null or there is no question do not show anything
        if (is.null(rv$click) | input$question == ""){
            output$kpi_name <- NULL
            output$table <- NULL
            output$plot <- NULL
            
        }
        #If the village id clicked is not in the selection do not show anything
        else if(!(rv$click$id %in% gps_reactive()$hh_village_code)){
            output$table <- NULL
            output$plot <- NULL
        }
        #If the type is numeric show the table and the plot
        else if(input$type == "numeric"){
            village_id <- rv$click$id
            output$kpi_name <- renderText({input$question})
            
            #Create DT control
            output$table <- renderUI({
                DTOutput("summary_table")
            })
            #Render the table
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
            
            #Create plot control
            output$plot <- renderUI({
                plotOutput("num_histogram", height = 150)
            })
            #Render the plot
            output$num_histogram<- renderPlot({
                create_num_histogram(num_df_reactive(),village_id)
            })
            
            
        }
        #If the type is categorical show the table and the plot
        else if(input$type == "categorical"){
            village_id <- rv$click$id
            output$kpi_name <- renderText({input$question})
            
            #Create DT control
            output$table <- renderUI({
                DTOutput("summary_table")
            })
            #Render DT
            output$summary_table <-renderDT(
                DT::datatable(draw_cat_table(cat_df_reactive(),
                                             village_id,
                                             gps_reactive()),
                              rownames = FALSE, colnames = c("",""), filter = "none",
                              style = "bootstrap",
                              class = "compact",
                              options = list(
                                  dom = 'b', ordering = FALSE))
            )
            
            #Create plot control
            output$plot <- renderUI({
                plotOutput("cat_plot", height = 300)
            })
            #Render plot
            output$cat_plot<-renderPlot({
                create_cat_plot(cat_df_reactive(),village_id)
            })
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)