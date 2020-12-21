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
remove(list=ls())

####### read data #######
merged = read.csv('~/GitHub/health-in-harmony-baseline-/Baseline_Survey/data/menage_latlong.csv')

##### Map Set up ######
wardpal <- colorFactor(palette ='PuBuGn', domain=gps$gpstrate)




# Define UI for application that draws a histogram
ui <-
    ### HEADER
    ###NAVBAR
        navbarPage(
            theme=shinytheme("journal"),
            title = div(img(src="https://pbs.twimg.com/profile_images/1319346219714424833/9J3Uetn6_400x400.jpg", style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
            id = "main",
            
            tabPanel(
                "About Us", div(class = "outer",
                                style = "display::inline-block;vertical-align:top;",
                
                br(),
                
                h4("MISSION"),
                p("To Reverse Tropical Rainforest Deforestation for planetary health."),
                
                h4("VISION"),
                p("Healthy people. Healthy forests. Healthy planet"),
                
                
                absolutePanel(right = 100, 
                              top = 70,
                              column(7, 
                                box(
                                    width = NULL, 
                                    title = "Saving Lives by Saving Trees: Kinari Webb | TEDxRainier", 
                                    HTML('<iframe width="560" height="315" src="https://www.youtube.com/watch?reload=9&v=tJkeZ_4wuYg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                )))
                )
                ),
            
            absolutePanel(id = "logo", class = "card", bottom = 30, right = 50, width= 20, fixed=TRUE, draggable = FALSE, height = 20,
                          tags$a(href='healthinharmony.org', img(src='https://pbs.twimg.com/profile_images/1319346219714424833/9J3Uetn6_400x400.jpg',height='40',width='40'))),
            
            tabPanel(
                "Baseline Survey Results", div(class="outer",
                
                br(),
                leafletOutput("map", height = "750px"),
                absolutePanel(id = "controls", top = 10, left = 10, width = "auto",
                              class = "panel panel-default",
                              draggable = TRUE, height = 'auto', 
                              collapsible = TRUE,
                              h4("Manombo Map")
                ),
                
                h4("Manombo"),
                p("In 2019, with rainforest communities, local nonprofits, and renowned conservation organizations,
                  we launched programs in the Manombo Special Reserve, a 14,300-acre protected area in southeast Madagascar. 
                  The reserve is home to nine species of lemur, all of which are endemic to Madagascar and threatened by extinction. 
                  We address the challenges of access to high-quality healthcare, resilient and adaptable farming techniques, 
                  and, most urgently, hunger, allowing communities to live in balance with this precious rainforest.")
                )),
               
                
            tabPanel(
                "Get Involved", div(class = "Outer",
                
                br(),
                
                h4(a("Join us", href = "https://healthinharmony.org/get-involved/"),
                "to protect forests, people, and the planet today.  Everyone has something to give to create a healthy future for all."),
                
                br(),
                
                p("For general inquiries, email info@healthinharmony.org, or call 503 688-5579"))
                
                )
            )
        
        
    
    
    
# Define server logic required to draw a histogram
server <- function(input, output)  { 
    output$map <- renderLeaflet({
        map= leaflet() %>%
            addProviderTiles('CartoDB.Positron') %>%
            addProviderTiles('Stamen.TonerLines',
                             options = providerTileOptions(opacity = 0.35)) %>%
            addProviderTiles('Stamen.TonerLabels')
        
        map= map %>% 
            addCircleMarkers(data = gps,
                             lng = ~gpslon, 
                             lat = ~gpslat,
                             color = ~wardpal(gpstrate),
                             label = ~hh1a,
                             radius = 4,
                             opacity = 1
            ) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
