library(leaflet)
library(tidyverse)
library(plotly)



ui <- 
navbarPage("SPARC VISUALIZER", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width = "100%", height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 60, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      # checkboxInput("layA", label = "LAYER 1", value = T),
                                      
                                      # column(6, numericInput("thresholds_A", "Number Colors",
                                      #              min = 2, max = 3, value = 2)),
                                      # 
                                      # column(6, selectInput("color_A", "Palette",
                                      #                       c("Purple to yellow" = "viridis",
                                      #                         "Yellow to black" = "magma",
                                      #                         "Greens" = "Greens",
                                      #                         "Blue to purple" = "BuPu",
                                      #                         "Oranges" = "Oranges",
                                      #                         "Blues" = "Blues",
                                      #                         "Greys" = "Greys",
                                      #                         "Reds" = "Reds",
                                      #                         "Purples" = "Purples"),
                                      #                       selected = "Oranges",
                                      #                       width = 800)),
                                      # 
                                      sliderInput("thr1", "Solution threshold:",
                                                  min = 1, max = 100, step = 1,
                                                  value = 17),
                                      
                                      hr(),
                                      
                                      sliderInput("thr2", "Carbon offset",
                                                  min = 0, max = 100, step = 1,
                                                  value = tbl %>% 
                                                    filter(abs(perc_pixels - 0.17) == min(abs(perc_pixels - 0.17))) %>% 
                                                    pull(prop_carbon_strg) %>% 
                                                    `*` (100) %>% 
                                                    round(0)),
                                      
                                      plotlyOutput("plotly"),
                                      
                                      hr()
                                      
                        ),
                        
                        tags$div(id="cite",
                                 'Priority conservation sites from ', tags$em('SPARC'))
                    )
           ),

           tabPanel("About",
                    fluidRow(
                      column(3, 
                             h4("A description of the research and a link to the paper will appear here")
                             )
                      )
                    ),
                    
           
           conditionalPanel("false", icon("crosshair"))
)
