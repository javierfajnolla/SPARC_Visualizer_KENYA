library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)
library(tidyverse)
library(raster)
library(rgdal)




ui <- 
navbarPage("SPARC VISUALIZER", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("D:/shiny/SPARC_Visualizer/styles.css"),
                          includeScript("D:/shiny/SPARC_Visualizer/gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width = "100%", height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 60, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      checkboxInput("layA", label = "LAYER 1", value = T),
                                      
                                      column(6, numericInput("thresholds_A", "Number Colors",
                                                   min = 2, max = 3, value = 2)),
                                      
                                      column(6, selectInput("color_A", "Palette",
                                                            c("Purple to yellow" = "viridis",
                                                              "Yellow to black" = "magma",
                                                              "Greens" = "Greens",
                                                              "Blue to purple" = "BuPu",
                                                              "Oranges" = "Oranges",
                                                              "Blues" = "Blues",
                                                              "Greys" = "Greys",
                                                              "Reds" = "Reds",
                                                              "Purples" = "Purples"),
                                                            selected = "Oranges",
                                                            width = 800)),
                                      
                                      sliderInput("thr1", "Solution threshold:",
                                                  min = 1, max = 100, step = 1,
                                                  value = 20),
                                      
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


# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

# Reactive values object storage
rvs <- reactiveValues()

server <- function(input, output,session) {
  
  ## Interactive Map ###########################################
  
  output$map <- renderLeaflet({
    # leaflet(TAC_border) %>% 
    leaflet() %>% 
      addTiles() %>%
      fitBounds(lng1 = extent(solution)[1],    # zoom to raster extent
                lat1 = extent(solution)[3],
                lng2 = extent(solution)[2],
                lat2 = extent(solution)[4]) #%>%
  })
  
  # Create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  # thrA_value <- 17
  
  observeEvent(input$thr1, {
    rvs$sel_row <- tbl %>% 
      filter(abs(perc_pixels - (input$thr1) / 100) == min(abs(perc_pixels - (input$thr1) / 100)))
    ## Update Carbon offset slider
    updateSliderInput(session = session, inputId = "thr2", 
                      value = rvs$sel_row %>% pull(prop_carbon_strg) %>% `*` (100) %>% round(0))
  })
  observeEvent(input$thr2, {
    rvs$sel_row <- tbl %>% 
      filter(abs(prop_carbon_strg - (input$thr2) / 100) == min(abs(prop_carbon_strg - (input$thr2) / 100)))
    ## Update threshold slider
    updateSliderInput(session = session, inputId = "thr1", 
                      value = rvs$sel_row %>% pull(perc_pixels) %>% `*` (100))
  })
  
  observeEvent(rvs$sel_row, {
    # Threshold and reclassify
    sel_thr <- rvs$sel_row %>% pull(cutoff_sol)
    
    sol_rcl <- solution %>% 
      reclassify(tibble(from = c(0, sel_thr),
                        to = c(sel_thr, max(values(solution), na.rm = T)),
                        becomes = c(NA, 1)))
    
    # Leaflet
    pal_A <- colorFactor(
      palette = input$color_A,
      levels = c(0.5, 1),
      na.color = "transparent",
      reverse = FALSE)
    
    map %>% 
      clearControls() %>%    # Refreshes the legend
      clearGroup("solutions_A") %>%
      addRasterImage(sol_rcl,
                     colors = pal_A,
                     # opacity = input$opacity_A,
                     group = "solutions_A") %>%
					 
		 addRasterImage(pa,
                     colors = 'green',
                     opacity = 1)%>%
                    
			addLegend(pal = pal_A,
                values = c(NA, 1),
                group = "solutions_A",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                position = "topright")
  })
  
  ### Plotly graph
  observeEvent(input$thr1, {
    plot1 <- tbl %>% 
      mutate(perc_pixels = 100 * perc_pixels,
             prop_carbon_strg = 100 * prop_carbon_strg) %>% 
      ggplot +
      geom_area(aes(x = perc_pixels, y = prop_carbon_strg)) +
      geom_area(data = tbl %>% 
                  mutate(perc_pixels = 100 * perc_pixels,
                         prop_carbon_strg = 100 * prop_carbon_strg) %>% 
                  filter(perc_pixels <= input$thr1),
                aes(x = perc_pixels, y = prop_carbon_strg),
                fill = "#E45621") +
      xlab("Proportion of carbon storage saved") +
      ylab("Proportion of study area protected") +
      theme_minimal()
    
    output$plotly <- renderPlotly(
      plotly::ggplotly(plot1)  
    )
  })
  
}

