library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

# Reactive values object storage
rvs <- reactiveValues()

function(input, output, session) {
  
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
      filter(perc_pixels <= input$thr1) %>% 
      filter(abs(perc_pixels - (input$thr1) / 100) == min(abs(perc_pixels - (input$thr1) / 100)))
    ## Update Carbon offset slider
    new_value <- rvs$sel_row %>% pull(prop_carbon_strg) %>% `*` (100) %>% floor
    if(!(input$thr2 %in% new_value)){
      updateSliderInput(session = session, inputId = "thr2", 
                        value = rvs$sel_row %>% pull(prop_carbon_strg) %>% `*` (100) %>% floor)
    }
  })
  observeEvent(input$thr2, {
    rvs$sel_row <- tbl %>% 
      filter(prop_carbon_strg <= input$thr2) %>% 
      filter(abs(prop_carbon_strg - (input$thr2) / 100) == min(abs(prop_carbon_strg - (input$thr2) / 100)))
    ## Update threshold slider
    new_value <- rvs$sel_row %>% pull(perc_pixels) %>% `*` (100) %>% floor
    if(!(input$thr1 %in% new_value)){
      updateSliderInput(session = session, inputId = "thr1", 
                        value = rvs$sel_row %>% pull(perc_pixels) %>% `*` (100) %>% floor)
    }
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
      # palette = input$color_A,
      palette = "Oranges",
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
      addPolygons(data = country_borders, 
                  group = "solutions_A",
                  weight = 4,
                  # fillColor = "#8e113f",
                  # fillColor = 0) %>% #,
                  fillOpacity = 0,
                  color = "#561a44") %>%
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
      xlab("Proportion of study area protected") +
      ylab("Proportion of carbon storage saved") +
      theme_minimal()
    
    output$plotly <- renderPlotly(
      plotly::ggplotly(plot1)  
    )
  })
  
}