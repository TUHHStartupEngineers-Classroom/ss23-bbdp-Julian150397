# Core
library(shiny)
library(tidyverse)
library(lubridate)

# Interactive Visualizations
library(plotly)

# Spatial Data
library(sf)
library(raster)

# Currency formatting
source("C:/Users/Coding/Documents/GitHub/ss23-bbdp-Julian150397/challenges/01_salesreport_plots.R")

#load data

bikes_tbl      <- readRDS("C:/Users/Coding/Documents/GitHub/ss23-bbdp-Julian150397/reporting_rmarkdown/bike_data_s14/bikes_tbl.rds")
bikeshops_tbl  <- readRDS("C:/Users/Coding/Documents/GitHub/ss23-bbdp-Julian150397/reporting_rmarkdown/bike_data_s14/bikeshops_tbl.rds")
orderlines_tbl <- readRDS("C:/Users/Coding/Documents/GitHub/ss23-bbdp-Julian150397/reporting_rmarkdown/bike_data_s14/orderlines_tbl.rds")

#merge data

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  mutate(total_price = price_euro * quantity)

# German spatial data
germany_sp <- getData('GADM', country='DE', level=1) 
# Convert SpatialPolygonsDataFrame to an sf dataframe
germany_sf <- st_as_sf(germany_sp) %>% 
  # Add english names
  mutate(VARNAME_1 = ifelse(is.na(VARNAME_1), NAME_1, VARNAME_1))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Date Range"),
      sliderInput(inputId = "date_range", 
                  label = NULL, 
                  min = as_date("2015-07-01", tz = NULL), 
                  max = as_date("2023-07-01", tz = NULL),
                  value = c(as_date("2016-01-01", tz = NULL),as_date("2017-01-01", tz = NULL)),
                  step = 1,
                  round = T,
                  pre = ""),
      h4("Bike Type"),
      checkboxGroupInput(inputId = "checkbox_group_cat1", 
                         label = NULL, 
                         choices = c("E-Bikes" = "E-Bikes",
                                     "Mountain" = "Mountain",
                                     "Hybrid/City" = "Hybrid/City",
                                     "Gravel" = "Gravel",
                                     "Road" = "Road"),
                         selected = c("Mountain")),
      h4("Bike Family"),
      checkboxGroupInput(inputId = "checkbox_group_cat2", 
                         label = NULL, 
                         choices = c("Adventure" = "Adventure",
                                     "All-Road" = "All-Road",
                                     "City" = "City",
                                     "Cross-Country" = "Cross-Country",
                                     "Cyclocross" = "Cyclocross",
                                     "Dirt Jump" = "Dirt Jump",
                                     "Downhill" = "Downhill",
                                     "E-City" = "E-City",
                                     "E-Fitness" = "E-Fitness",
                                     "E-Gravel" = "E-Gravel",
                                     "E-Mountain" = "E-Mountain",
                                     "E-Road" = "E-Road",
                                     "E-Trekking" = "E-Trekking",
                                     "Endurance" = "Endurance",
                                     "Enduro" = "Enduro",
                                     "Fat Bikes" = "Fat Bikes",
                                     "Race" = "Race",
                                     "Touring" = "Touring",
                                     "Trail" = "Trail",
                                     "Triathlon Bike" = "Triathlon Bike"),
                         selected = c("E-Mountain"))
    ),
    mainPanel(
      dataTableOutput(outputId = "dt_output"),
      plotlyOutput(outputId = "plot_output")
    )
  )
)

server <- function(input, output) {
  # Reactive Filter
  filtered_data <- reactive({
    bike_orderlines_tbl %>%
      filter(category_1 %in% input$checkbox_group_cat1,
             category_2 %in% input$checkbox_group_cat2,
             order_date >= ymd(input$date_range[1]) & order_date <= ymd(input$date_range[2])) %>%
      group_by(state) %>%
      summarise(total_revenue = sum(total_price)) %>%
      ungroup() %>%
      right_join(germany_sf, by = c("state" = "VARNAME_1")) %>% 
      mutate(total_revenue = ifelse(is.na(total_revenue), 0, total_revenue)) %>%
      mutate(label_text = str_glue("State: {state}
                                         Revenue: {format_to_euro(total_revenue)}")) %>%
      st_as_sf()
  })
  
  output$dt_output <- renderDataTable({
    DT::datatable(filtered_data(), options = list(scrollY = "250px"))
  })
  
  output$plot_output <- renderPlotly({
    plot_ly(data = filtered_data(), 
            split      = ~NAME_1, 
            color      = ~total_revenue,
            colors     = "Blues",
            stroke     = I("black"),
            hoverinfo  = 'text', 
            text       = ~label_text, 
            hoveron    = "fills", 
            showlegend = FALSE)
  })
}

shinyApp(ui, server)