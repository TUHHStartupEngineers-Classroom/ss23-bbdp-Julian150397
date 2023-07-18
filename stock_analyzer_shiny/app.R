# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary

# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

library(zoo)

source(file = "stock_analysis_functions.R")

# UI ----

ui <- fluidPage( title = "Stock Analyzer",
                 # 1.0 HEADER ----
                 div(
                   h1("Stock Analyzer"),
                   p("This is my second shiny project")
                 ),
                 # 2.0 APPLICATION UI -----
                 div(
                   column(width = 4,
                          wellPanel(
                            
                            # Add content here 
                            pickerInput(inputId = "index_selection", 
                                        choices = c("DAX30" = "dax", 
                                                    "SP500" = "sp500",
                                                    "DOW30" = "dow", 
                                                    "NASDAQ100" = "nasdaq"),
                                        selected = "DAX30",
                                        multiple = F),
                            uiOutput("indices"),
                            actionButton(inputId = "analyze",
                                         label = "Analyze",
                                         icon("download")),
                            textOutput("selected_symbol"),
                            dateRangeInput(inputId = "date_range", 
                                           label   = h4("Date Range"), 
                                           start   = "2020-01-01", 
                                           end     = today(),        
                                           min     = "2020-01-01", 
                                           max     = today(), 
                                           startview = "year"),
                            hr(),
                            sliderInput(inputId = "mavg_short", 
                                        label   = h4("Short Moving Average"), 
                                        min     = 5, 
                                        max     = 40,
                                        value = 20,
                                        step = 1,
                                        round = T,
                                        pre = ""),
                            sliderInput(inputId = "mavg_long", 
                                        label   = h4("Long Moving Average"), 
                                        min     = 50, 
                                        max     = 120,
                                        value = 50,
                                        step = 1,
                                        round = T,
                                        pre = ""),
                            uiOutput("index_selection")
                          )),
                   column(width = 8,
                          div(h4(textOutput("plot_header"))),
                          div(
                            plotlyOutput("plotly_plot")
                          )
                 ),
                 # 3.0 ANALYST COMMENTARY ----
                 div(
                   h4("Analyst Commentary"),
                   textOutput("commentary")
                 )
                 )
)

# SERVER ----

server <- function(input, output, session) {
  # Create Stock list ----
  stock_list_tbl <- reactive({get_stock_list(input$index_selection)})
  output$indices <- renderUI({
    pickerInput(inputId = "stock_selection",
                choices = stock_list_tbl() %>% purrr::pluck("label"),
                selected = "1COV.DE, Covestro",
                multiple = F)
  })
  # Stock Symbol ----
  stock_symbol <- eventReactive(input$analyze, ignoreNULL = F, {
    get_symbol_from_user_input(input$stock_selection)
  })
  output$selected_symbol <- renderText({stock_symbol()})
  # Plot Header ----
  plot_header <- eventReactive(input$analyze, ignoreNULL = F, {
    input$stock_selection
  })
  output$plot_header <- renderText({stock_symbol()})
  # Get Stock Data ----
  stock_data_tbl <- reactive({
    stock_symbol() %>% 
      get_stock_data(from = input$date_range[1], 
                     to   = input$date_range[2],
                     mavg_short = input$mavg_short,
                     mavg_long  = input$mavg_long)
  })
  output$stock_data <- renderPrint({stock_data_tbl()})
  # Plot Stock Data ----
  output$plotly_plot <- renderPlotly({plot_stock_data(stock_data_tbl())})
  # Analyst Commentary ----
  updated_commentary <- eventReactive(input$analyze, {
    generate_commentary(stock_data_tbl(), input$stock_selection)
  })
  output$commentary <- renderText({updated_commentary()})
}

# RUN APP ----

shinyApp(ui = ui, server = server)