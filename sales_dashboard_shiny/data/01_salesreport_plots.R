#Challenge 01: Sales Report

#Interactive plots

#load libraries

library(tidyverse)
library(lubridate)
library(plotly)
library(stringr)

#load data

bikes_tbl      <- readRDS("C:/Users/Coding/Documents/GitHub/ss23-bbdp-Julian150397/reporting_rmarkdown/bike_data_s14/bikes_tbl.rds")
bikeshops_tbl  <- readRDS("C:/Users/Coding/Documents/GitHub/ss23-bbdp-Julian150397/reporting_rmarkdown/bike_data_s14/bikeshops_tbl.rds")
orderlines_tbl <- readRDS("C:/Users/Coding/Documents/GitHub/ss23-bbdp-Julian150397/reporting_rmarkdown/bike_data_s14/orderlines_tbl.rds")

#merge data

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  mutate(total_price = price_euro * quantity)

bike_orderlines_tbl

#function: format to €

format_to_euro <- function(x, suffix = " €") {
  
  scales::dollar(x,
                 suffix       = suffix,
                 prefix       = "",
                 big.mark     = ".",
                 decimal.mark = ",")
}

euro_format <- function(scale        = 1,
                        prefix       = "",
                        suffix       = " €",
                        big.mark     = ".",
                        decimal.mark = ",") {
  
  scales::dollar_format(suffix       = suffix,
                        prefix       = prefix,
                        big.mark     = big.mark,
                        decimal.mark = decimal.mark,
                        scale        = scale)
}

#monthly sales

total_sales_m_tbl <- bike_orderlines_tbl %>%
  
  dplyr::select(order_date, total_price) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
  
  group_by(date_rounded) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: format_to_euro(bike_orderlines_tbl)
                               Date: date_rounded %>% format('%B %Y')"))

total_sales_m_tbl

#static plot

sales_m <- total_sales_m_tbl %>%
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

sales_m
