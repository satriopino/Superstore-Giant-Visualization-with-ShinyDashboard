library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)
library(glue)
library(plotly)
library(dplyr)
library(tidyverse)
library(lubridate)
library(echarts4r)
library(highcharter)
library(htmlwidgets)

df <- read_csv("data_input/Superstore.csv")

df_clean <- df  %>% 
  select(-c(`Row ID`, `Product ID`, Country)) %>% 
  mutate(`Order Date` = mdy(`Order Date`),
         `Ship Date` = mdy(`Ship Date`),
         `Ship Mode` = as.factor(`Ship Mode`),
         Segment = as.factor(Segment),
         State = as.factor(State),
         Region = as.factor(Region),
         Category = as.factor(Category),
         `Sub-Category` = as.factor(`Sub-Category`),
         Year = year(`Order Date`),
         Month = month(`Order Date`),
         Mon.Year = floor_date(`Order Date`, "month"))

database <- df_clean  %>% 
  select(-c(`Order ID`, `Customer ID`, `Customer Name`, `Postal Code`, `Product Name`, Discount, Quantity, Year, Month, Mon.Year))


source("ui.R")
source("server.R")
shinyApp(ui,server)