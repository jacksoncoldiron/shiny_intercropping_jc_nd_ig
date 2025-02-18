# Intercropping Shiny App
# Assignment 2 Task 3: Shiny Infrastructure
# Jackson Coldiron, Nicolas DeStephano, Isa Elias

###########################################################

# Load libraries

library(shiny)
library(tidyverse)
library(here)

intercrop <- read_delim(delim = ';', here("data", "Database.csv"))

                      