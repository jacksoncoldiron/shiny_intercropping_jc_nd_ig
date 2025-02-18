# Intercropping Shiny App
# Assignment 2 Task 3: Shiny Infrastructure
# Jackson Coldiron, Nicolas DeStephano, Isa Elias

###########################################################

# Load libraries

library(shiny)
library(tidyverse)
library(here)

intercrop <- read_delim(delim = ';', here("data", "Database.csv"))


########### Requirements########### 
#  A nicely formatted, professional looking user interface (not the Shiny defaults) 
# 1. At least 4 functional, useful widgets (using at least 3 different TYPES of widgets)
# 2. At least 3 finalized, professionally formatted tabs (one should contain a summary of the data, what the app does, and other background information the user might need)
# 3. At least 4 finalized reactive outputs (figures/tables/graphs/maps/etc.) that change based on one or more widget inputs
# 4. At least one application of an advanced data analysis technique that is accurately and appropriately applied to the dataset.
#       The meaning/interpretation of the analysis/results should be explained clearly so the user can make sense of the analysis.
#       This analysis may be linked to a visualization or table.



########### 1. Widget 1: Yield Metric Selection ###########

# A checkbox group menu to select yield metric 
# Interactivity: The selected metric will update the primary yield visualization, 
#    allowing users to compare different measures of yield across intercropping setups.
