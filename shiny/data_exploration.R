#### Intercropping Shiny App: Data Exploration
# Isa Elias

###########################################################

# Load libraries

library(tidyverse)
library(janitor)

### Data expo

intercrop_raw <- read_delim(delim = ';', here("data", "Database.csv"))

intercrop_clean <- intercrop_raw |>
  select('Country', 'Latitude', 'Longitude', 'Continent', 'Intercropping_design', 'Intercropping_pattern')


summary(intercrop_clean$Intercropping_design)

unique(intercrop_clean$Intercropping_design)
unique(intercrop_clean$Intercropping_pattern)
unique(intercrop_clean$Country)
