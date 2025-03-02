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



########### 3. Widget 3: Reactive Table for Top 20 pairings of a specific Crop 1 ###########



########### PCA ###########
library(ggfortify) # For PCA biplot


# Select out the relevant variables we want to assess for PCA
intercrop_pca_data<-intercrop |>
  select(LER_tot_calc,Crop_1_Common_Name,Crop_2_Common_Name,
         Intercropping_design,Intercropping_pattern,Nitrogen_rate_kg_ha,
         Experiment_period,Country,Latitude)|>
  mutate(end_year=as.numeric(str_extract(Experiment_period,"(?<=-)\\d{4}$")))|> # Extract end year from experiment period
  mutate(maize=ifelse(Crop_1_Common_Name=="Maize"|Crop_2_Common_Name=="Maize",1,0))|>
  mutate(no_maize=ifelse(!(Crop_1_Common_Name=="Maize"|Crop_2_Common_Name=="Maize"),1,0))|>
  mutate(china=ifelse(Country=="China",1,0))|>
  mutate(rest_of_world=ifelse(Country!="China",1,0))|>
  mutate(Nitrogen_rate_kg_ha=na_if(Nitrogen_rate_kg_ha,"Unclear"))|>
  mutate(Additive_pattern=ifelse(Intercropping_design=="Additive",1,0))|>
  mutate(Replacement_pattern=ifelse(Intercropping_design=="Replacement",1,0))|>
  mutate(Row_intercropping=ifelse(Intercropping_pattern=="Row",1,0))|>
  mutate(AF_intercropping=ifelse(Intercropping_pattern=="AF",1,0))|>
  mutate(Strip_intercropping=ifelse(Intercropping_pattern=="Strip",1,0))|>
  mutate(Mixed_intercropping=ifelse(Intercropping_pattern=="Mixed",1,0))|>
  select(-Crop_1_Common_Name,-Crop_2_Common_Name,-Country,-Experiment_period,-Intercropping_design,-Intercropping_pattern)|>
  drop_na()


# Change "," values in numeric values to "."
intercrop_pca_data$LER_tot_calc <-as.numeric(gsub(",",".",intercrop_pca_data$LER_tot_calc))
intercrop_pca_data$Nitrogen_rate_kg_ha <-as.numeric(gsub(",",".",intercrop_pca_data$Nitrogen_rate_kg_ha))
intercrop_pca_data$Latitude <-as.numeric(gsub(",",".",intercrop_pca_data$Latitude))

# Check for NAs
intercrop_pca_data_clean<-na.omit(intercrop_pca_data)

# Scale PCA data
intercrop_pca_scale<-intercrop_pca_data_clean|>
  prcomp(scale.=TRUE)

# See Loadings for each PC
# intercrop_pca_scale$rotation

# Scree Plot to assess variance explained by each PC
screeplot(intercrop_pca_scale,type="lines")
screeplot(intercrop_pca_scale,type="barplot")


# Plot a PCA
PCA_plot<-autoplot(intercrop_pca_scale,
         data = intercrop_pca_data_clean,
         loadings = TRUE,
         colour = 'end_year',
         loadings.label = TRUE,
         loadings.colour = "black",
         loadings.label.colour = "black",
         loadings.label.vjust = -0.5
) +
  theme_minimal()

