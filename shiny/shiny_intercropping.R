library(tidyverse)
library(shiny)
library(bslib)

### load data
intercrop <- read_delim(delim = ';', here::here("data", "Database.csv"))

### Convert relevant columns to numeric data and create new column extracting experiment start year from the experimental period
intercrop$Yield_total_intercropping <- as.numeric(gsub(",", ".", intercrop$Yield_total_intercropping))
intercrop$LER_tot <- as.numeric(gsub(",", ".", intercrop$LER_tot))

intercrop <- intercrop|>
  mutate(start_year = as.numeric(str_extract(intercrop$Experiment_period, "^[0-9]{4}")))|>
  mutate(end_year = as.numeric(str_extract(intercrop$Experiment_period, "[0-9]{4}$")))

### Adding in different dataframers for plots
# Take only the time and country for Widget 1
intercrop_time <- intercrop |>
  janitor::clean_names() |>
  mutate(year = end_year) |>
  select(c(country, continent, year)) |>
  drop_na() |>
  group_by(year, country, continent) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(year, country)

cumulative <- intercrop_time |>
  group_by(continent) |>
  mutate(cumulative_experiments = cumsum(count)) |>
  ungroup()

### PCA Biplot data setup
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



### create the user interface
ui <- fluidPage(
  navbarPage(
    'Exploring Intercropping Experiments',
    # Add in theme
    theme = bs_theme(bootswatch = "sandstone"),
    
    # Add the option to select the continent
    tabPanel("Widget 1: Intercropping Yield by Continent",
             titlePanel("Cumulative Experiments Over Time by Continent"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("continent", "Select Continents:",
                                    choices = unique(cumulative$continent),
                                    selected = unique(cumulative$continent))
               ),
               
               # Plotting the cumulative experiments over time
               mainPanel(
                 plotOutput("plotCumulative"),
                 sliderInput("yearRange", "Select Year Range:",
                             min = min(cumulative$year),
                             max = max(cumulative$year),
                             value = c(min(cumulative$year), max(cumulative$year)),
                             step = 1)
               )
             )
    ),
    
    tabPanel("LER by Crop Types",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "crop1_type",
                   label = "Crop 1 type:",
                   choices = unique(intercrop$Crop_1_Common_Name)
                 ),
                 selectInput(
                   inputId = "crop2_type",
                   label = "Crop 2 type:",
                   choices = unique(intercrop$Crop_1_Common_Name)
                 )
               ),
               mainPanel(
                 'Barchart of different LERs for crops selected'
               )
             )
    ),
    
    tabPanel("PCA Biplot", 
             # Place for the chart
             plotOutput("PCA_plot")
    )
  )
)


### create the server function (where all the magic happens from data analysis)
# Year range slider for user to select the year range
server<-function(input,output){
  filteredData <- reactive({
    cumulative |>
      filter(year >= input$yearRange[1],
             year <= input$yearRange[2],
             continent %in% input$continent)
  })
  
  # Create the plot for experiments over time
  output$plotCumulative <- renderPlot({
    ggplot(filteredData(), aes(x = year, y = cumulative_experiments, 
                               color = continent, group = continent)) +
      geom_line() +
      geom_point(size = 1) +
      theme_classic() +
      scale_color_viridis_d(option = "viridis") +
      labs(x = "Year", 
           y = "Cumulative Experiments", 
           title = paste("Cumulative Experiments Over Time in", input$continent))
  })
  
  intercrop_sum_table<-reactive({
    intercrop_summary_df<-intercrop |>
      filter(Continent==input$Continent_type)|>
      group_by(start_year)|>
      summarize(mean_intercropped_yield=mean(Yield_total_intercropping,na.rm=TRUE),
                mean_LER=mean(LER_tot,na.rm=TRUE))
  })
  
  output$intercrop_table<-renderTable({
    intercrop_sum_table()
  })
  
  output$PCA_plot<-renderPlot({
    autoplot(intercrop_pca_scale,
             data = intercrop_pca_data_clean,
             loadings = TRUE,
             colour = 'end_year',
             loadings.label = TRUE,
             loadings.colour = "black",
             loadings.label.colour = "black",
             loadings.label.vjust = -0.5
    ) +
      scale_x_continuous()+
      theme_minimal()
  })
  
}

### To finalize shiny app we have to combine them into an app

shinyApp(ui=ui,server=server)
