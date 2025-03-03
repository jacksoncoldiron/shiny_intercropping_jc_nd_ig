library(tidyverse)
library(shiny)
library(bslib)


### load data ###  
intercrop <- read_delim(delim = ';', here::here("data", "Database.csv"))

### Convert relevant columns to numeric data and create new column extracting experiment start year from the experimental period ### 
intercrop$Yield_total_intercropping <- as.numeric(gsub(",", ".", intercrop$Yield_total_intercropping))
intercrop$LER_tot <- as.numeric(gsub(",", ".", intercrop$LER_tot))
intercrop$LER_crop1 <- as.numeric(gsub(",", ".", intercrop$LER_crop1))
intercrop$LER_crop2 <- as.numeric(gsub(",", ".", intercrop$LER_crop2))

intercrop <- intercrop|>
  mutate(start_year = as.numeric(str_extract(intercrop$Experiment_period, "^[0-9]{4}")))|>
  mutate(end_year = as.numeric(str_extract(intercrop$Experiment_period, "[0-9]{4}$")))

### Adding in different dataframes for plots ### 
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

### PCA Biplot data setup ### 
# Select out the relevant variables we want to assess for PCA
intercrop_pca_data <- intercrop |>
  select(LER_tot_calc,Crop_1_Common_Name,Crop_2_Common_Name,
         Intercropping_design,Continent,Intercropping_pattern,
         Experiment_period,Country,Latitude)|>
  mutate(Year=as.numeric(str_extract(Experiment_period,"(?<=-)\\d{4}$")))|> # Extract end year from experiment period
  mutate(Maize=ifelse(Crop_1_Common_Name=="Maize"|Crop_2_Common_Name=="Maize",1,0))|>
  mutate(No_Maize=ifelse(!(Crop_1_Common_Name=="Maize"|Crop_2_Common_Name=="Maize"),1,0))|>
  mutate(China=ifelse(Country=="China",1,0))|>
  mutate(Rest_of_world=ifelse(Country!="China",1,0))|>
  mutate(Additive=ifelse(Intercropping_design=="Additive",1,0))|>
  mutate(Replacement=ifelse(Intercropping_design=="Replacement",1,0))|>
  mutate(Row=ifelse(Intercropping_pattern=="Row",1,0))|>
  mutate(AF=ifelse(Intercropping_pattern=="AF",1,0))|>
  mutate(Strip=ifelse(Intercropping_pattern=="Strip",1,0))|>
  mutate(Mixed=ifelse(Intercropping_pattern=="Mixed",1,0))|>
  select(-Crop_1_Common_Name,-Crop_2_Common_Name,-Country,-Experiment_period,-Intercropping_design,-Intercropping_pattern)|>
  drop_na()

# Change "," values in numeric values to "."
intercrop_pca_data$LER_tot_calc <-as.numeric(gsub(",",".",intercrop_pca_data$LER_tot_calc))
intercrop_pca_data$Latitude <-as.numeric(gsub(",",".",intercrop_pca_data$Latitude))

# Check for NAs
intercrop_pca_data_clean<-na.omit(intercrop_pca_data)

# Scale PCA data
intercrop_pca_scale<-intercrop_pca_data_clean|>
  select(-Continent)|>
  prcomp(scale.=TRUE)



### LER plots data setup ###
intercrop_LER <- intercrop |>
  select('Country', 'Continent', 'end_year', 'Crop_1_Common_Name', 'Crop_2_Common_Name', 'LER_crop1', 'LER_crop2', 'LER_tot', 'Intercropping_design', 'Intercropping_pattern', 'Greenhouse', 'Experiment_year', 'Organic_ferti', 'Mineral_ferti') |>
  janitor::clean_names() |>
  rename(crop1 = crop_1_common_name, crop2 = crop_2_common_name, year = end_year) |>
  mutate(intercropping_pattern = as.factor(intercropping_pattern)) |>
  mutate(country = as.factor(country))



### create the user interface ### 
ui <- fluidPage(
  navbarPage(
    'Exploring Intercropping Experiments',
    # Add in theme
    theme = bs_theme(bootswatch = "sandstone"),
    
    # Add the option to select the continent
    tabPanel("Intercropping Yield by Continent",
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
                   label = "Crop 1:",
                   selected = 'Maize',
                   choices = unique(intercrop$Crop_1_Common_Name)
                 ),
                 selectInput(
                   inputId = "crop2_type",
                   label = "Crop 2:",
                   selected = 'Cowpea',
                   choices = NULL # initialize empty, will be updated dynamically
                 )
               ),
               mainPanel(
                 plotOutput('LER_plot')
               )
             )
    ),
    
    tabPanel("PCA Biplot", 
             # Place for the chart
             plotOutput("PCA_plot")
    )
  )
)


### create the server function (where all the magic happens from data analysis) ### 
# Year range slider for user to select the year range
server<-function(input,output, session){
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
             colour = 'Continent',
             loadings.label = TRUE,
             loadings.colour = "black",
             loadings.label.colour = "black",
             loadings.label.vjust = -0.5
    ) +
      theme_minimal()
  })
  
  #### Tab 2: LER plots ####
  
  observe({
    # Get valid crop2 choices for default crop1 (Maize)
    initial_choices <- unique(intercrop_LER |>
                                filter(crop1 == "Maize") |>
                                pull(crop2))
    
    updateSelectInput(session, "crop2_type",
                      choices = initial_choices,
                      selected = "Cowpea")
  })
  
  # Update crop2 choices dynamically based on crop1 selection
  observeEvent(input$crop1_type, {
    filtered_choices <- unique(intercrop |> 
                                 filter(Crop_1_Common_Name == input$crop1_type) |> 
                                 pull(Crop_2_Common_Name))
    
    updateSelectInput(session, "crop2_type", choices = filtered_choices)
  })
  
  intercrop_LER_filtered <- reactive({
    intercrop_LER |>
    filter(crop1 == input$crop1_type, 
           crop2 == input$crop2_type) |>
      drop_na(ler_crop1, ler_crop2)
  })
  
  output$LER_plot <- renderPlot({
    
    ggplot(data = intercrop_LER_filtered(), aes(x=ler_crop1, y = ler_crop2, color = country))+
      geom_point(size = 3)+
      geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), 
                   linetype = "dashed", color = "black") +
      #xlim(0,1.25)+
      #ylim(0,1.25)+
      labs(x = paste0(input$crop1_type, ' LER'), y = paste0(input$crop2_type, ' LER'), color = 'Country')+
      theme_bw()+
      theme(
        text = element_text(size = 20),              # Change all text size
        axis.text = element_text(size = 20),         # Axis tick labels
        axis.title = element_text(size = 20),        # Axis titles
        legend.text = element_text(size = 20),       # Legend labels
        legend.title = element_text(size = 20, face = "bold"),  # Legend title
        plot.margin = margin(0, 0, 0, 0, "cm")       # Remove extra margin space
        )
  })
}

### To finalize shiny app we have to combine them into an app

shinyApp(ui=ui,server=server)
