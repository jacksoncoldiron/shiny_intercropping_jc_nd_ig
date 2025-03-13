library(tidyverse)
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(countrycode)
library(plotly)



### load data ###  
intercrop <- read_delim(delim = ';', here::here("data", "Database.csv"))

### Convert relevant columns to numeric data and create new column extracting experiment start year from the experimental period ### 
intercrop$Yield_total_intercropping <- as.numeric(gsub(",", ".", intercrop$Yield_total_intercropping))
intercrop$LER_tot <- as.numeric(gsub(",", ".", intercrop$LER_tot))
intercrop$LER_crop1 <- as.numeric(gsub(",", ".", intercrop$LER_crop1))
intercrop$LER_crop2 <- as.numeric(gsub(",", ".", intercrop$LER_crop2))

intercrop <- intercrop|>
  mutate(start_year = as.numeric(str_extract(intercrop$Experiment_period, "^[0-9]{4}")))|>
  mutate(end_year = as.numeric(str_extract(intercrop$Experiment_period, "[0-9]{4}$"))) |>
  mutate(iso3 = countrycode(Country, origin = "country.name", destination = "iso3c")) |>
  mutate(Country = recode(Country, "Philipines" = "Phillippines"))

### Custom Theme ###
sandstone_theme <- bs_theme(bootswatch = "sandstone") |>
  bs_theme_update(
  bg = "#2E8B57",           # Light beige background
  fg = "#3E3E3E",           # Dark gray text
  primary = "#8B5E3C",      # Warm brown primary color
  secondary = "#D2B48C",    # Tan secondary color
  success = "#4CAF50",      # Green for success messages
  info = "#2E8B57",         # Sea green for informational messages
  warning = "#E9967A",      # Light coral for warnings
  danger = "#A52A2A",       # Brownish-red for danger alerts
  base_font = font_google("Signika"),  
  heading_font = font_google("Crete Round"), 
  font_scale = 1.1  
)

### TAB 1: Experiments over time data setup ### 
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

# Build the data for the map
aggregate2 <- intercrop_time |>
  group_by(country, year) |>
  summarize(year_count_continent = sum(count), .groups = "drop") |>
  drop_na()

agg_noyear <- aggregate2 |>
  group_by(country) |>
  summarize(count = sum(year_count_continent), .groups = "drop")

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Convert country names to match map
intercrop <- intercrop |>
  mutate(iso3 = countrycode(Country, origin = "country.name", destination = "iso3c"))
intercrop <- intercrop |> filter(!is.na(iso3))

# Aggregate experiment count per country
agg_noyear <- intercrop |>
  group_by(iso3) |>
  summarise(count = n(), .groups = "drop")

# Merge with world map
map_data2 <- world |>
  left_join(agg_noyear, by = c("iso_a3" = "iso3")) |>
  mutate(count = replace_na(count, 0))


### TAB 2: LER plots data setup ###
intercrop_LER <- intercrop |>
  select('Country', 'Continent', 'end_year', 'Crop_1_Common_Name', 'Crop_2_Common_Name', 'LER_crop1', 'LER_crop2', 'LER_tot', 'Intercropping_design', 'Intercropping_pattern', 'Greenhouse', 'Experiment_year', 'Organic_ferti', 'Mineral_ferti') |>
  janitor::clean_names() |>
  rename(crop1 = crop_1_common_name, crop2 = crop_2_common_name, year = end_year) |>
  mutate(intercropping_pattern = as.factor(intercropping_pattern)) |>
  mutate(country = as.factor(country))


### TAB 3: PCA Biplot data setup ### 
library(ggfortify)

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



### create the user interface ### 
ui <- page_fluid(
  navbarPage(
    'Exploring Intercropping Experiments',
    # Add in theme
    theme = sandstone_theme,
    
    ### About Tab ### 
    tabPanel("About",
             
             layout_columns(  
               card(tags$b('Intro to Intercropping')),  
               card(tags$b("Graphics")),
               card(tags$b("Widgets overview"),
                    tags$i("Widget 1:"), "Explore intercropping yield by continent.", tags$br(),
                    tags$i("Widget 2:"), "Compares the LER of different crop types.", tags$br(),
                    tags$i("Widget 3:"), "Displays a biplot of a PCA, demonstrating the relatedness and effect of each variable"),
               col_widths = c(4, 8, 4)
             )
    ),
        
    ### Tab 1 ###
    # Add the option to select the continent
    tabPanel("Intercropping by Continent",
             titlePanel("Experiments over time for Continents of your choosing!"),
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 checkboxGroupInput("continent", "Select Continents:",
                                    choices = unique(cumulative$continent),
                                    selected = unique(cumulative$continent)),
            
               ),
          
               # Plotting the cumulative experiments over time
               mainPanel(
                 fluidRow(
                   column(12, plotOutput("plotCumulative")),
                   column(8, offset = 1, 
                          sliderInput("yearRange", "Select Year Range:",
                                          min = min(cumulative$year),
                                          max = max(cumulative$year),
                                          value = c(min(cumulative$year), max(cumulative$year)),
                                          step = 1,
                                          sep = "",
                                          width = "100%"))
                 )
                )
              )
  ),
    
  ### Tab 1A ###
  # Input map
    tabPanel("Experiments by Country",
             titlePanel(""),
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 uiOutput("country_info")
                 ),
               mainPanel(
                 titlePanel("Click a Country to learn more!"),
               fluidRow(
                 column(12, plotlyOutput("interactive_map"))
               )
              )
            )
    ),
            
    
    ### Tab 2 ###
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
                 plotOutput('LER_plot'), 
                 plotOutput('crop1_exp_over_time_plot')
               )
             )
    ),
    
    ### Tab 3 ###
    tabPanel("PCA Biplot", 
             # Place for the chart
             plotOutput("PCA_plot")
    )
  )
)



### create the server function (where all the magic happens from data analysis) ### 
# Year range slider for user to select the year range
server <- function(input,output, session){
  
  ### Tab 1: Experiments Overview ###
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
           color = NULL)
    
  })
  
  ### Tab 1A: Interactive Map ###
  output$interactive_map <- renderPlotly({
    p <- ggplot(map_data2) +
      geom_sf(aes(fill = count), color = "gray40") +
      scale_fill_gradientn(
        colours = c("grey90", viridisLite::viridis(5)),
        values = scales::rescale(c(0, 1, max(map_data2$count, na.rm = TRUE))),
        limits = c(0, max(map_data2$count, na.rm = TRUE)),
        name = ""
      ) +
      theme_minimal() +
      labs(title = "", x = "", y  = "")
    
    ggplotly(p) |>
      event_register("plotly_click")
  })
  
  # Display country info
  output$country_info <- renderUI({
    click <- event_data("plotly_click")
    
    print(click)
    
    # Check if click data is valid
    if (is.null(click) || !("x" %in% names(click)) || !("y" %in% names(click))) {
      print("No country selected")
      return(NULL)
    }
    
    clicked_long <- click$x  # Extract longitude
    clicked_lat <- click$y   # Extract latitude
    
    print(paste("Clicked coordinates: ", clicked_long, clicked_lat))
    
    # Ensure correct CRS
    if (st_crs(map_data2)$epsg != 4326) {
      map_data2 <- st_transform(map_data2, crs = 4326)
    }
    
    
    # Find the Country Name from Coordinates
    clicked_point <- st_sfc(st_point(c(clicked_long, clicked_lat)), crs = 4326)
    nearest_country_index <- tryCatch(
      st_nearest_feature(clicked_point, map_data2),
      error = function(e) NA)
    
    # Check if a valid country was found
    if (is.na(nearest_country_index) || nearest_country_index > nrow(map_data2)) {
      print("Warning: No country found for clicked location!")
      return(NULL)
    }
    
    clicked_country <- map_data2$iso_a3[nearest_country_index]
    
    print(paste("Matched country:", clicked_country))
    
    # Check if the country exists in the dataset
    if (!clicked_country %in% intercrop$iso3) {
      print("Warning: Clicked country not found in dataset!")
      return(NULL)
    }
    
    country_data <- intercrop |>
      filter(iso3 == clicked_country) 
    
    if (nrow(country_data) == 0) {
      return(NULL)
    }
        
    total_experiments <- nrow(country_data)
    
    common_crop <- country_data |>
      select(Crop_1_Common_Name, Crop_2_Common_Name) |>
      pivot_longer(cols = everything(), values_to = "crop") |>
      filter(!is.na(crop)) |>
      count(crop, sort = TRUE) |>
      slice_head(n = 1) |>
      pull(crop) 
    
    common_pattern <- country_data |>
      filter(!is.na(Intercropping_pattern)) |>
      count(Intercropping_pattern, sort = TRUE) |>
      slice_head(n = 1) |>
      pull(Intercropping_pattern)
    
    # Return the updated table
    
    tagList(
      h4(""),
      tags$p(strong("Country: "), unique(country_data$Country)),
      tags$p(strong("Experiments: "), total_experiments),
      tags$p(strong("Top Crop: "), common_crop),
      tags$p(strong("Top Pattern: "), common_pattern)
    )
    
  })
  
###########################################################
 
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
    req(intercrop_LER)
    # Get valid crop2 choices for default crop1 (Maize)
    initial_choices <- unique(intercrop_LER |>
                                filter(crop1 == "Maize") |>
                                pull(crop2))
    
    updateSelectInput(session, "crop2_type",
                      selected = "Cowpea",
                      choices = initial_choices)
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
  
  # set up data for cumulative experiments by crop 
  # find top 10 crop2s based on crop1
  crop2_top <- reactive({
    intercrop_LER |>
    filter(crop1 == input$crop1_type)|>
    group_by(crop2)|>
    summarize(n = n())|>
    arrange(desc(n))|>
    slice(1:10)|>
    pull(crop2)
  })
  
  # big pipe op that filters to just crop1 and top crop2, 
  # groups by crop1 and 2, 
  # counts how many experiments are in a given year, groups by crop2, 
  # calculate cumulative sum as the years go on, 
  # and finally makes crop2 a factor with levels ordered by cumulative count and label with n=cumulative count
  # for plotting purposes
  crop1_crops_over_time <- reactive({
    intercrop_LER |>
    select(c(crop1, crop2, year)) |>
    filter(crop1 == input$crop1_type, crop2 %in% crop2_top())|>
    drop_na() |>
    group_by(year, crop1, crop2) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(crop2) |>
    mutate(cumulative_count = cumsum(count), 
           total_cumulative = max(cumulative_count)) |>
    mutate(crop2 = factor(crop2, 
                          levels = crop2,  # Order by cumulative count
                          labels = paste0(crop2, " (n=", total_cumulative, ")"))) # Add count to label
  })
  
  
  # Plot experiments over time based of crop1
  output$crop1_exp_over_time_plot <- renderPlot({
    ggplot(data = crop1_crops_over_time(), aes(x = year, y = cumulative_count, color = crop2))+
      geom_point()+
      geom_line()+
      labs(x = 'Year', 
           y = 'Cumulative experiment count', 
           color = 'Crop 2 type')+
      theme_bw()
    
    
  })
}

### To finalize shiny app we have to combine them into an app

shinyApp(ui=ui,server=server)
