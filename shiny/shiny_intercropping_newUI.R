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
library(viridis)
library(showtext)
library(png)
library(shinydashboard)
library(shinycssloaders)



showtext_auto()

### load viridis### load data ###  
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
yeti_theme <- bs_theme(bootswatch = "yeti") |>
  bs_theme_update(
    bg = "#FFFFFF",        # ✅ White background
    fg = "#333333",        # ✅ Dark gray text
    primary = "#008CBA",   # ✅ Yeti's bright blue
    secondary = "#E7E7E7", # ✅ Light gray accents
    success = "#43AC6A",   # ✅ Green for success
    info = "#5BC0DE",      # ✅ Sky blue info
    warning = "#E99002",   # ✅ Orange warnings
    danger = "#DA4F49",    # ✅ Red for errors
    base_font = font_google("Lato"),  
    heading_font = font_google("Roboto Slab"),  
    font_scale = 1.1  
  )

### Declare global plot variables
text_size <- 16
point_size <- 3

# Using theme_classic

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
  mutate("No Maize"=ifelse(!(Crop_1_Common_Name=="Maize"|Crop_2_Common_Name=="Maize"),1,0))|>
  mutate(China=ifelse(Country=="China",1,0))|>
  mutate("Rest of World"=ifelse(Country!="China",1,0))|>
  mutate(Additive=ifelse(Intercropping_design=="Additive",1,0))|>
  mutate(Replacement=ifelse(Intercropping_design=="Replacement",1,0))|>
  mutate(Row=ifelse(Intercropping_pattern=="Row",1,0))|>
  mutate(AF=ifelse(Intercropping_pattern=="AF",1,0))|>
  mutate(Strip=ifelse(Intercropping_pattern=="Strip",1,0))|>
  mutate(Mixed = ifelse(Intercropping_pattern == "Mixed", 1, 0)) |>
  rename(LER = LER_tot_calc) |>
  select(-Crop_1_Common_Name,-Crop_2_Common_Name,-Country,-Experiment_period,-Intercropping_design,-Intercropping_pattern)|>
  drop_na()

# Change "," values in numeric values to "."
intercrop_pca_data$LER <-as.numeric(gsub(",",".",intercrop_pca_data$LER))
intercrop_pca_data$Latitude <-as.numeric(gsub(",",".",intercrop_pca_data$Latitude))

# Check for NAs
intercrop_pca_data_clean<-na.omit(intercrop_pca_data)

# Scale PCA data
intercrop_pca_scale<-intercrop_pca_data_clean|>
  select(-Continent)|>
  prcomp(scale.=TRUE)

# Create a data frame with the PCA scores
pca_df <- data.frame(intercrop_pca_scale$x)

# % Variance Explained setup code
pc_names <- colnames(intercrop_pca_scale$rotation)
sd_vec <- intercrop_pca_scale$sdev
var_vec <- sd_vec^2

pct_expl_df <- data.frame(v = var_vec,
                          pct_v = var_vec / sum(var_vec),
                          pc = pc_names)

pct_expl_df<-pct_expl_df|>
  arrange(desc(pct_v))|>
  head(6) 


# ### create the user interface ###

ui <- fluidPage(

  ### declare css styling ### 
  tags$head(
    tags$style(HTML("
        /* Change the background color of the whole page */
    body {
      background-color: #f0f2f5 !important; /* #f0f2f5 Light gray */
    }

    /* Sidebar background color */
    .main-sidebar {
      background-color: #2c3e50 !important; /* Dark blue-gray */
    }
    
        /* Sidebar text color */
    .sidebar-menu > li > a {
      color: white !important;
    }
    
        /* Change background color of tab panels */
    .tab-content {
      background-color: #f0f2f5 !important;
      padding: 20px !important;
      border-radius: 10px !important;
    }


    
      /* Ensure header title bar and sidebar toggle button are the same height */
      .main-header .logo {
        height: 80px !important;
        line-height: 80px !important;
        padding-top: 10px !important; /* Adjust spacing if needed */
        padding-bottom: 10px !important;
        white-space: normal !important; /* Allows text to wrap */
        font-size: 26px !important;
        font-weight: bold !important;    /* Makes it bold */
        line-height: 1.2 !important; /* Adjust line spacing */
        text-align: center !important;
        word-wrap: break-word !important;
      }

      .main-header .navbar {
        min-height: 80px !important;
        height: 80px !important;
      }

      /* Ensure sidebar toggle (three lines button) matches header height */
      .main-header .sidebar-toggle {
        height: 80px !important;
        line-height: 80px !important;
        padding: -1000px 500px !important; /* Fine-tune spacing */
        display: flex !important;
        margin-right: 2000px !important;
        margin-bottom: 2000px !important;
        align-items: center !important;
      }

      /* Force sidebar toggle to align left */
      .navbar .sidebar-toggle {
        float: left !important;
        margin-left: 0px !important;
      }

      /* Increase sidebar menu font size */
      .sidebar-menu > li > a {
        font-size: 20px !important;
      }
      
      /* Increase font size of the numbers on the slider */
      .irs-grid-text {
        font-size: 20px !important;  /* Adjust as needed */
      }

      /* Increase font size for the selected value */
      .irs-single, .irs-from, .irs-to {
        font-size: 20px !important;
      }
    "))
  ),
  
  # remove shiny "red" warning messages on GUI
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  
  # load page layout
  dashboardPage(
    
    skin = "blue",
    
    dashboardHeader(
      title = "Intercropping Around the World",  
      titleWidth = 320
    ),
    
    dashboardSidebar(width = 320,
                     sidebarMenu(id = 'selected_tab',
                                 # Add margin-top to move the logo down
                                 tags$style(HTML("
                               .logo-center {
                                 display: flex;
                                 justify-content: center;
                                 align-items: center;
                                 margin-top: 25px; /* Adjust this value as needed */
                                 margin-bottom: 10px;
                               }
                               .logo-img {
                                 width: 200px; /* Adjust the size as needed */
                               }
                             ")),
                                 # Wrap the logo in a div with the 'logo-center' class
                                 div(class = "logo-center", 
                                     tags$img(src = "logo2.png", class = "logo-img")
                                 ),
                       menuItem("Home", tabName = "home", icon = icon("home")),
                       menuItem("Intercropping by Continent", tabName = "continent", icon = icon("thumbtack")),
                       menuItem("Map of Experiments", tabName = "map", icon = icon("map marked alt")),
                       menuItem("LER by Crop Types", tabName = "LER_comp", icon = icon("random", lib = "glyphicon")),
                       menuItem("Principal Component Analysis", tabName = "pca", icon = icon("stats", lib = "glyphicon")),
                       tags$p(
                         tags$br(),
                         "Developed by",
                         tags$a(href = "https://www.linkedin.com/in/jackson-coldiron/", "Jackson Coldiron", target = "_blank"), tags$br(),
                         tags$a(href = "https://www.linkedin.com/in/nicolasdestephano/", "Nicolas DeStephano", target = "_blank"), tags$br(),
                         "and", tags$a(href = "https://www.linkedin.com/in/isa-elias/", "Isa Elias", target = "_blank"), tags$br(),
                         style = "font-style: italic; font-size: 14px; text-align: left; padding-left: 15px;")
                     )
    ), # end dashboardSidebar
    
    dashboardBody(

      tabItems(
        
        tabItem(tabName = "home",
                
                
                # home section
                includeHTML("www/home.html")
                
        ),
        
        tabItem(tabName = "continent",
                titlePanel("Experiments over time for continents of your choosing!"),
                sidebarLayout(
                  position = "left",
                  sidebarPanel(
                    width = 3,
                    checkboxGroupInput("continent", "Select Continents:",
                                       choices = unique(cumulative$continent),
                                       selected = unique(cumulative$continent)),
                    
                  ),
                  
                  # Plotting the cumulative experiments over time
                  mainPanel(
                    width = 9,
                    tags$div(
                      style = "padding: 5px; border-radius: 10px; background-color: #f0f2f5;",
                      fluidRow(
                        column(12, offset = 0,
                                 plotOutput("plotCumulative")
                        ),
                        column(8, offset = 1, 
                           sliderInput("yearRange", "",
                                       min = min(cumulative$year),
                                       max = 2025,
                                       value = c(min(cumulative$year), max(cumulative$year)),
                                       step = 1,
                                       sep = "",
                                       width = "89%",
                                       )
                               )
                        )
                    )
                  )
                )                
        ),
        
        tabItem(tabName = 'map',
          titlePanel("Click a country to learn more!"),
          sidebarLayout(
            position = "right",
            sidebarPanel(
              uiOutput("country_info"),
              width = 3,  
              style = "padding: 10px; margin: 0px; width: 100%;"
            ),
            mainPanel(
              width = 9,  
              style = "padding-left: 0px; margin-left: 0px",
              fluidRow(
                column(12, plotlyOutput("interactive_map", height = "65vh"))
              )
            )
          )          
        ),
        
        tabItem(tabName = "LER_comp", 
            titlePanel("Choose two crops to compare their relative LER"),
            sidebarLayout(
              sidebarPanel(
                width = 6,
                fluidRow(
                  column(6, 
                         selectInput(
                           inputId = "crop1_type",
                           label = "Crop 1:",
                           selected = 'Maize',
                           choices = unique(intercrop$Crop_1_Common_Name)
                         )
                  ),
                  
                  column(6, 
                         selectInput(
                           inputId = "crop2_type",
                           label = "Crop 2:",
                           selected = 'Cowpea',
                           choices = NULL # initialize empty, will be updated dynamically. NOT WORKING
                         )
                  )
                )
              ),
              
              mainPanel(
                width = 12,
                fluidRow(
                  # First plot (LER_plot)
                  column(10, offset = 0, 
                         plotOutput('LER_plot', width = "100%", height = "500px"),
                         tags$hr(style = "border-top: 2px solid blue; margin: 30px 0;")  # Adds separation line
                  ),
                  
                  # Second plot (crop1_exp_over_time_plot)
                  column(10, offset = 0, 
                           # Title for the second plot
                           tags$h3("Cumulative Experiments of Crop 1 Over Time", style = "text-align: center; color: black;"),
                           plotOutput('crop1_exp_over_time_plot', width = "100%", height = "500px")
                  )
                )
              )
            )   
        ),
        
        tabItem(tabName = "pca",
                titlePanel("Explore a Principal Component Analysis of the data"),
                sidebarLayout(
                  sidebarPanel(
                    width = 8,
                    fluidRow(
                      column(5, offset = 0,
                        selectInput("pc_select_1", 
                                    label = "Select First Principal Component", 
                                    choices = paste("PC", 1:6, sep = ""),
                                    selected = "PC1")
                      ),
                      column(5, offset = 1,  
                        selectInput("pc_select_2", 
                                    label = "Select Second Principal Component", 
                                    choices = paste("PC", 1:6, sep = ""),
                                    selected = "PC2")
                      )
                    )
                  ),
                  
                  mainPanel(
                      width = 12,
                      fluidRow(
                        # First plot (PCA plot)
                        column(12, offset = 0, 
                               tags$div(
                                 style = "padding: 5px; border-radius: 10px; background-color: #f0f2f5;",
                                 plotOutput('PCA_plot', width = "100%", height = "500px"),
                                 
                                 wellPanel(
                                   h4("PCA Results Summary"),
                                   p("Above is a Principal Component Analysis (PCA) biplot showing the distribution of experiments and the linear relationship between experimental variables based on the two principal components of your choosing."),
                                   p("Using a biplot, we reduce a multidimensional relationship into two-dimensional space. The biplot shows us 1. The loading (eigenvalues) of experimental variables for two PCs (grey arrows) and 2. The distribution of experiments based on those PCs."),
                                   p("The plot reveals the clustering of experiments color-coded by continent as well as the correlation between the relative loadings of each principle component."),
                                   p("The percentage of variance explained by each PC is indicated on the axes. The length of each arrow indicates the variance associated with a PC direction and the angle between arrows reveals their correlation."),
                                   )
                               ),
                               
                               tags$hr(style = "border-top: 2px black; margin: 30px 0;")  # Adds separation line
                        ),
                        
                        # Second plot (pct_var_PCA)
                        column(10, offset = 0, 
                               tags$div(
                                 style = "padding: 5px; border-radius: 10px; background-color: #f0f2f5;",
                                 
                                 # Title for the second plot
                                 tags$h3("Percentage of Variance Explained", style = "text-align: center; color: black;"),
                                 
                                 plotOutput('PCA_var', width = "100%", height = "500px")
                               )
                        )
                      )
                    )
                )
        )
      ) # end tab items
    ) # end dashboardBody
  ) # end dashboardPage
) # end shiny fluid page

### create the server function (where all the magic happens from data analysis) ### 
# Year range slider for user to select the year range
server <- function(input,output, session){
  
  session$onFlushed(function() {
    updateTabItems(session, "selected_tab", "home")
  })
  
  ### Tab 1: Experiments Overview ###
  filteredData <- reactive({
    cumulative |>
      filter(year >= input$yearRange[1],
             year <= input$yearRange[2],
             year <= 2025,
             continent %in% input$continent)
  })
  
  # Create the plot for experiments over time
  output$plotCumulative <- renderPlot({
    ggplot(filteredData(), aes(x = year, y = cumulative_experiments, 
                               color = continent, group = continent)) +
      geom_line() +
      geom_point(size = 1) +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "#f0f2f5", color = NA),  # Change plot panel background
        plot.background = element_rect(fill = "#f0f2f5", color = NA),   # Change full plot background
        legend.background = element_rect(fill = "#f0f2f5", color = NA), # Change legend background
        legend.key = element_rect(fill = "#f0f2f5", color = NA),        # Change legend key background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_text(size = text_size, color = "black"),
        axis.title = element_text(size = text_size, color = "black"),
        legend.text = element_text(size = text_size, color = "black"),
        legend.title = element_text(size = text_size, color = "black", face = "bold")
      )+
      
      theme(
        text = element_text(size = text_size, family = "Lato"),              # Change all text size
        axis.text = element_text(size = text_size),  
        axis.text.y = element_text(family = "Open Sans"),# Axis tick labels
        axis.title = element_text(size = text_size, family = "Open Sans"),        # Axis titles
        legend.text = element_text(size = text_size), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16, family = "Open Sans"), 
        legend.title = element_text(size = text_size, face = "bold")) +
      scale_color_viridis_d(option = "viridis") +
      scale_x_continuous(
        breaks = seq(
          from = min(cumulative$year, na.rm = TRUE),  
          to = 2025, 
          by = 5
        )
      ) +
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
        name = "Count"
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#f0f2f5", color = NA),  # Change plot panel background
        plot.background = element_rect(fill = "#f0f2f5", color = NA),   # Change full plot background
        legend.background = element_rect(fill = "#f0f2f5", color = NA), # Change legend background
        legend.key = element_rect(fill = "#f0f2f5", color = NA),        # Change legend key background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_text(size = text_size, color = "black"),
        axis.title = element_text(size = text_size, color = "black"),
        legend.text = element_text(size = text_size, color = "black"),
        legend.title = element_text(size = text_size, color = "black", face = "bold")
      )+
      theme(plot.margin = margin(0, 0, 0, 0)) +
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
  
  #### Tab: PCA ####
  filtered_pca_scale <- reactive({
    # Extract the selected PCs
    selected_pc_1 <- as.numeric(gsub("PC", "", input$pc_select_1))  # Extract the first selected PC
    selected_pc_2 <- as.numeric(gsub("PC", "", input$pc_select_2))  # Extract the second selected PC
    
    list(selected_pc_1 = selected_pc_1, selected_pc_2 = selected_pc_2)
  })
  
  # Update the available choices for the second PC when the first PC is selected
  observe({
    # Get the currently selected PC1
    selected_pc_1 <- as.numeric(gsub("PC", "", input$pc_select_1))
    
    # Update the choices for the second PC to exclude the first selected PC
    updateSelectInput(session, "pc_select_2", 
                      choices = setdiff(paste("PC", 1:10, sep = ""), paste("PC", selected_pc_1, sep = "")),
                      selected = paste("PC", ifelse(selected_pc_1 == 1, 2, 1), sep = ""))
  })
  
  # Render the plot based on selected PCs
  output$PCA_plot <- renderPlot({
    # Get the selected PCs from the reactive expression
    selected_pc <- filtered_pca_scale()
    
    # Use autoplot to plot the selected PCs
    autoplot(intercrop_pca_scale, 
             data = intercrop_pca_data_clean,
             x = selected_pc$selected_pc_1, 
             y = selected_pc$selected_pc_2, 
             loadings = TRUE,
             colour = 'Continent',
             loadings.label = TRUE,
             loadings.colour = "grey",
             loadings.label.colour = "black",
             loadings.label.repel = TRUE,
             loadings.label.fontface = "bold",  
             force = 10,  
             max.iter = 1000,  
             box.padding = 0.5,  
             nudge_x = 0.1,  
             nudge_y = 0.1
    ) +
      scale_color_viridis(discrete = TRUE) +   
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "#f0f2f5", color = NA),  # Change plot panel background
        plot.background = element_rect(fill = "#f0f2f5", color = NA),   # Change full plot background
        legend.background = element_rect(fill = "#f0f2f5", color = NA), # Change legend background
        legend.key = element_rect(fill = "#f0f2f5", color = NA),        # Change legend key background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_text(size = text_size, color = "black"),
        axis.title = element_text(size = text_size, color = "black"),
        legend.text = element_text(size = text_size, color = "black"),
        legend.title = element_text(size = text_size, color = "black", face = "bold")
      ) +
      theme(
        text = element_text(size = 12, family = "Lato"),  
        axis.text = element_text(size = 12),
        axis.text.y = element_text(hjust = 1, size = 16, family = "Open Sans"),  
        axis.title = element_text(size = 12, family = "Open Sans"),  
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16, family = "Open Sans"),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
 
  output$PCA_var <- renderPlot({
    ggplot(pct_expl_df, aes(x = pc, y = v, fill = v)) +
      geom_col(fill = viridis(1, option = "C")[1]) +  # Use light blue from viridis palette
      geom_text(aes(label = scales::percent(pct_v)), vjust = 0, nudge_y = .05) +
      labs(x = 'Principal component', y = 'Variance explained') +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "#f0f2f5", color = NA),  # Change plot panel background
        plot.background = element_rect(fill = "#f0f2f5", color = NA),   # Change full plot background
        legend.background = element_rect(fill = "#f0f2f5", color = NA), # Change legend background
        legend.key = element_rect(fill = "#f0f2f5", color = NA),        # Change legend key background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_text(size = text_size, color = "black"),
        axis.title = element_text(size = text_size, color = "black"),
        legend.text = element_text(size = text_size, color = "black"),
        legend.title = element_text(size = text_size, color = "black", face = "bold")
      ) +
      theme(
        text = element_text(size = 12, family = "Lato"),  
        axis.text = element_text(size = 12),
        axis.text.y = element_text(hjust = 1, size = 16, family = "Open Sans"),  
        axis.title = element_text(size = 12, family = "Open Sans"),  
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16, family = "Open Sans"),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  
  #### Tab: LER plots ####
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
                                 drop_na(LER_crop1, LER_crop2)|>
                                 pull(Crop_2_Common_Name))
    
    updateSelectInput(session, "crop2_type", choices = filtered_choices)
  })
  
  intercrop_LER_filtered <- reactive({
    intercrop_LER |>
      filter(crop1 == input$crop1_type, crop2 == input$crop2_type) |>
      mutate(intercropping_design = ifelse(is.na(intercropping_design), "Not specified", intercropping_design)) |>
      drop_na(ler_crop1, ler_crop2)
  })
  
  output$LER_plot <- renderPlot({
    
    ggplot(data = intercrop_LER_filtered(), aes(x=ler_crop1, y = ler_crop2, color = country, shape = intercropping_design))+
      geom_point(size = 3)+
      geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), 
                   linetype = "dashed", color = "black") +
      #xlim(0,1.25)+
      #ylim(0,1.25)+
      labs(x = paste0(input$crop1_type, ' LER'), y = paste0(input$crop2_type, ' LER'), 
           color = 'Country', 
           shape = 'Intercropping design')+
      theme_classic()+
      theme(
        panel.background = element_rect(fill = "#f0f2f5", color = NA),  # Change plot panel background
        plot.background = element_rect(fill = "#f0f2f5", color = NA),   # Change full plot background
        legend.background = element_rect(fill = "#f0f2f5", color = NA), # Change legend background
        legend.key = element_rect(fill = "#f0f2f5", color = NA),        # Change legend key background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_text(size = text_size, color = "black"),
        axis.title = element_text(size = text_size, color = "black"),
        legend.text = element_text(size = text_size, color = "black"),
        legend.title = element_text(size = text_size, color = "black", face = "bold")
      )+
      theme(
        text = element_text(size = text_size),              # Change all text size
        axis.text = element_text(size = text_size),         # Axis tick labels
        axis.title = element_text(size = text_size),        # Axis titles
        legend.text = element_text(size = text_size),       # Legend labels
        legend.title = element_text(size = text_size, face = "bold")  # Legend title
        # plot.margin = margin(0, 0, 0, 0, "cm")       # Remove extra margin space
        )+
      scale_color_viridis(discrete = TRUE)    # Apply the Viridis color palette
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
           color = 'Crop 2')+
      theme_classic()+
      theme(
        panel.background = element_rect(fill = "#f0f2f5", color = NA),  # Change plot panel background
        plot.background = element_rect(fill = "#f0f2f5", color = NA),   # Change full plot background
        legend.background = element_rect(fill = "#f0f2f5", color = NA), # Change legend background
        legend.key = element_rect(fill = "#f0f2f5", color = NA),        # Change legend key background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_text(size = text_size, color = "black"),
        axis.title = element_text(size = text_size, color = "black"),
        legend.text = element_text(size = text_size, color = "black"),
        legend.title = element_text(size = text_size, color = "black", face = "bold")
      )+
      theme(
        text = element_text(size = text_size),              # Change all text size
        axis.text = element_text(size = text_size),         # Axis tick labels
        axis.title = element_text(size = text_size),        # Axis titles
        legend.text = element_text(size = text_size),       # Legend labels
        legend.title = element_text(size = text_size, face = "bold")  # Legend title
        #plot.margin = margin(0, 0, 0, 0, "cm")       # Remove extra margin space
      )+
      scale_color_viridis(discrete = TRUE)    # Apply the Viridis color palette
  })
}

### To finalize shiny app we have to combine them into an app

shinyApp(ui=ui,server=server)
