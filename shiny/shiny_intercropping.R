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


### create the user interface
ui <- fluidPage(
  navbarPage(
    'Exploring Intercropping Experiments',
    theme = bs_theme(bootswatch = "sandstone"),
    
    tabPanel("Widget 1: Intercropping Yield by Continent",
             titlePanel("Cumulative Experiments Over Time by Continent"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("continent", "Select Continents:",
                                    choices = unique(cumulative$continent),
                                    selected = unique(cumulative$continent))
               ),
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
    )
  )
)


### create the server function (where all the magic happens from data analysis)

server<-function(input,output){
  filteredData <- reactive({
    cumulative |>
      filter(year >= input$yearRange[1],
             year <= input$yearRange[2],
             continent %in% input$continent)
  })
  
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
  
}

### To finalize shiny app we have to combine them into an app

shinyApp(ui=ui,server=server)
