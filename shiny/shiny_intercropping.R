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


### create the user interface
ui <- fluidPage(
  navbarPage(
    'Exploring Intercropping Experiments',
    theme = bs_theme(bootswatch = "sandstone"),
    
    tabPanel("Widget 1: Intercropping Yield by Continent",
             sidebarLayout(
               sidebarPanel(
                 'Areas Researched',
                 radioButtons(
                   inputId = 'Continent_type',
                   label = 'Select Continent',
                   choices = c("South Asia","Middle East & North Africa", "Sub-Saharan Africa",
                               "Latin America & Caribbean","East Asia & Pacific","Europe & Central Asia",
                               "North America","NA")
                 )
               ),               
               mainPanel(
                 'Intercropping experimental yield over time',
                 plotOutput(outputId = 'intercrop_plot'),
                 h3('Summary Table'),
                 tableOutput(outputId = 'intercrop_table')
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
  intercrop_select<-reactive({
    intercrop_df<-intercrop |>
      filter(Continent==input$Continent_type)
  })
  
  output$intercrop_plot<-renderPlot({
    ggplot(data=intercrop_select(),
           aes(x=start_year,y=Yield_total_intercropping,fill=Continent))+
      geom_line(outlier.shape = NA)+
      geom_point(outlier.shape = NA)+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none")
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
