library(tidyverse)
library(palmerpenguins)
library(shiny)
library(bslib)

### create the user interface

ui<-fluidPage(
  theme = bs_theme(bootswatch = "sandstone"),
  titlePanel("I am adding a title"),
  sidebarLayout(
    sidebarPanel('put my widgets here',
                 radioButtons(
                   inputId = 'penguin_species',
                   label='Choose penguin species',
                   choices=c('Adelie','Gentoo','Chinstrap')
                 ),
                 selectInput(inputId = 'pt_color',
                             label='select point color',
                             choices = c('Roses are red'='red',
                                         'Violets are purple'='purple',
                                         'oranges are ...'='orange'))),
    mainPanel('put my graph here',
              plotOutput(outputId = 'penguin_plot'),
              h3('summary table'),
              tableOutput(outputId = 'penguin_table'))
  )
)

### create the server function (where all the magic happens from data analysis)

server<-function(input,output){
  penguin_select<-reactive({
    penguins_df<-penguins |>
      filter(species==input$penguin_species)
  })
  
  output$penguin_plot<-renderPlot({
    ggplot(data=penguin_select())+
      geom_point(aes(x=flipper_length_mm,y=body_mass_g),
                 color=input$pt_color)
  })
  
  penguin_sum_table<-reactive({
    penguin_summary_df<-penguins |>
      filter(species==input$penguin_species)|>
      group_by(sex)|>
      summarize(mean_flip=mean(flipper_length_mm,na.rm=TRUE),
                mean_mass=mean(body_mass_g,na.rm=TRUE))
  })
  
  output$penguin_table<-renderTable({
    penguin_sum_table()
  })
    
}

### To finalize shiny app we have to combine them into an app

shinyApp(ui=ui,server=server)
