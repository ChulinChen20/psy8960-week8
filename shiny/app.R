# load all necessary packages
library(shiny)
library(rsconnect)
library(ggplot2)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# deploy app to shinyapps.io
deployApp()

# load data
week8_tbl <- readRDS("week8_tbl")

# create three choice input variables and one plot output
# place input options in the sidebar and the plot in the main panel for clearer display
ui <- fluidPage(
  titlePanel("Correlation Betwwen Means"),
  sidebarLayout(
    sidebarPanel(selectInput(
      'sex', 
      'Select Gender', 
      selected = 'All', 
      choices = c('Male', 'Female', 'All')
    ),
    selectInput(
      'errorband', 
      'Select whether error band is displayed', 
      selected = "Display Error Band", 
      choices = c("Display Error Band"="TRUE", "Suppress Error Band"="FALSE")
    ),
    selectInput(
      'date', 
      'Select whether to include or exclude participants that completed the assessment before August 1, 2017', 
      selected = "Include", 
      choices = c("Include", "Exclude")
    )),
    mainPanel(plotOutput('correlation'))
  )
)

# display a scatterplot with a purple OLS regression line 
# based on user-selected subgroups
server <- function(input, output, session) {
  output$correlation <- renderPlot({
    week8_tbl %>%
      {if(input$sex!="All") filter(.,gender==input$sex) else .} %>%
      {if(input$date!="Include") filter(.,timeEnd < "2017-08-01 00:00:00") else .} %>%
      rowwise() %>%
      mutate(mean1 = mean(q1:q6),
             mean2 = mean(q8:q10)) %>%
      ggplot(aes(mean1,mean2)) +
      geom_point() +
      geom_smooth(method="lm", color = "purple", se = as.logical(input$errorband)) +
      labs(x="mean scores on Q1-Q6",y="mean scores on Q8-Q10")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)