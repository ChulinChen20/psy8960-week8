#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#deployApp(.)

# load data
week8_tbl <- readRDS("week8_tbl")

# 
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
      selected = 'All', 
      label = c("Display Error Band","Suppress Error Band"),
      choices = c('TRUE', 'FALSE')
    )),
    mainPanel(plotOutput('correlation'))
  )
)

# display a scatterplot with a purple line based on user-selected gender group
server <- function(input, output, session) {
  output$correlation <- renderPlot({
    week8_tbl %>%
      {if(input$sex!="All") filter(.,gender==input$sex) else .} %>%
      rowwise() %>%
      mutate(mean1 = mean(q1:q6),
             mean2 = mean(q8:q10)) %>%
      ggplot(aes(mean1,mean2)) +
      geom_point() +
      geom_smooth(method="lm", color = "purple", se = input$errorband) +
      labs(x="mean scores on Q1-Q6",y="mean scores on Q8-Q10")
  })
}


