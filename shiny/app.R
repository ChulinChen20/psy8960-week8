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
deployApp(.)

ui <- fluidPage(
  titlePanel("Correlation Betwwen Means"),
  sidebarLayout(
    sidebarPanel(selectInput(
      'sex', 
      'Select Gender', 
      selected = 'All', 
      choices = c('Male', 'Female', 'All')
    )),
    mainPanel(plotOutput('correlation'))
  )
)

server <- function(input, output, session) {
  output$correlation <- renderPlot({
    # CODE BELOW: Update to display a line plot of the input name
    week8_tbl %>%
      filter(gender==input$sex)
    rowwise() %>%
      mutate(mean1 = mean(q1:q6),
             mean2 = mean(q8:q10)) %>%
      ggplot(aes(mean1,mean2)) +
      geom_point() +
      geom_smooth(method="lm", color = "purple") +
      labs(x="mean scores on Q1-Q6",y="mean scores on Q8-Q10")
  })
}

ggplot(bfi, aes_string(x="A1",y="A2"))

