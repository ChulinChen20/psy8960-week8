# load all necessary packages
library(shiny)
library(rsconnect)
library(ggplot2)
library(tidyverse)
library(conflicted)

# solving package conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# load data
week8_tbl <- readRDS("shiny_skinny.RDS")

# create three choice input variables and one plot output. 
# selectInput() are used for all three inputs as the choices are discrete.
# label choices when variable names are not suitable to be used as displayed options.
# place input options in the sidebar and the plot in the main panel for clearer display
ui <- fluidPage(
  titlePanel("Correlation Between Variable Means"),
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

# display a scatterplot with a purple OLS regression line based on user-selected subgroups.
# define working tbl to iteratively modify for later display in ggplot
# the selected subgroups are filtered based on input values selected by users.
# if else statements are used for sex and date 
# as some choices involving returning the whole dataset cannot be treated as variable names.
# convert character to boolean type for errorband options.
server <- function(input, output, session) {
  output$correlation <- renderPlot({
    week8_disp_tbl <- week8_tbl
    week8_disp_tbl %>%
      {if(input$sex!="All") filter(.,gender==input$sex) else .} %>%
      {if(input$date!="Include") filter(.,after_aug2017 == T) else .} %>%
      ggplot(aes(mean1,mean2)) +
      geom_point() +
      geom_smooth(method="lm", color = "purple", se = as.logical(input$errorband)) +
      labs(x="mean scores on Q1-Q6",y="mean scores on Q8-Q10")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
