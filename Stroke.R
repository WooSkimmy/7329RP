# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

# Load data --------------------------------------------------------------------

stroke <- read.csv(file = "https://raw.githubusercontent.com/WooSkimmy/7329RP/main/edited-healthcare-dataset-stroke-data.csv", header = TRUE, sep = ",")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("superhero"),
  br(),
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("age", "work_type", "avg_glucose_level", "hypertension", "heart_disease", "stroke", "smoking_status", "bmi", "Residence_type", "ever_married", "gender"),
        selected = "age"
      ),
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c("age", "work_type", "avg_glucose_level", "hypertension", "heart_disease", "stroke", "smoking_status", "bmi", "Residence_type", "ever_married", "gender"),
        selected = "avg_glucose_level"
      ),
      # Select variable for color
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c("age", "work_type", "avg_glucose_level", "hypertension", "heart_disease", "stroke", "smoking_status", "bmi", "Residence_type", "ever_married", "gender"),
        selected = "hypertension"
    )
  ),
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = "plot_brush"),
      textOutput(outputId = "correlation"),
      DT::dataTableOutput(outputId = "stroketable"),
      br()
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    ggplot(data = stroke, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point()
  })
  
  # Create text output stating the correlation between the two ploted
  output$correlation <- renderText({
    if (is.numeric(stroke[, input$x]) & is.numeric(stroke[, input$y])) {
      r <- round(cor(stroke[, input$x], stroke[, input$y], use = "pairwise"), 3)
      paste0(
        "Correlation = ", r,
        ". Note: If the relationship between the two variables is not linear, the correlation coefficient will not be meaningful."
      )
    } else {
      return("Selected variables must be numeric to calculate correlation.")
    }
  })
  
  output$stroketable <- renderDataTable({
    brushedPoints(stroke, brush = input$plot_brush) %>%
      select(bmi, avg_glucose_level, age, work_type, hypertension, heart_disease, stroke, smoking_status, bmi, Residence_type, ever_married, gender)
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)

