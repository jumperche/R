library(randomForest)
library(tidyr)
library(dplyr)
library(shiny)

# Load data
wine_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")

# Define input and output variables
input_vars <- names(wine_data)[1:11]
target_var <- "quality"

# Convert target variable to factor
wine_data[[target_var]] <- as.factor(wine_data[[target_var]])

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(wine_data), 0.7 * nrow(wine_data))
train_data <- wine_data[train_indices, ]
test_data <- wine_data[-train_indices, ]

# Fit Random Forest model
rf_model <- randomForest(as.formula(paste(target_var, "~ .")), data = train_data)

# Server component
server <- function(input, output) {
  
  # Make prediction
  prediction_text <- eventReactive(input$submit, {
    input_data <- data.frame(
      fixed.acidity = as.numeric(input$fixed.acidity),
      volatile.acidity = as.numeric(input$volatile.acidity),
      citric.acid = as.numeric(input$citric.acid),
      residual.sugar = as.numeric(input$residual.sugar),
      chlorides = as.numeric(input$chlorides),
      free.sulfur.dioxide = as.numeric(input$free.sulfur.dioxide),
      total.sulfur.dioxide = as.numeric(input$total.sulfur.dioxide),
      density = as.numeric(input$density),
      pH = as.numeric(input$pH),
      sulphates = as.numeric(input$sulphates),
      alcohol = as.numeric(input$alcohol)
    )
    
    predicted_quality <- predict(rf_model, input_data)
    
    # Calculate the accuracy of the model on the test set
    test_pred <- predict(rf_model, test_data[, input_vars])
    accuracy <- mean(test_pred == test_data[[target_var]])
    
    # Return text with the prediction and the accuracy of the model
    paste("Predicted quality:", predicted_quality, "Model accuracy:", round(accuracy,2))
  })
  
  output$prediction_text <- renderText({
    prediction_text()
  })
}

# UI component
ui <- fluidPage(
  titlePanel("Wine Quality Analysis"),
  sidebarLayout(
    sidebarPanel(
      # Add input elements here
      numericInput("fixed.acidity", "Fixed Acidity", value = 7.4),
      numericInput("volatile.acidity", "Volatile Acidity", value = 0.7),
      numericInput("citric.acid", "Citric Acid", value = 0),
      numericInput("residual.sugar", "Residual Sugar", value = 1.9),
      numericInput("chlorides", "Chlorides", value = 0.076),
      numericInput("free.sulfur.dioxide", "Free Sulfur Dioxide", value = 11),
      numericInput("total.sulfur.dioxide", "Total Sulfur Dioxide", value = 34),
      numericInput("density", "Density", value = 0.9978),
      numericInput("pH", "pH", value = 3.51),
      numericInput("sulphates", "Sulphates", value = 0.56),
      numericInput("alcohol", "Alcohol", value = 9.4),
      
      # Add submit button
      actionButton("submit", "Submit")
    ),
    mainPanel(
      # Add output element to display the result
      textOutput("prediction_text")
    )
  )
)

# Run the app
shinyApp(ui = ui, server = server)
