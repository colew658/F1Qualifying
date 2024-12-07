library(shiny)
library(bslib)
library(reactable)
library(ggplot2)

final_fit <- readRDS("Data/final_fit.rds")

ui <- page_navbar(
  title = "F1 Qualifying Analysis Dashboard",
  theme = bs_theme(version = 5),
  
  nav_panel("Predictions",
            layout_sidebar(
              sidebar = sidebar(
                title = "Input Parameters",
                numericInput("airtemp", "Air Temperature (°C):", value = 0),
                numericInput("humidity", "Relative Humidity (%):", value = 0),
                numericInput("pressure", "Pressure (mbar):", value = 0),
                selectInput("rainfall", "Is is raining?:", choices = c("No", "Yes")),
                numericInput("tracktemp", "Track Temperature (°C):", value = 0),
                numericInput("windspeed", "Wind Speed (m/s):", value = 0),
                numericInput("rainlaps", "# of Raining Laps:", value = 0)
              ),
              card(
                card_header("Model Prediction"),
                textOutput("prediction_value")
              ),
              layout_column_wrap(
                width = 1/2,
                card(
                  card_header("Feature Importance"),
                  plotOutput("importance_plot")
                ),
                card(
                  card_header("ALE Plot"),
                  selectInput("ale_feature", "Select Feature:", 
                              choices = c("Air Temperature", "Humidity",
                                          "Pressure", "Rainfall",
                                          "Track Temperature", "Wind Speed",
                                          "# of Raining Laps")),
                  plotOutput("ale_plot")
                )
              )
            )
  ),
  
  nav_panel("Driver Summary",
            card(
              card_header("Driver Summary Table"),
              reactableOutput("driver_summary")
            )
  ),
  
  nav_panel("Model Performance",
            card(
              card_header("Model Metrics"),
              DTOutput("model_metrics")
            )
  ),
  
  nav_panel("Correlation Matrix",
            card(
              card_header("Variable Correlations"),
              DTOutput("correlation_matrix")
            )
  )
)

server <- function(input, output) {
  # Reactive prediction
  prediction <- reactive({
    new_data <- data.frame(
      AIR_TEMPERATURE = input$airtemp,
      HUMIDITY = input$humidity,
      PRESSURE = input$pressure,
      RAINFALL = factor(ifelse(input$rainfall == "Yes", 1, 0)),
      TRACK_TEMPERATURE = input$tracktemp,
      WIND_SPEED = input$windspeed,
      rain_laps = input$rainlaps
    )
    predict(final_fit, new_data)
  })
  
  # Prediction output
  output$prediction_value <- renderText({
    paste("Predicted Lap Time (s):", round(prediction(), 3))
  })
  
  # Variable importance plot
  output$importance_plot <- renderPlot({
    plot(readRDS("Data/imp_weatheronly.rds")) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12))
  })
  
  # ALE plot
  output$ale_plot <- renderPlot({
    if (input$ale_feature == "Air Temperature") {
      aleplot <- readRDS("Data/airtemp_ale.rds")
    } else if (input$ale_feature == "Humidity") {
      aleplot <- readRDS("Data/humidity_ale.rds")
    } else if (input$ale_feature == "Pressure") {
      aleplot <- readRDS("Data/pressure_ale.rds")
    } else if (input$ale_feature == "Rainfall") {
      aleplot <- readRDS("Data/rainfall_ale.rds")
    } else if (input$ale_feature == "Track Temperature") {
      aleplot <- readRDS("Data/tracktemp_ale.rds")
    } else if (input$ale_feature == "Wind Speed") {
      aleplot <- readRDS("Data/windspeed_ale.rds")
    } else if (input$ale_feature == "# of Raining Laps") {
      aleplot <- readRDS("Data/rainlaps_ale.rds")
    }
    
    plot(aleplot) +
      labs(x = input$ale_feature, y = "ALE of Lap Duration") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12))
  })
  
  # Driver summary table
  output$driver_summary <- renderReactable({
    readRDS("Data/driver_reactable.rds")
  })
  
  # Model performance metrics
  output$model_metrics <- renderDT({
    metrics <- data.frame(
      Metric = c("R-squared", "MSE", "MAE"),
      Value = c(
        round(rf_model$rsq[length(rf_model$rsq)], 3),
        round(mean(rf_model$mse), 3),
        round(mean(abs(rf_model$y - predict(rf_model))), 3)
      )
    )
    datatable(metrics)
  })
  
  # Correlation matrix
  output$correlation_matrix <- renderDT({
    cor_matrix <- cor(data[,1:7])
    datatable(round(cor_matrix, 3))
  })
}

shinyApp(ui, server)
