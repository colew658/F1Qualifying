library(shiny)
library(bslib)
library(randomForest)
library(ggplot2)
library(DT)

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
                              choices = paste0("x", 1:7)),
                  plotOutput("ale_plot")
                )
              )
            )
  ),
  
  nav_panel("Variable Summary",
            card(
              card_header("Statistical Summary"),
              DTOutput("var_summary")
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
    importance_df <- data.frame(
      Variable = rownames(importance(rf_model)),
      Importance = importance(rf_model)[,1]
    )
    ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Variables", y = "Importance")
  })
  
  # ALE plot
  output$ale_plot <- renderPlot({
    effect <- FeatureEffect$new(predictor, feature = input$ale_feature, method = "ale")
    plot(effect) +
      theme_minimal() +
      labs(title = paste("ALE Plot for", input$ale_feature),
           y = "Effect on Prediction")
  })
  
  # Variable summary table
  output$var_summary <- renderDT({
    summary_stats <- sapply(data[,1:7], function(x) {
      c(Mean = mean(x),
        SD = sd(x),
        Min = min(x),
        Max = max(x))
    })
    datatable(round(summary_stats, 3))
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
