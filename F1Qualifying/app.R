library(shiny)
library(bslib)
library(randomForest)
library(ggplot2)
library(DT)

# Generate example data and train model
set.seed(123)
n <- 1000
data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  x5 = rnorm(n),
  x6 = rnorm(n),
  x7 = rnorm(n)
)
data$y = with(data, 2*x1 - 1.5*x2 + 0.5*x3 + 0.8*x4 - 0.3*x5 + 1.2*x6 - 0.7*x7) + rnorm(n, 0, 0.1)
rf_model <- randomForest(y ~ ., data = data)

ui <- page_navbar(
  title = "Model Predictions Dashboard",
  theme = bs_theme(version = 5),
  
  nav_panel("Predictions",
            layout_sidebar(
              sidebar = sidebar(
                title = "Input Parameters",
                numericInput("x1", "X1:", value = 0),
                numericInput("x2", "X2:", value = 0),
                numericInput("x3", "X3:", value = 0),
                numericInput("x4", "X4:", value = 0),
                numericInput("x5", "X5:", value = 0),
                numericInput("x6", "X6:", value = 0),
                numericInput("x7", "X7:", value = 0)
              ),
              layout_column_wrap(
                width = 1/2,
                card(
                  card_header("Model Prediction"),
                  textOutput("prediction_value")
                ),
                card(
                  card_header("Feature Importance"),
                  plotOutput("importance_plot")
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
      x1 = input$x1,
      x2 = input$x2,
      x3 = input$x3,
      x4 = input$x4,
      x5 = input$x5,
      x6 = input$x6,
      x7 = input$x7
    )
    predict(rf_model, new_data)
  })
  
  # Prediction output
  output$prediction_value <- renderText({
    paste("Predicted Value:", round(prediction(), 3))
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
