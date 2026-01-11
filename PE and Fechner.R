library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Transit Observations and Psychophysics"),

  tabsetPanel(
    tabPanel("Transit Observations",
             sidebarLayout(
               sidebarPanel(
                 h4("Observation Method"),
                 selectInput("transit_method", "Method:",
                             choices = list("Bradley Eye & Ear" = "bradley",
                                            "Hipp Chronoscope" = "hipp",
                                            "Person-Guided Wire" = "wire_person",
                                            "Mechanical Wire Machine" = "wire_mech")),
                 sliderInput("observer1_quality", "Observer 1 Training Quality:",
                             min = 0, max = 1, value = 0.5, step = 0.05),
                 hr(),
                 checkboxInput("show_observer2", "Show Second Observer", FALSE),
                 conditionalPanel(
                   condition = "input.show_observer2 == true",
                   sliderInput("observer2_quality", "Observer 2 Training Quality:",
                               min = 0, max = 1, value = 0.5, step = 0.05),
                   sliderInput("personal_eq", "Personal Equation (Obs2 - Obs1, ms):",
                               min = -500, max = 500, value = 0, step = 10)
                 ),
                 hr(),
                 h4("Method Details"),
                 textOutput("method_description")
               ),

               mainPanel(
                 plotOutput("transit_dist_plot", height = "400px"),
                 hr(),
                 h4("Performance Metrics"),
                 tableOutput("transit_metrics")
               )
             )
    ),

    tabPanel("Fechner's Psychophysics",
             sidebarLayout(
               sidebarPanel(
                 h4("Fechner's Weight Comparison (1860)"),
                 p("Two weights (S_A and S_B) produce sensations W_A and W_B."),
                 p("Each sensation = true heaviness + Gaussian error with SD σ = 7.4g."),
                 p("Threshold positioned midway: (a+b)/2, giving c = d'/2."),
                 sliderInput("weight1", "Weight A (grams):",
                             min = 100, max = 500, value = 300, step = 5),
                 sliderInput("weight2", "Weight B (grams):",
                             min = 100, max = 500, value = 310, step = 5),
                 hr(),
                 h4("Fechner's Key Insight"),
                 p("σ = 7.4g is a CONSTANT property of the sensory system!"),
                 p("As weight difference D increases, d' = D/σ increases (distributions separate)."),
                 p("When D = 10g, error rate = 25% (t = 0.4769)."),
                 p("Larger D → smaller error. Smaller D → larger error."),
                 hr(),
                 h4("Calculated Parameters"),
                 tableOutput("fechner_params")
               ),

               mainPanel(
                 plotOutput("fechner_dist_plot", height = "400px"),
                 hr(),
                 h4("Fechner's Predictions"),
                 tableOutput("fechner_predictions")
               )
             )
    )
  )
)

server <- function(input, output, session) {

  # Transit Observations Tab
  transit_params <- reactive({
    # Base parameters for each method
    base_params <- switch(input$transit_method,
                          "bradley" = list(base_d = 1.2, base_sigma = 1.8, quality_effect = 0.8),
                          "hipp" = list(base_d = 2.0, base_sigma = 1.3, quality_effect = 0.5),
                          "wire_person" = list(base_d = 2.8, base_sigma = 1.1, quality_effect = 0.3),
                          "wire_mech" = list(base_d = 4.0, base_sigma = 1.0, quality_effect = 0.1)
    )

    qe <- base_params$quality_effect
    obs1_d <- base_params$base_d * (1 - qe + qe * input$observer1_quality)
    obs1_sigma <- base_params$base_sigma * (1 + qe - qe * input$observer1_quality)
    obs1_c <- 0

    result <- list(
      obs1_d = obs1_d,
      obs1_c = obs1_c,
      obs1_sigma = obs1_sigma
    )

    if(input$show_observer2) {
      obs2_d <- base_params$base_d * (1 - qe + qe * input$observer2_quality)
      obs2_sigma <- base_params$base_sigma * (1 + qe - qe * input$observer2_quality)
      obs2_c <- (input$personal_eq / 500) * obs2_sigma

      result$obs2_d <- obs2_d
      result$obs2_c <- obs2_c
      result$obs2_sigma <- obs2_sigma
    }

    result
  })

  output$method_description <- renderText({
    switch(input$transit_method,
           "bradley" = "Eye & Ear method: Observer counts clock beats and mentally divides intervals. High variability, improves greatly with training.",
           "hipp" = "Hipp Chronoscope: Mechanical timer triggered by observer. Moderate precision, some training benefit.",
           "wire_person" = "Person-Guided Wire: Observer manually triggers recording mechanism. Good precision, modest training effect.",
           "wire_mech" = "Mechanical Wire: Fully automated mechanical recording. Excellent precision, minimal training needed."
    )
  })

  output$transit_dist_plot <- renderPlot({
    params <- transit_params()
    x <- seq(-6, 6, length.out = 400)

    df1 <- data.frame(
      x = x,
      obs_dist = dnorm(x, params$obs1_c, params$obs1_sigma)
    )

    p <- ggplot() +
      geom_ribbon(data = df1, aes(x = x, ymin = 0, ymax = obs_dist),
                  fill = "steelblue", alpha = 0.6) +
      geom_line(data = df1, aes(x = x, y = obs_dist),
                color = "steelblue", size = 1.2) +
      geom_vline(xintercept = 0, color = "red", size = 1.5, linetype = "solid") +
      annotate("text", x = 0.2, y = max(df1$obs_dist) * 0.95,
               label = "True Transit Time", color = "red", hjust = 0, fontface = "bold") +
      geom_vline(xintercept = params$obs1_c, color = "steelblue",
                 size = 1, linetype = "dashed", alpha = 0.7) +
      annotate("text", x = -5, y = max(df1$obs_dist) * 0.5,
               label = "Observer 1", color = "steelblue", hjust = 0, size = 5) +
      theme_minimal() +
      labs(title = "Transit Timing Observations",
           x = "Time Error from True Transit (standardized units, ~100ms each)",
           y = "Probability Density") +
      xlim(-6, 6)

    if(input$show_observer2) {
      df2 <- data.frame(
        x = x,
        obs_dist = dnorm(x, params$obs2_c, params$obs2_sigma)
      )

      p <- p +
        geom_ribbon(data = df2, aes(x = x, ymin = 0, ymax = obs_dist),
                    fill = "darkgreen", alpha = 0.4) +
        geom_line(data = df2, aes(x = x, y = obs_dist),
                  color = "darkgreen", size = 1.2, linetype = "dotted") +
        geom_vline(xintercept = params$obs2_c, color = "darkgreen",
                   size = 1, linetype = "dashed", alpha = 0.7) +
        annotate("text", x = -5, y = max(df1$obs_dist) * 0.4,
                 label = "Observer 2", color = "darkgreen", hjust = 0, size = 5) +
        annotate("text", x = params$obs2_c + 0.2, y = max(df2$obs_dist) * 0.8,
                 label = paste("Personal Eq:", input$personal_eq, "ms"),
                 color = "darkgreen", hjust = 0, size = 3)
    }

    p
  })

  output$transit_metrics <- renderTable({
    params <- transit_params()

    result <- data.frame(
      Observer = "Observer 1",
      Precision_sigma = round(params$obs1_sigma, 3),
      Systematic_Bias = round(params$obs1_c, 3),
      Mean_Abs_Error = round(params$obs1_sigma * sqrt(2/pi), 3),
      SD_of_errors = round(params$obs1_sigma, 3)
    )

    if(input$show_observer2) {
      result <- rbind(result, data.frame(
        Observer = "Observer 2",
        Precision_sigma = round(params$obs2_sigma, 3),
        Systematic_Bias = round(params$obs2_c, 3),
        Mean_Abs_Error = round(params$obs2_sigma * sqrt(2/pi), 3),
        SD_of_errors = round(params$obs2_sigma, 3)
      ))
    }

    result
  })

  # Fechner's Psychophysics Tab
  fechner_vals <- reactive({
    D <- abs(input$weight2 - input$weight1)

    if(D == 0) {
      return(list(d = 0, c = 0, sigma = 1, D = 0, h = 0, t = 0, error_prob = 0.5))
    }

    sigma <- 7.4
    h <- 1 / (sqrt(2) * sigma)
    d_prime <- D / sigma
    c <- d_prime / 2
    t <- h * (D / 2)

    p_error_light <- 1 - pnorm(c, 0, 1)
    p_error_heavy <- pnorm(c, d_prime, 1)
    error_prob <- p_error_light

    list(d = d_prime, c = c, sigma = sigma, D = D, h = h, t = t, error_prob = error_prob)
  })

  output$fechner_params <- renderTable({
    fv <- fechner_vals()
    data.frame(
      Parameter = c("Weight Difference D (g)", "Precision h (1/g)", "σ (in grams)",
                    "d' (standardized)", "c (threshold)"),
      Value = c(fv$D, round(fv$h, 6), round(fv$sigma, 3), round(fv$d, 4), round(fv$c, 4))
    )
  })

  output$fechner_dist_plot <- renderPlot({
    fv <- fechner_vals()

    if(input$weight1 == input$weight2) {
      x <- seq(-3, 3, length.out = 400)
      df <- data.frame(x = x, weight = dnorm(x, 0, 1))

      ggplot(df, aes(x = x)) +
        geom_line(aes(y = weight), color = "black", size = 1) +
        geom_ribbon(aes(ymin = 0, ymax = weight), fill = "gray", alpha = 0.3) +
        theme_minimal() +
        labs(title = "Equal Weights - No Discrimination",
             x = "Sensation of Heaviness", y = "Probability Density") +
        annotate("text", x = 0, y = max(df$weight)/2,
                 label = "Both weights are equal", fontface = "bold")
    } else {
      x <- seq(-3, max(5, fv$d + 3), length.out = 400)

      df <- data.frame(
        x = x,
        WA = dnorm(x, 0, 1),
        WB = dnorm(x, fv$d, 1)
      )

      threshold <- fv$c

      ggplot(df, aes(x = x)) +
        geom_ribbon(data = subset(df, x > threshold),
                    aes(ymin = 0, ymax = WB), fill = "#2ca02c", alpha = 0.4) +
        geom_ribbon(data = subset(df, x <= threshold),
                    aes(ymin = 0, ymax = WB), fill = "#d62728", alpha = 0.4) +
        geom_ribbon(data = subset(df, x > threshold),
                    aes(ymin = 0, ymax = WA), fill = "#ff7f0e", alpha = 0.4) +
        geom_ribbon(data = subset(df, x <= threshold),
                    aes(ymin = 0, ymax = WA), fill = "#1f77b4", alpha = 0.4) +
        geom_line(aes(y = WA), linetype = "dashed", color = "steelblue", size = 1.2) +
        geom_line(aes(y = WB), linetype = "solid", color = "darkgreen", size = 1.2) +
        geom_vline(xintercept = threshold, color = "red", size = 1.2) +
        geom_hline(yintercept = 0, color = "black", size = 0.3) +
        annotate("text", x = 0, y = max(df$WA) * 1.05,
                 label = paste0("W_A\n(", input$weight1, "g)"),
                 color = "steelblue", fontface = "bold", size = 4) +
        annotate("text", x = fv$d, y = max(df$WB) * 1.05,
                 label = paste0("W_B\n(", input$weight2, "g)"),
                 color = "darkgreen", fontface = "bold", size = 4) +
        annotate("text", x = threshold, y = max(df$WA) * 1.2,
                 label = paste0("Threshold\n(a+b)/2 = ", round(threshold, 3)),
                 color = "red", fontface = "bold", hjust = 0.5, size = 3.5) +
        annotate("segment", x = 0, xend = fv$d, y = -0.05, yend = -0.05,
                 arrow = arrow(ends = "both", length = unit(0.15, "cm")),
                 color = "black", size = 0.8) +
        annotate("text", x = fv$d/2, y = -0.08,
                 label = paste0("b - a = d' = ", round(fv$d, 3)),
                 hjust = 0.5, size = 3.5, fontface = "bold") +
        annotate("segment", x = 0, xend = threshold, y = -0.15, yend = -0.15,
                 arrow = arrow(ends = "both", length = unit(0.12, "cm")),
                 color = "purple", size = 0.7) +
        annotate("text", x = threshold/2, y = -0.18,
                 label = paste0("d'/2 = ", round(fv$d/2, 3), " (scaled: t = ", round(fv$t, 4), ")"),
                 hjust = 0.5, size = 3, color = "purple") +
        theme_minimal() +
        labs(title = "Fechner's Weight Discrimination Model (1860)",
             subtitle = paste0("Error regions show 25% probability each (shaded red/orange). Both distributions have σ = ", round(fv$sigma, 2), " grams"),
             x = "Sensation of Heaviness (standardized units)",
             y = "Probability Density") +
        expand_limits(y = c(-0.2, max(df$WA) * 1.25))
    }
  })

  output$fechner_predictions <- renderTable({
    fv <- fechner_vals()

    if(fv$d == 0) {
      data.frame(
        Prediction = c("P(W_A judged heavier)", "P(W_B judged heavier)",
                       "P(Error lighter weight)", "P(Error heavier weight)"),
        Value = c(0.5, 0.5, 0.5, 0.5),
        Explanation = c("Equal weights - 50/50 guess",
                        "Equal weights - 50/50 guess",
                        "Equal weights - always wrong or right equally",
                        "Equal weights - always wrong or right equally")
      )
    } else {
      p_error_light <- 1 - pnorm(fv$c, 0, 1)
      p_error_heavy <- pnorm(fv$c, fv$d, 1)

      data.frame(
        Prediction = c("P(W_A misjudged heavier)",
                       "P(W_B misjudged lighter)",
                       "Error rate (either judgment)",
                       "P(Correct discrimination)"),
        Value = c(round(p_error_light, 4),
                  round(p_error_heavy, 4),
                  round(p_error_light, 4),
                  round(1 - p_error_light, 4)),
        Explanation = c(
          paste0("Light weight sensation > ", round(fv$c, 3)),
          paste0("Heavy weight sensation < ", round(fv$c, 3)),
          "By symmetry, both errors equal",
          "Correctly judge which is heavier"
        )
      )
    }
  })
}

shinyApp(ui, server)
