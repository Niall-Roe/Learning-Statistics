library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Interactive Duality: Sliding the Sample Mean"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        tags$h4("1. Global Parameters"),
        sliderInput("mu_true", "True Pop. Mean (μ):", 100, min = 80, max = 120),
        sliderInput("mu_null", "Null Mean (μ₀):", 100, min = 80, max = 120),
        sliderInput("sigma", "Pop. SD (σ):", 15, min = 5, max = 30),
        sliderInput("n", "Sample Size (n):", 30, min = 5, max = 100),
        sliderInput("alpha", "Alpha (α):", 0.05, min = 0.01, max = 0.20, step = 0.01)
      ),
      wellPanel(
        tags$h4("2. Control Mode"),
        checkboxInput("manual_mode", "Enable Manual Mean (x̄)", FALSE),
        conditionalPanel(
          condition = "input.manual_mode == true",
          sliderInput("x_manual", "Move Sample Mean (x̄):", 100, min = 80, max = 120, step = 0.1)
        ),
        conditionalPanel(
          condition = "input.manual_mode == false",
          actionButton("draw1", "Draw 1 Random Sample", class = "btn-primary btn-block"),
          actionButton("draw100", "Draw 100 Samples", class = "btn-success btn-block")
        ),
        br(),
        actionButton("reset", "Reset History", class = "btn-danger btn-block")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Live Duality", 
                 br(),
                 uiOutput("verdict_box"),
                 plotOutput("unified_plot", height = "500px"),
                 wellPanel(
                   tags$b("The Duality Discovery:"), 
                   "Drag the manual mean. Notice that as the Blue Dot enters the Red Shaded Region (Rejection Region), 
                   the Green CI bar simultaneously stops touching the black dashed Null line. 
                   This is why a CI and a p-value test always give the same answer.")
        ),
        tabPanel("Long-Run History",
                 br(),
                 fluidRow(
                   column(8, plotOutput("history_plot", height = "600px")),
                   column(4, 
                          plotOutput("capture_rate_plot", height = "300px"),
                          uiOutput("stats_summary"))
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  history_data <- reactiveVal(data.frame())
  
  # Logic to calculate stats based on either random draw or manual slider
  current_sample <- reactive({
    mu_t <- input$mu_true
    mu_n <- input$mu_null
    sig <- input$sigma
    n <- input$n
    alp <- input$alpha
    se <- sig / sqrt(n)
    z_crit <- qnorm(1 - alp/2)
    
    # Determine x_bar based on mode
    x_bar <- if(input$manual_mode) input$x_manual else {
      if(nrow(history_data()) == 0) mu_t else tail(history_data()$x_bar, 1)
    }
    
    lower <- x_bar - z_crit * se
    upper <- x_bar + z_crit * se
    rejected_null <- (mu_n < lower | mu_n > upper)
    captured_true <- (mu_t >= lower & mu_t <= upper)
    
    list(x_bar = x_bar, lower = lower, upper = upper, 
         rejected_null = rejected_null, captured_true = captured_true, se = se)
  })
  
  # Helper to add rows to history
  generate_samples <- function(count) {
    mu_t <- input$mu_true
    mu_n <- input$mu_null
    sig <- input$sigma
    n <- input$n
    alp <- input$alpha
    se <- sig / sqrt(n)
    z_crit <- qnorm(1 - alp/2)
    
    new_data <- replicate(count, {
      samp <- rnorm(n, mean = mu_t, sd = sig)
      xb <- mean(samp)
      lo <- xb - z_crit * se
      up <- xb + z_crit * se
      c(x_bar = xb, lower = lo, upper = up, 
        captured_true = (mu_t >= lo & mu_t <= up), 
        rejected_null = (mu_n < lo | mu_n > up))
    }, simplify = FALSE)
    
    df <- do.call(rbind, new_data) %>% as.data.frame()
    df$id <- if(nrow(history_data()) == 0) 1:count else (max(history_data()$id) + 1):(max(history_data()$id) + count)
    history_data(rbind(history_data(), df))
  }
  
  observeEvent(input$draw1, { generate_samples(1) })
  observeEvent(input$draw100, { generate_samples(100) })
  observeEvent(input$reset, { history_data(data.frame()) })
  
  # --- VIZ: Unified Duality ---
  output$unified_plot <- renderPlot({
    s <- current_sample()
    mu_0 <- input$mu_null
    z_crit <- qnorm(1 - input$alpha/2)
    
    x_range <- seq(min(mu_0, s$lower) - 10, max(mu_0, s$upper) + 10, length.out = 300)
    dist_df <- data.frame(x = x_range, y = dnorm(x_range, mu_0, s$se))
    
    ggplot(dist_df, aes(x, y)) +
      geom_area(data = filter(dist_df, x <= mu_0 - z_crit*s$se), fill = "#e74c3c", alpha = 0.4) +
      geom_area(data = filter(dist_df, x >= mu_0 + z_crit*s$se), fill = "#e74c3c", alpha = 0.4) +
      geom_line(size = 1) +
      geom_vline(xintercept = mu_0, linetype = "dashed") +
      # CI Bar
      annotate("segment", x = s$lower, xend = s$upper, y = -0.005, yend = -0.005, 
               color = ifelse(s$rejected_null, "#e74c3c", "#27ae60"), size = 3) +
      geom_point(aes(x = s$x_bar, y = -0.005), color = "blue", size = 4) +
      coord_cartesian(ylim = c(-0.015, max(dist_df$y)*1.1)) +
      labs(title = paste0("Current Mode: ", ifelse(input$manual_mode, "Manual Exploration", "Random Sampling")),
           subtitle = "Watch the bar leave the Null line as the dot enters the Red Zone",
           x = "Value Scale", y = "Density") +
      theme_minimal()
  })
  
  # --- VIZ: Long Run History ---
  output$history_plot <- renderPlot({
    req(nrow(history_data()) > 0)
    df_plot <- tail(history_data(), 50)
    ggplot(df_plot, aes(y = factor(id), x = x_bar)) +
      geom_vline(xintercept = input$mu_true, color = "orange", size = 1.5) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, color = as.factor(captured_true))) +
      geom_point(size = 1) +
      scale_color_manual(values = c("0" = "#e74c3c", "1" = "#27ae60"), name = "Captured True μ?") +
      theme_minimal() + labs(title = "History of Last 50 Random Samples")
  })
  
  output$capture_rate_plot <- renderPlot({
    req(nrow(history_data()) > 0)
    df <- history_data() %>% mutate(cum_cap = cumsum(captured_true) / row_number())
    ggplot(df, aes(x = 1:nrow(df), y = cum_cap)) +
      geom_line(color = "#2980b9") + geom_hline(yintercept = 1 - input$alpha, linetype = "dashed", color = "red") +
      theme_minimal() + labs(title = "Long-Run Coverage Rate") + scale_y_continuous(limits=c(0,1))
  })
  
  output$verdict_box <- renderUI({
    s <- current_sample()
    color <- if(s$rejected_null) "#e74c3c" else "#27ae60"
    text <- if(s$rejected_null) "NULL REJECTED: Null Value is outside the CI" else "FAIL TO REJECT: Null Value is inside the CI"
    div(style = paste0("background-color:", color, "; color:white; padding:10px; text-align:center; border-radius:5px; margin-bottom:10px; font-weight:bold;"), text)
  })
}

shinyApp(ui, server)