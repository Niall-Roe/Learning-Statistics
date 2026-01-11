library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Exploring Confidence Intervals: The Long Run Interpretation"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Simulation Parameters"),
      
      numericInput("pop_mean", "True Population Mean (μ):", value = 0),
      numericInput("pop_sd", "True Population SD (σ):", value = 15),
      
      sliderInput("n", "Sample Size (n):", 
                  min = 5, max = 500, value = 30, step = 1),
      
      sliderInput("conf_level", "Confidence Level (%):", 
                  min = 80, max = 99.9, value = 95, step = 0.1),
      
      sliderInput("n_sims", "Number of Simulations:", 
                  min = 10, max = 1000, value = 100, step = 10),
      
      hr(),
      checkboxInput("fixed_axis", "Fix X-Axis Scale", value = TRUE),
      helpText("Keep scale fixed to see how Sample Size affects CI width."),
      
      hr(),
      actionButton("run", "Generate New Samples", class = "btn-primary btn-lg")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        # TAB 1: The Classic Forest Plot
        tabPanel("Visualization of Intervals", 
                 br(),
                 textOutput("coverage_text"),
                 plotOutput("ci_plot", height = "600px"),
                 wellPanel(
                   h4("Understanding the colors:"),
                   p("The vertical black line is the TRUE Population Mean (μ)."),
                   p(tags$b("Green Lines:"), "Intervals that successfully captured μ."),
                   p(tags$b("Red Lines:"), "Intervals that missed μ."),
                   p("Try increasing the Sample Size (n) with 'Fix Axis Scale' checked to see the intervals shrink!")
                 )
        ),
        
        # TAB 2: Single Sample Detail
        tabPanel("Single Sample View",
                 br(),
                 h4("Anatomy of One Confidence Interval"),
                 p("This plot shows the raw data from just ONE of your samples. The gray bars are the data points. The Blue Box is the calculated Confidence Interval."),
                 plotOutput("single_sample_plot", height = "500px"),
                 verbatimTextOutput("single_sample_stats"),
                 div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px;",
                     p("Notice how the Confidence Interval is centered on the SAMPLE mean (dashed blue line), not the TRUE mean (solid black line)."),
                     p("If the solid black line falls inside the blue shaded area, this specific sample was a 'success'."))
        ),
        
        # TAB 3: Method vs Result (Distribution of CIs)
        tabPanel("Theory: Method vs. Result",
                 br(),
                 h3("The Distribution of Possible Intervals"),
                 p("You asked about the 'distribution of possible CIs'. This plot shows exactly that. Across all your simulations, where did the Lower Bounds (Red) and Upper Bounds (Blue) land?"),
                 plotOutput("ci_dist_plot", height = "400px"),
                 
                 hr(),
                 h4("The Philosophical Trap"),
                 fluidRow(
                   column(6, 
                          h5("Before the Experiment (Random)"),
                          p("Before we collect data, the CI boundaries are random variables. We know that 95% of the time, the random interval will trap μ."),
                          p("This is the 'Distribution' view above. The method is reliable.")
                   ),
                   column(6,
                          h5("After the Experiment (Fixed)"),
                          p("Once you calculate the interval (e.g., [4.2, 8.9]), there is no more randomness. μ is either in there or it isn't."),
                          p("We cannot say 'There is a 95% probability μ is in [4.2, 8.9]'. Instead, we say: 'This interval came from a method that works 95% of the time'.")
                   )
                 )
        ),
        
        # TAB 4: Width Analysis
        tabPanel("Interval Widths", 
                 br(),
                 h4("Why are the widths different?"),
                 p("Even though n is constant, the sample standard deviation (s) varies from sample to sample."),
                 plotOutput("width_hist"),
                 verbatimTextOutput("width_summary")
        ),
        
        # TAB 5: Convergence
        tabPanel("Convergence", 
                 br(),
                 plotOutput("convergence_plot")
        ),
        
        # TAB 6: Raw Data
        tabPanel("Raw Data", 
                 DTOutput("data_table")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive block to generate the main simulation data
  sim_data <- eventReactive(input$run, {
    
    means <- numeric(input$n_sims)
    lower <- numeric(input$n_sims)
    upper <- numeric(input$n_sims)
    widths <- numeric(input$n_sims)
    captured <- logical(input$n_sims)
    
    alpha <- 1 - (input$conf_level / 100)
    t_crit <- qt(1 - alpha/2, df = input$n - 1)
    
    for(i in 1:input$n_sims) {
      samp <- rnorm(input$n, mean = input$pop_mean, sd = input$pop_sd)
      
      means[i] <- mean(samp)
      s <- sd(samp)
      se_val <- s / sqrt(input$n)
      margin_error <- t_crit * se_val
      
      lower[i] <- means[i] - margin_error
      upper[i] <- means[i] + margin_error
      widths[i] <- upper[i] - lower[i]
      captured[i] <- (input$pop_mean >= lower[i]) & (input$pop_mean <= upper[i])
    }
    
    data.frame(
      ID = 1:input$n_sims,
      Mean = means,
      Lower = lower,
      Upper = upper,
      Width = widths,
      Captured = captured
    )
  }, ignoreNULL = FALSE)
  
  # Reactive block for a SINGLE detailed sample (for Tab 2)
  single_sample_data <- eventReactive(input$run, {
    samp <- rnorm(input$n, mean = input$pop_mean, sd = input$pop_sd)
    mean_val <- mean(samp)
    sd_val <- sd(samp)
    
    alpha <- 1 - (input$conf_level / 100)
    t_crit <- qt(1 - alpha/2, df = input$n - 1)
    me <- t_crit * (sd_val / sqrt(input$n))
    
    list(
      data = samp,
      mean = mean_val,
      lower = mean_val - me,
      upper = mean_val + me,
      captured = (input$pop_mean >= (mean_val - me)) & (input$pop_mean <= (mean_val + me))
    )
  })
  
  # --- Output: Dynamic Text ---
  output$coverage_text <- renderText({
    df <- sim_data()
    hits <- sum(df$Captured)
    total <- nrow(df)
    perc <- round((hits/total)*100, 1)
    paste0("Simulation Result: ", hits, " out of ", total, 
           " intervals captured the true mean (", perc, "% coverage).")
  })
  
  # --- Output: Main CI Plot ---
  output$ci_plot <- renderPlot({
    df <- sim_data()
    df$Status <- ifelse(df$Captured, "Hit (Contains μ)", "Miss (Does not contain μ)")
    
    p <- ggplot(df, aes(x = ID, y = Mean, color = Status)) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.5, size = 0.8) +
      geom_point(size = 1.5) +
      geom_hline(yintercept = input$pop_mean, linetype = "dashed", size = 1, color = "black") +
      coord_flip() +
      labs(title = paste("Simulation of", input$n_sims, "Confidence Intervals"),
           subtitle = paste("True Mean =", input$pop_mean),
           y = "Value", x = "Simulation ID") +
      scale_color_manual(values = c("Hit (Contains μ)" = "#2ECC71", 
                                    "Miss (Does not contain μ)" = "#E74C3C")) +
      theme_minimal() +
      theme(legend.position = "top", text = element_text(size = 14))
    
    # Logic to fix axis scale
    if(input$fixed_axis) {
      # Show roughly 3 standard deviations of the POPULATION to keep scale consistent
      # regardless of sample size n.
      limit_range <- 3 * input$pop_sd
      p <- p + ylim(input$pop_mean - limit_range, input$pop_mean + limit_range)
    }
    
    p
  })
  
  # --- Output: Single Sample Plot ---
  output$single_sample_plot <- renderPlot({
    s <- single_sample_data()
    df_samp <- data.frame(val = s$data)
    
    # Calculate density for the true population curve
    grid <- seq(min(df_samp$val) - 5, max(df_samp$val) + 5, length.out = 200)
    pop_density <- data.frame(val = grid, 
                              dens = dnorm(grid, mean = input$pop_mean, sd = input$pop_sd))
    
    # Scaling factor to match histogram height roughly
    scale_factor <- nrow(df_samp) * (max(df_samp$val) - min(df_samp$val)) / 10 
    
    ggplot() +
      # Histogram of data
      geom_histogram(data = df_samp, aes(x = val), fill = "gray80", color = "white", bins = 15) +
      # The CI Rectangle
      annotate("rect", xmin = s$lower, xmax = s$upper, ymin = 0, ymax = Inf, 
               fill = "blue", alpha = 0.1) +
      # True Population Curve
      stat_function(fun = function(x) dnorm(x, mean = input$pop_mean, sd = input$pop_sd) * scale_factor * 5, 
                    geom = "line", color = "black", alpha = 0.4, size = 1) +
      # Lines
      geom_vline(xintercept = input$pop_mean, color = "black", size = 1.5) +
      geom_vline(xintercept = s$mean, color = "blue", linetype = "dashed", size = 1) +
      geom_vline(xintercept = c(s$lower, s$upper), color = "blue", size = 1) +
      labs(title = "One Sample, One Interval",
           subtitle = "Histogram = Data | Blue Zone = 95% CI | Black Line = True Mean",
           x = "Value", y = "Count") +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })
  
  output$single_sample_stats <- renderText({
    s <- single_sample_data()
    paste0("Sample Mean: ", round(s$mean, 3), 
           "\n95% CI: [", round(s$lower, 3), ", ", round(s$upper, 3), "]",
           "\nContains Truth?: ", s$captured)
  })
  
  # --- Output: Method vs Result (Distribution of CIs) ---
  output$ci_dist_plot <- renderPlot({
    df <- sim_data()
    
    # We want to plot the distribution of Lower bounds and Upper bounds
    # relative to the true mean
    
    ggplot() +
      geom_density(data = df, aes(x = Lower, fill = "Lower Bound"), alpha = 0.5) +
      geom_density(data = df, aes(x = Upper, fill = "Upper Bound"), alpha = 0.5) +
      geom_vline(xintercept = input$pop_mean, linetype="dashed", size=1) +
      scale_fill_manual(name = "Bound Type", 
                        values = c("Lower Bound" = "red", "Upper Bound" = "blue")) +
      labs(title = "Distribution of CI Bounds across Simulations",
           subtitle = "Ideally, the True Mean (dashed line) sits comfortably between these two distributions.",
           x = "Value", y = "Density") +
      theme_minimal() +
      theme(legend.position = "top", text = element_text(size = 14))
  })
  
  # --- Output: Width Histogram ---
  output$width_hist <- renderPlot({
    df <- sim_data()
    
    ggplot(df, aes(x = Width)) +
      geom_histogram(bins = 15, fill = "skyblue", color = "white", alpha = 0.8) +
      geom_vline(aes(xintercept = mean(Width)), color = "darkblue", linetype="dashed", size=1) +
      labs(title = "Distribution of CI Widths",
           subtitle = paste("Average Width:", round(mean(df$Width), 2)),
           x = "Interval Width (Upper - Lower)",
           y = "Frequency") +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })
  
  output$width_summary <- renderPrint({
    summary(sim_data()$Width)
  })
  
  # --- Output: Convergence Plot ---
  output$convergence_plot <- renderPlot({
    df <- sim_data()
    df$Running_Sum <- cumsum(df$Captured)
    df$Running_Prop <- (df$Running_Sum / df$ID) * 100
    
    ggplot(df, aes(x = ID, y = Running_Prop)) +
      geom_line(color = "purple", size = 1) +
      geom_hline(yintercept = input$conf_level, color = "black", linetype = "dashed") +
      ylim(min(70, input$conf_level - 10), 100) +
      labs(title = "Running Coverage Percentage",
           x = "Number of Simulations",
           y = "Cumulative % Capturing Mean") +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })
  
  # --- Output: Raw Data Table ---
  output$data_table <- renderDT({
    datatable(sim_data(), options = list(pageLength = 10)) %>%
      formatRound(columns = c("Mean", "Lower", "Upper", "Width"), digits = 3)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)