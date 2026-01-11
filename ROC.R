library(shiny)
library(ggplot2)

# Define exact colors used in ggplot (standard SDT palette)
col_hit <- "44, 160, 44"   # #2ca02c
col_miss <- "214, 39, 40"   # #d62728
col_fa   <- "255, 127, 14"  # #ff7f0e
col_cr   <- "31, 119, 180"  # #1f77b4

ui <- fluidPage(
  tags$style(HTML(paste0("
    .conf-table { border-collapse: collapse; width: 100%; text-align: center; table-layout: fixed; }
    .conf-table td, .conf-table th { border: 2px solid #fff; padding: 20px; font-weight: bold; }
    .label-cell { background-color: #f8f9fa; color: #333; }
  "))),
  
  titlePanel("Signal Detection Theory: Advanced Explorer"),
  
  tabsetPanel(
    tabPanel("SDT Parameters",
             sidebarLayout(
               sidebarPanel(
                 h4("Preset Scenarios"),
                 selectInput("preset", "Load Preset:",
                             choices = list("Custom" = "custom",
                                            "Finley's Tornado Data (1884)" = "finley")),
                 hr(),
                 radioButtons("constraint_mode", "Constraint Mode:",
                              choices = list("None" = "none",
                                             "Fix Likelihood Ratio (Beta)" = "beta",
                                             "Fix Hit Rate" = "hitrate")),
                 conditionalPanel(
                   condition = "input.constraint_mode == 'beta'",
                   numericInput("target_beta", "Target Beta (Likelihood Ratio):", value = 1, min = 0.01, step = 0.1)
                 ),
                 conditionalPanel(
                   condition = "input.constraint_mode == 'hitrate'",
                   sliderInput("target_hitrate", "Target Hit Rate:", min = 0.01, max = 0.99, value = 0.75, step = 0.01)
                 ),
                 hr(),
                 sliderInput("d_prime", "Sensitivity (d'):", min = -2, max = 4, value = 1.5, step = 0.01),
                 conditionalPanel(
                   condition = "input.constraint_mode == 'none'",
                   sliderInput("crit", "Criterion (c):", min = -2, max = 4, value = 1, step = 0.01)
                 ),
                 conditionalPanel(
                   condition = "input.constraint_mode != 'none'",
                   div(style = "opacity: 0.5; pointer-events: none;",
                       sliderInput("crit_disabled", "Criterion (c) [Fixed by constraint]:", min = -2, max = 4, value = 1, step = 0.01)
                   )
                 ),
                 sliderInput("sigma_s", "Signal Std Dev (sigma_s):", min = 0.5, max = 2.0, value = 1.0, step = 0.1),
                 hr(),
                 checkboxInput("show_beta", "Show Likelihood Ratio (Beta)", FALSE),
                 checkboxInput("show_pss", "Show Peirce Skill Score (PSS)", FALSE),
                 checkboxInput("separate_dists", "Separate by True State", FALSE),
                 checkboxInput("bw_mode", "Black & White (Publication Mode)", FALSE),
                 conditionalPanel(
                   condition = "input.bw_mode == true",
                   textInput("x_label", "X-axis label:", value = "Evidence"),
                   textInput("y_label", "Y-axis label:", value = "Density"),
                   downloadButton("download_plot", "Download Plot (PNG)")
                 ),
                 hr(),
                 h4("Metrics"),
                 tableOutput("stats_table")
               ),
               
               mainPanel(
                 plotOutput("roc_plot", click = "roc_click", height = "400px"),
                 hr(),
                 plotOutput("dist_plot", height = "350px"),
                 hr(),
                 h4("Magnitude-Aware Confusion Matrix"),
                 uiOutput("colored_conf_matrix")
               )
             )
    ),
    
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
    ),
    
    tabPanel("Simulate Observations",
             sidebarLayout(
               sidebarPanel(
                 h4("Simulation Parameters"),
                 selectInput("sim_source", "Simulate from:",
                             choices = list("SDT Parameters (Tab 1)" = "sdt",
                                            "Transit Observations (Tab 3)" = "transit",
                                            "Fechner Psychophysics (Tab 4)" = "fechner")),
                 conditionalPanel(
                   condition = "input.sim_source == 'sdt'",
                   p("Using d' =", textOutput("sim_d_display", inline = TRUE),
                     ", c =", textOutput("sim_c_display", inline = TRUE))
                 ),
                 conditionalPanel(
                   condition = "input.sim_source == 'transit'",
                   p("Simulating transit timing observations"),
                   checkboxInput("sim_both_observers", "Simulate both observers", FALSE)
                 ),
                 conditionalPanel(
                   condition = "input.sim_source == 'fechner'",
                   p("Simulating Fechner weight comparisons")
                 ),
                 numericInput("n_total", "Total Number of Observations:", value = 200, min = 1, max = 10000),
                 radioButtons("prop_input_mode", "Signal Trial Input:",
                              choices = list("Proportion" = "prop", "Count" = "count")),
                 conditionalPanel(
                   condition = "input.prop_input_mode == 'prop'",
                   sliderInput("prop_signal", "Proportion Signal Trials:", 
                               min = 0, max = 1, value = 0.5, step = 0.01)
                 ),
                 conditionalPanel(
                   condition = "input.prop_input_mode == 'count'",
                   numericInput("count_signal", "Number of Signal Trials:", value = 100, min = 0)
                 ),
                 checkboxInput("randomize_order", "Randomize Trial Order", FALSE),
                 sliderInput("speed", "Animation Speed (obs/frame):", 
                             min = 1, max = 100, value = 10, step = 1),
                 checkboxInput("separate_dists_sim", "Separate by True State", FALSE),
                 actionButton("start_sim", "Start Simulation", class = "btn-primary"),
                 actionButton("reset_sim", "Reset", class = "btn-warning"),
                 hr(),
                 h4("Current Counts"),
                 tableOutput("sim_counts")
               ),
               
               mainPanel(
                 plotOutput("sim_hist", height = "400px"),
                 hr(),
                 h4("Simulated Confusion Matrix"),
                 uiOutput("sim_conf_matrix"),
                 hr(),
                 textOutput("sim_progress")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  val <- reactiveValues(d = 1.5, c = 1, s = 1)
  sim <- reactiveValues(
    observations = numeric(0),
    labels = character(0),
    running = FALSE,
    current_idx = 0
  )
  
  # Handle preset selection
  observeEvent(input$preset, {
    if(input$preset == "finley") {
      # Finley's tornado data: 28 hits, 72 FA, 23 misses, 2680 CR
      hit_rate <- 28/51
      fa_rate <- 72/2752
      
      hr_adj <- pmin(pmax(hit_rate, 0.001), 0.999)
      fa_adj <- pmin(pmax(fa_rate, 0.001), 0.999)
      
      c_est <- -qnorm(fa_adj)
      d_est <- qnorm(hr_adj) + qnorm(1 - fa_adj)
      
      val$d <- d_est
      val$c <- c_est
      val$s <- 1.0
      
      updateSliderInput(session, "d_prime", value = round(d_est, 3))
      updateSliderInput(session, "crit", value = round(c_est, 3))
      updateSliderInput(session, "crit_disabled", value = round(c_est, 3))
      updateSliderInput(session, "sigma_s", value = 1.0)
      updateRadioButtons(session, "constraint_mode", selected = "none")
      
      updateNumericInput(session, "n_total", value = 2803)
      updateNumericInput(session, "count_signal", value = 51)
    }
  })
  
  # Handle constraint modes
  observe({
    if(input$constraint_mode == "beta") {
      # Fix beta (likelihood ratio), adjust c based on d
      # beta = dnorm(c, d, s) / dnorm(c, 0, 1)
      # For beta = 1, c = d/2 when s = 1
      # General formula: c = d / (1 + s^2) * log(beta) adjustment
      target_beta <- input$target_beta
      d <- input$d_prime
      s <- input$sigma_s
      
      # Simplified: for equal variance (s=1) and beta=1, c = d/2
      if(abs(target_beta - 1) < 0.01 && abs(s - 1) < 0.01) {
        new_c <- d / 2
      } else {
        # Numerical solution for general case
        objective <- function(c_val) {
          beta_calc <- dnorm(c_val, d, s) / dnorm(c_val, 0, 1)
          (beta_calc - target_beta)^2
        }
        result <- optimize(objective, interval = c(-2, 4))
        new_c <- result$minimum
      }
      
      val$c <- new_c
      updateSliderInput(session, "crit_disabled", value = round(new_c, 3))
    } else if(input$constraint_mode == "hitrate") {
      # Fix hit rate, adjust c based on d
      target_hr <- input$target_hitrate
      d <- input$d_prime
      s <- input$sigma_s
      
      # Hit rate = P(response > c | signal) = 1 - Phi((c - d) / s)
      # So c = d - s * Phi^(-1)(1 - target_hr)
      new_c <- d - s * qnorm(1 - target_hr)
      
      val$c <- new_c
      updateSliderInput(session, "crit_disabled", value = round(new_c, 3))
    } else {
      # No constraint - use slider values
      val$c <- input$crit
      updateSliderInput(session, "crit_disabled", value = input$crit)
    }
    
    # d' and sigma_s always follow sliders
    val$d <- input$d_prime
    val$s <- input$sigma_s
  })
  
  observeEvent(input$roc_click, {
    fp_clicked <- pmin(pmax(input$roc_click$x, 0.001), 0.999)
    tp_clicked <- pmin(pmax(input$roc_click$y, 0.001), 0.999)
    
    new_c <- -qnorm(fp_clicked)
    new_d <- new_c - (qnorm(1 - tp_clicked) * input$sigma_s)
    
    val$c <- new_c
    val$d <- new_d
    val$s <- input$sigma_s
    
    updateSliderInput(session, "d_prime", value = round(new_d, 3))
    updateSliderInput(session, "crit", value = round(new_c, 3))
    updateSliderInput(session, "crit_disabled", value = round(new_c, 3))
    updateSelectInput(session, "preset", selected = "custom")
    updateRadioButtons(session, "constraint_mode", selected = "none")
  })
  
  metrics <- reactive({
    fp <- 1 - pnorm(val$c)
    tp <- 1 - pnorm((val$c - val$d) / val$s)
    list(fp = fp, tp = tp, pss = tp - fp, 
         beta = dnorm(val$c, val$d, val$s) / dnorm(val$c, 0, 1))
  })
  
  output$roc_plot <- renderPlot({
    x_vals <- seq(0.001, 0.999, length.out = 100)
    y_vals <- 1 - pnorm(((-qnorm(x_vals)) - val$d) / val$s)
    m <- metrics()
    
    ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
      annotate("point", x = m$fp, y = m$tp, color = "red", size = 5) +
      {if(input$show_pss) geom_segment(aes(x = m$fp, xend = m$fp, y = m$fp, yend = m$tp), color = "purple", size = 1)} +
      labs(title = "ROC Space", x = "P(False Alarm)", y = "P(Hit)") +
      theme_minimal() + coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1))
  })
  
  output$dist_plot <- renderPlot({
    x <- seq(-4, 8, length.out = 400)
    df <- data.frame(x = x, noise = dnorm(x, 0, 1), signal = dnorm(x, val$d, val$s))
    m <- metrics()
    
    # Set up colors or patterns based on mode
    if(input$bw_mode) {
      # For B&W we'll build custom hatched patterns
      fill_vals <- c("Hit"="white","Miss"="gray40","False Alarm"="gray70","Correct Rejection"="white")
    } else {
      fill_vals <- c("Hit"="#2ca02c","Miss"="#d62728","False Alarm"="#ff7f0e","Correct Rejection"="#1f77b4")
    }
    
    line_col <- if(input$bw_mode) "black" else "steelblue"
    vline_col <- "black"
    
    x_lab <- if(input$bw_mode) input$x_label else "x"
    y_lab <- if(input$bw_mode) input$y_label else "Density"
    
    # For B&W mode, we'll use different visual approaches for each outcome
    # Hit: white fill, solid border
    # Miss: gray fill
    # FA: darker gray fill  
    # CR: white fill, dashed border (will approximate with styling)
    
    if(input$separate_dists) {
      # Separate plots: Signal Present | Signal Absent
      alpha_val <- if(input$bw_mode) 0.3 else 0.5
      p <- ggplot(df, aes(x = x)) +
        # Signal present trials
        geom_ribbon(data = subset(df, x > val$c), aes(ymin = 0, ymax = signal, fill = "Hit"), alpha = alpha_val) +
        geom_ribbon(data = subset(df, x <= val$c), aes(ymin = 0, ymax = signal, fill = "Miss"), alpha = alpha_val) +
        geom_line(aes(y = signal), color = line_col, size = 1) +
        # Signal absent trials
        geom_ribbon(data = subset(df, x > val$c), aes(ymin = -0.45, ymax = -0.45 + noise, fill = "False Alarm"), alpha = alpha_val) +
        geom_ribbon(data = subset(df, x <= val$c), aes(ymin = -0.45, ymax = -0.45 + noise, fill = "Correct Rejection"), alpha = alpha_val) +
        geom_line(aes(y = -0.45 + noise), linetype = "dashed", color = line_col, size = 1) +
        geom_vline(xintercept = val$c, color = vline_col, size = 1) +
        geom_hline(yintercept = -0.45, color = "gray50", linetype = "dotted") +
        geom_hline(yintercept = 0, color = "black", size = 0.3) +
        scale_fill_manual(values = fill_vals) +
        theme_minimal() +
        labs(title = "Evidence Distributions (Separated by True State)", x = x_lab, y = y_lab, fill = "Outcome") +
        annotate("text", x = -3, y = 0.35, label = "Signal PRESENT", fontface = "bold", hjust = 0) +
        annotate("text", x = -3, y = -0.1, label = "Signal ABSENT", fontface = "bold", hjust = 0) +
        expand_limits(y = c(-0.45, 0))
    } else {
      # Original overlapping view
      if(input$bw_mode) {
        # B&W mode with semi-transparent grayscale fills
        p <- ggplot(df, aes(x = x)) +
          # Overlapping ribbons with semi-transparency
          geom_ribbon(data = subset(df, x > val$c), aes(ymin = 0, ymax = signal, fill = "Hit"), alpha = 0.3) +
          geom_ribbon(data = subset(df, x <= val$c), aes(ymin = 0, ymax = signal, fill = "Miss"), alpha = 0.3) +
          geom_ribbon(data = subset(df, x > val$c), aes(ymin = 0, ymax = noise, fill = "False Alarm"), alpha = 0.3) +
          geom_ribbon(data = subset(df, x <= val$c), aes(ymin = 0, ymax = noise, fill = "Correct Rejection"), alpha = 0.3) +
          geom_line(aes(y = noise), linetype = "dashed", color = line_col, size = 1) +
          geom_line(aes(y = signal), color = line_col, size = 1) +
          geom_vline(xintercept = val$c, color = vline_col, size = 1) +
          geom_hline(yintercept = 0, color = "black", size = 0.3) +
          scale_fill_manual(values = fill_vals) +
          theme_minimal() +
          labs(title = "Evidence Distributions", x = x_lab, y = y_lab, fill = "Outcome") +
          expand_limits(y = 0)
      } else {
        p <- ggplot(df, aes(x = x)) +
          geom_ribbon(data = subset(df, x > val$c), aes(ymin = 0, ymax = signal, fill = "Hit"), alpha = 0.5) +
          geom_ribbon(data = subset(df, x <= val$c), aes(ymin = 0, ymax = signal, fill = "Miss"), alpha = 0.5) +
          geom_ribbon(data = subset(df, x > val$c), aes(ymin = 0, ymax = noise, fill = "False Alarm"), alpha = 0.5) +
          geom_ribbon(data = subset(df, x <= val$c), aes(ymin = 0, ymax = noise, fill = "Correct Rejection"), alpha = 0.5) +
          geom_line(aes(y = noise), linetype = "dashed", color = line_col, size = 1) +
          geom_line(aes(y = signal), color = line_col, size = 1) +
          geom_vline(xintercept = val$c, color = vline_col, size = 1) +
          geom_hline(yintercept = 0, color = "black", size = 0.3) +
          scale_fill_manual(values = fill_vals) +
          theme_minimal() +
          labs(title = "Evidence Distributions", x = x_lab, y = y_lab, fill = "Outcome") +
          expand_limits(y = 0)
      }
    }
    
    if(input$show_beta && !input$separate_dists) {
      p <- p + 
        annotate("point", x = val$c, y = dnorm(val$c, 0, 1), color = "black", fill = "white", shape = 21, size = 3) +
        annotate("point", x = val$c, y = dnorm(val$c, val$d, val$s), color = "black", fill = "white", shape = 21, size = 3) +
        annotate("text", x = val$c + 0.8, y = 0.38, label = paste("beta:", round(m$beta, 2)), fontface = "bold")
    }
    p
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("sdt_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  output$colored_conf_matrix <- renderUI({
    m <- metrics()
    tp <- m$tp; fn <- 1-m$tp; fp <- m$fp; tn <- 1-m$fp
    
    HTML(paste0('
      <table class="conf-table">
        <tr>
          <th class="label-cell"></th>
          <th class="label-cell">Response: YES</th>
          <th class="label-cell">Response: NO</th>
        </tr>
        <tr>
          <th class="label-cell">Signal PRESENT</th>
          <td style="background-color: rgba(', col_hit, ',', tp, '); color: black;">Hit<br>', round(tp, 3), '</td>
          <td style="background-color: rgba(', col_miss, ',', fn, '); color: black;">Miss<br>', round(fn, 3), '</td>
        </tr>
        <tr>
          <th class="label-cell">Signal ABSENT</th>
          <td style="background-color: rgba(', col_fa, ',', fp, '); color: black;">False Alarm<br>', round(fp, 3), '</td>
          <td style="background-color: rgba(', col_cr, ',', tn, '); color: black;">Correct Rejection<br>', round(tn, 3), '</td>
        </tr>
      </table>
    '))
  })
  
  output$stats_table <- renderTable({
    m <- metrics()
    data.frame(
      Metric = c("d' (Sensitivity)", "c (Criterion)", "Beta (Likelihood Ratio)", "PSS (Peirce Skill)"),
      Value = c(val$d, val$c, m$beta, m$pss)
    )
  })
  
  # Simulation tab outputs
  output$sim_d_display <- renderText({ round(val$d, 2) })
  output$sim_c_display <- renderText({ round(val$c, 2) })
  
  observeEvent(input$start_sim, {
    # Generate all observations at once
    n_total <- input$n_total
    
    # Get signal count based on input mode
    if(input$prop_input_mode == "prop") {
      n_sig <- round(n_total * input$prop_signal)
    } else {
      n_sig <- min(input$count_signal, n_total)
    }
    n_noi <- n_total - n_sig
    
    signal_obs <- rnorm(n_sig, mean = val$d, sd = val$s)
    noise_obs <- rnorm(n_noi, mean = 0, sd = 1)
    
    obs <- c(signal_obs, noise_obs)
    labels <- c(rep("signal", n_sig), rep("noise", n_noi))
    
    # Randomize order if requested
    if(input$randomize_order) {
      idx <- sample(length(obs))
      obs <- obs[idx]
      labels <- labels[idx]
    }
    
    sim$observations <- obs
    sim$labels <- labels
    sim$current_idx <- 0
    sim$running <- TRUE
    
    # Start animation
    observe({
      invalidateLater(100, session)
      isolate({
        if(sim$running && sim$current_idx < length(sim$observations)) {
          increment <- min(input$speed, length(sim$observations) - sim$current_idx)
          sim$current_idx <- sim$current_idx + increment
          
          if(sim$current_idx >= length(sim$observations)) {
            sim$running <- FALSE
          }
        }
      })
    })
  })
  
  observeEvent(input$reset_sim, {
    sim$observations <- numeric(0)
    sim$labels <- character(0)
    sim$running <- FALSE
    sim$current_idx <- 0
  })
  
  output$sim_hist <- renderPlot({
    req(length(sim$observations) > 0, sim$current_idx > 0)
    
    current_obs <- sim$observations[1:sim$current_idx]
    current_labels <- sim$labels[1:sim$current_idx]
    
    df <- data.frame(value = current_obs, label = current_labels)
    df$outcome <- ifelse(df$label == "signal",
                         ifelse(df$value > val$c, "Hit", "Miss"),
                         ifelse(df$value > val$c, "False Alarm", "Correct Rejection"))
    
    # Theoretical distributions
    x <- seq(-4, 8, length.out = 400)
    theory <- data.frame(x = x, noise = dnorm(x, 0, 1), signal = dnorm(x, val$d, val$s))
    
    if(input$separate_dists_sim) {
      # Separate by true state
      df_signal <- subset(df, label == "signal")
      df_noise <- subset(df, label == "noise")
      
      ggplot() +
        # Signal present trials (top)
        geom_histogram(data = df_signal, aes(x = value, fill = outcome, y = after_stat(count/sum(count))), 
                       bins = 50, alpha = 0.7, position = "identity") +
        geom_line(data = theory, aes(x = x, y = signal), size = 1) +
        # Signal absent trials (bottom, mirrored)
        geom_histogram(data = df_noise, aes(x = value, fill = outcome, y = -after_stat(count/sum(count))), 
                       bins = 50, alpha = 0.7, position = "identity") +
        geom_line(data = theory, aes(x = x, y = -noise), linetype = "dashed", size = 1) +
        geom_vline(xintercept = val$c, color = "black", size = 1) +
        geom_hline(yintercept = 0, color = "gray50", linetype = "dotted") +
        scale_fill_manual(values = c("Hit"="#2ca02c","Miss"="#d62728",
                                     "False Alarm"="#ff7f0e","Correct Rejection"="#1f77b4")) +
        theme_minimal() + 
        labs(title = paste("Simulated Observations (n =", sim$current_idx, ")"),
             x = "Evidence Value", y = "Density", fill = "Outcome") +
        annotate("text", x = -3, y = 0.35, label = "Signal PRESENT", fontface = "bold", hjust = 0) +
        annotate("text", x = -3, y = -0.35, label = "Signal ABSENT", fontface = "bold", hjust = 0)
    } else {
      # Original overlapping view
      ggplot() +
        geom_histogram(data = df, aes(x = value, fill = outcome, y = after_stat(count/sum(count))), 
                       bins = 50, alpha = 0.7, position = "identity") +
        geom_line(data = theory, aes(x = x, y = noise), linetype = "dashed", size = 1) +
        geom_line(data = theory, aes(x = x, y = signal), size = 1) +
        geom_vline(xintercept = val$c, color = "black", size = 1) +
        scale_fill_manual(values = c("Hit"="#2ca02c","Miss"="#d62728",
                                     "False Alarm"="#ff7f0e","Correct Rejection"="#1f77b4")) +
        theme_minimal() + 
        labs(title = paste("Simulated Observations (n =", sim$current_idx, ")"),
             x = "Evidence Value", y = "Density", fill = "Outcome")
    }
  })
  
  output$sim_conf_matrix <- renderUI({
    req(length(sim$observations) > 0, sim$current_idx > 0)
    
    current_obs <- sim$observations[1:sim$current_idx]
    current_labels <- sim$labels[1:sim$current_idx]
    
    hits <- sum(current_labels == "signal" & current_obs > val$c)
    misses <- sum(current_labels == "signal" & current_obs <= val$c)
    fas <- sum(current_labels == "noise" & current_obs > val$c)
    crs <- sum(current_labels == "noise" & current_obs <= val$c)
    
    total_signal <- sum(current_labels == "signal")
    total_noise <- sum(current_labels == "noise")
    
    hit_prop <- if(total_signal > 0) hits/total_signal else 0
    miss_prop <- if(total_signal > 0) misses/total_signal else 0
    fa_prop <- if(total_noise > 0) fas/total_noise else 0
    cr_prop <- if(total_noise > 0) crs/total_noise else 0
    
    HTML(paste0('
      <table class="conf-table">
        <tr>
          <th class="label-cell"></th>
          <th class="label-cell">Response: YES</th>
          <th class="label-cell">Response: NO</th>
        </tr>
        <tr>
          <th class="label-cell">Signal PRESENT</th>
          <td style="background-color: rgba(', col_hit, ',', hit_prop, '); color: black;">Hit<br>', 
                hits, ' (', round(hit_prop, 3), ')</td>
          <td style="background-color: rgba(', col_miss, ',', miss_prop, '); color: black;">Miss<br>', 
                misses, ' (', round(miss_prop, 3), ')</td>
        </tr>
        <tr>
          <th class="label-cell">Signal ABSENT</th>
          <td style="background-color: rgba(', col_fa, ',', fa_prop, '); color: black;">False Alarm<br>', 
                fas, ' (', round(fa_prop, 3), ')</td>
          <td style="background-color: rgba(', col_cr, ',', cr_prop, '); color: black;">Correct Rejection<br>', 
                crs, ' (', round(cr_prop, 3), ')</td>
        </tr>
      </table>
    '))
  })
  
  output$sim_counts <- renderTable({
    req(length(sim$observations) > 0, sim$current_idx > 0)
    
    current_obs <- sim$observations[1:sim$current_idx]
    current_labels <- sim$labels[1:sim$current_idx]
    
    hits <- sum(current_labels == "signal" & current_obs > val$c)
    misses <- sum(current_labels == "signal" & current_obs <= val$c)
    fas <- sum(current_labels == "noise" & current_obs > val$c)
    crs <- sum(current_labels == "noise" & current_obs <= val$c)
    
    data.frame(
      Outcome = c("Hits", "Misses", "False Alarms", "Correct Rejections"),
      Count = c(hits, misses, fas, crs)
    )
  })
  
  output$sim_progress <- renderText({
    if(length(sim$observations) == 0) {
      "Click 'Start Simulation' to begin"
    } else if(sim$running) {
      paste("Simulating...", sim$current_idx, "of", length(sim$observations), "observations")
    } else if(sim$current_idx > 0) {
      paste("Simulation complete:", sim$current_idx, "observations")
    }
  })
  
  # Transit Observations Tab
  transit_params <- reactive({
    # Base parameters for each method
    # As methods improve, training matters less (smaller quality_effect)
    base_params <- switch(input$transit_method,
                          "bradley" = list(base_d = 1.2, base_sigma = 1.8, quality_effect = 0.8),
                          "hipp" = list(base_d = 2.0, base_sigma = 1.3, quality_effect = 0.5),
                          "wire_person" = list(base_d = 2.8, base_sigma = 1.1, quality_effect = 0.3),
                          "wire_mech" = list(base_d = 4.0, base_sigma = 1.0, quality_effect = 0.1)
    )
    
    # Training quality improves d' and reduces sigma
    # quality_effect determines how much training matters
    qe <- base_params$quality_effect
    obs1_d <- base_params$base_d * (1 - qe + qe * input$observer1_quality)
    obs1_sigma <- base_params$base_sigma * (1 + qe - qe * input$observer1_quality)
    # For transit timing, c represents systematic bias (should be near 0 for unbiased observer)
    obs1_c <- 0
    
    result <- list(
      obs1_d = obs1_d,
      obs1_c = obs1_c,
      obs1_sigma = obs1_sigma
    )
    
    if(input$show_observer2) {
      obs2_d <- base_params$base_d * (1 - qe + qe * input$observer2_quality)
      obs2_sigma <- base_params$base_sigma * (1 + qe - qe * input$observer2_quality)
      # Personal equation is the systematic bias (in ms), scaled to observer's precision
      # Scale so that max personal_eq (500ms) = ~1 sigma, making it consistent across methods
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
    
    # For transit observations:
    # Noise distribution = timing errors when there's no actual transit (centered at 0)
    # Signal distribution = timing errors when detecting actual transit (centered at d')
    # But conceptually: true transit time is at 0, observations spread around it
    
    # Observer 1 - distribution of timing measurements
    df1 <- data.frame(
      x = x,
      obs_dist = dnorm(x, params$obs1_c, params$obs1_sigma)
    )
    
    p <- ggplot() +
      # Show the distribution of observed transit times
      geom_ribbon(data = df1, aes(x = x, ymin = 0, ymax = obs_dist), 
                  fill = "steelblue", alpha = 0.6) +
      geom_line(data = df1, aes(x = x, y = obs_dist), 
                color = "steelblue", size = 1.2) +
      # Mark the true transit time
      geom_vline(xintercept = 0, color = "red", size = 1.5, linetype = "solid") +
      annotate("text", x = 0.2, y = max(df1$obs_dist) * 0.95, 
               label = "True Transit Time", color = "red", hjust = 0, fontface = "bold") +
      # Mark observer's systematic bias
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
    
    # For transit observations, we care about precision (sigma) and systematic bias (c)
    # Accuracy = how close mean is to 0; Precision = how small sigma is
    
    result <- data.frame(
      Observer = "Observer 1",
      Precision_sigma = round(params$obs1_sigma, 3),
      Systematic_Bias = round(params$obs1_c, 3),
      Mean_Abs_Error = round(params$obs1_sigma * sqrt(2/pi), 3),  # Expected |error|
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
    # Physical difference between weights in grams
    D <- abs(input$weight2 - input$weight1)
    
    # For equal weights, d' = 0
    if(D == 0) {
      return(list(d = 0, c = 0, sigma = 1, D = 0, h = 0, t = 0, error_prob = 0.5))
    }
    
    # Fechner's methodology from the paper:
    # σ is a PROPERTY OF THE SENSORY SYSTEM - it doesn't change with weight difference!
    # From the paper: "Supposing the physical difference, D, to be 10 g yields
    # a measure of the standard deviation equal to 7.4 g"
    # This means: for D=10g and 25% error rate (t=0.4769), we get σ=7.4g
    # σ = (10/2) / (0.4769 × √2) = 7.4g
    
    # The sensory system has CONSTANT precision
    sigma <- 7.4  # grams - constant property of the sensory system
    h <- 1 / (sqrt(2) * sigma)  # Precision h = 1/(√2 * σ)
    
    # In SDT terms: the two weights create distributions separated by d'
    # As D increases, d' increases (distributions move apart)
    # As D decreases, d' decreases (distributions closer together)
    d_prime <- D / sigma
    
    # Fechner's threshold: midway between the two means
    # This gives c = d'/2
    c <- d_prime / 2
    
    # Calculate t value for THIS weight difference
    # t = h(D/2) - this changes with D!
    t <- h * (D / 2)
    
    # Calculate actual error probability for this configuration
    # Error occurs when lighter weight WA > threshold OR heavier weight WB < threshold
    # P(WA > (a+b)/2) = P(WA > a + d/2) where d = b-a
    # In standardized units: P(WA > c) where WA ~ N(0, 1)
    p_error_light <- 1 - pnorm(c, 0, 1)  # P(lighter judged heavier)
    p_error_heavy <- pnorm(c, d_prime, 1)  # P(heavier judged lighter)
    # Both errors are equal by symmetry
    error_prob <- p_error_light  # Will be 0.25 only when D=10g
    
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
    
    # Determine which weight is actually heavier
    if(input$weight1 == input$weight2) {
      # Equal weights - special case
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
      # Standard case: two different weights
      # Weight A (lighter) centered at a=0, Weight B (heavier) centered at b=d'
      x <- seq(-3, max(5, fv$d + 3), length.out = 400)
      
      # Both distributions have EQUAL variance (σ = 1 in standardized units)
      # This is KEY to Fechner's model
      df <- data.frame(
        x = x,
        WA = dnorm(x, 0, 1),      # Sensation from lighter weight (true value = a)
        WB = dnorm(x, fv$d, 1)    # Sensation from heavier weight (true value = b)
      )
      
      # Calculate midpoint (a+b)/2 which equals d'/2 in standardized units
      threshold <- fv$c  # This is (a+b)/2 = d'/2
      
      ggplot(df, aes(x = x)) +
        # WB (heavier weight) - errors when sensation < threshold
        geom_ribbon(data = subset(df, x > threshold),
                    aes(ymin = 0, ymax = WB), fill = "#2ca02c", alpha = 0.4) +
        geom_ribbon(data = subset(df, x <= threshold),
                    aes(ymin = 0, ymax = WB), fill = "#d62728", alpha = 0.4) +
        # WA (lighter weight) - errors when sensation > threshold
        geom_ribbon(data = subset(df, x > threshold),
                    aes(ymin = 0, ymax = WA), fill = "#ff7f0e", alpha = 0.4) +
        geom_ribbon(data = subset(df, x <= threshold),
                    aes(ymin = 0, ymax = WA), fill = "#1f77b4", alpha = 0.4) +
        # Distribution curves
        geom_line(aes(y = WA), linetype = "dashed", color = "steelblue", size = 1.2) +
        geom_line(aes(y = WB), linetype = "solid", color = "darkgreen", size = 1.2) +
        # Threshold line
        geom_vline(xintercept = threshold, color = "red", size = 1.2) +
        geom_hline(yintercept = 0, color = "black", size = 0.3) +
        # Labels on distributions
        annotate("text", x = 0, y = max(df$WA) * 1.05,
                 label = paste0("W_A\n(", input$weight1, "g)"),
                 color = "steelblue", fontface = "bold", size = 4) +
        annotate("text", x = fv$d, y = max(df$WB) * 1.05,
                 label = paste0("W_B\n(", input$weight2, "g)"),
                 color = "darkgreen", fontface = "bold", size = 4) +
        # Threshold annotation
        annotate("text", x = threshold, y = max(df$WA) * 1.2,
                 label = paste0("Threshold\n(a+b)/2 = ", round(threshold, 3)),
                 color = "red", fontface = "bold", hjust = 0.5, size = 3.5) +
        # Distance annotation - showing (b-a) = d'
        annotate("segment", x = 0, xend = fv$d, y = -0.05, yend = -0.05,
                 arrow = arrow(ends = "both", length = unit(0.15, "cm")),
                 color = "black", size = 0.8) +
        annotate("text", x = fv$d/2, y = -0.08,
                 label = paste0("b - a = d' = ", round(fv$d, 3)),
                 hjust = 0.5, size = 3.5, fontface = "bold") +
        # Distance from a to threshold (= d'/2)
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
      # Error probabilities according to Fechner's theory:
      # For lighter weight (W_A): error when sensation W_A > (a+b)/2
      # P(W_A > a + d/2) where W_A ~ N(a, σ) = N(0, 1) in standardized units
      p_error_light <- 1 - pnorm(fv$c, 0, 1)  # P(W_A > threshold)
      
      # For heavier weight (W_B): error when sensation W_B < (a+b)/2
      # P(W_B < a + d/2) where W_B ~ N(b, σ) = N(d', 1) in standardized units
      p_error_heavy <- pnorm(fv$c, fv$d, 1)   # P(W_B < threshold)
      
      # Due to symmetry, both error rates are equal
      # This is a key insight of Fechner's model
      
      data.frame(
        Prediction = c("P(W_A misjudged heavier)",
                       "P(W_B misjudged lighter)",
                       "Error rate (either judgment)",
                       "P(Correct discrimination)"),
        Value = c(round(p_error_light, 4),
                  round(p_error_heavy, 4),
                  round(p_error_light, 4),  # Both errors equal by symmetry
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
