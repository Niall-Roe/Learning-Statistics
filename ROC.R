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

    tabPanel("Simulate Observations",
             sidebarLayout(
               sidebarPanel(
                 h4("Simulation Parameters"),
                 p("Using d' =", textOutput("sim_d_display", inline = TRUE),
                   ", c =", textOutput("sim_c_display", inline = TRUE),
                   "from SDT Parameters tab"),
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
      target_beta <- input$target_beta
      d <- input$d_prime
      s <- input$sigma_s

      if(abs(target_beta - 1) < 0.01 && abs(s - 1) < 0.01) {
        new_c <- d / 2
      } else {
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
      target_hr <- input$target_hitrate
      d <- input$d_prime
      s <- input$sigma_s

      new_c <- d - s * qnorm(1 - target_hr)

      val$c <- new_c
      updateSliderInput(session, "crit_disabled", value = round(new_c, 3))
    } else {
      val$c <- input$crit
      updateSliderInput(session, "crit_disabled", value = input$crit)
    }

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

    if(input$bw_mode) {
      fill_vals <- c("Hit"="white","Miss"="gray40","False Alarm"="gray70","Correct Rejection"="white")
    } else {
      fill_vals <- c("Hit"="#2ca02c","Miss"="#d62728","False Alarm"="#ff7f0e","Correct Rejection"="#1f77b4")
    }

    line_col <- if(input$bw_mode) "black" else "steelblue"
    vline_col <- "black"

    x_lab <- if(input$bw_mode) input$x_label else "x"
    y_lab <- if(input$bw_mode) input$y_label else "Density"

    if(input$separate_dists) {
      alpha_val <- if(input$bw_mode) 0.3 else 0.5
      p <- ggplot(df, aes(x = x)) +
        geom_ribbon(data = subset(df, x > val$c), aes(ymin = 0, ymax = signal, fill = "Hit"), alpha = alpha_val) +
        geom_ribbon(data = subset(df, x <= val$c), aes(ymin = 0, ymax = signal, fill = "Miss"), alpha = alpha_val) +
        geom_line(aes(y = signal), color = line_col, size = 1) +
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
      if(input$bw_mode) {
        p <- ggplot(df, aes(x = x)) +
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
    n_total <- input$n_total

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

    if(input$randomize_order) {
      idx <- sample(length(obs))
      obs <- obs[idx]
      labels <- labels[idx]
    }

    sim$observations <- obs
    sim$labels <- labels
    sim$current_idx <- 0
    sim$running <- TRUE

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

    x <- seq(-4, 8, length.out = 400)
    theory <- data.frame(x = x, noise = dnorm(x, 0, 1), signal = dnorm(x, val$d, val$s))

    if(input$separate_dists_sim) {
      df_signal <- subset(df, label == "signal")
      df_noise <- subset(df, label == "noise")

      ggplot() +
        geom_histogram(data = df_signal, aes(x = value, fill = outcome, y = after_stat(count/sum(count))),
                       bins = 50, alpha = 0.7, position = "identity") +
        geom_line(data = theory, aes(x = x, y = signal), size = 1) +
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
}

shinyApp(ui, server)
