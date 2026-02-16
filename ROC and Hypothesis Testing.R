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
    .mode-toggle { margin-bottom: 15px; padding: 10px; background-color: #f0f0f0; border-radius: 5px; }
    .result-box { padding: 15px; margin: 10px 0; border-radius: 5px; }
    .reject-h0 { background-color: #ffcccc; border: 2px solid #d62728; }
    .fail-reject { background-color: #cce5ff; border: 2px solid #1f77b4; }
    .correct-decision { background-color: #d4edda; border: 2px solid #28a745; }
    .incorrect-decision { background-color: #f8d7da; border: 2px solid #dc3545; }
    .comparison-table { width: 100%; border-collapse: collapse; margin: 20px 0; }
    .comparison-table th, .comparison-table td { border: 1px solid #ddd; padding: 12px; text-align: left; }
    .comparison-table th { background-color: #f8f9fa; }
    .comparison-table tr:nth-child(even) { background-color: #f9f9f9; }
    .sdt-col { background-color: #e3f2fd !important; }
    .ht-col { background-color: #fff3e0 !important; }
    .severity-box { padding: 15px; margin: 10px 0; border-radius: 5px; background-color: #f5f5f5; border: 1px solid #ddd; }
    .severity-high { background-color: #c8e6c9; border-color: #4caf50; }
    .severity-medium { background-color: #fff9c4; border-color: #fbc02d; }
    .severity-low { background-color: #ffcdd2; border-color: #e53935; }
    .severity-bar { height: 20px; border-radius: 3px; margin: 5px 0; }
    .snap-btn { margin: 2px; }
  "))),

  titlePanel(uiOutput("main_title")),

  # Mode toggle at top level
  div(class = "mode-toggle",
      radioButtons("analysis_mode", "Analysis Framework:",
                   choices = list("Signal Detection Theory" = "sdt",
                                  "Neyman-Pearson Hypothesis Testing" = "ht"),
                   inline = TRUE)
  ),

  tabsetPanel(id = "main_tabs",
    # ========== TAB 1: Parameters ==========
    tabPanel(uiOutput("tab1_title"),
             sidebarLayout(
               sidebarPanel(
                 # SDT-specific presets
                 conditionalPanel(
                   condition = "input.analysis_mode == 'sdt'",
                   h4("Preset Scenarios"),
                   selectInput("preset", "Load Preset:",
                               choices = list("Custom" = "custom",
                                              "Finley's Tornado Data (1884)" = "finley"))
                 ),
                 # HT-specific presets
                 conditionalPanel(
                   condition = "input.analysis_mode == 'ht'",
                   h4("Preset Scenarios"),
                   selectInput("preset_ht", "Load Preset:",
                               choices = list("Custom" = "custom",
                                              "Drug Trial (d = 0.8)" = "drug_trial",
                                              "A/B Test (d = 0.3)" = "ab_test",
                                              "Classic Psychology (d = 0.5)" = "psych"))
                 ),
                 hr(),

                 # SDT constraint modes
                 conditionalPanel(
                   condition = "input.analysis_mode == 'sdt'",
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
                   )
                 ),

                 # HT constraint modes
                 conditionalPanel(
                   condition = "input.analysis_mode == 'ht'",
                   radioButtons("constraint_mode_ht", "Constraint Mode:",
                                choices = list("None (set critical value directly)" = "none",
                                               "Fix Alpha (Type I Error Rate)" = "alpha",
                                               "Fix Power (1 - Beta)" = "power")),
                   conditionalPanel(
                     condition = "input.constraint_mode_ht == 'alpha'",
                     selectInput("target_alpha", "Target Alpha Level:",
                                 choices = list("0.10 (one-tailed)" = 0.10,
                                                "0.05 (one-tailed)" = 0.05,
                                                "0.01 (one-tailed)" = 0.01,
                                                "0.001 (one-tailed)" = 0.001),
                                 selected = 0.05)
                   ),
                   conditionalPanel(
                     condition = "input.constraint_mode_ht == 'power'",
                     sliderInput("target_power", "Target Power (1 - Beta):", min = 0.50, max = 0.99, value = 0.80, step = 0.01)
                   )
                 ),
                 hr(),

                 # ===== SDT parameter section =====
                 conditionalPanel(
                   condition = "input.analysis_mode == 'sdt'",
                   h4("Distribution Parameters"),
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
                   sliderInput("sigma_s", "Signal Std Dev:", min = 0.5, max = 2.0, value = 1.0, step = 0.1)
                 ),

                 # ===== HT parameter section =====
                 conditionalPanel(
                   condition = "input.analysis_mode == 'ht'",
                   h4("Hypotheses (in standardized units)"),
                   helpText("H0: mu = 0 (fixed at origin)"),
                   sliderInput("mu_1_std", "H1: True Mean (mu_1):", min = 0, max = 3, value = 0.5, step = 0.01),
                   hr(),
                   h4("Sample Size"),
                   sliderInput("sample_size_ht", "Sample Size (n):", min = 5, max = 200, value = 25, step = 5),
                   helpText("Effect size d = mu_1 (since sigma = 1)"),
                   helpText("Standardized effect = d * sqrt(n)"),
                   hr(),
                   h4("Critical Value"),
                   conditionalPanel(
                     condition = "input.constraint_mode_ht == 'none'",
                     sliderInput("critical_value", "Critical Value (z_crit):", min = 0, max = 4, value = 1.645, step = 0.01)
                   ),
                   conditionalPanel(
                     condition = "input.constraint_mode_ht != 'none'",
                     div(style = "opacity: 0.5; pointer-events: none;",
                         sliderInput("crit_disabled_ht", "Critical Value [Fixed]:", min = 0, max = 4, value = 1.645, step = 0.01)
                     )
                   )
                 ),
                 hr(),

                 # Display options
                 conditionalPanel(
                   condition = "input.analysis_mode == 'sdt'",
                   checkboxInput("show_beta", "Show Likelihood Ratios", FALSE),
                   conditionalPanel(
                     condition = "input.show_beta == true",
                     checkboxInput("show_beta_slope", "Show LR as Slope on ROC", FALSE)
                   ),
                   checkboxInput("show_pss", "Show Peirce Skill Score (PSS)", FALSE),
                   checkboxInput("separate_dists", "Separate by True State", FALSE)
                 ),

                 conditionalPanel(
                   condition = "input.analysis_mode == 'ht'",
                   checkboxInput("show_effect_size_ht", "Show Effect Size on Plot", FALSE),
                   checkboxInput("show_power_line", "Show Power - Alpha Line", FALSE),
                   checkboxInput("separate_dists_ht", "Separate Hypotheses", FALSE)
                 ),

                 checkboxInput("bw_mode", "Black & White (Publication Mode)", FALSE),
                 conditionalPanel(
                   condition = "input.bw_mode == true",
                   textInput("x_label", "X-axis label:", value = "Test Statistic"),
                   textInput("y_label", "Y-axis label:", value = "Density"),
                   downloadButton("download_plot", "Download Plot (PNG)")
                 ),
                 hr(),
                 h4(uiOutput("metrics_header")),
                 tableOutput("stats_table")
               ),

               mainPanel(
                 plotOutput("roc_plot", click = "roc_click", height = "400px"),
                 hr(),
                 plotOutput("dist_plot", height = "350px"),
                 hr(),
                 h4(uiOutput("matrix_title")),
                 uiOutput("colored_conf_matrix")
               )
             )
    ),

    # ========== TAB 2: Simulation ==========
    tabPanel(uiOutput("tab2_title"),
             sidebarLayout(
               sidebarPanel(
                 # SDT simulation controls
                 conditionalPanel(
                   condition = "input.analysis_mode == 'sdt'",
                   h4("SDT Simulation Parameters"),
                   p("Using d' =", textOutput("sim_d_display", inline = TRUE),
                     ", c =", textOutput("sim_c_display", inline = TRUE)),
                   sliderInput("n_total", "Total Number of Trials:",
                               min = 10, max = 1000, value = 200, step = 10),
                   sliderInput("prop_signal", "Proportion Signal Trials:",
                               min = 0, max = 1, value = 0.5, step = 0.01),
                   checkboxInput("randomize_order", "Randomize Trial Order", FALSE),
                   sliderInput("speed", "Animation Speed (obs/frame):",
                               min = 1, max = 100, value = 10, step = 1),
                   checkboxInput("separate_dists_sim", "Separate by True State", FALSE)
                 ),

                 # HT simulation controls
                 conditionalPanel(
                   condition = "input.analysis_mode == 'ht'",
                   h4("Hypothesis Test Simulation"),
                   p("Using parameters from Test Parameters tab:"),
                   p(strong("Effect size (d):"), textOutput("sim_effect_display", inline = TRUE)),
                   p(strong("Sample size (n):"), textOutput("sim_n_display", inline = TRUE)),
                   p(strong("Critical value (z):"), textOutput("sim_zcrit_display", inline = TRUE)),
                   p(strong("Alpha:"), textOutput("sim_alpha_display", inline = TRUE)),
                   p(strong("Power:"), textOutput("sim_power_display", inline = TRUE)),
                   hr(),
                   h4("The True State of Reality"),
                   fluidRow(
                     column(6, actionButton("snap_h0", "Snap to H0", class = "btn-info btn-sm snap-btn")),
                     column(6, actionButton("snap_h1", "Snap to H1", class = "btn-warning btn-sm snap-btn"))
                   ),
                   sliderInput("true_mean_sim", "True Population Mean:",
                               min = -1, max = 3, value = 0, step = 0.05),
                   hr(),
                   checkboxInput("show_severity_shading", "Show Severity Shading", FALSE),
                   sliderInput("speed_ht", "Animation Speed:",
                               min = 1, max = 50, value = 5, step = 1)
                 ),

                 actionButton("start_sim", "Run Simulation", class = "btn-primary"),
                 actionButton("reset_sim", "Reset", class = "btn-warning"),
                 hr(),

                 # SDT counts
                 conditionalPanel(
                   condition = "input.analysis_mode == 'sdt'",
                   h4("Current Counts"),
                   tableOutput("sim_counts")
                 ),

                 # HT results summary
                 conditionalPanel(
                   condition = "input.analysis_mode == 'ht'",
                   h4("Sample Statistics"),
                   tableOutput("ht_sample_stats")
                 )
               ),

               mainPanel(
                 # SDT simulation plot
                 conditionalPanel(
                   condition = "input.analysis_mode == 'sdt'",
                   plotOutput("sim_hist", height = "400px"),
                   hr(),
                   h4("Simulated Confusion Matrix"),
                   uiOutput("sim_conf_matrix"),
                   hr(),
                   h4("Severity Analysis (Mayo)"),
                   uiOutput("sdt_severity")
                 ),

                 # HT simulation plot
                 conditionalPanel(
                   condition = "input.analysis_mode == 'ht'",
                   plotOutput("ht_sim_plot", height = "400px"),
                   hr(),
                   h4("Test Decision"),
                   uiOutput("ht_decision_box"),
                   hr(),
                   h4("Interpretation"),
                   uiOutput("ht_interpretation"),
                   hr(),
                   h4("Severity Analysis (Mayo)"),
                   uiOutput("ht_severity")
                 ),

                 hr(),
                 textOutput("sim_progress")
               )
             )
    ),

    # ========== TAB 3: Comparison ==========
    tabPanel("Comparison: SDT vs NP",
             fluidRow(
               column(12,
                      h3("Signal Detection Theory vs Neyman-Pearson Hypothesis Testing"),
                      p("These two frameworks are mathematically equivalent but arise from different traditions and use different terminology."),
                      hr(),

                      h4("Interactive Comparison"),
                      p("Adjust the parameters below to see how the same situation is described in both frameworks:"),
                      fluidRow(
                        column(4,
                               sliderInput("comp_effect", "Effect Size / d':", min = 0, max = 3, value = 1, step = 0.1),
                               sliderInput("comp_criterion", "Criterion / Critical Value:", min = -1, max = 3, value = 1, step = 0.1),
                               checkboxInput("comp_show_severity", "Show Severity Regions", FALSE)
                        ),
                        column(8,
                               plotOutput("comparison_plot", height = "350px")
                        )
                      ),
                      uiOutput("comparison_metrics"),
                      hr(),

                      h4("Conceptual Mapping"),
                      uiOutput("comparison_table"),
                      hr(),

                      h4("Key Insight: The ROC Curve"),
                      p("The ROC curve is central to both frameworks:"),
                      tags$ul(
                        tags$li(strong("In SDT:"), " The ROC curve shows all possible combinations of Hit Rate vs False Alarm Rate as the criterion varies."),
                        tags$li(strong("In Hypothesis Testing:"), " The same curve shows Power vs Alpha (Type I Error Rate) - this is called the ", em("power function"), ".")
                      ),
                      p("Both are plotting the same mathematical relationship - the tradeoff between detecting true signals/effects and making false alarms/Type I errors."),
                      hr(),

                      h4("The Likelihood Ratio Connection"),
                      p("The likelihood ratio (LR) at any point on the ROC curve equals the ", strong("slope of the tangent"), " to the curve at that point."),
                      p("This has profound implications:"),
                      tags$ul(
                        tags$li("LR = 1 corresponds to the diagonal line (chance performance / no discrimination)"),
                        tags$li("Higher LR means steeper slope = better discrimination at that operating point"),
                        tags$li("The optimal criterion (for equal costs) is where LR = 1, which is where d'/2 falls")
                      ),
                      hr(),

                      h4("Mayo's Severity: Beyond Binary Decisions"),
                      p("Deborah Mayo's ", strong("severity"), " concept (from ", em("Error and the Growth of Experimental Knowledge"), ", 1996)
                        addresses a key limitation of both SDT and NP frameworks: they focus on binary decisions,
                        but the ", em("strength"), " of evidence matters."),

                      p(strong("Severity asks:"), " Given the specific data we observed, how severely did we test the hypothesis?"),

                      div(style = "background-color: #e8f5e9; padding: 15px; border-radius: 5px; margin: 15px 0;",
                          h5("Severity for Passing (Failing to Reject H0)"),
                          p("SEV(H0 passes) = P(we would have rejected H0 | H0 is false by at least as much as some alternative)"),
                          p("A test result that ", em("barely"), " passes (just below the criterion) provides ",
                            strong("low severity"), " for concluding H0."),
                          p("A result far below the criterion provides ", strong("high severity"), " for H0.")
                      ),

                      div(style = "background-color: #fff3e0; padding: 15px; border-radius: 5px; margin: 15px 0;",
                          h5("Severity for Rejecting H0"),
                          p("SEV(reject H0) = P(we would have gotten a less extreme result | H0 is true)"),
                          p("This is simply 1 - p-value when we reject!"),
                          p("A result that ", em("barely"), " exceeds the critical value provides ",
                            strong("low severity"), " for the alternative."),
                          p("A result far above the critical value provides ", strong("high severity"), ".")
                      ),

                      h5("The Severity Mapping"),
                      HTML('
                        <table class="comparison-table">
                          <tr>
                            <th>Concept</th>
                            <th class="sdt-col">SDT</th>
                            <th class="ht-col">Hypothesis Testing</th>
                            <th>Severity</th>
                          </tr>
                          <tr>
                            <td><strong>Observed Data</strong></td>
                            <td class="sdt-col">Evidence value (x)</td>
                            <td class="ht-col">Sample mean (x&#772;) or z-statistic</td>
                            <td>The specific result that determines severity</td>
                          </tr>
                          <tr>
                            <td><strong>Distance from Threshold</strong></td>
                            <td class="sdt-col">|x - c| (how far from criterion)</td>
                            <td class="ht-col">|z - z_crit| (how far from critical value)</td>
                            <td>Larger distance = higher severity</td>
                          </tr>
                          <tr>
                            <td><strong>Barely Passed/Failed</strong></td>
                            <td class="sdt-col">x &#8776; c</td>
                            <td class="ht-col">z &#8776; z_crit, p &#8776; &#945;</td>
                            <td>Low severity for either conclusion</td>
                          </tr>
                        </table>
                      '),

                      p(style = "margin-top: 15px;",
                        em("Try the simulation tabs to see severity calculated for specific results!"))
               )
             )
    )
  )
)

server <- function(input, output, session) {

  # ========== REACTIVE VALUES ==========
  # For SDT mode
  sdt_vals <- reactiveValues(d = 1.5, c = 1, s = 1)

  # For HT mode - computed separately
  ht_vals <- reactiveValues(
    mu_0 = 0,
    mu_1 = 0.5,
    n = 25,
    z_crit = 1.645,
    effect_size = 0.5,
    d_standardized = 2.5,  # d * sqrt(n)
    se = 0.2,
    alpha = 0.05,
    power = 0.8,
    beta = 0.2
  )

  # SDT simulation state
  sim <- reactiveValues(
    observations = numeric(0),
    labels = character(0),
    running = FALSE,
    current_idx = 0
  )

  # HT simulation state
  ht_sim <- reactiveValues(
    sample_data = numeric(0),
    sample_mean = NA,
    z_statistic = NA,
    p_value = NA,
    decision = NA,
    true_mean = NA,
    sigma = NA,
    se = NA,
    z_crit = NA,
    n = NA,
    effect_size = NA,
    running = FALSE,
    current_idx = 0,
    completed = FALSE
  )

  # ========== DYNAMIC UI ELEMENTS ==========
  output$main_title <- renderUI({
    if(input$analysis_mode == "sdt") {
      "Signal Detection Theory: Advanced Explorer"
    } else {
      "Neyman-Pearson Hypothesis Testing Explorer"
    }
  })

  output$tab1_title <- renderUI({
    if(input$analysis_mode == "sdt") "SDT Parameters" else "Test Parameters"
  })

  output$tab2_title <- renderUI({
    if(input$analysis_mode == "sdt") "Simulate Observations" else "Simulate Hypothesis Test"
  })

  output$metrics_header <- renderUI({
    if(input$analysis_mode == "sdt") "Metrics" else "Test Characteristics"
  })

  output$matrix_title <- renderUI({
    if(input$analysis_mode == "sdt") {
      "Magnitude-Aware Confusion Matrix"
    } else {
      "Decision Outcome Probabilities"
    }
  })

  # ========== HT PARAMETER UPDATES ==========
  observe({
    req(input$analysis_mode == "ht")

    mu_1 <- input$mu_1_std
    n <- input$sample_size_ht
    se <- 1 / sqrt(n)  # sigma = 1 in standardized units
    effect_size <- mu_1  # d = mu_1 - 0 = mu_1 when sigma = 1
    d_standardized <- effect_size * sqrt(n)

    # Critical value
    if(input$constraint_mode_ht == "alpha") {
      z_crit <- qnorm(1 - as.numeric(input$target_alpha))
    } else if(input$constraint_mode_ht == "power") {
      z_crit <- qnorm(1 - input$target_power) + d_standardized
      z_crit <- max(0, z_crit)
    } else {
      z_crit <- input$critical_value
    }

    alpha <- 1 - pnorm(z_crit)
    power <- 1 - pnorm(z_crit - d_standardized)

    ht_vals$mu_0 <- 0
    ht_vals$mu_1 <- mu_1
    ht_vals$n <- n
    ht_vals$se <- se
    ht_vals$effect_size <- effect_size
    ht_vals$d_standardized <- d_standardized
    ht_vals$z_crit <- z_crit
    ht_vals$alpha <- alpha
    ht_vals$power <- power
    ht_vals$beta <- 1 - power

    updateSliderInput(session, "crit_disabled_ht", value = round(z_crit, 3))
  })

  # Snap buttons for Reality
  observeEvent(input$snap_h0, {
    updateSliderInput(session, "true_mean_sim", value = 0)
  })

  observeEvent(input$snap_h1, {
    updateSliderInput(session, "true_mean_sim", value = ht_vals$mu_1)
  })

  # ========== HT PRESETS ==========
  observeEvent(input$preset_ht, {
    if(input$preset_ht == "drug_trial") {
      updateSliderInput(session, "mu_1_std", value = 0.8)
      updateSliderInput(session, "sample_size_ht", value = 25)
      updateRadioButtons(session, "constraint_mode_ht", selected = "alpha")
      updateSelectInput(session, "target_alpha", selected = 0.05)
    } else if(input$preset_ht == "ab_test") {
      updateSliderInput(session, "mu_1_std", value = 0.3)
      updateSliderInput(session, "sample_size_ht", value = 100)
      updateRadioButtons(session, "constraint_mode_ht", selected = "alpha")
      updateSelectInput(session, "target_alpha", selected = 0.05)
    } else if(input$preset_ht == "psych") {
      updateSliderInput(session, "mu_1_std", value = 0.5)
      updateSliderInput(session, "sample_size_ht", value = 30)
      updateRadioButtons(session, "constraint_mode_ht", selected = "alpha")
      updateSelectInput(session, "target_alpha", selected = 0.05)
    }
  })

  # ========== SDT LOGIC ==========
  observeEvent(input$preset, {
    if(input$preset == "finley") {
      hit_rate <- 28/51
      fa_rate <- 72/2752

      hr_adj <- pmin(pmax(hit_rate, 0.001), 0.999)
      fa_adj <- pmin(pmax(fa_rate, 0.001), 0.999)

      c_est <- -qnorm(fa_adj)
      d_est <- qnorm(hr_adj) + qnorm(1 - fa_adj)

      sdt_vals$d <- d_est
      sdt_vals$c <- c_est
      sdt_vals$s <- 1.0

      updateSliderInput(session, "d_prime", value = round(d_est, 3))
      updateSliderInput(session, "crit", value = round(c_est, 3))
      updateSliderInput(session, "crit_disabled", value = round(c_est, 3))
      updateSliderInput(session, "sigma_s", value = 1.0)
      updateRadioButtons(session, "constraint_mode", selected = "none")
    }
  })

  # Handle SDT constraint modes
  observe({
    req(input$analysis_mode == "sdt")

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

      sdt_vals$c <- new_c
      updateSliderInput(session, "crit_disabled", value = round(new_c, 3))
    } else if(input$constraint_mode == "hitrate") {
      target_hr <- input$target_hitrate
      d <- input$d_prime
      s <- input$sigma_s

      new_c <- d - s * qnorm(1 - target_hr)

      sdt_vals$c <- new_c
      updateSliderInput(session, "crit_disabled", value = round(new_c, 3))
    } else {
      sdt_vals$c <- input$crit
      updateSliderInput(session, "crit_disabled", value = input$crit)
    }

    sdt_vals$d <- input$d_prime
    sdt_vals$s <- input$sigma_s
  })

  observeEvent(input$roc_click, {
    fp_clicked <- pmin(pmax(input$roc_click$x, 0.001), 0.999)
    tp_clicked <- pmin(pmax(input$roc_click$y, 0.001), 0.999)

    if(input$analysis_mode == "sdt") {
      new_c <- -qnorm(fp_clicked)
      new_d <- new_c - (qnorm(1 - tp_clicked) * input$sigma_s)

      sdt_vals$c <- new_c
      sdt_vals$d <- new_d
      sdt_vals$s <- input$sigma_s

      updateSliderInput(session, "d_prime", value = round(new_d, 3))
      updateSliderInput(session, "crit", value = round(new_c, 3))
      updateSliderInput(session, "crit_disabled", value = round(new_c, 3))
      updateSelectInput(session, "preset", selected = "custom")
      updateRadioButtons(session, "constraint_mode", selected = "none")
    } else {
      new_z_crit <- qnorm(1 - fp_clicked)
      new_d_std <- new_z_crit - qnorm(1 - tp_clicked)
      new_effect_size <- new_d_std / sqrt(ht_vals$n)

      updateSliderInput(session, "mu_1_std", value = round(max(0, new_effect_size), 3))
      updateSliderInput(session, "critical_value", value = round(max(0, new_z_crit), 3))
      updateSelectInput(session, "preset_ht", selected = "custom")
      updateRadioButtons(session, "constraint_mode_ht", selected = "none")
    }
  })

  # ========== METRICS ==========
  sdt_metrics <- reactive({
    fp <- 1 - pnorm(sdt_vals$c)
    tp <- 1 - pnorm((sdt_vals$c - sdt_vals$d) / sdt_vals$s)
    lr <- dnorm(sdt_vals$c, sdt_vals$d, sdt_vals$s) / dnorm(sdt_vals$c, 0, 1)
    lr_plus <- tp / fp
    miss_rate <- 1 - tp
    cr_rate <- 1 - fp
    lr_minus <- miss_rate / cr_rate
    list(fp = fp, tp = tp, pss = tp - fp, lr = lr, lr_plus = lr_plus, lr_minus = lr_minus)
  })

  ht_metrics <- reactive({
    list(
      fp = ht_vals$alpha,
      tp = ht_vals$power,
      pss = ht_vals$power - ht_vals$alpha
    )
  })

  # ========== ROC / POWER PLOT ==========
  output$roc_plot <- renderPlot({
    if(input$analysis_mode == "sdt") {
      d <- sdt_vals$d
      s <- sdt_vals$s
      m <- sdt_metrics()

      x_vals <- seq(0.001, 0.999, length.out = 100)
      y_vals <- 1 - pnorm(((-qnorm(x_vals)) - d) / s)

      p <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
        geom_line(color = "steelblue", linewidth = 1.2) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
        annotate("point", x = m$fp, y = m$tp, color = "red", size = 5)

      if(input$show_pss) {
        p <- p + geom_segment(aes(x = m$fp, xend = m$fp, y = m$fp, yend = m$tp),
                              color = "purple", linewidth = 1)
      }

      if(input$show_beta && input$show_beta_slope) {
        slope <- m$lr
        intercept <- m$tp - slope * m$fp
        p <- p +
          geom_abline(slope = slope, intercept = intercept,
                      color = "darkgreen", linewidth = 1, linetype = "solid") +
          annotate("text", x = 0.7, y = 0.2,
                   label = paste("Slope = LR =", round(m$lr, 2)),
                   color = "darkgreen", fontface = "bold", size = 4)
      }

      p <- p +
        labs(title = "ROC Space", x = "P(False Alarm)", y = "P(Hit)") +
        theme_minimal() + coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1))

    } else {
      d_std <- ht_vals$d_standardized
      m <- ht_metrics()

      x_vals <- seq(0.001, 0.999, length.out = 100)
      y_vals <- 1 - pnorm(qnorm(1 - x_vals) - d_std)

      p <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
        geom_line(color = "steelblue", linewidth = 1.2) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
        annotate("point", x = m$fp, y = m$tp, color = "red", size = 5)

      if(input$show_power_line) {
        p <- p + geom_segment(aes(x = m$fp, xend = m$fp, y = m$fp, yend = m$tp),
                              color = "purple", linewidth = 1)
      }

      p <- p +
        labs(title = "Power Function",
             x = expression(alpha ~ "(Type I Error Rate)"),
             y = expression("Power (1 - " * beta * ")")) +
        theme_minimal() + coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1))
    }
    p
  })

  # ========== DISTRIBUTION PLOT ==========
  output$dist_plot <- renderPlot({
    if(input$analysis_mode == "sdt") {
      d <- sdt_vals$d
      s <- sdt_vals$s
      c_val <- sdt_vals$c
      m <- sdt_metrics()

      x <- seq(-4, 8, length.out = 400)
      df <- data.frame(x = x, noise = dnorm(x, 0, 1), signal = dnorm(x, d, s))

      fill_labels <- c("Hit", "Miss", "False Alarm", "Correct Rejection")
      separate_dists <- input$separate_dists
      show_annotation <- input$show_beta
      x_lab <- if(input$bw_mode) input$x_label else "Evidence (x)"

    } else {
      d_std <- ht_vals$d_standardized
      z_crit <- ht_vals$z_crit

      x <- seq(-4, max(8, d_std + 4), length.out = 400)
      df <- data.frame(x = x, noise = dnorm(x, 0, 1), signal = dnorm(x, d_std, 1))
      c_val <- z_crit

      fill_labels <- c("Power (1-\u03b2)", "Type II Error (\u03b2)", "Type I Error (\u03b1)", "Correct Retention (1-\u03b1)")
      separate_dists <- input$separate_dists_ht
      show_annotation <- input$show_effect_size_ht
      x_lab <- if(input$bw_mode) input$x_label else "Z-statistic"
    }

    y_lab <- if(input$bw_mode) input$y_label else "Density"

    fill_vals_named <- c("#2ca02c", "#d62728", "#ff7f0e", "#1f77b4")
    if(input$bw_mode) fill_vals_named <- c("white", "gray40", "gray70", "white")
    names(fill_vals_named) <- fill_labels

    line_col <- if(input$bw_mode) "black" else "steelblue"
    alpha_val <- if(input$bw_mode) 0.3 else 0.5

    if(separate_dists) {
      dist1_label <- if(input$analysis_mode == "sdt") "Signal PRESENT" else "H1 True"
      dist2_label <- if(input$analysis_mode == "sdt") "Signal ABSENT" else "H0 True"
      plot_title <- if(input$analysis_mode == "sdt") {
        "Evidence Distributions (Separated by True State)"
      } else {
        "Sampling Distributions (Separated by Hypothesis)"
      }

      p <- ggplot(df, aes(x = x)) +
        geom_ribbon(data = subset(df, x > c_val), aes(ymin = 0, ymax = signal, fill = fill_labels[1]), alpha = alpha_val) +
        geom_ribbon(data = subset(df, x <= c_val), aes(ymin = 0, ymax = signal, fill = fill_labels[2]), alpha = alpha_val) +
        geom_line(aes(y = signal), color = line_col, linewidth = 1) +
        geom_ribbon(data = subset(df, x > c_val), aes(ymin = -0.45, ymax = -0.45 + noise, fill = fill_labels[3]), alpha = alpha_val) +
        geom_ribbon(data = subset(df, x <= c_val), aes(ymin = -0.45, ymax = -0.45 + noise, fill = fill_labels[4]), alpha = alpha_val) +
        geom_line(aes(y = -0.45 + noise), linetype = "dashed", color = line_col, linewidth = 1) +
        geom_vline(xintercept = c_val, color = "black", linewidth = 1) +
        geom_hline(yintercept = -0.45, color = "gray50", linetype = "dotted") +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
        scale_fill_manual(values = fill_vals_named) +
        theme_minimal() +
        labs(title = plot_title, x = x_lab, y = y_lab, fill = "Outcome") +
        annotate("text", x = min(x) + 1, y = 0.35, label = dist1_label, fontface = "bold", hjust = 0, size = 3.5) +
        annotate("text", x = min(x) + 1, y = -0.1, label = dist2_label, fontface = "bold", hjust = 0, size = 3.5) +
        expand_limits(y = c(-0.45, 0))
    } else {
      plot_title <- if(input$analysis_mode == "sdt") {
        "Evidence Distributions"
      } else {
        "Sampling Distributions of Test Statistic"
      }

      p <- ggplot(df, aes(x = x)) +
        geom_ribbon(data = subset(df, x > c_val), aes(ymin = 0, ymax = signal, fill = fill_labels[1]), alpha = alpha_val) +
        geom_ribbon(data = subset(df, x <= c_val), aes(ymin = 0, ymax = signal, fill = fill_labels[2]), alpha = alpha_val) +
        geom_ribbon(data = subset(df, x > c_val), aes(ymin = 0, ymax = noise, fill = fill_labels[3]), alpha = alpha_val) +
        geom_ribbon(data = subset(df, x <= c_val), aes(ymin = 0, ymax = noise, fill = fill_labels[4]), alpha = alpha_val) +
        geom_line(aes(y = noise), linetype = "dashed", color = line_col, linewidth = 1) +
        geom_line(aes(y = signal), color = line_col, linewidth = 1) +
        geom_vline(xintercept = c_val, color = "black", linewidth = 1) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
        scale_fill_manual(values = fill_vals_named) +
        theme_minimal() +
        labs(title = plot_title, x = x_lab, y = y_lab, fill = "Outcome") +
        expand_limits(y = 0)
    }

    if(show_annotation && !separate_dists) {
      if(input$analysis_mode == "sdt") {
        m <- sdt_metrics()
        p <- p +
          annotate("point", x = c_val, y = dnorm(c_val, 0, 1), color = "black", fill = "white", shape = 21, size = 3) +
          annotate("point", x = c_val, y = dnorm(c_val, sdt_vals$d, sdt_vals$s), color = "black", fill = "white", shape = 21, size = 3) +
          annotate("text", x = c_val + 0.8, y = 0.38, label = paste("LR =", round(m$lr, 2)), fontface = "bold")
      } else {
        p <- p +
          annotate("text", x = c_val + 0.8, y = 0.38,
                   label = paste("d =", round(ht_vals$effect_size, 2)), fontface = "bold")
      }
    }
    p
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      prefix <- if(input$analysis_mode == "sdt") "sdt_plot_" else "ht_plot_"
      paste(prefix, Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6, dpi = 300)
    }
  )

  # ========== CONFUSION / DECISION MATRIX ==========
  output$colored_conf_matrix <- renderUI({
    if(input$analysis_mode == "sdt") {
      m <- sdt_metrics()
      col_header <- c("Response: YES", "Response: NO")
      row_labels <- c("Signal PRESENT", "Signal ABSENT")
      cell_labels <- c("Hit", "Miss", "False Alarm", "Correct Rejection")
    } else {
      m <- ht_metrics()
      col_header <- c("Reject H\u2080", "Fail to Reject H\u2080")
      row_labels <- c("H\u2081 True (Effect Exists)", "H\u2080 True (No Effect)")
      cell_labels <- c("Power (1-\u03b2)", "Type II Error (\u03b2)",
                       "Type I Error (\u03b1)", "Correct Retention")
    }

    tp <- m$tp; fn <- 1-m$tp; fp <- m$fp; tn <- 1-m$fp

    HTML(paste0('
      <table class="conf-table">
        <tr>
          <th class="label-cell"></th>
          <th class="label-cell">', col_header[1], '</th>
          <th class="label-cell">', col_header[2], '</th>
        </tr>
        <tr>
          <th class="label-cell">', row_labels[1], '</th>
          <td style="background-color: rgba(', col_hit, ',', tp, '); color: black;">', cell_labels[1], '<br>', round(tp, 3), '</td>
          <td style="background-color: rgba(', col_miss, ',', fn, '); color: black;">', cell_labels[2], '<br>', round(fn, 3), '</td>
        </tr>
        <tr>
          <th class="label-cell">', row_labels[2], '</th>
          <td style="background-color: rgba(', col_fa, ',', fp, '); color: black;">', cell_labels[3], '<br>', round(fp, 3), '</td>
          <td style="background-color: rgba(', col_cr, ',', tn, '); color: black;">', cell_labels[4], '<br>', round(tn, 3), '</td>
        </tr>
      </table>
    '))
  })

  # ========== STATS TABLE ==========
  output$stats_table <- renderTable({
    if(input$analysis_mode == "sdt") {
      m <- sdt_metrics()
      data.frame(
        Metric = c("d' (Sensitivity)", "c (Criterion)",
                   "LR (at criterion)", "LR+ (Positive LR)", "LR- (Negative LR)",
                   "PSS (Peirce Skill)"),
        Value = c(round(sdt_vals$d, 3), round(sdt_vals$c, 3),
                  round(m$lr, 3), round(m$lr_plus, 3), round(m$lr_minus, 3),
                  round(m$pss, 3))
      )
    } else {
      data.frame(
        Metric = c("Effect Size (Cohen's d)",
                   "Standardized Effect (d\u221An)",
                   "Critical Value (z)",
                   "Alpha (\u03b1)",
                   "Beta (\u03b2)",
                   "Power (1-\u03b2)",
                   "Standard Error"),
        Value = c(round(ht_vals$effect_size, 3),
                  round(ht_vals$d_standardized, 3),
                  round(ht_vals$z_crit, 3),
                  round(ht_vals$alpha, 4),
                  round(ht_vals$beta, 3),
                  round(ht_vals$power, 3),
                  round(ht_vals$se, 4))
      )
    }
  })

  # ========== COMPARISON TAB ==========
  output$comparison_table <- renderUI({
    HTML('
      <table class="comparison-table">
        <tr>
          <th>Concept</th>
          <th class="sdt-col">Signal Detection Theory</th>
          <th class="ht-col">Hypothesis Testing</th>
        </tr>
        <tr>
          <td><strong>The Two States</strong></td>
          <td class="sdt-col">Signal Present vs Signal Absent</td>
          <td class="ht-col">H1 True (Effect Exists) vs H0 True (No Effect)</td>
        </tr>
        <tr>
          <td><strong>Distributions</strong></td>
          <td class="sdt-col">Noise distribution & Signal distribution</td>
          <td class="ht-col">Null distribution & Alternative distribution</td>
        </tr>
        <tr>
          <td><strong>Decision Threshold</strong></td>
          <td class="sdt-col">Criterion (c)</td>
          <td class="ht-col">Critical value (z_crit or t_crit)</td>
        </tr>
        <tr>
          <td><strong>Sensitivity/Effect</strong></td>
          <td class="sdt-col">d\' (d-prime)</td>
          <td class="ht-col">Effect size (Cohen\'s d) or Standardized effect (d&#8730;n)</td>
        </tr>
        <tr>
          <td><strong>Correct Detection</strong></td>
          <td class="sdt-col">Hit (True Positive)</td>
          <td class="ht-col">Power (1 - &#946;) - Correctly rejecting H0</td>
        </tr>
        <tr>
          <td><strong>Missed Detection</strong></td>
          <td class="sdt-col">Miss (False Negative)</td>
          <td class="ht-col">Type II Error (&#946;) - Failing to reject false H0</td>
        </tr>
        <tr>
          <td><strong>False Detection</strong></td>
          <td class="sdt-col">False Alarm (False Positive)</td>
          <td class="ht-col">Type I Error (&#945;) - Rejecting true H0</td>
        </tr>
        <tr>
          <td><strong>Correct Rejection</strong></td>
          <td class="sdt-col">Correct Rejection (True Negative)</td>
          <td class="ht-col">Correct Retention (1 - &#945;)</td>
        </tr>
        <tr>
          <td><strong>The Tradeoff Curve</strong></td>
          <td class="sdt-col">ROC Curve (Hit Rate vs False Alarm Rate)</td>
          <td class="ht-col">Power Function (Power vs Alpha)</td>
        </tr>
        <tr>
          <td><strong>Evidence Strength</strong></td>
          <td class="sdt-col">Likelihood Ratio (LR)</td>
          <td class="ht-col">Likelihood Ratio (same math!)</td>
        </tr>
        <tr>
          <td><strong>Response Bias</strong></td>
          <td class="sdt-col">c (liberal/conservative responding)</td>
          <td class="ht-col">Choice of &#945; level (strict/lenient testing)</td>
        </tr>
      </table>
    ')
  })

  # Interactive comparison plot
  output$comparison_plot <- renderPlot({
    d <- input$comp_effect
    c_val <- input$comp_criterion

    x <- seq(-4, max(6, d + 3), length.out = 400)
    df <- data.frame(
      x = x,
      h0 = dnorm(x, 0, 1),
      h1 = dnorm(x, d, 1)
    )

    # Calculate metrics
    fp <- 1 - pnorm(c_val)
    tp <- 1 - pnorm(c_val - d)
    lr <- dnorm(c_val, d, 1) / dnorm(c_val, 0, 1)

    p <- ggplot(df, aes(x = x)) +
      geom_ribbon(data = subset(df, x > c_val), aes(ymin = 0, ymax = h1),
                  fill = "#2ca02c", alpha = 0.4) +
      geom_ribbon(data = subset(df, x <= c_val), aes(ymin = 0, ymax = h1),
                  fill = "#d62728", alpha = 0.4) +
      geom_ribbon(data = subset(df, x > c_val), aes(ymin = 0, ymax = h0),
                  fill = "#ff7f0e", alpha = 0.4) +
      geom_ribbon(data = subset(df, x <= c_val), aes(ymin = 0, ymax = h0),
                  fill = "#1f77b4", alpha = 0.4) +
      geom_line(aes(y = h0), linetype = "dashed", color = "steelblue", linewidth = 1) +
      geom_line(aes(y = h1), color = "steelblue", linewidth = 1) +
      geom_vline(xintercept = c_val, color = "black", linewidth = 1) +
      annotate("text", x = 0, y = max(df$h0) * 1.1, label = "H0 / Noise", fontface = "bold") +
      annotate("text", x = d, y = max(df$h1) * 1.1, label = "H1 / Signal", fontface = "bold") +
      annotate("text", x = c_val, y = -0.02, label = paste("c / z_crit =", round(c_val, 2)),
               vjust = 1, fontface = "bold") +
      theme_minimal() +
      labs(title = "Same Math, Different Names",
           subtitle = paste("d' / Effect =", round(d, 2), " | LR =", round(lr, 2)),
           x = "Evidence / Test Statistic",
           y = "Density")

    if(input$comp_show_severity) {
      # Add severity shading annotation
      p <- p +
        annotate("segment", x = c_val, xend = c_val + 1.5, y = 0.35, yend = 0.35,
                 arrow = arrow(length = unit(0.2, "cm")), color = "darkgreen") +
        annotate("text", x = c_val + 0.75, y = 0.37, label = "Higher severity\nfor rejection",
                 size = 3, color = "darkgreen") +
        annotate("segment", x = c_val, xend = c_val - 1.5, y = 0.35, yend = 0.35,
                 arrow = arrow(length = unit(0.2, "cm")), color = "purple") +
        annotate("text", x = c_val - 0.75, y = 0.37, label = "Higher severity\nfor H0",
                 size = 3, color = "purple")
    }
    p
  })

  output$comparison_metrics <- renderUI({
    d <- input$comp_effect
    c_val <- input$comp_criterion

    fp <- 1 - pnorm(c_val)
    tp <- 1 - pnorm(c_val - d)
    lr <- dnorm(c_val, d, 1) / dnorm(c_val, 0, 1)
    lr_plus <- tp / fp
    lr_minus <- (1 - tp) / (1 - fp)

    HTML(paste0('
      <div style="display: flex; justify-content: space-around; margin-top: 15px;">
        <div class="sdt-col" style="padding: 15px; border-radius: 5px; width: 45%;">
          <h5>SDT View</h5>
          <p><strong>d\' = ', round(d, 3), '</strong></p>
          <p>Hit Rate = ', round(tp, 3), '</p>
          <p>FA Rate = ', round(fp, 3), '</p>
          <p>LR = ', round(lr, 3), '</p>
          <p>LR+ = ', round(lr_plus, 3), '</p>
          <p>LR- = ', round(lr_minus, 3), '</p>
        </div>
        <div class="ht-col" style="padding: 15px; border-radius: 5px; width: 45%;">
          <h5>Hypothesis Testing View</h5>
          <p><strong>Effect size = ', round(d, 3), '</strong></p>
          <p>Power = ', round(tp, 3), '</p>
          <p>Alpha = ', round(fp, 3), '</p>
          <p>Beta = ', round(1-tp, 3), '</p>
          <p>z_crit = ', round(c_val, 3), '</p>
        </div>
      </div>
    '))
  })

  # ========== SIMULATION TAB: DISPLAYS ==========
  output$sim_d_display <- renderText({ round(sdt_vals$d, 2) })
  output$sim_c_display <- renderText({ round(sdt_vals$c, 2) })

  output$sim_effect_display <- renderText({ round(ht_vals$effect_size, 3) })
  output$sim_n_display <- renderText({ ht_vals$n })
  output$sim_zcrit_display <- renderText({ round(ht_vals$z_crit, 3) })
  output$sim_alpha_display <- renderText({ round(ht_vals$alpha, 4) })
  output$sim_power_display <- renderText({ round(ht_vals$power, 3) })

  # ========== START SIMULATION ==========
  observeEvent(input$start_sim, {
    if(input$analysis_mode == "sdt") {
      n_total <- input$n_total
      n_sig <- round(n_total * input$prop_signal)
      n_noi <- n_total - n_sig

      signal_obs <- rnorm(n_sig, mean = sdt_vals$d, sd = sdt_vals$s)
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

    } else {
      # HT simulation - use parameters from Test Parameters tab
      n <- ht_vals$n
      effect_size <- ht_vals$effect_size
      z_crit <- ht_vals$z_crit
      true_mean <- input$true_mean_sim
      sigma <- 1  # Standardized
      se <- sigma / sqrt(n)

      # Store parameters
      ht_sim$true_mean <- true_mean
      ht_sim$sigma <- sigma
      ht_sim$se <- se
      ht_sim$z_crit <- z_crit
      ht_sim$n <- n
      ht_sim$effect_size <- effect_size
      ht_sim$completed <- FALSE

      # Generate sample
      sample_data <- rnorm(n, mean = true_mean, sd = sigma)

      ht_sim$sample_data <- sample_data
      ht_sim$current_idx <- 0
      ht_sim$running <- TRUE
      ht_sim$sample_mean <- NA
      ht_sim$z_statistic <- NA
      ht_sim$p_value <- NA
      ht_sim$decision <- NA

      observe({
        invalidateLater(100, session)
        isolate({
          if(ht_sim$running && ht_sim$current_idx < length(ht_sim$sample_data)) {
            increment <- min(input$speed_ht, length(ht_sim$sample_data) - ht_sim$current_idx)
            ht_sim$current_idx <- ht_sim$current_idx + increment

            if(ht_sim$current_idx >= length(ht_sim$sample_data)) {
              ht_sim$running <- FALSE
              ht_sim$completed <- TRUE

              current_data <- ht_sim$sample_data
              ht_sim$sample_mean <- mean(current_data)
              ht_sim$z_statistic <- (ht_sim$sample_mean - 0) / ht_sim$se
              ht_sim$p_value <- 1 - pnorm(ht_sim$z_statistic)
              ht_sim$decision <- ifelse(ht_sim$z_statistic > ht_sim$z_crit, "Reject H0", "Fail to Reject H0")
            }
          }
        })
      })
    }
  })

  # ========== RESET SIMULATION ==========
  observeEvent(input$reset_sim, {
    sim$observations <- numeric(0)
    sim$labels <- character(0)
    sim$running <- FALSE
    sim$current_idx <- 0

    ht_sim$sample_data <- numeric(0)
    ht_sim$sample_mean <- NA
    ht_sim$z_statistic <- NA
    ht_sim$p_value <- NA
    ht_sim$decision <- NA
    ht_sim$true_mean <- NA
    ht_sim$running <- FALSE
    ht_sim$current_idx <- 0
    ht_sim$completed <- FALSE
  })

  # ========== SDT SIMULATION OUTPUTS ==========
  output$sim_hist <- renderPlot({
    req(input$analysis_mode == "sdt")
    req(length(sim$observations) > 0, sim$current_idx > 0)

    current_obs <- sim$observations[1:sim$current_idx]
    current_labels <- sim$labels[1:sim$current_idx]

    df <- data.frame(value = current_obs, label = current_labels)
    df$outcome <- ifelse(df$label == "signal",
                         ifelse(df$value > sdt_vals$c, "Hit", "Miss"),
                         ifelse(df$value > sdt_vals$c, "False Alarm", "Correct Rejection"))

    x <- seq(-4, 8, length.out = 400)
    theory <- data.frame(x = x, noise = dnorm(x, 0, 1), signal = dnorm(x, sdt_vals$d, sdt_vals$s))

    if(input$separate_dists_sim) {
      df_signal <- subset(df, label == "signal")
      df_noise <- subset(df, label == "noise")

      ggplot() +
        geom_histogram(data = df_signal, aes(x = value, fill = outcome, y = after_stat(count/sum(count))),
                       bins = 50, alpha = 0.7, position = "identity") +
        geom_line(data = theory, aes(x = x, y = signal), linewidth = 1) +
        geom_histogram(data = df_noise, aes(x = value, fill = outcome, y = -after_stat(count/sum(count))),
                       bins = 50, alpha = 0.7, position = "identity") +
        geom_line(data = theory, aes(x = x, y = -noise), linetype = "dashed", linewidth = 1) +
        geom_vline(xintercept = sdt_vals$c, color = "black", linewidth = 1) +
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
        geom_line(data = theory, aes(x = x, y = noise), linetype = "dashed", linewidth = 1) +
        geom_line(data = theory, aes(x = x, y = signal), linewidth = 1) +
        geom_vline(xintercept = sdt_vals$c, color = "black", linewidth = 1) +
        scale_fill_manual(values = c("Hit"="#2ca02c","Miss"="#d62728",
                                     "False Alarm"="#ff7f0e","Correct Rejection"="#1f77b4")) +
        theme_minimal() +
        labs(title = paste("Simulated Observations (n =", sim$current_idx, ")"),
             x = "Evidence Value", y = "Density", fill = "Outcome")
    }
  })

  output$sim_conf_matrix <- renderUI({
    req(input$analysis_mode == "sdt")
    req(length(sim$observations) > 0, sim$current_idx > 0)

    current_obs <- sim$observations[1:sim$current_idx]
    current_labels <- sim$labels[1:sim$current_idx]

    hits <- sum(current_labels == "signal" & current_obs > sdt_vals$c)
    misses <- sum(current_labels == "signal" & current_obs <= sdt_vals$c)
    fas <- sum(current_labels == "noise" & current_obs > sdt_vals$c)
    crs <- sum(current_labels == "noise" & current_obs <= sdt_vals$c)

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
    req(input$analysis_mode == "sdt")
    req(length(sim$observations) > 0, sim$current_idx > 0)

    current_obs <- sim$observations[1:sim$current_idx]
    current_labels <- sim$labels[1:sim$current_idx]

    hits <- sum(current_labels == "signal" & current_obs > sdt_vals$c)
    misses <- sum(current_labels == "signal" & current_obs <= sdt_vals$c)
    fas <- sum(current_labels == "noise" & current_obs > sdt_vals$c)
    crs <- sum(current_labels == "noise" & current_obs <= sdt_vals$c)

    data.frame(
      Outcome = c("Hits", "Misses", "False Alarms", "Correct Rejections"),
      Count = c(hits, misses, fas, crs)
    )
  })

  # ========== SDT SEVERITY ANALYSIS ==========
  output$sdt_severity <- renderUI({
    req(input$analysis_mode == "sdt")
    req(length(sim$observations) > 0, sim$current_idx > 0)

    current_obs <- sim$observations[1:sim$current_idx]
    current_labels <- sim$labels[1:sim$current_idx]

    last_obs <- current_obs[sim$current_idx]
    last_label <- current_labels[sim$current_idx]
    last_response <- if(last_obs > sdt_vals$c) "YES" else "NO"

    d <- sdt_vals$d
    s <- sdt_vals$s
    c_val <- sdt_vals$c
    distance <- last_obs - c_val

    if(last_response == "YES") {
      sev <- pnorm(last_obs, mean = 0, sd = 1)
      lr_obs <- dnorm(last_obs, d, s) / dnorm(last_obs, 0, 1)

      if(sev >= 0.95) { sev_class <- "severity-high"; sev_label <- "High"
      } else if(sev >= 0.80) { sev_class <- "severity-medium"; sev_label <- "Moderate"
      } else { sev_class <- "severity-low"; sev_label <- "Low" }

      result_type <- if(last_label == "signal") "Hit" else "False Alarm"
      truth_text <- if(last_label == "signal") "Signal was actually PRESENT" else "Signal was actually ABSENT"

      div(class = paste("severity-box", sev_class),
          HTML(paste0(
            "<strong>Most Recent Trial:</strong><br>",
            "Evidence value: x = ", round(last_obs, 3), "<br>",
            "Response: ", last_response, " (", result_type, ")<br>",
            truth_text, "<br><br>",
            "<strong>Severity for claiming 'Signal Present':</strong> ", round(sev, 3), " (", sev_label, ")<br>",
            "<div class='severity-bar' style='background: linear-gradient(to right, #4caf50 ", round(sev*100), "%, #eee ", round(sev*100), "%);'></div>",
            "<p>LR at this observation: ", round(lr_obs, 2), "</p>",
            "<p><strong>Interpretation:</strong> ",
            round(sev * 100, 1), "% of noise-only observations would fall below this value. ",
            if(sev >= 0.95) "Strong evidence for signal." else if(sev >= 0.80) "Moderate evidence for signal." else "Weak evidence - could easily be noise.",
            "</p>"
          ))
      )
    } else {
      sev <- 1 - pnorm(last_obs, mean = d, sd = s)
      lr_obs <- dnorm(last_obs, d, s) / dnorm(last_obs, 0, 1)

      if(sev >= 0.95) { sev_class <- "severity-high"; sev_label <- "High"
      } else if(sev >= 0.80) { sev_class <- "severity-medium"; sev_label <- "Moderate"
      } else { sev_class <- "severity-low"; sev_label <- "Low" }

      result_type <- if(last_label == "noise") "Correct Rejection" else "Miss"
      truth_text <- if(last_label == "noise") "Signal was actually ABSENT" else "Signal was actually PRESENT"

      div(class = paste("severity-box", sev_class),
          HTML(paste0(
            "<strong>Most Recent Trial:</strong><br>",
            "Evidence value: x = ", round(last_obs, 3), "<br>",
            "Response: ", last_response, " (", result_type, ")<br>",
            truth_text, "<br><br>",
            "<strong>Severity for claiming 'Signal Absent':</strong> ", round(sev, 3), " (", sev_label, ")<br>",
            "<div class='severity-bar' style='background: linear-gradient(to right, #4caf50 ", round(sev*100), "%, #eee ", round(sev*100), "%);'></div>",
            "<p>LR at this observation: ", round(lr_obs, 2), "</p>",
            "<p><strong>Interpretation:</strong> ",
            round(sev * 100, 1), "% of signal observations would produce higher evidence. ",
            if(sev >= 0.95) "Severely tests signal - strong evidence against." else if(sev >= 0.80) "Moderate test of signal." else "Weak test - signal might still exist.",
            "</p>"
          ))
      )
    }
  })

  # ========== HT SIMULATION OUTPUTS ==========
  output$ht_sim_plot <- renderPlot({
    req(input$analysis_mode == "ht")

    # Get current parameters
    z_crit <- ht_vals$z_crit
    d_std <- ht_vals$d_standardized
    true_mean <- input$true_mean_sim
    n <- ht_vals$n
    se <- 1 / sqrt(n)

    # Always show the preview with H0, H1, and Reality distributions
    if(length(ht_sim$sample_data) == 0 || ht_sim$current_idx == 0) {
      # Preview mode - show theoretical distributions
      x_crit <- z_crit * se

      x_min <- min(-3 * se, true_mean - 3 * se)
      x_max <- max(3 * se, ht_vals$mu_1 + 3 * se, true_mean + 3 * se)
      x <- seq(x_min, x_max, length.out = 400)

      df <- data.frame(
        x = x,
        h0 = dnorm(x, 0, se),
        h1 = dnorm(x, ht_vals$mu_1, se),
        reality = dnorm(x, true_mean, se)
      )

      p <- ggplot(df, aes(x = x)) +
        geom_line(aes(y = h0), linetype = "dashed", color = "steelblue", linewidth = 1) +
        geom_line(aes(y = h1), linetype = "dotted", color = "orange", linewidth = 1) +
        geom_line(aes(y = reality), color = "darkgreen", linewidth = 1.2) +
        geom_vline(xintercept = x_crit, color = "black", linewidth = 1, linetype = "dashed") +
        annotate("text", x = 0, y = max(df$h0) * 1.05, label = "H0", color = "steelblue", fontface = "bold") +
        annotate("text", x = ht_vals$mu_1, y = max(df$h1) * 1.05, label = "H1", color = "orange", fontface = "bold") +
        annotate("text", x = true_mean, y = max(df$reality) * 1.15, label = "REALITY", color = "darkgreen", fontface = "bold") +
        annotate("text", x = x_crit, y = -max(df$h0) * 0.1, label = paste("z_crit =", round(z_crit, 2)),
                 hjust = 0.5, size = 3) +
        theme_minimal() +
        labs(title = "Preview: Sampling Distributions",
             subtitle = paste("Reality:", round(true_mean, 2), "| H1:", round(ht_vals$mu_1, 2),
                              "| Click 'Run Simulation' to sample"),
             x = "Sample Mean", y = "Density")

      # Add severity shading if requested
      if(input$show_severity_shading) {
        p <- p +
          geom_ribbon(data = subset(df, x > x_crit), aes(ymin = 0, ymax = h0),
                      fill = "#ff7f0e", alpha = 0.2) +
          geom_ribbon(data = subset(df, x > x_crit), aes(ymin = 0, ymax = reality),
                      fill = "#2ca02c", alpha = 0.2)
      }
      p

    } else {
      # Simulation running or complete
      current_data <- ht_sim$sample_data[1:ht_sim$current_idx]
      current_mean <- mean(current_data)
      se <- ht_sim$se

      # Rescale to focus on the distributions after completion
      if(ht_sim$completed) {
        # Tighter bounds after completion
        x_min <- min(-2.5 * se, current_mean - 2 * se, 0 - 3 * se)
        x_max <- max(2.5 * se + max(ht_sim$true_mean, ht_vals$mu_1), current_mean + 2 * se)
      } else {
        x_min <- min(-3 * se, min(current_data) - se)
        x_max <- max(3 * se + max(ht_sim$true_mean, ht_vals$mu_1), max(current_data) + se)
      }

      x <- seq(x_min, x_max, length.out = 400)
      x_crit <- ht_sim$z_crit * se

      df <- data.frame(
        x = x,
        h0 = dnorm(x, 0, se),
        h1 = dnorm(x, ht_vals$mu_1, se),
        reality = dnorm(x, ht_sim$true_mean, se)
      )

      z_current <- current_mean / se

      p <- ggplot() +
        geom_line(data = df, aes(x = x, y = h0), linetype = "dashed", color = "steelblue", linewidth = 1) +
        geom_line(data = df, aes(x = x, y = h1), linetype = "dotted", color = "orange", linewidth = 1) +
        geom_line(data = df, aes(x = x, y = reality), color = "darkgreen", linewidth = 1.2)

      # Add severity shading if requested
      if(input$show_severity_shading) {
        p <- p +
          geom_ribbon(data = subset(df, x > x_crit), aes(x = x, ymin = 0, ymax = h0),
                      fill = "#ff7f0e", alpha = 0.3) +
          geom_ribbon(data = subset(df, x > x_crit), aes(x = x, ymin = 0, ymax = reality),
                      fill = "#2ca02c", alpha = 0.3) +
          geom_ribbon(data = subset(df, x <= x_crit), aes(x = x, ymin = 0, ymax = reality),
                      fill = "#d62728", alpha = 0.3)
      }

      p <- p +
        geom_vline(xintercept = x_crit, color = "black", linewidth = 1, linetype = "dashed") +
        geom_vline(xintercept = current_mean, color = "red", linewidth = 1.5) +
        geom_point(data = data.frame(x = current_data),
                   aes(x = x, y = -max(df$h0, df$reality) * 0.08),
                   alpha = 0.4, size = 1.5) +
        annotate("text", x = 0, y = max(df$h0) * 1.05, label = "H0", color = "steelblue", fontface = "bold", size = 3) +
        annotate("text", x = ht_vals$mu_1, y = max(df$h1) * 1.05, label = "H1", color = "orange", fontface = "bold", size = 3) +
        annotate("text", x = ht_sim$true_mean, y = max(df$reality) * 1.15, label = "REALITY",
                 color = "darkgreen", fontface = "bold", size = 3) +
        annotate("text", x = x_crit, y = max(df$h0) * 1.1,
                 label = paste("Critical\nz =", round(ht_sim$z_crit, 2)), hjust = 0.5, size = 2.5) +
        annotate("text", x = current_mean, y = max(df$h0) * 0.9,
                 label = paste("x\u0305 =", round(current_mean, 3), "\nz =", round(z_current, 2)),
                 hjust = ifelse(current_mean < x_crit, 1.1, -0.1), color = "red", size = 3) +
        theme_minimal() +
        labs(title = paste("Hypothesis Test (n =", ht_sim$current_idx, "of", length(ht_sim$sample_data), ")"),
             subtitle = paste("z =", round(z_current, 3), "| Reality =", round(ht_sim$true_mean, 2)),
             x = "Sample Mean", y = "Density")
      p
    }
  })

  output$ht_sample_stats <- renderTable({
    req(input$analysis_mode == "ht")
    req(length(ht_sim$sample_data) > 0, ht_sim$current_idx > 0)

    current_data <- ht_sim$sample_data[1:ht_sim$current_idx]
    current_mean <- mean(current_data)
    current_sd <- sd(current_data)
    se <- ht_sim$se
    z_stat <- current_mean / se
    p_val <- 1 - pnorm(z_stat)

    data.frame(
      Statistic = c("Sample Size (n)", "Sample Mean", "Sample SD", "Standard Error",
                    "Z-statistic", "P-value (one-tailed)"),
      Value = c(ht_sim$current_idx, round(current_mean, 4), round(current_sd, 4),
                round(se, 4), round(z_stat, 4), format(p_val, digits = 4, scientific = TRUE))
    )
  })

  output$ht_decision_box <- renderUI({
    req(input$analysis_mode == "ht")
    req(!is.na(ht_sim$decision))

    z_crit <- ht_sim$z_crit
    alpha <- 1 - pnorm(z_crit)

    if(ht_sim$decision == "Reject H0") {
      box_class <- "result-box reject-h0"
      decision_text <- paste0(
        "<strong>Decision: REJECT H\u2080</strong><br>",
        "z = ", round(ht_sim$z_statistic, 3), " > ", round(z_crit, 3), " (critical value)<br>",
        "p-value = ", format(ht_sim$p_value, digits = 4), " < \u03b1 = ", round(alpha, 4)
      )
    } else {
      box_class <- "result-box fail-reject"
      decision_text <- paste0(
        "<strong>Decision: FAIL TO REJECT H\u2080</strong><br>",
        "z = ", round(ht_sim$z_statistic, 3), " \u2264 ", round(z_crit, 3), " (critical value)<br>",
        "p-value = ", format(ht_sim$p_value, digits = 4), " \u2265 \u03b1 = ", round(alpha, 4)
      )
    }

    div(class = box_class, HTML(decision_text))
  })

  output$ht_interpretation <- renderUI({
    req(input$analysis_mode == "ht")
    req(!is.na(ht_sim$decision), !is.na(ht_sim$true_mean))

    h0_true <- abs(ht_sim$true_mean) < 0.001

    if(h0_true) {
      if(ht_sim$decision == "Reject H0") {
        outcome <- "TYPE I ERROR (False Positive)"
        explanation <- "H\u2080 was true (true mean = 0) but we incorrectly rejected it."
        box_class <- "result-box incorrect-decision"
      } else {
        outcome <- "CORRECT RETENTION"
        explanation <- "H\u2080 was true (true mean = 0) and we correctly failed to reject it."
        box_class <- "result-box correct-decision"
      }
    } else {
      if(ht_sim$decision == "Reject H0") {
        outcome <- "CORRECT DETECTION (Power)"
        explanation <- paste0("H\u2081 was true (true mean = ", round(ht_sim$true_mean, 2), ") and we correctly rejected H\u2080.")
        box_class <- "result-box correct-decision"
      } else {
        outcome <- "TYPE II ERROR (False Negative)"
        explanation <- paste0("H\u2081 was true (true mean = ", round(ht_sim$true_mean, 2), ") but we failed to reject H\u2080.")
        box_class <- "result-box incorrect-decision"
      }
    }

    div(class = box_class,
        HTML(paste0(
          "<strong>Reality: True mean = ", round(ht_sim$true_mean, 2), "</strong><br>",
          "<strong>Outcome: ", outcome, "</strong><br><br>",
          explanation
        ))
    )
  })

  # ========== HT SEVERITY ANALYSIS ==========
  output$ht_severity <- renderUI({
    req(input$analysis_mode == "ht")
    req(!is.na(ht_sim$decision), !is.na(ht_sim$z_statistic))

    z <- ht_sim$z_statistic
    z_crit <- ht_sim$z_crit
    se <- ht_sim$se
    n <- ht_sim$n

    if(ht_sim$decision == "Reject H0") {
      sev_reject <- pnorm(z)
      discrepancy <- (z - z_crit) * se

      if(sev_reject >= 0.95) { sev_class <- "severity-high"; sev_label <- "High"
      } else if(sev_reject >= 0.80) { sev_class <- "severity-medium"; sev_label <- "Moderate"
      } else { sev_class <- "severity-low"; sev_label <- "Low" }

      div(class = paste("severity-box", sev_class),
          HTML(paste0(
            "<strong>Severity for Rejecting H\u2080:</strong> ", round(sev_reject, 3), " (", sev_label, ")<br><br>",
            "<div class='severity-bar' style='background: linear-gradient(to right, #4caf50 ", round(sev_reject*100), "%, #eee ", round(sev_reject*100), "%);'></div>",
            "<p>", round(sev_reject * 100, 1), "% probability of less extreme result if H\u2080 true.</p>",
            "<p><strong>Effect warranted:</strong> Claims of effects \u2265 ", round(discrepancy, 3), " are severely tested.</p>",
            "<p><em>Severity = 1 - p-value when we reject.</em></p>"
          ))
      )
    } else {
      d_small <- 0.2; d_med <- 0.5; d_large <- 0.8
      sev_small <- 1 - pnorm(z_crit - d_small * sqrt(n))
      sev_med <- 1 - pnorm(z_crit - d_med * sqrt(n))
      sev_large <- 1 - pnorm(z_crit - d_large * sqrt(n))

      if(sev_large >= 0.80) {
        sev_class <- "severity-high"; sev_label <- "High"
        sev_msg <- "Good power for large effects - strong evidence against them."
      } else if(sev_med >= 0.80) {
        sev_class <- "severity-medium"; sev_label <- "Moderate"
        sev_msg <- "Power for medium effects but may miss small ones."
      } else {
        sev_class <- "severity-low"; sev_label <- "Low"
        sev_msg <- "Low power - failing to reject does NOT rule out effects."
      }

      div(class = paste("severity-box", sev_class),
          HTML(paste0(
            "<strong>Severity for Failing to Reject H\u2080:</strong> ", sev_label, "<br><br>",
            "<p>Power to detect effects:</p>",
            "<table style='width:100%; margin: 10px 0;'>",
            "<tr><td>d = 0.2 (small):</td><td><strong>", round(sev_small * 100, 1), "%</strong></td></tr>",
            "<tr><td>d = 0.5 (medium):</td><td><strong>", round(sev_med * 100, 1), "%</strong></td></tr>",
            "<tr><td>d = 0.8 (large):</td><td><strong>", round(sev_large * 100, 1), "%</strong></td></tr>",
            "</table>",
            "<p><strong>Interpretation:</strong> ", sev_msg, "</p>"
          ))
      )
    }
  })

  # ========== PROGRESS TEXT ==========
  output$sim_progress <- renderText({
    if(input$analysis_mode == "sdt") {
      if(length(sim$observations) == 0) {
        "Click 'Run Simulation' to begin"
      } else if(sim$running) {
        paste("Simulating...", sim$current_idx, "of", length(sim$observations), "observations")
      } else if(sim$current_idx > 0) {
        paste("Simulation complete:", sim$current_idx, "observations")
      }
    } else {
      if(length(ht_sim$sample_data) == 0) {
        "Set the True Mean (Reality) above, then click 'Run Simulation'"
      } else if(ht_sim$running) {
        paste("Collecting sample...", ht_sim$current_idx, "of", length(ht_sim$sample_data), "observations")
      } else if(ht_sim$current_idx > 0) {
        paste("Sample complete:", ht_sim$current_idx, "observations")
      }
    }
  })
}

shinyApp(ui, server)
