library(shiny)
library(ggplot2)
library(gridExtra)
library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Hypothesis Testing Paradigms"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Bayesian", tabName = "bayesian", icon = icon("brain")),
      menuItem("Fisherian", tabName = "fisherian", icon = icon("fish")),
      menuItem("Neyman-Pearson", tabName = "neyman", icon = icon("balance-scale")),
      menuItem("Comparison", tabName = "compare", icon = icon("table"))
    ),

    hr(),

    h4("Data Settings", style = "padding-left: 15px;"),

    sliderInput("sample_size", "Sample Size (n):",
                min = 10, max = 500, value = 100, step = 10),

    numericInput("sample_mean", "Sample Mean:",
                 value = 52, step = 0.1),

    numericInput("sample_sd", "Sample SD:",
                 value = 10, step = 0.1, min = 0.1),

    numericInput("null_value", "Null Hypothesis (μ₀):",
                 value = 50, step = 0.1),

    actionButton("generate", "Generate New Sample",
                 icon = icon("refresh"),
                 class = "btn-primary",
                 style = "margin-left: 15px; margin-top: 10px;")
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f5; }
        .box { margin-bottom: 20px; }
        .interpretation {
          background-color: #f9f9f9;
          padding: 15px;
          border-left: 4px solid #3c8dbc;
          margin-top: 10px;
        }
        .key-result {
          font-size: 18px;
          font-weight: bold;
          color: #3c8dbc;
          margin: 10px 0;
        }
      "))
    ),

    tabItems(
      # Introduction Tab
      tabItem(tabName = "intro",
              fluidRow(
                box(width = 12, title = "Three Philosophies of Hypothesis Testing",
                    status = "primary", solidHeader = TRUE,
                    h3("Welcome to the Hypothesis Testing Paradigms App"),
                    p("This app demonstrates three fundamentally different approaches to statistical hypothesis testing:"),

                    h4("1. Bayesian Approach"),
                    p("Uses Bayes' theorem to update prior beliefs about parameters based on data.
                      Produces probability statements about hypotheses (e.g., 'There's a 95% probability the effect is positive').
                      Requires specification of prior distributions."),

                    h4("2. Fisherian Approach"),
                    p("Focuses on the p-value as a continuous measure of evidence against the null hypothesis.
                      Does not use fixed significance levels or alternative hypotheses.
                      Interprets p-values as the probability of observing data as extreme (or more) if the null is true."),

                    h4("3. Neyman-Pearson Approach"),
                    p("Uses fixed decision rules (α level) to accept or reject hypotheses.
                      Considers Type I and Type II errors, statistical power, and the alternative hypothesis.
                      Focuses on long-run error rates rather than evidence in a single experiment."),

                    hr(),

                    h4("How to Use This App"),
                    tags$ol(
                      tags$li("Adjust the data settings in the sidebar (sample size, mean, SD, and null hypothesis value)"),
                      tags$li("Click 'Generate New Sample' to simulate data or use the current settings"),
                      tags$li("Explore each tab to see how the different approaches analyze the same data"),
                      tags$li("Compare the conclusions in the 'Comparison' tab")
                    )
                )
              )
      ),

      # Bayesian Tab
      tabItem(tabName = "bayesian",
              fluidRow(
                box(width = 4, title = "Bayesian Settings", status = "info", solidHeader = TRUE,
                    h4("Prior Distribution for μ"),
                    selectInput("prior_type", "Prior Type:",
                                choices = c("Weakly Informative (Normal)" = "normal",
                                           "Uninformative (Flat)" = "flat",
                                           "Skeptical (centered on null)" = "skeptical")),

                    conditionalPanel(
                      condition = "input.prior_type == 'normal'",
                      numericInput("prior_mean", "Prior Mean:", value = 50),
                      numericInput("prior_sd", "Prior SD:", value = 20, min = 0.1)
                    ),

                    conditionalPanel(
                      condition = "input.prior_type == 'skeptical'",
                      numericInput("skeptical_sd", "Prior SD (around null):",
                                   value = 5, min = 0.1)
                    ),

                    hr(),

                    sliderInput("credible_interval", "Credible Interval:",
                                min = 80, max = 99, value = 95, step = 1,
                                post = "%"),

                    checkboxInput("show_bf", "Show Bayes Factor", value = TRUE)
                ),

                box(width = 8, title = "Posterior Distribution", status = "info", solidHeader = TRUE,
                    plotOutput("bayes_plot", height = "400px"),

                    div(class = "interpretation",
                        h4("Bayesian Interpretation:"),
                        htmlOutput("bayes_interpretation")
                    )
                )
              )
      ),

      # Fisherian Tab
      tabItem(tabName = "fisherian",
              fluidRow(
                box(width = 4, title = "Fisherian Settings", status = "warning", solidHeader = TRUE,
                    selectInput("test_type", "Test Type:",
                                choices = c("Two-tailed" = "two.sided",
                                           "Greater than null" = "greater",
                                           "Less than null" = "less")),

                    hr(),

                    h4("P-value Interpretation Guide:"),
                    tags$ul(
                      tags$li("p > 0.10: Little to no evidence against null"),
                      tags$li("0.05 < p < 0.10: Weak evidence"),
                      tags$li("0.01 < p < 0.05: Moderate evidence"),
                      tags$li("0.001 < p < 0.01: Strong evidence"),
                      tags$li("p < 0.001: Very strong evidence")
                    ),

                    p(style = "font-style: italic; margin-top: 15px;",
                      "Note: Fisher advocated for interpreting p-values continuously,
                      not using arbitrary cutoffs.")
                ),

                box(width = 8, title = "P-value Visualization", status = "warning", solidHeader = TRUE,
                    plotOutput("fisher_plot", height = "400px"),

                    div(class = "interpretation",
                        h4("Fisherian Interpretation:"),
                        htmlOutput("fisher_interpretation")
                    )
                )
              )
      ),

      # Neyman-Pearson Tab
      tabItem(tabName = "neyman",
              fluidRow(
                box(width = 4, title = "Neyman-Pearson Settings", status = "success", solidHeader = TRUE,
                    sliderInput("alpha", "Significance Level (α):",
                                min = 0.01, max = 0.10, value = 0.05, step = 0.01),

                    selectInput("np_alternative", "Alternative Hypothesis:",
                                choices = c("Two-sided (μ ≠ μ₀)" = "two.sided",
                                           "One-sided (μ > μ₀)" = "greater",
                                           "One-sided (μ < μ₀)" = "less")),

                    numericInput("effect_size", "Expected Effect Size (for power):",
                                 value = 5, step = 0.1),

                    hr(),

                    h4("Error Types:"),
                    p(strong("Type I Error (α):"), "Rejecting true null hypothesis"),
                    p(strong("Type II Error (β):"), "Failing to reject false null hypothesis"),
                    p(strong("Power (1-β):"), "Probability of detecting true effect")
                ),

                box(width = 8, title = "Decision and Power Analysis", status = "success", solidHeader = TRUE,
                    plotOutput("neyman_plot", height = "400px"),

                    div(class = "interpretation",
                        h4("Neyman-Pearson Interpretation:"),
                        htmlOutput("neyman_interpretation")
                    )
                )
              )
      ),

      # Comparison Tab
      tabItem(tabName = "compare",
              fluidRow(
                box(width = 12, title = "Side-by-Side Comparison", status = "primary", solidHeader = TRUE,
                    tableOutput("comparison_table"),

                    hr(),

                    h4("Key Philosophical Differences:"),

                    div(style = "margin-top: 20px;",
                        column(4,
                               h4("Bayesian", style = "color: #3c8dbc;"),
                               tags$ul(
                                 tags$li("Probability of hypothesis given data"),
                                 tags$li("Incorporates prior knowledge"),
                                 tags$li("Direct inference about parameters"),
                                 tags$li("Credible intervals have intuitive interpretation")
                               )
                        ),
                        column(4,
                               h4("Fisherian", style = "color: #f39c12;"),
                               tags$ul(
                                 tags$li("Probability of data given null hypothesis"),
                                 tags$li("P-value as continuous evidence"),
                                 tags$li("No alternative hypothesis needed"),
                                 tags$li("Focuses on single experiment")
                               )
                        ),
                        column(4,
                               h4("Neyman-Pearson", style = "color: #00a65a;"),
                               tags$ul(
                                 tags$li("Binary decision: reject or fail to reject"),
                                 tags$li("Controls long-run error rates"),
                                 tags$li("Requires alternative hypothesis"),
                                 tags$li("Emphasizes power and Type II errors")
                               )
                        )
                    )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive values for data
  data <- reactiveVal(NULL)

  # Initialize or generate data
  observe({
    input$generate

    isolate({
      set.seed(as.numeric(Sys.time()))
      n <- input$sample_size
      x <- rnorm(n, mean = input$sample_mean, sd = input$sample_sd)
      data(x)
    })
  })

  # Initialize data on startup
  observeEvent(session$clientData, {
    if(is.null(data())) {
      set.seed(123)
      x <- rnorm(input$sample_size, mean = input$sample_mean, sd = input$sample_sd)
      data(x)
    }
  }, once = TRUE)

  # Calculate basic statistics
  sample_stats <- reactive({
    req(data())
    x <- data()
    list(
      n = length(x),
      mean = mean(x),
      sd = sd(x),
      se = sd(x) / sqrt(length(x))
    )
  })

  # Bayesian Analysis
  bayes_results <- reactive({
    stats <- sample_stats()
    n <- stats$n
    xbar <- stats$mean
    s <- stats$sd
    se <- stats$se
    mu0 <- input$null_value

    # Define prior
    if(input$prior_type == "normal") {
      prior_mean <- input$prior_mean
      prior_var <- input$prior_sd^2
    } else if(input$prior_type == "skeptical") {
      prior_mean <- mu0
      prior_var <- input$skeptical_sd^2
    } else { # flat prior
      prior_mean <- xbar
      prior_var <- 10000
    }

    # Posterior with normal prior and normal likelihood
    likelihood_var <- s^2 / n

    posterior_var <- 1 / (1/prior_var + 1/likelihood_var)
    posterior_mean <- posterior_var * (prior_mean/prior_var + xbar/likelihood_var)
    posterior_sd <- sqrt(posterior_var)

    # Credible interval
    ci_level <- input$credible_interval / 100
    ci_lower <- qnorm((1-ci_level)/2, posterior_mean, posterior_sd)
    ci_upper <- qnorm(1-(1-ci_level)/2, posterior_mean, posterior_sd)

    # Probability that mu > mu0
    prob_greater <- 1 - pnorm(mu0, posterior_mean, posterior_sd)

    # Simple Bayes Factor approximation (BIC-based)
    # This is simplified; full BF would require integrating over parameter space
    t_stat <- (xbar - mu0) / se
    bf_approx <- exp(-0.5 * t_stat^2 + log(sqrt(n)))

    list(
      prior_mean = prior_mean,
      prior_sd = sqrt(prior_var),
      posterior_mean = posterior_mean,
      posterior_sd = posterior_sd,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      prob_greater = prob_greater,
      bf = bf_approx
    )
  })

  output$bayes_plot <- renderPlot({
    results <- bayes_results()
    stats <- sample_stats()
    mu0 <- input$null_value

    # Create x-axis range
    x_range <- seq(results$posterior_mean - 4*results$posterior_sd,
                   results$posterior_mean + 4*results$posterior_sd,
                   length.out = 500)

    # Prior and posterior densities
    if(input$prior_type != "flat") {
      prior_dens <- dnorm(x_range, results$prior_mean, results$prior_sd)
    }
    posterior_dens <- dnorm(x_range, results$posterior_mean, results$posterior_sd)

    # Create data frame
    df <- data.frame(x = x_range, posterior = posterior_dens)
    if(input$prior_type != "flat") {
      df$prior <- prior_dens
    }

    # Base plot
    p <- ggplot(df, aes(x = x)) +
      geom_line(aes(y = posterior, color = "Posterior"), linewidth = 1.5) +
      theme_minimal() +
      labs(x = "μ (Population Mean)", y = "Density",
           title = "Prior and Posterior Distributions") +
      theme(legend.position = "top", legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 16))

    # Add prior if not flat
    if(input$prior_type != "flat") {
      p <- p + geom_line(aes(y = prior, color = "Prior"), linewidth = 1.5, linetype = "dashed")
    }

    # Add credible interval shading
    ci_data <- df[df$x >= results$ci_lower & df$x <= results$ci_upper, ]
    p <- p + geom_area(data = ci_data, aes(y = posterior), fill = "lightblue", alpha = 0.4)

    # Add null value line
    p <- p + geom_vline(xintercept = mu0, linetype = "dotted", color = "red", linewidth = 1) +
      annotate("text", x = mu0, y = max(posterior_dens) * 0.9,
               label = paste0("H₀: μ = ", mu0), color = "red", hjust = -0.1)

    # Add sample mean
    p <- p + geom_vline(xintercept = stats$mean, linetype = "solid",
                       color = "darkgreen", linewidth = 1, alpha = 0.6) +
      annotate("text", x = stats$mean, y = max(posterior_dens) * 0.8,
               label = paste0("x̄ = ", round(stats$mean, 2)),
               color = "darkgreen", hjust = -0.1)

    p + scale_color_manual(values = c("Prior" = "gray40", "Posterior" = "steelblue"))
  })

  output$bayes_interpretation <- renderUI({
    results <- bayes_results()
    stats <- sample_stats()
    mu0 <- input$null_value
    ci_level <- input$credible_interval

    HTML(paste0(
      "<div class='key-result'>Posterior Mean: ", round(results$posterior_mean, 3),
      " (SD = ", round(results$posterior_sd, 3), ")</div>",

      "<p><strong>", ci_level, "% Credible Interval:</strong> [",
      round(results$ci_lower, 3), ", ", round(results$ci_upper, 3), "]<br>",
      "There is a ", ci_level, "% probability that μ lies in this interval.</p>",

      "<p><strong>Probability that μ > ", mu0, ":</strong> ",
      round(results$prob_greater * 100, 2), "%</p>",

      if(input$show_bf) {
        paste0("<p><strong>Bayes Factor (BF₁₀):</strong> ", round(results$bf, 3), "<br>",
               if(results$bf > 10) "Strong evidence for alternative"
               else if(results$bf > 3) "Moderate evidence for alternative"
               else if(results$bf > 1) "Weak evidence for alternative"
               else if(results$bf > 1/3) "Inconclusive evidence"
               else if(results$bf > 1/10) "Weak evidence for null"
               else "Moderate to strong evidence for null",
               "</p>")
      } else "",

      "<p><em>Interpretation:</em> Based on the data and our prior beliefs, ",
      "the posterior distribution represents our updated belief about μ. ",
      if(results$ci_lower > mu0) {
        "The credible interval does not contain the null value, suggesting the mean is likely greater than the null hypothesis."
      } else if(results$ci_upper < mu0) {
        "The credible interval does not contain the null value, suggesting the mean is likely less than the null hypothesis."
      } else {
        "The credible interval contains the null value, suggesting uncertainty about whether the mean differs from the null hypothesis."
      },
      "</p>"
    ))
  })

  # Fisherian Analysis
  fisher_results <- reactive({
    stats <- sample_stats()
    mu0 <- input$null_value

    t_stat <- (stats$mean - mu0) / stats$se
    df <- stats$n - 1

    if(input$test_type == "two.sided") {
      p_value <- 2 * pt(-abs(t_stat), df)
    } else if(input$test_type == "greater") {
      p_value <- pt(t_stat, df, lower.tail = FALSE)
    } else {
      p_value <- pt(t_stat, df, lower.tail = TRUE)
    }

    list(
      t_stat = t_stat,
      df = df,
      p_value = p_value
    )
  })

  output$fisher_plot <- renderPlot({
    results <- fisher_results()
    stats <- sample_stats()
    mu0 <- input$null_value

    # Create t-distribution
    x_range <- seq(-4, 4, length.out = 500)
    y_dens <- dt(x_range, df = results$df)

    df <- data.frame(x = x_range, y = y_dens)

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(linewidth = 1.5, color = "darkblue") +
      theme_minimal() +
      labs(x = "t-statistic", y = "Density",
           title = paste0("Sampling Distribution under H₀ (t-distribution, df=", results$df, ")")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16))

    # Shade p-value region
    if(input$test_type == "two.sided") {
      critical_t <- abs(results$t_stat)
      shade_left <- df[df$x <= -critical_t, ]
      shade_right <- df[df$x >= critical_t, ]
      p <- p +
        geom_area(data = shade_left, aes(x = x, y = y), fill = "red", alpha = 0.3) +
        geom_area(data = shade_right, aes(x = x, y = y), fill = "red", alpha = 0.3)
    } else if(input$test_type == "greater") {
      shade_area <- df[df$x >= results$t_stat, ]
      p <- p + geom_area(data = shade_area, aes(x = x, y = y), fill = "red", alpha = 0.3)
    } else {
      shade_area <- df[df$x <= results$t_stat, ]
      p <- p + geom_area(data = shade_area, aes(x = x, y = y), fill = "red", alpha = 0.3)
    }

    # Add observed t-statistic
    p <- p + geom_vline(xintercept = results$t_stat, linetype = "dashed",
                       color = "red", linewidth = 1.2) +
      annotate("text", x = results$t_stat, y = max(y_dens) * 0.8,
               label = paste0("Observed t = ", round(results$t_stat, 3)),
               color = "red", hjust = ifelse(results$t_stat > 0, -0.1, 1.1))

    p
  })

  output$fisher_interpretation <- renderUI({
    results <- fisher_results()
    stats <- sample_stats()
    mu0 <- input$null_value

    evidence <- if(results$p_value < 0.001) "very strong"
    else if(results$p_value < 0.01) "strong"
    else if(results$p_value < 0.05) "moderate"
    else if(results$p_value < 0.10) "weak"
    else "little to no"

    HTML(paste0(
      "<div class='key-result'>P-value: ", round(results$p_value, 5), "</div>",

      "<p><strong>Test Statistic:</strong> t = ", round(results$t_stat, 3),
      " (df = ", results$df, ")</p>",

      "<p><strong>Sample Mean:</strong> ", round(stats$mean, 3),
      " (SE = ", round(stats$se, 3), ")</p>",

      "<p><em>Fisherian Interpretation:</em> ",
      "If the true population mean were ", mu0, " (the null hypothesis), ",
      "the probability of observing a sample mean as extreme as or more extreme than ",
      round(stats$mean, 3), " is ", round(results$p_value, 5), ". ",
      "This represents <strong>", evidence, " evidence</strong> against the null hypothesis.</p>",

      "<p>Fisher would emphasize that this p-value should be interpreted as a ",
      "<strong>continuous measure of evidence</strong>, not compared against an arbitrary threshold. ",
      "The smaller the p-value, the more surprising the data would be under the null hypothesis.</p>"
    ))
  })

  # Neyman-Pearson Analysis
  np_results <- reactive({
    stats <- sample_stats()
    mu0 <- input$null_value
    alpha <- input$alpha

    t_stat <- (stats$mean - mu0) / stats$se
    df <- stats$n - 1

    # Critical value
    if(input$np_alternative == "two.sided") {
      critical_value <- qt(1 - alpha/2, df)
      reject <- abs(t_stat) > critical_value
      p_value <- 2 * pt(-abs(t_stat), df)
    } else if(input$np_alternative == "greater") {
      critical_value <- qt(1 - alpha, df)
      reject <- t_stat > critical_value
      p_value <- pt(t_stat, df, lower.tail = FALSE)
    } else {
      critical_value <- qt(alpha, df)
      reject <- t_stat < critical_value
      p_value <- pt(t_stat, df, lower.tail = TRUE)
    }

    # Power calculation
    effect <- input$effect_size
    mu1 <- mu0 + effect
    ncp <- (mu1 - mu0) / stats$se  # non-centrality parameter

    if(input$np_alternative == "two.sided") {
      power <- pt(-critical_value, df, ncp = ncp) +
        pt(critical_value, df, ncp = ncp, lower.tail = FALSE)
    } else if(input$np_alternative == "greater") {
      power <- pt(critical_value, df, ncp = ncp, lower.tail = FALSE)
    } else {
      power <- pt(critical_value, df, ncp = ncp)
    }

    list(
      t_stat = t_stat,
      critical_value = critical_value,
      reject = reject,
      power = power,
      beta = 1 - power,
      p_value = p_value,
      df = df
    )
  })

  output$neyman_plot <- renderPlot({
    results <- np_results()
    stats <- sample_stats()
    mu0 <- input$null_value
    mu1 <- mu0 + input$effect_size

    # Create distributions
    x_range <- seq(-4, 4, length.out = 500)

    # Null distribution
    null_dens <- dt(x_range, df = results$df)

    # Alternative distribution (non-central t)
    ncp <- (mu1 - mu0) / stats$se
    alt_dens <- dt(x_range, df = results$df, ncp = ncp)

    df_plot <- data.frame(
      x = x_range,
      null = null_dens,
      alternative = alt_dens
    )

    p <- ggplot(df_plot, aes(x = x)) +
      geom_line(aes(y = null, color = "Null (H₀)"), linewidth = 1.2) +
      geom_line(aes(y = alternative, color = "Alternative (H₁)"), linewidth = 1.2) +
      theme_minimal() +
      labs(x = "t-statistic", y = "Density",
           title = "Null and Alternative Distributions with Error Regions") +
      theme(legend.position = "top", legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 16))

    # Add critical region and errors
    if(input$np_alternative == "two.sided") {
      crit <- results$critical_value

      # Type I error (alpha)
      alpha_left <- df_plot[df_plot$x <= -crit, ]
      alpha_right <- df_plot[df_plot$x >= crit, ]
      p <- p +
        geom_area(data = alpha_left, aes(x = x, y = null), fill = "red", alpha = 0.3) +
        geom_area(data = alpha_right, aes(x = x, y = null), fill = "red", alpha = 0.3)

      # Type II error (beta)
      beta_area <- df_plot[df_plot$x > -crit & df_plot$x < crit, ]
      p <- p + geom_area(data = beta_area, aes(x = x, y = alternative), fill = "blue", alpha = 0.3)

      # Critical values
      p <- p +
        geom_vline(xintercept = c(-crit, crit), linetype = "dashed", color = "black") +
        annotate("text", x = crit, y = max(null_dens) * 0.9,
                label = paste0("Critical = ±", round(crit, 2)), hjust = -0.1)

    } else if(input$np_alternative == "greater") {
      crit <- results$critical_value

      # Type I error
      alpha_area <- df_plot[df_plot$x >= crit, ]
      p <- p + geom_area(data = alpha_area, aes(x = x, y = null), fill = "red", alpha = 0.3)

      # Type II error
      beta_area <- df_plot[df_plot$x < crit, ]
      p <- p + geom_area(data = beta_area, aes(x = x, y = alternative), fill = "blue", alpha = 0.3)

      # Critical value
      p <- p +
        geom_vline(xintercept = crit, linetype = "dashed", color = "black") +
        annotate("text", x = crit, y = max(null_dens) * 0.9,
                label = paste0("Critical = ", round(crit, 2)), hjust = -0.1)

    } else {  # less
      crit <- results$critical_value

      # Type I error
      alpha_area <- df_plot[df_plot$x <= crit, ]
      p <- p + geom_area(data = alpha_area, aes(x = x, y = null), fill = "red", alpha = 0.3)

      # Type II error
      beta_area <- df_plot[df_plot$x > crit, ]
      p <- p + geom_area(data = beta_area, aes(x = x, y = alternative), fill = "blue", alpha = 0.3)

      # Critical value
      p <- p +
        geom_vline(xintercept = crit, linetype = "dashed", color = "black") +
        annotate("text", x = crit, y = max(null_dens) * 0.9,
                label = paste0("Critical = ", round(crit, 2)), hjust = 1.1)
    }

    # Add observed t-statistic
    p <- p + geom_vline(xintercept = results$t_stat, linetype = "dotted",
                       color = "darkgreen", linewidth = 1.2) +
      annotate("text", x = results$t_stat, y = max(null_dens) * 0.7,
               label = paste0("Observed t = ", round(results$t_stat, 2)),
               color = "darkgreen", hjust = ifelse(results$t_stat > 0, -0.1, 1.1))

    p + scale_color_manual(values = c("Null (H₀)" = "red", "Alternative (H₁)" = "blue"))
  })

  output$neyman_interpretation <- renderUI({
    results <- np_results()
    stats <- sample_stats()
    mu0 <- input$null_value
    alpha <- input$alpha

    decision <- if(results$reject) "REJECT" else "FAIL TO REJECT"
    decision_color <- if(results$reject) "red" else "green"

    HTML(paste0(
      "<div class='key-result' style='color: ", decision_color, ";'>Decision: ",
      decision, " H₀ at α = ", alpha, "</div>",

      "<p><strong>Test Statistic:</strong> t = ", round(results$t_stat, 3), "<br>",
      "<strong>Critical Value:</strong> ",
      if(input$np_alternative == "two.sided") "±" else "",
      round(abs(results$critical_value), 3), "</p>",

      "<p><strong>Error Rates:</strong><br>",
      "• Type I Error Rate (α): ", alpha, " (fixed by design)<br>",
      "• Type II Error Rate (β): ", round(results$beta, 3), "<br>",
      "• Statistical Power (1-β): ", round(results$power, 3), "</p>",

      "<p><em>Neyman-Pearson Interpretation:</em> ",
      if(results$reject) {
        paste0("We <strong>reject the null hypothesis</strong> at the ", alpha, " significance level. ",
               "This decision procedure has a ", alpha * 100, "% long-run error rate ",
               "(we will incorrectly reject true null hypotheses in ", alpha * 100, "% of repeated experiments). ")
      } else {
        paste0("We <strong>fail to reject the null hypothesis</strong> at the ", alpha, " significance level. ",
               "This does NOT mean we accept the null; we simply don't have sufficient evidence to reject it. ",
               "The Type II error rate (failing to detect the assumed effect of ", input$effect_size,
               ") is ", round(results$beta * 100, 1), "%. ")
      },
      "</p>",

      "<p>The Neyman-Pearson framework emphasizes <strong>pre-specified decision rules</strong> ",
      "and <strong>long-run error rates</strong> rather than the evidence in any single experiment. ",
      "The goal is to control errors over many repeated applications of the test.</p>"
    ))
  })

  # Comparison Table
  output$comparison_table <- renderTable({
    stats <- sample_stats()
    bayes <- bayes_results()
    fisher <- fisher_results()
    np <- np_results()
    mu0 <- input$null_value

    data.frame(
      Aspect = c(
        "Primary Output",
        "Conclusion",
        "Uses Prior Info?",
        "Uses Alternative?",
        "Interpretation",
        "Key Value"
      ),
      Bayesian = c(
        "Posterior distribution of μ",
        if(bayes$ci_lower > mu0 || bayes$ci_upper < mu0)
          paste0(input$credible_interval, "% CI excludes null")
        else paste0(input$credible_interval, "% CI includes null"),
        "Yes (explicit prior)",
        "Not directly",
        paste0(round(bayes$prob_greater * 100, 1), "% prob. μ > ", mu0),
        paste0("Posterior mean: ", round(bayes$posterior_mean, 3))
      ),
      Fisherian = c(
        "P-value",
        if(fisher$p_value < 0.05) "Moderate to strong evidence against H₀"
        else if(fisher$p_value < 0.10) "Weak evidence against H₀"
        else "Little evidence against H₀",
        "No",
        "No",
        "Continuous evidence measure",
        paste0("p = ", round(fisher$p_value, 5))
      ),
      `Neyman-Pearson` = c(
        "Binary decision",
        if(np$reject) paste0("Reject H₀ at α=", input$alpha)
        else paste0("Fail to reject H₀ at α=", input$alpha),
        "No",
        "Yes (for power)",
        "Long-run error control",
        paste0("Power = ", round(np$power, 3))
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
}

shinyApp(ui = ui, server = server)
