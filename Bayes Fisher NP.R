library(shiny)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Three Schools of Statistical Inference"),

  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Experiment Setup"),
        p("Testing the fairness of a coin"),
        sliderInput("true_p", "True coin bias (p):",
                    min = 0.3, max = 0.7, value = 0.5, step = 0.01),
        p(class = "text-muted", "p = 0.5 is a fair coin"),
        hr(),
        sliderInput("n_flips", "Number of flips per step:",
                    min = 1, max = 10, value = 5, step = 1),
        sliderInput("animation_speed", "Animation speed (ms between steps):",
                    min = 100, max = 2000, value = 500, step = 100),
        hr(),
        h4("Hypothesis Test Settings"),
        numericInput("null_p", "Null hypothesis (p₀):",
                     value = 0.5, min = 0, max = 1, step = 0.01),
        sliderInput("alpha", "Significance level (α):",
                    min = 0.01, max = 0.10, value = 0.05, step = 0.01)
      ),

      wellPanel(
        h4("Prior (Bayesian)"),
        p("Beta distribution parameters:"),
        numericInput("prior_a", "α (prior):", value = 2, min = 0.1, step = 0.1),
        numericInput("prior_b", "β (prior):", value = 2, min = 0.1, step = 0.1),
        p(class = "text-muted", "Uniform prior: α = β = 1"),
        p(class = "text-muted", "Jeffreys prior: α = β = 0.5")
      ),

      wellPanel(
        actionButton("start", "Start Simulation", class = "btn-primary btn-lg btn-block"),
        actionButton("step", "Step Forward", class = "btn-info btn-block"),
        actionButton("reset", "Reset", class = "btn-danger btn-block"),
        hr(),
        h4("Current Status"),
        uiOutput("current_stats")
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Bayesian",
                 br(),
                 div(class = "alert alert-info",
                     h4("Bayesian Inference"),
                     p("Shows how the prior distribution is updated to the posterior distribution as data arrives."),
                     p("The posterior represents our updated belief about the coin bias p after seeing the data.")
                 ),
                 plotOutput("bayes_plot", height = "500px"),
                 hr(),
                 uiOutput("bayes_summary")
        ),

        tabPanel("Neyman-Pearson",
                 br(),
                 div(class = "alert alert-info",
                     h4("Neyman-Pearson Hypothesis Testing"),
                     p("Pre-specify H₀, α, and the alternative hypothesis. The test ends with an accept/reject decision."),
                     p("Uses the sampling distribution under H₀ to control Type I error rate.")
                 ),
                 plotOutput("np_plot", height = "500px"),
                 hr(),
                 uiOutput("np_summary")
        ),

        tabPanel("Fisher",
                 br(),
                 div(class = "alert alert-info",
                     h4("Fisher's Significance Testing"),
                     p("Calculates a p-value: the probability of observing data at least as extreme as what we saw, assuming H₀ is true."),
                     p("Does not make a binary decision; instead provides a continuous measure of evidence against H₀.")
                 ),
                 plotOutput("fisher_plot", height = "500px"),
                 hr(),
                 uiOutput("fisher_summary")
        ),

        tabPanel("Frequentist CI",
                 br(),
                 div(class = "alert alert-info",
                     h4("Frequentist Confidence Interval"),
                     p("Shows how a confidence interval forms around the point estimate as data accumulates."),
                     p("The interval is constructed so that if we repeated the experiment many times, ",
                       "95% of such intervals would contain the true parameter.")
                 ),
                 plotOutput("freq_plot", height = "500px"),
                 hr(),
                 uiOutput("freq_summary")
        ),

        tabPanel("Comparison",
                 br(),
                 h3("Comparing the Three Schools"),
                 fluidRow(
                   column(4,
                          div(class = "well", style = "background-color: #e3f2fd; min-height: 500px;",
                              h4(style = "color: #1976d2;", "Bayesian"),
                              hr(),
                              h5("Philosophy:"),
                              p("Probability represents degree of belief. Parameters are random variables."),
                              hr(),
                              h5("Input:"),
                              tags$ul(
                                tags$li("Prior distribution"),
                                tags$li("Likelihood function"),
                                tags$li("Observed data")
                              ),
                              hr(),
                              h5("Output:"),
                              p("Posterior distribution - complete uncertainty about the parameter"),
                              hr(),
                              h5("Advantages:"),
                              tags$ul(
                                tags$li("Incorporates prior knowledge"),
                                tags$li("Direct probability statements about parameters"),
                                tags$li("Natural for sequential updating")
                              ),
                              hr(),
                              h5("Disadvantages:"),
                              tags$ul(
                                tags$li("Subjective prior choice"),
                                tags$li("Computationally intensive"),
                                tags$li("Can be sensitive to prior")
                              )
                          )
                   ),
                   column(4,
                          div(class = "well", style = "background-color: #fff3e0; min-height: 500px;",
                              h4(style = "color: #f57c00;", "Neyman-Pearson"),
                              hr(),
                              h5("Philosophy:"),
                              p("Focus on long-run error rates. Parameters are fixed but unknown."),
                              hr(),
                              h5("Input:"),
                              tags$ul(
                                tags$li("Null hypothesis H₀"),
                                tags$li("Alternative hypothesis H₁"),
                                tags$li("Significance level α"),
                                tags$li("Observed data")
                              ),
                              hr(),
                              h5("Output:"),
                              p("Binary decision: Reject or Fail to Reject H₀"),
                              hr(),
                              h5("Advantages:"),
                              tags$ul(
                                tags$li("Controls Type I error rate"),
                                tags$li("Clear decision rule"),
                                tags$li("Pre-specified test procedure")
                              ),
                              hr(),
                              h5("Disadvantages:"),
                              tags$ul(
                                tags$li("Binary decision loses information"),
                                tags$li("Doesn't measure strength of evidence"),
                                tags$li("Requires pre-specification")
                              )
                          )
                   ),
                   column(4,
                          div(class = "well", style = "background-color: #fce4ec; min-height: 500px;",
                              h4(style = "color: #c2185b;", "Fisher"),
                              hr(),
                              h5("Philosophy:"),
                              p("Use data to measure evidence against H₀. No prior probabilities."),
                              hr(),
                              h5("Input:"),
                              tags$ul(
                                tags$li("Null hypothesis H₀"),
                                tags$li("Test statistic"),
                                tags$li("Observed data")
                              ),
                              hr(),
                              h5("Output:"),
                              p("P-value - continuous measure of evidence against H₀"),
                              hr(),
                              h5("Advantages:"),
                              tags$ul(
                                tags$li("Continuous evidence measure"),
                                tags$li("No prior needed"),
                                tags$li("Widely understood")
                              ),
                              hr(),
                              h5("Disadvantages:"),
                              tags$ul(
                                tags$li("Often misinterpreted"),
                                tags$li("Doesn't provide evidence FOR H₀"),
                                tags$li("Arbitrary thresholds (e.g., 0.05)")
                              )
                          )
                   )
                 ),
                 hr(),
                 div(class = "well",
                     h4("Key Takeaways"),
                     tags$ul(
                       tags$li(strong("Bayesian:"), " Updates beliefs using prior + data → posterior"),
                       tags$li(strong("Neyman-Pearson:"), " Controls long-run error rates → accept/reject decision"),
                       tags$li(strong("Fisher:"), " Measures evidence against H₀ → p-value"),
                       tags$li(strong("Frequentist CI:"), " Interval estimation → quantifies uncertainty")
                     ),
                     p(style = "margin-top: 15px; font-style: italic;",
                       "Each approach answers different questions and has its place in statistical practice. ",
                       "Understanding all three perspectives gives you a richer understanding of inference.")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive values to store simulation state
  sim <- reactiveValues(
    running = FALSE,
    n_heads = 0,
    n_tails = 0,
    history = data.frame()
  )

  # Timer for animation
  timer <- reactiveVal(NULL)

  observeEvent(input$start, {
    if(!sim$running) {
      sim$running <- TRUE
      # Set up repeating timer
      observe({
        invalidateLater(input$animation_speed, session)
        isolate({
          if(sim$running) {
            # Flip coins
            flips <- rbinom(input$n_flips, 1, input$true_p)
            sim$n_heads <- sim$n_heads + sum(flips)
            sim$n_tails <- sim$n_tails + (input$n_flips - sum(flips))
          }
        })
      })
    } else {
      sim$running <- FALSE
    }

    # Update button label
    if(sim$running) {
      updateActionButton(session, "start", "Pause Simulation")
    } else {
      updateActionButton(session, "start", "Resume Simulation")
    }
  })

  observeEvent(input$step, {
    flips <- rbinom(input$n_flips, 1, input$true_p)
    sim$n_heads <- sim$n_heads + sum(flips)
    sim$n_tails <- sim$n_tails + (input$n_flips - sum(flips))
  })

  observeEvent(input$reset, {
    sim$running <- FALSE
    sim$n_heads <- 0
    sim$n_tails <- 0
    updateActionButton(session, "start", "Start Simulation")
  })

  # Current statistics
  output$current_stats <- renderUI({
    total <- sim$n_heads + sim$n_tails
    if(total == 0) {
      p("No data yet. Click 'Start Simulation' or 'Step Forward'")
    } else {
      prop <- sim$n_heads / total
      div(
        p(tags$b("Total flips:"), total),
        p(tags$b("Heads:"), sim$n_heads),
        p(tags$b("Tails:"), sim$n_tails),
        p(tags$b("Proportion:"), round(prop, 3))
      )
    }
  })

  # Bayesian tab
  output$bayes_plot <- renderPlot({
    total <- sim$n_heads + sim$n_tails
    if(total == 0) {
      # Show only prior
      p_seq <- seq(0, 1, length.out = 200)
      prior_dens <- dbeta(p_seq, input$prior_a, input$prior_b)

      ggplot(data.frame(p = p_seq, dens = prior_dens), aes(p, dens)) +
        geom_line(color = "gray50", size = 1.5) +
        geom_area(fill = "gray50", alpha = 0.3) +
        geom_vline(xintercept = input$true_p, color = "red", linetype = "dashed", size = 1) +
        theme_minimal() +
        labs(title = "Prior Distribution",
             subtitle = paste0("Waiting for data... (True p = ", input$true_p, ")"),
             x = "Coin bias (p)", y = "Density") +
        annotate("text", x = input$true_p, y = max(prior_dens) * 0.9,
                 label = "True p", color = "red", hjust = -0.2)
    } else {
      # Show prior and posterior
      p_seq <- seq(0, 1, length.out = 200)
      prior_dens <- dbeta(p_seq, input$prior_a, input$prior_b)
      post_dens <- dbeta(p_seq, input$prior_a + sim$n_heads, input$prior_b + sim$n_tails)

      df <- data.frame(
        p = rep(p_seq, 2),
        density = c(prior_dens, post_dens),
        type = rep(c("Prior", "Posterior"), each = length(p_seq))
      )

      # Calculate posterior mean and credible interval
      post_mean <- (input$prior_a + sim$n_heads) / (input$prior_a + input$prior_b + total)
      post_lower <- qbeta(0.025, input$prior_a + sim$n_heads, input$prior_b + sim$n_tails)
      post_upper <- qbeta(0.975, input$prior_a + sim$n_heads, input$prior_b + sim$n_tails)

      ggplot(df, aes(p, density, color = type, fill = type)) +
        geom_line(size = 1.5) +
        geom_area(alpha = 0.2, position = "identity") +
        geom_vline(xintercept = input$true_p, color = "red", linetype = "dashed", size = 1) +
        geom_vline(xintercept = post_mean, color = "#2c3e50", linetype = "solid", size = 1, alpha = 0.7) +
        annotate("rect", xmin = post_lower, xmax = post_upper, ymin = 0, ymax = Inf,
                 fill = "blue", alpha = 0.1) +
        scale_color_manual(values = c("Prior" = "gray50", "Posterior" = "#3498db")) +
        scale_fill_manual(values = c("Prior" = "gray50", "Posterior" = "#3498db")) +
        theme_minimal() +
        labs(title = "Bayesian Updating",
             subtitle = paste0("n = ", total, " | Prior → Posterior"),
             x = "Coin bias (p)", y = "Density") +
        theme(legend.position = "top")
    }
  })

  output$bayes_summary <- renderUI({
    total <- sim$n_heads + sim$n_tails
    if(total == 0) {
      div(class = "well",
          p("No data yet. The plot shows your prior distribution."))
    } else {
      post_mean <- (input$prior_a + sim$n_heads) / (input$prior_a + input$prior_b + total)
      post_lower <- qbeta(0.025, input$prior_a + sim$n_heads, input$prior_b + sim$n_tails)
      post_upper <- qbeta(0.975, input$prior_a + sim$n_heads, input$prior_b + sim$n_tails)

      div(class = "well",
          h4("Bayesian Results"),
          p(tags$b("Posterior mean:"), round(post_mean, 3)),
          p(tags$b("95% Credible Interval:"), "[", round(post_lower, 3), ",", round(post_upper, 3), "]"),
          hr(),
          p("The credible interval means: Given the data and prior, there is a 95% probability ",
            "that the true coin bias lies in this interval."),
          p("This is a direct probability statement about the parameter - something frequentist methods cannot provide!")
      )
    }
  })

  # Neyman-Pearson tab
  output$np_plot <- renderPlot({
    total <- sim$n_heads + sim$n_tails
    if(total == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Waiting for data...\nTest will begin when first observation arrives",
                 size = 6, color = "gray50") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else {
      # Calculate test statistic and decision
      p_hat <- sim$n_heads / total
      se <- sqrt(input$null_p * (1 - input$null_p) / total)
      z_obs <- (p_hat - input$null_p) / se
      z_crit <- qnorm(1 - input$alpha / 2)

      # Sampling distribution under H0
      z_seq <- seq(-4, 4, length.out = 200)
      dens <- dnorm(z_seq, 0, 1)

      # Decision
      reject <- abs(z_obs) > z_crit

      df <- data.frame(z = z_seq, density = dens)

      ggplot(df, aes(z, density)) +
        geom_area(data = subset(df, z <= -z_crit), fill = "#e74c3c", alpha = 0.4) +
        geom_area(data = subset(df, z >= z_crit), fill = "#e74c3c", alpha = 0.4) +
        geom_line(size = 1) +
        geom_vline(xintercept = c(-z_crit, z_crit), linetype = "dashed", color = "red") +
        geom_vline(xintercept = z_obs, color = "blue", size = 1.5) +
        annotate("text", x = z_obs, y = max(dens) * 0.8,
                 label = paste0("Observed z = ", round(z_obs, 2)),
                 color = "blue", hjust = ifelse(z_obs > 0, -0.1, 1.1)) +
        theme_minimal() +
        labs(title = "Neyman-Pearson Test",
             subtitle = paste0("n = ", total, " | Decision: ", ifelse(reject, "REJECT H₀", "FAIL TO REJECT H₀")),
             x = "Test Statistic (z)", y = "Density") +
        annotate("text", x = 3, y = max(dens) * 0.3,
                 label = paste0("α = ", input$alpha), color = "#e74c3c", size = 5)
    }
  })

  output$np_summary <- renderUI({
    total <- sim$n_heads + sim$n_tails
    if(total == 0) {
      div(class = "well",
          p("No data yet. The test will provide a decision once data arrives."))
    } else {
      p_hat <- sim$n_heads / total
      se <- sqrt(input$null_p * (1 - input$null_p) / total)
      z_obs <- (p_hat - input$null_p) / se
      z_crit <- qnorm(1 - input$alpha / 2)
      reject <- abs(z_obs) > z_crit

      color <- if(reject) "#e74c3c" else "#27ae60"

      div(class = "well", style = paste0("border-left: 5px solid ", color),
          h4("Neyman-Pearson Decision"),
          p(tags$b("H₀:"), "p =", input$null_p),
          p(tags$b("H₁:"), "p ≠", input$null_p),
          p(tags$b("Test statistic:"), "z =", round(z_obs, 3)),
          p(tags$b("Critical value:"), "±", round(z_crit, 3)),
          hr(),
          div(style = paste0("background-color: ", color, "; color: white; padding: 10px; border-radius: 5px; text-align: center; font-weight: bold;"),
              if(reject) "REJECT H₀" else "FAIL TO REJECT H₀"
          ),
          hr(),
          p("The decision is made by comparing the test statistic to the critical value. ",
            "This procedure controls the Type I error rate at α = ", input$alpha, ".")
      )
    }
  })

  # Fisher tab
  output$fisher_plot <- renderPlot({
    total <- sim$n_heads + sim$n_tails
    if(total == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Waiting for data...\nP-value will be calculated when observations arrive",
                 size = 6, color = "gray50") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else {
      # Calculate p-value
      p_hat <- sim$n_heads / total
      p_value <- binom.test(sim$n_heads, total, p = input$null_p)$p.value

      # Show binomial distribution under H0
      k_seq <- 0:total
      prob <- dbinom(k_seq, total, input$null_p)

      df <- data.frame(k = k_seq, probability = prob)
      df$extreme <- abs(k_seq - input$null_p * total) >= abs(sim$n_heads - input$null_p * total)

      ggplot(df, aes(k, probability, fill = extreme)) +
        geom_bar(stat = "identity", color = "black", alpha = 0.7) +
        scale_fill_manual(values = c("FALSE" = "gray70", "TRUE" = "#e74c3c"),
                          labels = c("Not extreme", "As or more extreme"),
                          name = "") +
        geom_vline(xintercept = sim$n_heads, color = "blue", size = 1.5) +
        theme_minimal() +
        labs(title = "Fisher's Significance Test",
             subtitle = paste0("n = ", total, " | p-value = ", round(p_value, 4)),
             x = "Number of heads (k)", y = "Probability under H₀") +
        theme(legend.position = "top") +
        annotate("text", x = sim$n_heads, y = max(prob) * 0.9,
                 label = paste0("Observed:\n", sim$n_heads, " heads"),
                 color = "blue", hjust = ifelse(sim$n_heads > total/2, 1.1, -0.1))
    }
  })

  output$fisher_summary <- renderUI({
    total <- sim$n_heads + sim$n_tails
    if(total == 0) {
      div(class = "well",
          p("No data yet. The p-value will be calculated once data arrives."))
    } else {
      p_value <- binom.test(sim$n_heads, total, p = input$null_p)$p.value

      # Determine color based on conventional thresholds
      color <- if(p_value < 0.01) "#c0392b" else if(p_value < 0.05) "#e67e22" else if(p_value < 0.10) "#f39c12" else "#27ae60"

      div(class = "well", style = paste0("border-left: 5px solid ", color),
          h4("Fisher's Evidence"),
          p(tags$b("P-value:"), round(p_value, 4)),
          hr(),
          div(style = paste0("background-color: ", color, "; color: white; padding: 10px; border-radius: 5px;"),
              if(p_value < 0.01) {
                "Very strong evidence against H₀"
              } else if(p_value < 0.05) {
                "Strong evidence against H₀"
              } else if(p_value < 0.10) {
                "Moderate evidence against H₀"
              } else {
                "Little to no evidence against H₀"
              }
          ),
          hr(),
          p("The p-value is the probability of observing ", sim$n_heads, " heads (or more extreme) ",
            "out of ", total, " flips, assuming H₀ (p = ", input$null_p, ") is true."),
          p(strong("Note:"), " Fisher did not advocate for rigid cutoffs like 0.05. ",
            "The p-value is a continuous measure of evidence.")
      )
    }
  })

  # Frequentist CI tab
  output$freq_plot <- renderPlot({
    total <- sim$n_heads + sim$n_tails
    if(total == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Waiting for data...\nConfidence interval will form as data accumulates",
                 size = 6, color = "gray50") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else {
      # Calculate CI using Wilson score interval (better for proportions)
      p_hat <- sim$n_heads / total
      z <- qnorm(0.975)
      denom <- 1 + z^2/total
      center <- (p_hat + z^2/(2*total)) / denom
      margin <- z * sqrt(p_hat*(1-p_hat)/total + z^2/(4*total^2)) / denom

      ci_lower <- center - margin
      ci_upper <- center + margin

      # Plot point estimate and CI
      ggplot() +
        geom_vline(xintercept = input$true_p, color = "red", linetype = "dashed", size = 1.5) +
        annotate("rect", xmin = ci_lower, xmax = ci_upper, ymin = 0, ymax = 1,
                 fill = "blue", alpha = 0.2) +
        geom_point(aes(x = p_hat, y = 0.5), color = "blue", size = 5) +
        geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, y = 0.5),
                       height = 0.1, color = "blue", size = 1.5) +
        theme_minimal() +
        labs(title = "Frequentist Confidence Interval",
             subtitle = paste0("n = ", total, " | 95% CI forming around point estimate"),
             x = "Coin bias (p)", y = "") +
        xlim(0, 1) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        annotate("text", x = input$true_p, y = 0.75,
                 label = "True p", color = "red", hjust = -0.1, size = 5) +
        annotate("text", x = p_hat, y = 0.25,
                 label = paste0("Estimate = ", round(p_hat, 3)),
                 color = "blue", size = 5)
    }
  })

  output$freq_summary <- renderUI({
    total <- sim$n_heads + sim$n_tails
    if(total == 0) {
      div(class = "well",
          p("No data yet. The confidence interval will form once data arrives."))
    } else {
      p_hat <- sim$n_heads / total
      z <- qnorm(0.975)
      denom <- 1 + z^2/total
      center <- (p_hat + z^2/(2*total)) / denom
      margin <- z * sqrt(p_hat*(1-p_hat)/total + z^2/(4*total^2)) / denom

      ci_lower <- center - margin
      ci_upper <- center + margin

      contains_true <- (input$true_p >= ci_lower) && (input$true_p <= ci_upper)
      color <- if(contains_true) "#27ae60" else "#e74c3c"

      div(class = "well", style = paste0("border-left: 5px solid ", color),
          h4("Frequentist Results"),
          p(tags$b("Point estimate:"), round(p_hat, 3)),
          p(tags$b("95% Confidence Interval:"), "[", round(ci_lower, 3), ",", round(ci_upper, 3), "]"),
          p(tags$b("Interval width:"), round(ci_upper - ci_lower, 3)),
          hr(),
          div(style = paste0("background-color: ", color, "; color: white; padding: 10px; border-radius: 5px; text-align: center;"),
              if(contains_true) "✓ Interval captures true p" else "✗ Interval misses true p"
          ),
          hr(),
          p("The confidence interval is constructed using a method that, if repeated many times, ",
            "would contain the true parameter in 95% of cases."),
          p(strong("Important:"), " We cannot say 'there is a 95% probability the true p is in this interval'. ",
            "The true p either is or isn't in the interval - there's no probability about it.")
      )
    }
  })
}

shinyApp(ui, server)
