# app.R
library(shiny)
library(ggplot2)
library(gridExtra)

# UI
ui <- fluidPage(
  titlePanel("Severity"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput("scenario", "Select Scenario:",
                  choices = c("Water Plant Temperature" = "water",
                              "Tornado Prediction (Finley)" = "tornado",
                              "Custom Test" = "custom",
                              "Lady Tasting Tea" = "tea")),
      
      conditionalPanel(
        condition = "input.scenario == 'water' || input.scenario == 'custom'",
        
        sliderInput("observed_mean", 
                    "Observed Mean:",
                    min = 148, max = 156, value = 152, step = 0.1),
        
        sliderInput("claim_mu", 
                    "Claim μ Value:",
                    min = 149, max = 155, value = 153, step = 0.1),
        
        sliderInput("alpha", 
                    "Significance Level α:",
                    min = 0.01, max = 0.10, value = 0.025, step = 0.005),
        
        sliderInput("n", 
                    "Sample Size (n):",
                    min = 10, max = 500, value = 100, step = 10),
        
        sliderInput("sigma", 
                    "Population SD (σ):",
                    min = 1, max = 20, value = 10, step = 1),
        
        sliderInput("mu0", 
                    "Null Hypothesis μ₀:",
                    min = 145, max = 155, value = 150, step = 1)
      ),
      
      conditionalPanel(
        condition = "input.scenario == 'tornado'",
        
        h4("Finley's Tornado Predictions"),
        p("2803 occasions in 1884-1891"),
        
        numericInput("tp", "True Positives:", value = 28, min = 0),
        numericInput("tn", "True Negatives:", value = 2680, min = 0),
        numericInput("fp", "False Positives:", value = 72, min = 0),
        numericInput("fn", "False Negatives:", value = 23, min = 0),
        
        hr(),
        
        sliderInput("tornado_claim_tpr", 
                    "Claim: TPR (Sensitivity) >",
                    min = 0, max = 1, value = 0.5, step = 0.01),
        
        sliderInput("tornado_claim_fpr", 
                    "Claim: FPR (1-Specificity) <",
                    min = 0, max = 1, value = 0.1, step = 0.01)
      ),
      
      conditionalPanel(
        condition = "input.scenario == 'tea'",
        
        h4("Lady Tasting Tea (Fisher, 1935)"),
        p("The classical design: 8 cups (4 milk-first, 4 tea-first)."),
        
        numericInput("tea_correct", 
                     "Number correctly identified (out of 8):", 
                     value = 8, min = 0, max = 8, step = 1),
        
        sliderInput("tea_claim", 
                    "Claim: True discrimination ability (prob correct) >",
                    min = 0.5, max = 1, value = 0.75, step = 0.05)
      ),
      
      hr(),
      
      uiOutput("test_decision"),
      
      hr(),
      
      h4("Severity Analysis:"),
      uiOutput("severity_output"),
      
      hr(),
      
      h4("Bayes Factor:"),
      uiOutput("bayes_output")
    ),
    
    mainPanel(
      width = 9,
      
      conditionalPanel(
        condition = "input.scenario == 'water' || input.scenario == 'custom'",
        plotOutput("distribution_plot", height = "500px"),
        
        hr(),
        
        h4("Severity Table:"),
        tableOutput("severity_table")
      ),
      
      conditionalPanel(
        condition = "input.scenario == 'tornado'",
        
        fluidRow(
          column(6, 
                 h4("Confusion Matrix"),
                 tableOutput("confusion_matrix")),
          column(6,
                 h4("Performance Metrics"),
                 tableOutput("performance_metrics"))
        ),
        
        hr(),
        
        plotOutput("roc_plot", height = "400px"),
        
        hr(),
        
        h4("Sampling Distributions for TPR and FPR:"),
        plotOutput("tornado_distributions", height = "500px"),
        
        hr(),
        
        h4("Severity Assessment for Tornado Prediction:"),
        tableOutput("tornado_severity_table")
      ),
      
      conditionalPanel(
        condition = "input.scenario == 'tea'",
        plotOutput("tea_plot", height = "400px"),
        hr()
      ),
      
      hr(),
      
      h4("Interpretation:"),
      uiOutput("interpretation")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values that depend on scenario
  mu0 <- reactive({
    if (input$scenario == "water") 150
    else input$mu0
  })
  
  sigma <- reactive({ input$sigma })
  n <- reactive({ input$n })
  se <- reactive({ sigma() / sqrt(n()) })
  
  # Critical value for continuous test
  critical_value <- reactive({
    mu0() + qnorm(1 - input$alpha) * se()
  })
  
  reject_h0 <- reactive({
    # If continuous inputs not present, safe default FALSE
    if (!(input$scenario %in% c("water", "custom"))) return(FALSE)
    input$observed_mean >= critical_value()
  })
  
  # Severity calculation for continuous test
  severity <- reactive({
    req(input$scenario %in% c("water", "custom"))
    z <- (input$observed_mean - input$claim_mu) / se()
    if (reject_h0()) {
      pnorm(z)
    } else {
      1 - pnorm(z)
    }
  })
  
  # Bayes Factor for continuous test (approximate closed-form for known-σ z test)
  # BF01 (evidence for H0) using Rouder-ish formula adapted for z: stable and sensible
  bayes_factor <- reactive({
    req(input$scenario %in% c("water", "custom"))
    z <- (input$observed_mean - mu0()) / se()
    # use n() from slider input
    nn <- n()
    # guard against nn = 0 (shouldn't happen)
    if (nn <= 0) return(NA)
    # BF01 formula (approximate, for normal with special Cauchy prior idea)
    bf01 <- sqrt(nn / (nn + 1)) * exp((z^2) / (2 * (nn + 1)) - (z^2) / 2)
    bf01
  })
  
  # Tornado metrics
  tornado_metrics <- reactive({
    req(input$scenario == "tornado")
    
    tp <- input$tp
    tn <- input$tn
    fp <- input$fp
    fn <- input$fn
    
    total <- tp + tn + fp + fn
    
    # Basic metrics
    tpr <- if ((tp + fn) == 0) NA else tp / (tp + fn)  # Sensitivity, Recall
    tnr <- if ((tn + fp) == 0) NA else tn / (tn + fp)  # Specificity
    fpr <- if ((fp + tn) == 0) NA else fp / (fp + tn)  # False Positive Rate
    fnr <- if ((fn + tp) == 0) NA else fn / (fn + tp)  # False Negative Rate
    
    ppv <- if ((tp + fp) == 0) NA else tp / (tp + fp)  # Precision, Positive Predictive Value
    npv <- if ((tn + fn) == 0) NA else tn / (tn + fn)  # Negative Predictive Value
    
    accuracy <- if (total == 0) NA else (tp + tn) / total
    
    # For severity calculations, use binomial approximations but guard SD
    tpr_se <- if (is.na(tpr)) NA else sqrt(max(tpr * (1 - tpr) / max((tp + fn),1), 1e-12))
    fpr_se <- if (is.na(fpr)) NA else sqrt(max(fpr * (1 - fpr) / max((fp + tn),1), 1e-12))
    
    list(
      tp = tp, tn = tn, fp = fp, fn = fn, total = total,
      tpr = tpr, tnr = tnr, fpr = fpr, fnr = fnr,
      ppv = ppv, npv = npv, accuracy = accuracy,
      tpr_se = tpr_se, fpr_se = fpr_se
    )
  })
  
  # Test decision output
  output$test_decision <- renderUI({
    if (input$scenario == "tornado") {
      m <- tornado_metrics()
      HTML(paste0(
        "<div style='background-color: #eef; padding: 10px; border-radius: 5px;'>",
        "<strong>Finley's Performance:</strong><br>",
        "TPR (Sensitivity): ", ifelse(is.na(m$tpr), "NA", round(m$tpr, 3)), "<br>",
        "FPR: ", ifelse(is.na(m$fpr), "NA", round(m$fpr, 3)), "<br>",
        "Accuracy: ", ifelse(is.na(m$accuracy), "NA", round(m$accuracy, 3)),
        "</div>"
      ))
    } else if (input$scenario == "tea") {
      r <- tea_results()
      HTML(paste0(
        "<div style='background-color:#eef;padding:10px;border-radius:5px;'>",
        "<strong>Fisher Exact Test</strong><br>",
        "Correct identifications: ", r$k, "/ 8<br>",
        "One-sided p-value: ", round(r$p_value, 4), "<br>",
        if (r$p_value < 0.05) "<strong>Reject Guessing</strong>" else "Do Not Reject Guessing",
        "</div>"
      ))
    } else {
      if (reject_h0()) {
        HTML(paste0(
          "<div style='background-color: #fee; padding: 10px; border-radius: 5px;'>",
          "<strong>Decision: REJECT H₀</strong><br>",
          "Observed (", round(input$observed_mean, 2), 
          ") ≥ Critical (", round(critical_value(), 2), ")<br>",
          "Indication that μ > ", mu0(),
          "</div>"
        ))
      } else {
        HTML(paste0(
          "<div style='background-color: #efe; padding: 10px; border-radius: 5px;'>",
          "<strong>Decision: DO NOT REJECT H₀</strong><br>",
          "Observed (", round(input$observed_mean, 2), 
          ") < Critical (", round(critical_value(), 2), ")",
          "</div>"
        ))
      }
    }
  })
  
  # Severity output
  output$severity_output <- renderUI({
    if (input$scenario == "tornado") {
      m <- tornado_metrics()
      
      # Severity for TPR claim
      z_tpr <- (m$tpr - input$tornado_claim_tpr) / m$tpr_se
      sev_tpr <- pnorm(z_tpr)
      
      # Severity for FPR claim
      z_fpr <- (input$tornado_claim_fpr - m$fpr) / m$fpr_se
      sev_fpr <- pnorm(z_fpr)
      
      HTML(paste0(
        "<div style='background-color: #efe; padding: 10px; border-radius: 5px;'>",
        "<strong>Claim: TPR > ", input$tornado_claim_tpr, "</strong><br>",
        "SEV: ", ifelse(is.nan(sev_tpr), "NA", round(sev_tpr, 3)), "<br><br>",
        "<strong>Claim: FPR < ", input$tornado_claim_fpr, "</strong><br>",
        "SEV: ", ifelse(is.nan(sev_fpr), "NA", round(sev_fpr, 3)),
        "</div>"
      ))
    } else if (input$scenario == "tea") {
      r <- tea_results()
      sev <- r$severity
      theta <- input$tea_claim
      
      color <- if (sev >= 0.84) "#efe" else if (sev >= 0.5) "#ffe" else "#fee"
      
      HTML(paste0(
        "<div style='background-color:", color, ";padding:10px;border-radius:5px;'>",
        "<strong>Claim:</strong> Ability > ", theta, "<br>",
        "<strong>Severity:</strong> ", round(sev, 3), "<br>",
        "</div>"
      ))
    } else {
      sev <- severity()
      claim_text <- if (reject_h0()) {
        paste0("μ > ", input$claim_mu)
      } else {
        paste0("μ ≤ ", input$claim_mu)
      }
      
      sev_level <- if (sev >= 0.95) {
        "Very High"
      } else if (sev >= 0.84) {
        "High"
      } else if (sev >= 0.5) {
        "Moderate"
      } else {
        "Low (BENT)"
      }
      
      color <- if (sev >= 0.84) "#efe" else if (sev >= 0.5) "#ffe" else "#fee"
      
      HTML(paste0(
        "<div style='background-color: ", color, "; padding: 10px; border-radius: 5px;'>",
        "<strong>Claim:</strong> ", claim_text, "<br>",
        "<strong>SEV:</strong> ", round(sev, 3), " (", sev_level, ")<br>",
        "<strong>SE:</strong> ", round(se(), 3),
        "</div>"
      ))
    }
  })
  
  # Bayes factor output
  output$bayes_output <- renderUI({
    if (input$scenario == "tornado") {
      return(HTML("<p>Bayes factors for classification tasks require specifying prior distributions on TPR and FPR.</p>"))
    }
    
    if (input$scenario %in% c("water", "custom")) {
      bf <- bayes_factor()
      interpretation <- if (is.na(bf)) {
        "NA"
      } else if (bf > 100) {
        "Extreme evidence for H₀"
      } else if (bf > 30) {
        "Very strong evidence for H₀"
      } else if (bf > 10) {
        "Strong evidence for H₀"
      } else if (bf > 3) {
        "Moderate evidence for H₀"
      } else if (bf > 1) {
        "Weak evidence for H₀"
      } else if (bf > 1/3) {
        "Weak evidence for H₁"
      } else if (bf > 1/10) {
        "Moderate evidence for H₁"
      } else if (bf > 1/30) {
        "Strong evidence for H₁"
      } else {
        "Very strong evidence for H₁"
      }
      
      HTML(paste0(
        "<div style='background-color: #eef; padding: 10px; border-radius: 5px;'>",
        "<strong>BF₀₁ (approx):</strong> ", ifelse(is.na(bf), "NA", round(bf, 3)), "<br>",
        "<strong>BF₁₀ (approx):</strong> ", ifelse(is.na(bf), "NA", round(1 / bf, 3)), "<br>",
        interpretation,
        "</div>"
      ))
    } else if (input$scenario == "tea") {
      # For tea we show the exact hypergeometric p and a simple BF (beta-binomial vs point null)
      r <- tea_results()
      bf_msg <- paste0("BF (simple, Beta(1,1) marginal / H0 likelihood) ≈ ", round(r$bf_simple, 3))
      HTML(paste0("<div style='background-color:#eef;padding:10px;border-radius:5px;'>", bf_msg, "</div>"))
    } else {
      HTML("<div>NA</div>")
    }
  })
  
  # Distribution plot (continuous case)
  output$distribution_plot <- renderPlot({
    req(input$scenario %in% c("water", "custom"))
    
    x_min <- mu0() - 4 * se()
    x_max <- mu0() + 7 * se()
    x_vals <- seq(x_min, x_max, length.out = 500)
    
    # Data for H0 distribution (RED)
    df_h0 <- data.frame(
      x = x_vals,
      y = dnorm(x_vals, mean = mu0(), sd = se()),
      dist = "H0"
    )
    
    # Data for Claim distribution (BLUE)
    df_claim <- data.frame(
      x = x_vals,
      y = dnorm(x_vals, mean = input$claim_mu, sd = se()),
      dist = "Claim"
    )
    
    df_all <- rbind(df_h0, df_claim)
    
    p <- ggplot() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 11),
        text = element_text(size = 12)
      )
    
    # Add rejection region shading (on H0 distribution)
    if (reject_h0()) {
      p <- p + geom_ribbon(
        data = df_h0[df_h0$x >= critical_value(), ],
        aes(x = x, ymin = 0, ymax = y),
        fill = "red", alpha = 0.2
      ) +
        annotate("text", x = critical_value() + 1.5 * se(), 
                 y = max(df_h0$y) * 0.5,
                 label = "Rejection\nRegion", color = "red", fontface = "bold")
    }
    
    # Add severity region shading (on Claim distribution)
    if (reject_h0()) {
      p <- p + geom_ribbon(
        data = df_claim[df_claim$x <= input$observed_mean, ],
        aes(x = x, ymin = 0, ymax = y),
        fill = "green", alpha = 0.2
      ) +
        annotate("text", x = input$claim_mu - 1.5 * se(), 
                 y = max(df_claim$y) * 0.7,
                 label = paste0("Severity\nRegion\n(", round(severity(), 3), ")"),
                 color = "darkgreen", fontface = "bold")
    } else {
      p <- p + geom_ribbon(
        data = df_claim[df_claim$x >= input$observed_mean, ],
        aes(x = x, ymin = 0, ymax = y),
        fill = "green", alpha = 0.2
      ) +
        annotate("text", x = input$claim_mu + 1.5 * se(), 
                 y = max(df_claim$y) * 0.7,
                 label = paste0("Severity\nRegion\n(", round(severity(), 3), ")"),
                 color = "darkgreen", fontface = "bold")
    }
    
    # Add distribution curves with CORRECTED colors
    p <- p +
      geom_line(data = df_all, aes(x = x, y = y, color = dist, linetype = dist),
                size = 1.2) +
      scale_color_manual(
        name = "",
        values = c("H0" = "red", "Claim" = "blue"),
        labels = c(paste0("H₀: μ = ", mu0()),
                   paste0("Claim: μ = ", input$claim_mu))
      ) +
      scale_linetype_manual(
        name = "",
        values = c("H0" = "solid", "Claim" = "dashed"),
        labels = c(paste0("H₀: μ = ", mu0()),
                   paste0("Claim: μ = ", input$claim_mu))
      )
    
    # Add critical value line
    p <- p +
      geom_vline(xintercept = critical_value(), 
                 color = "orange", linetype = "dotted", size = 1) +
      annotate("text", x = critical_value(), y = max(df_h0$y) * 1.05,
               label = paste0("Critical = ", round(critical_value(), 2)),
               color = "orange", fontface = "bold", hjust = -0.1)
    
    # Add observed value line
    p <- p +
      geom_vline(xintercept = input$observed_mean, 
                 color = "purple", size = 1.5) +
      annotate("text", x = input$observed_mean, y = max(df_h0$y) * 0.95,
               label = paste0("Observed = ", round(input$observed_mean, 2)),
               color = "purple", fontface = "bold", hjust = -0.1)
    
    # Labels
    p <- p +
      labs(
        title = paste0("Sampling Distributions (n=", n(), ", σ=", sigma(), ", SE=", round(se(), 3), ")"),
        x = if (input$scenario == "water") "Temperature (°F)" else "Observed Mean",
        y = "Probability Density"
      ) +
      coord_cartesian(xlim = c(x_min, x_max))
    
    p
  })
  
  # Severity table
  output$severity_table <- renderTable({
    req(input$scenario %in% c("water", "custom"))
    
    if (reject_h0()) {
      mu_vals <- seq(mu0() - 1, mu0() + 5, by = 1)
      sev_vals <- sapply(mu_vals, function(mu1) {
        z <- (input$observed_mean - mu1) / se()
        pnorm(z)
      })
      
      data.frame(
        Claim = paste0("μ > ", mu_vals),
        Severity = round(sev_vals, 3),
        Assessment = ifelse(sev_vals >= 0.84, "Pass",
                            ifelse(sev_vals >= 0.5, "Weak", "BENT"))
      )
    } else {
      mu_vals <- seq(mu0() - 1, mu0() + 5, by = 1)
      sev_vals <- sapply(mu_vals, function(mu1) {
        z <- (input$observed_mean - mu1) / se()
        1 - pnorm(z)
      })
      
      data.frame(
        Claim = paste0("μ ≤ ", mu_vals),
        Severity = round(sev_vals, 3),
        Assessment = ifelse(sev_vals >= 0.84, "Pass",
                            ifelse(sev_vals >= 0.5, "Weak", "BENT"))
      )
    }
  })
  
  # Tornado confusion matrix
  output$confusion_matrix <- renderTable({
    req(input$scenario == "tornado")
    m <- tornado_metrics()
    
    data.frame(
      ` ` = c("Predicted: Tornado", "Predicted: No Tornado"),
      `Actual Tornado` = c(m$tp, m$fn),
      `Actual No Tornado` = c(m$fp, m$tn)
    )
  }, rownames = FALSE)
  
  # Tornado performance metrics
  output$performance_metrics <- renderTable({
    req(input$scenario == "tornado")
    m <- tornado_metrics()
    
    data.frame(
      Metric = c("TPR (Sensitivity)", "TNR (Specificity)", 
                 "FPR", "FNR", "Precision (PPV)", "NPV", "Accuracy"),
      Value = round(c(m$tpr, m$tnr, m$fpr, m$fnr, m$ppv, m$npv, m$accuracy), 3)
    )
  })
  
  # ROC-like plot for tornado
  output$roc_plot <- renderPlot({
    req(input$scenario == "tornado")
    m <- tornado_metrics()
    
    # Calculate Peirce Skill Score
    pss <- ifelse(is.na(m$tpr) | is.na(m$fpr), NA, m$tpr - m$fpr)
    
    ggplot() +
      # Diagonal reference line (random classifier)
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray", size = 1) +
      annotate("text", x = 0.5, y = 0.45, label = "Random\nGuessing", 
               color = "gray40", size = 3, angle = 45) +
      
      # Finley's point
      geom_point(aes(x = m$fpr, y = m$tpr), size = 6, color = "red") +
      geom_text(aes(x = m$fpr, y = m$tpr, label = "Finley"),
                vjust = -1.2, fontface = "bold", size = 4) +
      
      # Line showing PSS
      geom_segment(aes(x = m$fpr, y = m$fpr, xend = m$fpr, yend = m$tpr),
                   color = "purple", size = 1.5, linetype = "solid") +
      annotate("text", x = m$fpr + 0.08, y = (m$fpr + m$tpr)/2,
               label = paste0("PSS = ", round(pss, 3)), color = "purple",
               fontface = "bold", size = 4) +
      
      # Perfect classifier (top-left)
      geom_point(aes(x = 0, y = 1), size = 4, color = "darkgreen") +
      annotate("text", x = 0.05, y = 1, label = "Perfect\nClassifier",
               vjust = 0.5, hjust = 0, color = "darkgreen", size = 3.5, fontface = "bold") +
      
      # Conservative (never predict positive)
      geom_point(aes(x = 0, y = 0), size = 4, color = "blue") +
      annotate("text", x = 0.05, y = 0, label = "Never Predict\nTornado",
               vjust = 0.5, hjust = 0, color = "blue", size = 3.5) +
      
      # Liberal (always predict positive)
      geom_point(aes(x = 1, y = 1), size = 4, color = "orange") +
      annotate("text", x = 0.95, y = 1, label = "Always Predict\nTornado",
               vjust = 0.5, hjust = 1, color = "orange", size = 3.5) +
      
      labs(
        title = paste0("ROC Space: Finley's Tornado Prediction (PSS/TSS = ", ifelse(is.na(pss), "NA", round(pss, 3)), ")"),
        x = "False Positive Rate (FPR)",
        y = "True Positive Rate (TPR / Sensitivity)"
      ) +
      theme_minimal() +
      theme(text = element_text(size = 12),
            plot.title = element_text(face = "bold")) +
      coord_fixed(xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05))
  })
  
  # Tornado distributions plot
  output$tornado_distributions <- renderPlot({
    req(input$scenario == "tornado")
    m <- tornado_metrics()
    
    # Create distributions for TPR and FPR
    tpr_center <- ifelse(is.na(m$tpr), 0, m$tpr)
    fpr_center <- ifelse(is.na(m$fpr), 0, m$fpr)
    tpr_se <- ifelse(is.na(m$tpr_se), 1e-6, m$tpr_se)
    fpr_se <- ifelse(is.na(m$fpr_se), 1e-6, m$fpr_se)
    
    tpr_vals <- seq(max(0, tpr_center - 4*tpr_se), min(1, tpr_center + 4*tpr_se), length.out = 500)
    fpr_vals <- seq(max(0, fpr_center - 4*fpr_se), min(1, fpr_center + 4*fpr_se), length.out = 500)
    
    df_tpr <- data.frame(
      x = tpr_vals,
      y = dnorm(tpr_vals, mean = tpr_center, sd = tpr_se),
      metric = "TPR"
    )
    
    df_fpr <- data.frame(
      x = fpr_vals,
      y = dnorm(fpr_vals, mean = fpr_center, sd = fpr_se),
      metric = "FPR"
    )
    
    # Scale FPR distribution to match TPR height for visibility (if both exist)
    scale_factor <- ifelse(max(df_fpr$y, na.rm = TRUE) == 0, 1, max(df_tpr$y, na.rm = TRUE) / max(df_fpr$y, na.rm = TRUE))
    df_fpr$y <- df_fpr$y * scale_factor * 0.8  # Scale down slightly for clarity
    
    df_all <- rbind(df_tpr, df_fpr)
    
    p <- ggplot() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top",
        text = element_text(size = 12)
      )
    
    # Shade severity regions based on current claims
    # For TPR > claim
    tpr_claim <- input$tornado_claim_tpr
    if (!is.na(m$tpr) && m$tpr > tpr_claim) {
      p <- p + geom_ribbon(
        data = df_tpr[df_tpr$x <= m$tpr, ],
        aes(x = x, ymin = 0, ymax = y),
        fill = "green", alpha = 0.2
      )
    }
    
    # For FPR < claim  
    fpr_claim <- input$tornado_claim_fpr
    if (!is.na(m$fpr) && m$fpr < fpr_claim) {
      p <- p + geom_ribbon(
        data = df_fpr[df_fpr$x >= m$fpr, ],
        aes(x = x, ymin = 0, ymax = y),
        fill = "blue", alpha = 0.2
      )
    }
    
    # Draw the distributions
    p <- p +
      geom_line(data = df_all, aes(x = x, y = y, color = metric, linetype = metric),
                size = 1.2) +
      scale_color_manual(
        name = "Metric",
        values = c("TPR" = "darkgreen", "FPR" = "darkred")
      ) +
      scale_linetype_manual(
        name = "Metric",
        values = c("TPR" = "solid", "FPR" = "dashed")
      )
    
    # Add observed values if present
    if (!is.na(m$tpr)) {
      p <- p + geom_vline(xintercept = m$tpr, color = "darkgreen", size = 1.5, alpha = 0.7) +
        annotate("text", x = m$tpr, y = max(df_tpr$y, na.rm = TRUE) * 0.9,
                 label = paste0("Observed TPR\n", round(m$tpr, 3)),
                 color = "darkgreen", fontface = "bold", hjust = -0.1)
    }
    if (!is.na(m$fpr)) {
      p <- p + geom_vline(xintercept = m$fpr, color = "darkred", size = 1.5, alpha = 0.7) +
        annotate("text", x = m$fpr, y = max(df_tpr$y, na.rm = TRUE) * 0.7,
                 label = paste0("Observed FPR\n", round(m$fpr, 3)),
                 color = "darkred", fontface = "bold", hjust = 1.1)
    }
    
    # Add claim lines
    p <- p +
      geom_vline(xintercept = tpr_claim, color = "green", size = 1, 
                 linetype = "dotted", alpha = 0.7) +
      annotate("text", x = tpr_claim, y = max(df_tpr$y, na.rm = TRUE) * 1.0,
               label = paste0("Claim: TPR > ", round(tpr_claim, 2)),
               color = "green", size = 3, hjust = -0.1) +
      geom_vline(xintercept = fpr_claim, color = "red", size = 1, 
                 linetype = "dotted", alpha = 0.7) +
      annotate("text", x = fpr_claim, y = max(df_tpr$y, na.rm = TRUE) * 0.5,
               label = paste0("Claim: FPR < ", round(fpr_claim, 2)),
               color = "red", size = 3, hjust = -0.1)
    
    p <- p +
      labs(
        title = paste0("Sampling Distributions (TPR: n=", m$tp + m$fn, 
                       ", SE=", round(m$tpr_se, 3), 
                       "; FPR: n=", m$fp + m$tn, ", SE=", round(m$fpr_se, 3), ")"),
        x = "Rate",
        y = "Probability Density (scaled for visibility)"
      ) +
      coord_cartesian(xlim = c(0, 1))
    
    p
  })
  
  output$tornado_severity_table <- renderTable({
    req(input$scenario == "tornado")
    m <- tornado_metrics()
    
    tpr_vals <- seq(0.1, 0.9, by = 0.1)
    fpr_vals <- seq(0.01, 0.15, by = 0.02)
    
    tpr_sev <- sapply(tpr_vals, function(claim_tpr) {
      z <- (m$tpr - claim_tpr) / m$tpr_se
      pnorm(z)
    })
    
    fpr_sev <- sapply(fpr_vals, function(claim_fpr) {
      z <- (claim_fpr - m$fpr) / m$fpr_se
      pnorm(z)
    })
    
    # Make sure both have the same length by padding the shorter one
    max_len <- max(length(tpr_vals), length(fpr_vals))
    
    tpr_claim_text <- c(paste0("TPR > ", round(tpr_vals, 2)), 
                        rep("", max_len - length(tpr_vals)))
    tpr_sev_text <- c(as.character(round(tpr_sev, 3)), 
                      rep("", max_len - length(tpr_vals)))
    tpr_assess <- c(ifelse(tpr_sev >= 0.84, "Pass",
                           ifelse(tpr_sev >= 0.5, "Weak", "BENT")),
                    rep("", max_len - length(tpr_vals)))
    
    fpr_claim_text <- c(paste0("FPR < ", round(fpr_vals, 2)), 
                        rep("", max_len - length(fpr_vals)))
    fpr_sev_text <- c(as.character(round(fpr_sev, 3)), 
                      rep("", max_len - length(fpr_vals)))
    fpr_assess <- c(ifelse(fpr_sev >= 0.84, "Pass",
                           ifelse(fpr_sev >= 0.5, "Weak", "BENT")),
                    rep("", max_len - length(fpr_vals)))
    
    data.frame(
      `TPR Claim` = tpr_claim_text,
      `Severity` = tpr_sev_text,
      `Assessment` = tpr_assess,
      ` ` = rep(" | ", max_len),
      `FPR Claim` = fpr_claim_text,
      `Severity ` = fpr_sev_text,
      `Assessment ` = fpr_assess,
      check.names = FALSE
    )
  }, colnames = TRUE)
  
  # --- Lady Tasting Tea ---
  tea_results <- reactive({
    req(input$scenario == "tea")
    
    k <- input$tea_correct
    n_t <- 8L  # classical design fixed to 8
    # hypergeometric: number of successes in 4 selected when there are 4 special items
    # P(X >= k) under H0 (random guessing) for number of correct among 4 "milk-first" choices
    # Using dhyper over support 0..4
    # One-sided p-value P(X >= k)
    p_value <- if (k <= 4) sum(dhyper(k:4, 4, 4, 4)) else 0
    # severity: probability of observing result at least as favorable (>= k) under claimed ability theta
    theta <- input$tea_claim
    # We use binomial as a capability model (as discussed): Pr(X >= k | p = theta)
    sev <- pbinom(k - 1, size = n_t, prob = theta, lower.tail = FALSE)
    
    # Simple BF: marginal likelihood under Beta(1,1) vs H0 likelihood at p=0.5
    # marginal = choose(n,k) * Beta(1+k,1+n-k) / Beta(1,1)  -> equivalently: Beta(k+1, n-k+1)
    marginal <- choose(n_t, k) * beta(1 + k, 1 + n_t - k)  # predictive for observed k under uniform prior
    lik0 <- dhyper(k, 4, 4, 4)  # likelihood under hypergeometric null for exactly k
    # Note: mixing models (binomial vs hypergeometric) — this BF is a pragmatic, simple comparison
    bf_simple <- ifelse(lik0 > 0, marginal / lik0, NA)
    
    list(k = k, p_value = p_value, severity = sev, bf_simple = bf_simple)
  })
  
  output$tea_plot <- renderPlot({
    req(input$scenario == "tea")
    r <- tea_results()
    xs <- 0:4
    probs <- dhyper(xs, 4, 4, 4)
    df <- data.frame(x = xs, p = probs)
    df$highlight <- df$x == r$k
    
    ggplot(df, aes(x = factor(x), y = p, fill = highlight)) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "skyblue"), guide = FALSE) +
      labs(
        title = "Hypergeometric Distribution Under Guessing (4 milk-first in 8 cups)",
        x = "Correct Identifications (out of 8; possible counts 0..4)",
        y = "Probability (H0: guessing)"
      ) +
      theme_minimal()
  })
  
  # Interpretation
  output$interpretation <- renderUI({
    if (input$scenario == "tornado") {
      m <- tornado_metrics()
      pss <- ifelse(is.na(m$tpr) | is.na(m$fpr), NA, m$tpr - m$fpr)
      
      HTML(paste0(
        "<h4>Finley's Tornado Predictions (1884-1891)</h4>",
        "<p>Finley predicted tornadoes with high accuracy (", ifelse(is.na(m$accuracy), "NA", round(m$accuracy, 3)), 
        "), but this is misleading! Tornadoes are rare events.</p>",
        
        "<p><strong>The Base Rate Problem:</strong> With only ", ifelse(is.na(m$tp) | is.na(m$fn), "NA", m$tp + m$fn), 
        " actual tornadoes out of ", m$total, " occasions (", 
        ifelse(is.na(m$tp) | is.na(m$fn), "NA", round((m$tp + m$fn)/m$total * 100, 1)), "%), ",
        "even a naive 'always predict no tornado' strategy achieves ", 
        ifelse(is.na(m$tn) | is.na(m$total), "NA", round(m$tn / m$total, 3)), " accuracy!</p>",
        
        "<h5>Peirce Skill Score (PSS) / True Skill Statistic (TSS):</h5>",
        "<p><strong>PSS = TPR - FPR = ", ifelse(is.na(pss), "NA", round(pss, 3)), "</strong></p>",
        "<p>The PSS is the <strong>vertical distance</strong> from Finley's point to the diagonal ",
        "line in the ROC chart. It measures skill above random guessing:</p>",
        "<ul>",
        "<li><strong>PSS = 1:</strong> Perfect classifier (top-left corner)</li>",
        "<li><strong>PSS = 0:</strong> No better than random guessing (on diagonal)</li>",
        "<li><strong>PSS < 0:</strong> Worse than random!</li>",
        "<li><strong>Finley's PSS = ", ifelse(is.na(pss), "NA", round(pss, 3)), ":</strong> Check sensitivity vs specificity.</li>",
        "</ul>",
        
        "<p><strong>Why PSS matters:</strong> Unlike accuracy, PSS is not inflated by class imbalance. ",
        "It equally weights performance on both classes.</p>",
        
        "<h5>Sampling Distributions:</h5>",
        "<p>The distribution plot shows the sampling distributions for TPR and FPR based on the observed data. ",
        "The shaded green area under TPR shows the severity region for testing claims like 'TPR > ", 
        input$tornado_claim_tpr, "'. The shaded blue area under FPR shows severity for 'FPR < ", 
        input$tornado_claim_fpr, "'.</p>",
        
        "<p><strong>Key Issues:</strong></p>",
        "<ul>",
        "<li><strong>Low TPR:</strong> Finley missed some actual tornadoes.</li>",
        "<li><strong>Low Precision:</strong> Many predicted tornadoes were false positives.</li>",
        "<li><strong>Severity Analysis:</strong> Adjust the claim sliders to see which ",
        "claims about Finley's true TPR and FPR pass severely.</li>",
        "</ul>",
        
        "<p><strong>Lesson:</strong> Accuracy alone can be very misleading for imbalanced ",
        "datasets. The ROC chart, PSS, and severity analysis reveal the true discriminatory ",
        "power of the classifier and what we can legitimately infer about its performance.</p>"
      ))
    } else if (input$scenario == "tea") {
      r <- tea_results()
      
      return(HTML(paste0(
        "<h4>Lady Tasting Tea</h4>",
        "<p>Under Fisher’s design, guessing corresponds to a hypergeometric distribution with 8 cups and exactly 4 ‘milk-first’ cups.</p>",
        "<p>The one-sided p-value tests whether success counts as extreme as ", r$k, "/8 would be likely under chance guessing (H0).</p>",
        "<p>The severity calculation answers a different question: assuming a claimed true discrimination ability of ", input$tea_claim, ", ",
        "what was the probability of observing a result at least as favorable as ", r$k, "?</p>",
        "<p>Severity = ", round(r$severity, 3), 
        ". Simple BF (Beta(1,1) marginal / H0 likelihood) ≈ ", round(r$bf_simple, 3), ".</p>"
      )))
    } else {
      if (reject_h0()) {
        HTML(paste0(
          "<p><strong>Rejection Case:</strong></p>",
          "<p>Observed mean (", round(input$observed_mean, 2), 
          ") ≥ Critical value (", round(critical_value(), 2), 
          "), so we reject H₀: μ ≤ ", mu0(), ".</p>",
          "<p><strong>Severity:</strong> For claim μ > μ₁, severity = Pr(X̄ ≤ observed | μ = μ₁). ",
          "High severity (≥0.84) means we'd rarely get a result this small if the claim were true.</p>",
          "<p><strong>Current Parameters:</strong> n=", n(), ", σ=", sigma(), 
          ", SE=", round(se(), 3), "</p>",
          "<p>The green region shows severity. Larger n gives smaller SE and sharper tests.</p>"
        ))
      } else {
        HTML(paste0(
          "<p><strong>Non-Rejection Case:</strong></p>",
          "<p>Observed mean (", round(input$observed_mean, 2), 
          ") < Critical value (", round(critical_value(), 2), 
          "), so we do not reject H₀.</p>",
          "<p><strong>Severity:</strong> For claim μ ≤ μ₁, severity = Pr(X̄ ≥ observed | μ = μ₁). ",
          "This tests if we had good capability to detect a violation.</p>",
          "<p><strong>Current Parameters:</strong> n=", n(), ", σ=", sigma(), 
          ", SE=", round(se(), 3), "</p>"
        ))
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
