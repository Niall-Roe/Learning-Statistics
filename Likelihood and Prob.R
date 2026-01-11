library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)

# --- UI: User Interface ---
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Probability Lab: Understanding Probability and Likelihood"),

  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Experiment Setup"),
        sliderInput("t1_n", "Number of coin flips:",
                    min = 5, max = 50, value = 20),
        sliderInput("t1_p", "True coin bias (p):",
                    min = 0, max = 1, value = 0.5, step = 0.01),
        p(class = "text-muted", "p = probability of heads. p=0.5 is a fair coin."),

        actionButton("t1_run", "Flip the Coins!",
                     class = "btn-primary btn-block btn-lg")
      ),

      conditionalPanel("input.t1_run > 0",
                       wellPanel(
                         h4("Your Data:"),
                         verbatimTextOutput("t1_result_text"),
                         div(style = "max-height: 150px; overflow-y: auto; font-family: monospace; font-size: 20px; line-height: 1.2;",
                             uiOutput("t1_coins_display")
                         )
                       ),
                       wellPanel(
                         h4("Detective Work"),
                         p("Move the slider to explore different hypotheses about p:"),
                         sliderInput("t1_guess", "Your hypothesis for p:",
                                     min = 0, max = 1, value = 0.5, step = 0.01),
                         actionButton("t1_snap", "Jump to Best Fit (MLE)",
                                      class = "btn-warning btn-block"),
                         hr(),
                         div(class = "alert alert-success",
                             h5("Interpretation:"),
                             verbatimTextOutput("t1_interpretation")
                         )
                       )
      )
    ),

    mainPanel(
      tabsetPanel(
        # TAB 1: 2D Visualization
        tabPanel("Understanding the Basics",
                 br(),
                 div(class = "alert alert-info",
                     h4("What You'll Learn:"),
                     p("This tab shows two perspectives on the same coin-flipping experiment:"),
                     tags$ul(
                       tags$li(strong("Top Graph (Probability):"), " Before we flip, what outcomes are possible?"),
                       tags$li(strong("Bottom Graph (Likelihood):"), " After we observe data, which p values are most plausible?")
                     )
                 ),
                 plotOutput("t1_prob", height = "300px"),
                 conditionalPanel("input.t1_run > 0",
                                  plotOutput("t1_like", height = "300px"))
        ),

        # TAB 2: 3D Mountain
        tabPanel("The Complete Picture (3D)",
                 br(),
                 div(class = "jumbotron",
                     style = "padding: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 10px;",
                     h2("The Probability Mountain"),
                     p(style = "font-size: 16px;",
                       "The 2D graphs you saw in Tab 1 are just slices through this 3D landscape. ",
                       "Every point on this surface shows P(k | n, p) = the probability of getting k successes in n trials with parameter p."
                     ),
                     p(style = "font-size: 14px; font-style: italic;",
                       "Use the camera buttons to see how different viewpoints give you the 'probability' or 'likelihood' perspective!")
                 ),

                 fluidRow(
                   column(3,
                          wellPanel(
                            h4("Step 1: Run Experiment"),
                            sliderInput("t3_n", "Number of trials:",
                                        min = 10, max = 50, value = 30),
                            sliderInput("t3_true_p", "True parameter (p):",
                                        min = 0.1, max = 0.9, value = 0.5, step = 0.05),
                            actionButton("t3_sim", "Generate Data",
                                         icon = icon("play"),
                                         class = "btn-success btn-block btn-lg"),

                            conditionalPanel("input.t3_sim > 0",
                                             div(class = "alert alert-info", style = "margin-top: 10px;",
                                                 h5("Result:"),
                                                 uiOutput("t3_result_summary")
                                             )
                            ),

                            hr(),

                            h4("Step 2: View Slices"),
                            p("Show cross-sections through the mountain:"),
                            checkboxInput("show_blue",
                                          HTML("<span style='color: #3498db;'>█</span> Blue: Probability slice (fixed p)"),
                                          TRUE),
                            checkboxInput("show_red",
                                          HTML("<span style='color: #e74c3c;'>█</span> Red: Likelihood slice (fixed k)"),
                                          TRUE),

                            conditionalPanel("input.t3_sim === 0",
                                             hr(),
                                             p(class = "text-muted", "Manual controls (active before simulation):"),
                                             sliderInput("t3_manual_p", "Slice at p =",
                                                         min = 0, max = 1, value = 0.5, step = 0.05),
                                             sliderInput("t3_manual_k", "Slice at k =",
                                                         min = 0, max = 50, value = 15)
                            ),

                            hr(),

                            div(class = "well", style = "background-color: #f8f9fa;",
                                h5("Key Concepts:"),
                                tags$ul(
                                  tags$li(strong("Blue line:"), " All possible outcomes for a given p"),
                                  tags$li(strong("Red line:"), " All possible p values for observed k"),
                                  tags$li(strong("Black dot:"), " Where theory meets reality!")
                                )
                            )
                          )
                   ),

                   column(9,
                          div(style = "text-align: center; margin-bottom: 15px; padding: 10px; background-color: #ecf0f1; border-radius: 5px;",
                              h4("Camera Controls - Change Your Perspective:"),
                              actionButton("cam_prob", "Probability View (Side)",
                                           icon = icon("chart-bar"),
                                           style = "background:#3498db; color:white; margin: 5px; padding: 10px 20px; font-weight: bold;"),
                              actionButton("cam_like", "Likelihood View (Front)",
                                           icon = icon("search"),
                                           style = "background:#e74c3c; color:white; margin: 5px; padding: 10px 20px; font-weight: bold;"),
                              actionButton("cam_reset", "3D Isometric View",
                                           icon = icon("cube"),
                                           style = "background:#95a5a6; color:white; margin: 5px; padding: 10px 20px; font-weight: bold;"),
                              p(style = "margin-top: 10px; font-size: 13px; color: #7f8c8d;",
                                "Tip: You can also click and drag to rotate the 3D plot manually!")
                          ),

                          plotlyOutput("plot3d", height = "600px"),

                          div(class = "alert alert-warning", style = "margin-top: 15px;",
                              h5("Understanding the Views:"),
                              tags$ul(
                                tags$li(strong("Probability View (Blue):"), " Looking along the p-axis. You see all possible k outcomes for your chosen p value."),
                                tags$li(strong("Likelihood View (Red):"), " Looking along the k-axis. You see how likely different p values are, given your observed k."),
                                tags$li(strong("3D View:"), " See the full mountain. The two slices are perpendicular cross-sections that intersect at your data point.")
                              )
                          )
                   )
                 )
        ),

        # TAB 3: Explanation of Probability Types
        tabPanel("Concepts: Probability vs Likelihood",
                 br(),
                 div(class = "jumbotron",
                     style = "background-color: #f8f9fa; padding: 30px; border-radius: 10px;",
                     h2("Understanding P(x|θ): Three Different Interpretations"),
                     p(style = "font-size: 16px;",
                       "The expression P(x|θ) appears throughout statistics, but it can mean different things ",
                       "depending on what we consider to be the variable and what we consider to be fixed. ",
                       "This tab explains the three main interpretations.")
                 ),

                 hr(),

                 fluidRow(
                   column(4,
                          div(class = "well", style = "background-color: #e3f2fd; min-height: 400px;",
                              h3(style = "color: #1976d2;", "1. Parameterized Probability"),
                              h4("P(x ; θ) or P_θ(x)"),
                              p(strong("What varies:"), " The data x"),
                              p(strong("What's fixed:"), " The parameter θ"),
                              p(strong("Question:"), " \"Given a model with parameter θ, what outcomes x are possible?\""),
                              hr(),
                              h5("Example:"),
                              p("If we have a fair coin (θ = p = 0.5), and we flip it 10 times (n = 10), ",
                                "what's the probability of getting exactly 7 heads?"),
                              p(code("P(X=7 | n=10, p=0.5) = 0.117")),
                              hr(),
                              h5("Key Point:"),
                              p("This is the", strong("forward problem"), "- we know the model, and we predict data.")
                          )
                   ),

                   column(4,
                          div(class = "well", style = "background-color: #fff3e0; min-height: 400px;",
                              h3(style = "color: #f57c00;", "2. Conditional Probability"),
                              h4("P(x | θ)"),
                              p(strong("What varies:"), " Both x and θ"),
                              p(strong("What's given:"), " θ is a random variable"),
                              p(strong("Question:"), " \"If θ happens to be true, what's the probability of x?\""),
                              hr(),
                              h5("Example (Bayesian):"),
                              p("If the coin bias θ is itself uncertain (maybe θ ~ Beta(2,2)), ",
                                "then P(X=7 | θ=0.6) is a conditional probability - the probability of 7 heads ",
                                "conditional on the bias being 0.6."),
                              p("Formally: P(X=7 | θ=0.6) = P(X=7 ∩ θ=0.6) / P(θ=0.6)"),
                              hr(),
                              h5("Key Point:"),
                              p("This treats both data and parameters as", strong("random variables"),
                                "- common in Bayesian statistics.")
                          )
                   ),

                   column(4,
                          div(class = "well", style = "background-color: #fce4ec; min-height: 400px;",
                              h3(style = "color: #c2185b;", "3. Likelihood"),
                              h4("L(θ | x)"),
                              p(strong("What varies:"), " The parameter θ"),
                              p(strong("What's fixed:"), " The observed data x"),
                              p(strong("Question:"), " \"Given that we observed x, which θ values are more plausible?\""),
                              hr(),
                              h5("Example:"),
                              p("We flipped a coin 10 times and got 7 heads. What bias values p are most consistent with this data?"),
                              p("The MLE (Maximum Likelihood Estimate) is p = 7/10 = 0.7"),
                              p("L(p=0.7 | k=7) > L(p=0.5 | k=7) > L(p=0.2 | k=7)"),
                              hr(),
                              h5("Key Point:"),
                              p("This is the", strong("inverse problem"),
                                "- we have data, and we infer the model. Used in parameter estimation.")
                          )
                   )
                 ),

                 hr(),

                 div(class = "alert alert-info",
                     h4("The Same Formula, Different Perspectives"),
                     p("Mathematically, all three use the same probability mass/density function. ",
                       "The difference is philosophical and computational:"),
                     tags$ul(
                       tags$li(strong("Parameterized Probability:"), " Used for prediction and hypothesis testing"),
                       tags$li(strong("Conditional Probability:"), " Used in Bayesian inference with uncertain parameters"),
                       tags$li(strong("Likelihood:"), " Used for parameter estimation (MLE) and model comparison")
                     ),
                     p(style = "font-style: italic;",
                       "As stated in the Stack Overflow summary: \"It is often a matter of emphasis. ",
                       "The expression P(x|θ) is still a function of both its arguments, so it is rather a matter of emphasis.\"")
                 ),

                 hr(),

                 div(class = "well",
                     h4("Try It Yourself!"),
                     p("Go back to Tab 1 and Tab 2 to see these concepts in action:"),
                     tags$ul(
                       tags$li(strong("Top graph in Tab 1:"), " Shows parameterized probability - fix p, vary k"),
                       tags$li(strong("Bottom graph in Tab 1:"), " Shows likelihood - fix k (observed), vary p"),
                       tags$li(strong("3D view in Tab 2:"), " Shows the complete picture where you can see both perspectives")
                     )
                 )
        )
      )
    )
  )
)

# --- SERVER: Logic ---
server <- function(input, output, session) {

  # === TAB 1 LOGIC ===
  v1 <- reactiveValues(k = NULL, flips = NULL, run = FALSE)

  observeEvent(input$t1_run, {
    # Generate individual flips
    flips <- rbinom(input$t1_n, 1, input$t1_p)
    v1$flips <- flips
    v1$k <- sum(flips)
    v1$run <- TRUE
    updateSliderInput(session, "t1_guess", value = 0.5)
  })

  observeEvent(input$t1_snap, {
    req(v1$k)
    updateSliderInput(session, "t1_guess", value = v1$k / input$t1_n)
  })

  # Show result text
  output$t1_result_text <- renderText({
    req(v1$run)
    sprintf("You got %d heads out of %d flips (%.1f%%)",
            v1$k, input$t1_n, 100 * v1$k / input$t1_n)
  })

  # Show actual coin flips
  output$t1_coins_display <- renderUI({
    req(v1$flips)
    coins <- ifelse(v1$flips == 1, "🟡", "⚪")
    HTML(paste(coins, collapse = " "))
  })

  # Interpretation text
  output$t1_interpretation <- renderText({
    req(v1$run)
    guess_lik <- dbinom(v1$k, input$t1_n, input$t1_guess)
    mle <- v1$k / input$t1_n
    mle_lik <- dbinom(v1$k, input$t1_n, mle)

    sprintf("Your hypothesis (p=%.2f) has likelihood %.4f\nThe best fit (p=%.2f) has likelihood %.4f\n\nYour hypothesis is %.1f%% as good as the best fit.",
            input$t1_guess, guess_lik, mle, mle_lik,
            100 * guess_lik / mle_lik)
  })

  output$t1_prob <- renderPlot({
    x <- 0:input$t1_n
    y <- dbinom(x, input$t1_n, input$t1_p)

    p <- ggplot(data.frame(x, y), aes(x, y)) +
      geom_bar(stat = "identity", fill = "#bdc3c7", color = "black", alpha = 0.7) +
      theme_minimal(base_size = 14) +
      labs(title = "BEFORE: All Possible Outcomes (The Probability Distribution)",
           subtitle = sprintf("If p=%.2f, what could we observe?", input$t1_p),
           x = "Number of Heads (k)",
           y = "Probability") +
      theme(plot.title = element_text(face = "bold", size = 16))

    if (v1$run) {
      p <- p +
        geom_vline(xintercept = v1$k, color = "#e74c3c", size = 2) +
        annotate("text", x = v1$k, y = max(y) * 0.9,
                 label = sprintf("← We observed\nk=%d", v1$k),
                 color = "#e74c3c", fontface = "bold", hjust = -0.1, size = 5)
    }
    p
  })

  output$t1_like <- renderPlot({
    req(v1$run)
    pg <- seq(0, 1, 0.01)
    lg <- dbinom(v1$k, input$t1_n, pg)

    mle <- v1$k / input$t1_n

    ggplot(data.frame(p = pg, l = lg), aes(p, l)) +
      geom_line(color = "#e74c3c", size = 2) +
      geom_vline(xintercept = input$t1_guess, linetype = "dashed",
                 color = "#2c3e50", size = 1.5) +
      geom_vline(xintercept = mle, linetype = "solid",
                 color = "#27ae60", size = 1.5, alpha = 0.5) +
      theme_minimal(base_size = 14) +
      labs(title = "AFTER: Which p Values Are Plausible? (The Likelihood Function)",
           subtitle = sprintf("Given we observed k=%d, which p is most believable?", v1$k),
           x = "Hypothetical Parameter Value (p)",
           y = "Likelihood") +
      annotate("text", x = mle, y = max(lg) * 0.5,
               label = sprintf("Best fit\n(MLE=%.2f)", mle),
               color = "#27ae60", fontface = "bold", hjust = -0.1, size = 4) +
      annotate("text", x = input$t1_guess, y = max(lg) * 0.7,
               label = sprintf("Your guess\n(p=%.2f)", input$t1_guess),
               color = "#2c3e50", fontface = "bold", hjust = -0.1, size = 4) +
      theme(plot.title = element_text(face = "bold", size = 16))
  })

  # === TAB 2 LOGIC (3D MOUNTAIN) ===

  v3 <- reactiveValues(
    sim_k = NULL,
    sim_p = NULL,
    has_run = FALSE,
    camera = list(x = 1.5, y = 1.5, z = 1.5)
  )

  observe({
    updateSliderInput(session, "t3_manual_k", max = input$t3_n)
  })

  observeEvent(input$t3_sim, {
    k <- rbinom(1, size = input$t3_n, prob = input$t3_true_p)
    v3$sim_k <- k
    v3$sim_p <- input$t3_true_p
    v3$has_run <- TRUE
  })

  output$t3_result_summary <- renderUI({
    req(v3$has_run)
    HTML(sprintf(
      "<strong>Observed:</strong> k = %d heads<br>
       <strong>True p:</strong> %.2f<br>
       <strong>Sample proportion:</strong> %.2f<br>
       <span style='color: #27ae60;'>● The black dot shows where these intersect on the mountain!</span>",
      v3$sim_k, v3$sim_p, v3$sim_k / input$t3_n
    ))
  })

  observeEvent(input$cam_prob, { v3$camera <- list(x = 0, y = -2.5, z = 0.3) })
  observeEvent(input$cam_like, { v3$camera <- list(x = -2.5, y = 0, z = 0.3) })
  observeEvent(input$cam_reset, { v3$camera <- list(x = 1.5, y = 1.5, z = 1.5) })

  output$plot3d <- renderPlotly({
    if (v3$has_run) {
      active_p <- v3$sim_p
      active_k <- v3$sim_k
    } else {
      active_p <- input$t3_manual_p
      active_k <- input$t3_manual_k
    }

    p_seq <- seq(0, 1, length.out = 40)
    k_seq <- 0:input$t3_n
    z_matrix <- outer(p_seq, k_seq, function(p, k) dbinom(k, input$t3_n, p))

    plt <- plot_ly() %>%
      add_surface(x = k_seq, y = p_seq, z = z_matrix,
                  colorscale = 'Viridis', opacity = 0.85, showscale = TRUE,
                  colorbar = list(title = "Probability"),
                  contours = list(z = list(show = TRUE, usecolormap = TRUE,
                                           project = list(z = TRUE))),
                  name = "Probability Surface")

    if (input$show_blue) {
      z_blue <- dbinom(k_seq, input$t3_n, active_p)
      plt <- plt %>% add_paths(
        x = k_seq, y = rep(active_p, length(k_seq)), z = z_blue + 0.01,
        line = list(width = 10, color = "#3498db"),
        name = sprintf("Probability (p=%.2f)", active_p)
      )
    }

    if (input$show_red) {
      z_red <- dbinom(active_k, input$t3_n, p_seq)
      plt <- plt %>% add_paths(
        x = rep(active_k, length(p_seq)), y = p_seq, z = z_red + 0.01,
        line = list(width = 10, color = "#e74c3c"),
        name = sprintf("Likelihood (k=%d)", active_k)
      )
    }

    if (v3$has_run) {
      z_point <- dbinom(v3$sim_k, input$t3_n, v3$sim_p)

      plt <- plt %>%
        add_markers(
          x = v3$sim_k, y = v3$sim_p, z = z_point + 0.02,
          marker = list(size = 12, color = "black",
                        line = list(color = "yellow", width = 3)),
          name = "Your Data Point"
        ) %>%
        add_segments(
          x = v3$sim_k, xend = v3$sim_k,
          y = v3$sim_p, yend = v3$sim_p,
          z = 0, zend = z_point,
          line = list(color = "black", dash = "dash", width = 4),
          showlegend = FALSE
        )
    }

    plt <- plt %>% layout(
      scene = list(
        xaxis = list(title = list(text = "Outcome (k = # heads)", font = list(size = 14, color = "black"))),
        yaxis = list(title = list(text = "Parameter (p = coin bias)", font = list(size = 14, color = "black"))),
        zaxis = list(title = list(text = "Probability P(k|n,p)", font = list(size = 14, color = "black"))),
        camera = list(eye = v3$camera)
      ),
      title = list(
        text = "The Complete Probability Space: P(k | n, p)",
        font = list(size = 18, color = "#2c3e50")
      )
    )

    plt
  })
}

shinyApp(ui, server)
