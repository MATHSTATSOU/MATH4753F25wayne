# R Shiny App Code
# Save this as app.R and run with shiny::runApp()

library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Demonstration of Weak and Strong Law of Large Numbers"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Number of trials (n):", min = 100, max = 10000, value = 1000, step = 100),
      sliderInput("num_paths", "Number of sample paths:", min = 1, max = 50, value = 10),
      numericInput("p", "Probability of success (for Bernoulli):", value = 0.5, min = 0, max = 1),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Convergence Plot",
                 plotOutput("conv_plot"),
                 p("This plot shows multiple sample paths of the cumulative average for Bernoulli trials.
                   All paths converge to the true mean (p = 0.5 by default) as n increases, illustrating
                   the Strong Law (almost sure convergence across paths).")
        ),
        tabPanel("Deviation Probability",
                 plotOutput("dev_plot"),
                 p("This histogram shows the distribution of averages at the final n, with the probability
                   of deviation > epsilon (e.g., 0.05) highlighted, demonstrating the Weak Law (convergence in probability).")
        ),
        tabPanel("Explanation",
                 h4("Weak Law of Large Numbers (WLLN)"),
                 p("Convergence in probability: The probability that the sample mean deviates from the true mean by more than epsilon goes to 0 as n increases."),
                 h4("Strong Law of Large Numbers (SLLN)"),
                 p("Almost sure convergence: For almost all sample paths, the sample mean converges to the true mean as n increases."),
                 p("In this app, the convergence plot visualizes SLLN by showing paths converging. The deviation plot illustrates WLLN by showing shrinking probability of large deviations.")
        )
      )
    )
  )
)

server <- function(input, output) {
  sim_data <- reactiveVal()

  observeEvent(input$simulate, {
    n <- input$n
    num_paths <- input$num_paths
    p <- input$p

    paths <- matrix(rbinom(n * num_paths, 1, p), nrow = num_paths)
    cum_avgs <- t(apply(paths, 1, function(x) cumsum(x) / (1:n)))

    sim_data(list(cum_avgs = cum_avgs, n = n, p = p))
  })

  output$conv_plot <- renderPlot({
    data <- sim_data()
    if (is.null(data)) return(NULL)

    df <- data.frame(
      trial = rep(1:data$n, data$cum_avgs %>% nrow()),
      avg = as.vector(t(data$cum_avgs)),
      path = rep(1:nrow(data$cum_avgs), each = data$n)
    )

    ggplot(df, aes(x = trial, y = avg, group = path, color = factor(path))) +
      geom_line(alpha = 0.7) +
      geom_hline(yintercept = data$p, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Sample Paths of Cumulative Averages",
           x = "Number of Trials", y = "Cumulative Average") +
      theme(legend.position = "none")
  })

  output$dev_plot <- renderPlot({
    data <- sim_data()
    if (is.null(data)) return(NULL)

    # Simulate many more for deviation prob (to estimate WLLN)
    num_sims <- 1000
    final_avgs <- rowMeans(matrix(rbinom(data$n * num_sims, 1, data$p), nrow = num_sims))

    epsilon <- 0.05
    dev_prob <- mean(abs(final_avgs - data$p) > epsilon)

    df <- data.frame(avg = final_avgs)

    ggplot(df, aes(x = avg)) +
      geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
      geom_vline(xintercept = data$p, linetype = "dashed", color = "red") +
      geom_vline(xintercept = data$p + epsilon, linetype = "dotted", color = "orange") +
      geom_vline(xintercept = data$p - epsilon, linetype = "dotted", color = "orange") +
      theme_minimal() +
      labs(title = paste("Distribution of Averages at n =", data$n, "(Deviation Prob > 0.05:", round(dev_prob, 3), ")"),
           x = "Sample Average", y = "Count")
  })
}

shinyApp(ui = ui, server = server)
