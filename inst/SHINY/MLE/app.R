# app.R
# Shiny App: Maximum Likelihood Estimation Demo for Univariate Distributions
# Encoding: UTF-8
# Author: Assistant
# Date: November 10, 2025

library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(VGAM)  # for Pareto
library(actuar) # for Log-normal (alternative), but we'll use base where possible

# Define UI
ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation (MLE) Demo – Univariate Distributions"),

  tags$head(
    tags$meta(charset = "UTF-8")
  ),

  sidebarLayout(
    sidebarPanel(
      h4("Data Generation"),
      selectInput("dist", "Choose Distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Gamma", "Log-Normal", "Pareto"),
                  selected = "Normal"),

      numericInput("n", "Sample Size (n):", value = 50, min = 10, max = 1000, step = 10),

      conditionalPanel(
        condition = "input.dist == 'Normal'",
        numericInput("true_mu", "True μ:", value = 0, step = 0.1),
        numericInput("true_sigma", "True σ:", value = 1, min = 0.01, step = 0.1)
      ),

      conditionalPanel(
        condition = "input.dist == 'Exponential'",
        numericInput("true_lambda", "True λ (rate):", value = 1, min = 0.01, step = 0.1)
      ),

      conditionalPanel(
        condition = "input.dist == 'Poisson'",
        numericInput("true_lambda_pois", "True λ:", value = 5, min = 0.1, step = 0.5)
      ),

      conditionalPanel(
        condition = "input.dist == 'Gamma'",
        numericInput("true_shape", "True Shape (α):", value = 2, min = 0.1, step = 0.1),
        numericInput("true_rate", "True Rate (β):", value = 1, min = 0.01, step = 0.1)
      ),

      conditionalPanel(
        condition = "input.dist == 'Log-Normal'",
        numericInput("true_mu_ln", "True μ (log-scale):", value = 0, step = 0.1),
        numericInput("true_sigma_ln", "True σ (log-scale):", value = 0.5, min = 0.01, step = 0.1)
      ),

      conditionalPanel(
        condition = "input.dist == 'Pareto'",
        numericInput("true_xm", "True Scale (xm):", value = 1, min = 0.1, step = 0.1),
        numericInput("true_alpha", "True Shape (α):", value = 3, min = 0.5, step = 0.1)
      ),

      actionButton("generate", "Generate New Sample", class = "btn-primary"),

      hr(),

      h4("MLE Optimization"),
      numericInput("tol", "Optimization Tolerance:", value = 1e-6, min = 1e-10, max = 1e-3, step = 1e-6),
      checkboxInput("show_loglik", "Show Log-Likelihood Surface (for 1-2 params)", value = FALSE)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Data & Histogram",
                 plotOutput("hist_plot", height = "400px"),
                 verbatimTextOutput("sample_summary")
        ),
        tabPanel("Log-Likelihood",
                 plotOutput("lik_plot", height = "500px"),
                 verbatimTextOutput("mle_result")
        ),
        tabPanel("MLE Derivation",
                 uiOutput("mle_derivation")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Reactive data generation
  sample_data <- eventReactive(input$generate, {
    set.seed(sample(1:10000, 1))  # Random seed each time
    n <- input$n
    dist <- input$dist

    data <- switch(dist,
                   "Normal" = rnorm(n, mean = input$true_mu, sd = input$true_sigma),
                   "Exponential" = rexp(n, rate = input$true_lambda),
                   "Poisson" = rpois(n, lambda = input$true_lambda_pois),
                   "Gamma" = rgamma(n, shape = input$true_shape, rate = input$true_rate),
                   "Log-Normal" = rlnorm(n, meanlog = input$true_mu_ln, sdlog = input$true_sigma_ln),
                   "Pareto" = rpareto(n, shape = input$true_alpha, scale = input$true_xm)
    )

    list(data = data, dist = dist)
  })

  # MLE computation
  mle_estimates <- reactive({
    data <- sample_data()$data
    dist <- sample_data()$dist
    tol <- input$tol

    if (dist == "Normal") {
      mu_hat <- mean(data)
      sigma_hat <- sd(data)
      loglik <- sum(dnorm(data, mu_hat, sigma_hat, log = TRUE))
      list(mu = mu_hat, sigma = sigma_hat, loglik = loglik, converged = TRUE)

    } else if (dist == "Exponential") {
      lambda_hat <- 1 / mean(data)
      loglik <- sum(dexp(data, lambda_hat, log = TRUE))
      list(lambda = lambda_hat, loglik = loglik, converged = TRUE)

    } else if (dist == "Poisson") {
      lambda_hat <- mean(data)
      loglik <- sum(dpois(data, lambda_hat, log = TRUE))
      list(lambda = lambda_hat, loglik = loglik, converged = TRUE)

    } else if (dist == "Gamma") {
      # Use method of moments as initial guess, then optimize
      s <- log(mean(data)) - mean(log(data))
      shape_init <- (3 - s + sqrt((s - 3)^2 + 24 * s)) / (12 * s)
      if (is.na(shape_init) || shape_init <= 0) shape_init <- 1
      rate_init <- shape_init / mean(data)

      negloglik <- function(par) {
        shape <- par[1]; rate <- par[2]
        if (shape <= 0 || rate <= 0) return(1e10)
        -sum(dgamma(data, shape = shape, rate = rate, log = TRUE))
      }

      opt <- optim(c(shape_init, rate_init), negloglik, method = "L-BFGS-B",
                   lower = c(1e-5, 1e-5), control = list(ftol = tol))

      shape_hat <- opt$par[1]
      rate_hat <- opt$par[2]
      loglik <- -opt$value
      list(shape = shape_hat, rate = rate_hat, loglik = loglik, converged = opt$convergence == 0)

    } else if (dist == "Log-Normal") {
      mu_hat <- mean(log(data))
      sigma_hat <- sd(log(data))
      loglik <- sum(dlnorm(data, mu_hat, sigma_hat, log = TRUE))
      list(mu = mu_hat, sigma = sigma_hat, loglik = loglik, converged = TRUE)

    } else if (dist == "Pareto") {
      xm <- min(data)
      alpha_hat <- length(data) / sum(log(data / xm))
      loglik <- sum(dpareto(data, shape = alpha_hat, scale = xm, log = TRUE))
      list(alpha = alpha_hat, xm = xm, loglik = loglik, converged = TRUE)
    }
  })

  # Histogram + True & MLE density
  output$hist_plot <- renderPlot({
    data <- sample_data()$data
    dist <- sample_data()$dist
    est <- mle_estimates()

    df <- data.frame(x = data)

    p <- ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.6, fill = "lightblue", color = "black") +
      theme_minimal(base_size = 14) +
      labs(title = paste("Histogram of", dist, "Sample (n =", input$n, ")"),
           x = "Value", y = "Density")

    # Add true density
    x_seq <- seq(min(data), max(data), length.out = 300)
    true_dens <- switch(dist,
                        "Normal" = dnorm(x_seq, input$true_mu, input$true_sigma),
                        "Exponential" = dexp(x_seq, input$true_lambda),
                        "Poisson" = dpois(round(x_seq), input$true_lambda_pois),
                        "Gamma" = dgamma(x_seq, input$true_shape, input$true_rate),
                        "Log-Normal" = dlnorm(x_seq, input$true_mu_ln, input$true_sigma_ln),
                        "Pareto" = dpareto(x_seq, input$true_alpha, input$true_xm)
    )

    if (dist != "Poisson") {
      p <- p + geom_line(data = data.frame(x = x_seq, y = true_dens),
                         aes(x, y), color = "red", size = 1.2, linetype = "dashed")
    }

    # Add MLE density
    mle_dens <- switch(dist,
                       "Normal" = dnorm(x_seq, est$mu, est$sigma),
                       "Exponential" = dexp(x_seq, est$lambda),
                       "Poisson" = dpois(round(x_seq), est$lambda),
                       "Gamma" = dgamma(x_seq, est$shape, est$rate),
                       "Log-Normal" = dlnorm(x_seq, est$mu, est$sigma),
                       "Pareto" = dpareto(x_seq, est$alpha, est$xm)
    )

    if (dist != "Poisson") {
      p <- p + geom_line(data = data.frame(x = x_seq, y = mle_dens),
                         aes(x, y), color = "blue", size = 1.2)
    } else {
      # For Poisson, show PMF
      pmf_df <- data.frame(x = 0:max(data), true = dpois(0:max(data), input$true_lambda_pois),
                           mle = dpois(0:max(data), est$lambda))
      p <- ggplot(df, aes(x = x)) +
        geom_histogram(bins = 30, alpha = 0.6, fill = "lightblue", color = "black") +
        geom_point(data = pmf_df, aes(x = x, y = true * length(data)), color = "red", size = 3, shape = 1) +
        geom_point(data = pmf_df, aes(x = x, y = mle * length(data)), color = "blue", size = 3, shape = 4) +
        theme_minimal() + labs(title = "Poisson Sample & PMFs", x = "Count", y = "Frequency")
    }

    if (dist != "Poisson") {
      p <- p + annotate("text", x = Inf, y = Inf, label = "Red dashed: True density",
                        hjust = 1.1, vjust = 2, color = "red", size = 4) +
        annotate("text", x = Inf, y = Inf, label = "Blue: MLE density",
                 hjust = 1.1, vjust = 3.5, color = "blue", size = 4)
    }

    print(p)
  })

  # Log-likelihood surface or profile
  output$lik_plot <- renderPlot({
    data <- sample_data()$data
    dist <- sample_data()$dist
    est <- mle_estimates()

    if (!input$show_loglik) {
      # Show profile log-likelihood for one parameter
      if (dist %in% c("Normal", "Gamma", "Log-Normal")) {
        # We'll profile over first parameter
        if (dist == "Normal") {
          param1 <- est$mu; param2 <- est$sigma
          grid <- seq(param1 - 3*param2/length(data)^0.5, param1 + 3*param2/length(data)^0.5, length.out = 100)
          loglik <- sapply(grid, function(m) sum(dnorm(data, m, param2, log = TRUE)))
          df <- data.frame(param = grid, loglik = loglik)
          p <- ggplot(df, aes(x = param, y = loglik)) +
            geom_line(color = "blue", size = 1) +
            geom_vline(xintercept = param1, color = "red", linetype = "dashed") +
            labs(title = "Profile Log-Likelihood (μ)", x = "μ", y = "Log-Likelihood") +
            theme_minimal()
        } else if (dist == "Gamma") {
          grid <- seq(est$shape * 0.5, est$shape * 1.5, length.out = 100)
          loglik <- sapply(grid, function(s) {
            r <- s / mean(data)
            sum(dgamma(data, s, r, log = TRUE))
          })
          df <- data.frame(param = grid, loglik = loglik)
          p <- ggplot(df, aes(x = param, y = loglik)) +
            geom_line(color = "blue", size = 1) +
            geom_vline(xintercept = est$shape, color = "red", linetype = "dashed") +
            labs(title = "Profile Log-Likelihood (Shape)", x = "Shape (α)", y = "Log-Likelihood") +
            theme_minimal()
        } else if (dist == "Log-Normal") {
          grid <- seq(est$mu - 2, est$mu + 2, length.out = 100)
          loglik <- sapply(grid, function(m) sum(dlnorm(data, m, est$sigma, log = TRUE)))
          df <- data.frame(param = grid, loglik = loglik)
          p <- ggplot(df, aes(x = param, y = loglik)) +
            geom_line(color = "blue", size = 1) +
            geom_vline(xintercept = est$mu, color = "red", linetype = "dashed") +
            labs(title = "Profile Log-Likelihood (μ)", x = "μ (log scale)", y = "Log-Likelihood") +
            theme_minimal()
        }
      } else {
        p <- ggplot() + theme_void() + geom_text(aes(0,0,label = "Profile plot not available")) + xlab(NULL)
      }
      print(p)
      return()
    }

    # Full log-likelihood surface (only for 2-param)
    if (dist %in% c("Normal", "Gamma", "Log-Normal")) {
      if (dist == "Normal") {
        mu_grid <- seq(est$mu - 2, est$mu + 2, length.out = 50)
        sigma_grid <- seq(max(0.01, est$sigma * 0.5), est$sigma * 2, length.out = 50)
        loglik_mat <- outer(mu_grid, sigma_grid, Vectorize(function(m, s) {
          sum(dnorm(data, m, s, log = TRUE))
        }))
        df <- expand.grid(mu = mu_grid, sigma = sigma_grid)
        df$loglik <- as.vector(loglik_mat)

        p <- ggplot(df, aes(x = mu, y = sigma, fill = loglik)) +
          geom_tile() +
          scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
          geom_point(aes(x = est$mu, y = est$sigma), color = "black", size = 4, shape = 4) +
          labs(title = "Log-Likelihood Surface (Normal)", fill = "Log-Lik") +
          theme_minimal()

      } else if (dist == "Gamma") {
        shape_grid <- seq(max(0.1, est$shape * 0.5), est$shape * 2, length.out = 40)
        rate_grid <- seq(max(0.01, est$rate * 0.5), est$rate * 2, length.out = 40)
        loglik_mat <- outer(shape_grid, rate_grid, Vectorize(function(s, r) {
          sum(dgamma(data, s, r, log = TRUE))
        }))
        df <- expand.grid(shape = shape_grid, rate = rate_grid)
        df$loglik <- as.vector(loglik_mat)

        p <- ggplot(df, aes(x = shape, y = rate, fill = loglik)) +
          geom_tile() +
          scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
          geom_point(aes(x = est$shape, y = est$rate), color = "black", size = 4, shape = 4) +
          labs(title = "Log-Likelihood Surface (Gamma)", fill = "Log-Lik") +
          theme_minimal()
      }
    } else {
      p <- ggplot() + theme_void() +
        geom_text(aes(0,0,label = "Log-likelihood surface only for 2-parameter continuous distributions")) +
        xlab(NULL)
    }

    print(p)
  })

  # MLE Results
  output$mle_result <- renderPrint({
    est <- mle_estimates()
    dist <- sample_data()$dist

    cat("=== MLE Results ===\n")
    cat("Distribution: ", dist, "\n")
    cat("Sample size: n =", length(sample_data()$data), "\n")
    cat("Log-Likelihood: ", round(est$loglik, 4), "\n")
    cat("Converged: ", ifelse(est$converged, "Yes", "No"), "\n\n")

    cat("Parameter Estimates:\n")
    if (dist == "Normal") {
      cat(sprintf("  μ̂ = %.4f  (true: %.4f)\n", est$mu, input$true_mu))
      cat(sprintf("  σ̂ = %.4f  (true: %.4f)\n", est$sigma, input$true_sigma))
    } else if (dist == "Exponential") {
      cat(sprintf("  λ̂ = %.4f  (true: %.4f)\n", est$lambda, input$true_lambda))
    } else if (dist == "Poisson") {
      cat(sprintf("  λ̂ = %.4f  (true: %.4f)\n", est$lambda, input$true_lambda_pois))
    } else if (dist == "Gamma") {
      cat(sprintf("  Shape (α̂) = %.4f  (true: %.4f)\n", est$shape, input$true_shape))
      cat(sprintf("  Rate (β̂) = %.4f   (true: %.4f)\n", est$rate, input$true_rate))
    } else if (dist == "Log-Normal") {
      cat(sprintf("  μ̂ (log) = %.4f  (true: %.4f)\n", est$mu, input$true_mu_ln))
      cat(sprintf("  σ̂ (log) = %.4f  (true: %.4f)\n", est$sigma, input$true_sigma_ln))
    } else if (dist == "Pareto") {
      cat(sprintf("  α̂ = %.4f  (true: %.4f)\n", est$alpha, input$true_alpha))
      cat(sprintf("  xm̂ = %.4f (true: %.4f)\n", est$xm, input$true_xm))
    }
  })

  # Sample summary
  output$sample_summary <- renderPrint({
    data <- sample_data()$data
    cat("Sample Summary:\n")
    cat(sprintf("  Min: %.4f\n", min(data)))
    cat(sprintf("  Mean: %.4f\n", mean(data)))
    cat(sprintf("  Median: %.4f\n", median(data)))
    cat(sprintf("  Max: %.4f\n", max(data)))
    cat(sprintf("  SD: %.4f\n", sd(data)))
  })

  # MLE Derivation (LaTeX)
  output$mle_derivation <- renderUI({
    dist <- input$dist
    withMathJax(
      tags$div(
        h3("MLE Derivation"),
        if (dist == "Normal") {
          tags$p("Likelihood: $$ L(\\mu, \\sigma) = \\prod_{i=1}^n \\frac{1}{\\sqrt{2\\pi\\sigma^2}} \\exp\\left(-\\frac{(x_i - \\mu)^2}{2\\sigma^2}\\right) $$",
                 "Log-likelihood: $$ \\ell(\\mu, \\sigma) = -\\frac{n}{2}\\ln(2\\pi) - n\\ln\\sigma - \\frac{1}{2\\sigma^2}\\sum(x_i - \\mu)^2 $$",
                 "∂ℓ/∂μ = 0 → $$ \\hat{\\mu} = \\bar{x} $$",
                 "∂ℓ/∂σ = 0 → $$ \\hat{\\sigma}^2 = \\frac{1}{n}\\sum(x_i - \\bar{x})^2 $$")
        } else if (dist == "Exponential") {
          tags$p("Likelihood: $$ L(\\lambda) = \\lambda^n \\exp(-\\lambda \\sum x_i) $$",
                 "Log-likelihood: $$ \\ell(\\lambda) = n\\ln\\lambda - \\lambda \\sum x_i $$",
                 "dℓ/dλ = 0 → $$ \\hat{\\lambda} = \\frac{n}{\\sum x_i} = \\frac{1}{\\bar{x}} $$")
        } else if (dist == "Poisson") {
          tags$p("Likelihood: $$ L(\\lambda) = \\prod \\frac{\\lambda^{x_i} e^{-\\lambda}}{x_i!} $$",
                 "Log-likelihood: $$ \\ell(\\lambda) = \\sum x_i \\ln\\lambda - n\\lambda - \\sum \\ln(x_i!) $$",
                 "dℓ/dλ = 0 → $$ \\hat{\\lambda} = \\bar{x} $$")
        } else if (dist == "Gamma") {
          tags$p("No closed form. Solve numerically:",
                 "$$ \\ell(\\alpha, \\beta) = n\\alpha\\ln\\beta - n\\ln\\Gamma(\\alpha) + (\\alpha-1)\\sum\\ln x_i - \\beta\\sum x_i $$",
                 "Use Newton-Raphson or BFGS to maximize.")
        } else if (dist == "Log-Normal") {
          tags$p("Let $$ y_i = \\ln x_i \\sim N(\\mu, \\sigma^2) $$ → MLE same as Normal on log-scale.")
        } else if (dist == "Pareto") {
          tags$p("For $$ x_i > x_m $$, $$ f(x) = \\frac{\\alpha x_m^\\alpha}{x^{\\alpha+1}} $$",
                 "$$ \\ell(\\alpha) = n\\ln\\alpha + n\\alpha\\ln x_m - (\\alpha+1)\\sum\\ln x_i $$",
                 "dℓ/dα = 0 → $$ \\hat{\\alpha} = \\frac{n}{\\sum \\ln(x_i / x_m)} $$")
        }
      )
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
