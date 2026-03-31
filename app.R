library(shiny)
library(plotly)
library(tidyverse)
library(MASS)

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("Neural Population Response Simulator"),
  sidebarLayout(
    sidebarPanel(
      # Contrast level for each stimulus (determines peak firing rate via Naka-Rushton)
      sliderInput("contrast1", "Contrast 1 (%)", min = 1, max = 100, value = 40),
      sliderInput("contrast2", "Contrast 2 (%)", min = 1, max = 100, value = 40),
      # Number of simulated trials per condition
      sliderInput("n_trials",
                  label = "Number of trials",
                  min = 50, max = 500, value = 100, step = 50),
      # Gain variability (sigma of gamma distribution):
      # low = small trial-to-trial fluctuation, high = large fluctuation
      sliderInput("sigma_g_low",  "Gain variability (low)",  0.01, 0.3, 0.05, 0.01),
      sliderInput("sigma_g_high", "Gain variability (high)", 0.1,  0.5, 0.2,  0.01),
      # Stimulus orientations to discriminate (degrees)
      sliderInput("stim1", "Stimulus 1 (°)", min = 1, max = 180, value = 90, step = 1),
      sliderInput("stim2", "Stimulus 2 (°)", min = 1, max = 180, value = 91, step = 1),
      actionButton("run", "Run simulation", class = "btn-primary")
    ),
    mainPanel(
      # Top row: Naka-Rushton curve and tuning curves
      h4("Naka-Rushton & Tuning curves"),
      fluidRow(
        column(6, plotOutput("g_nr")),       # contrast -> peak firing rate
        column(6, plotOutput("g_tuning"))    # orientation tuning curves
      ),
      hr(),
      h4("Total spike count"),
      plotOutput("g_density"),    # distribution of summed population spikes across trials
      hr(),
      h4("3D scatter"),
      plotlyOutput("p_3d"),       # 3 selected neurons in 3D spike space
      hr(),
      h4("Decision boundary"),
      plotlyOutput("p_boundary"), # logistic regression boundary separating stim1 vs stim2
      hr(),
      h4("LDA projection (2D)"),
      plotOutput("g_lda_2d"),     # 2D scatter with LDA discriminant axis w and mean difference df
      hr(),
      h4("LDA projection (1D)"),
      plotOutput("g_lda_1d")      # 1D projection onto w, with d' from projected distributions
    )
  )
)


# ============================================================
# Server
# ============================================================
server <- function(input, output) {
  
  # ---- sim_result ----
  # Runs when "Run simulation" is clicked.
  # Generates spike responses for two stimuli under two levels of gain variability.
  sim_result <- eventReactive(input$run, {
    withProgress(message = "Simulating...", value = 0, {
      
      # Naka-Rushton function: maps contrast -> peak firing rate
      # R(c) = Rmax * c^n / (c^n + C50^n)
      # add citation
      Rmax <- 115; C50 <- 19.3; n_nr <- 2.9
      
      max_fr1 <- Rmax * input$contrast1^n_nr / (input$contrast1^n_nr + C50^n_nr)
      max_fr2 <- Rmax * input$contrast2^n_nr / (input$contrast2^n_nr + C50^n_nr)
      
      n_neurons <- 180   # one neuron per degree of preferred orientation
      orientations <- seq(1, 180)
      preferred_orientations <- seq(1, 180, length.out = n_neurons)
      tuning_width <- 20  # Gaussian tuning width (SD in degrees)
      n_trials <- input$n_trials
      
      # Build Gaussian tuning curves for a given peak firing rate
      # 高さだけがコントラストに応じて変わる
      make_tuning_curves <- function(max_fr) {
        tc <- matrix(0, nrow = n_neurons, ncol = 180)
        for (i in 1:n_neurons) {
          # Circular Gaussian: wraps at 180 degrees
          tc[i, ] <- max_fr * exp(
            -0.5 * (pmin(abs(orientations - preferred_orientations[i]),
                         180 - abs(orientations - preferred_orientations[i]))^2
            ) / tuning_width^2
          )
        }
        tc
      }
      
      tuning_curves1 <- make_tuning_curves(max_fr1)  # tuning curves for stim1's contrast
      tuning_curves2 <- make_tuning_curves(max_fr2)  # tuning curves for stim2's contrast
      
      incProgress(0.2, message = "Running simulation...")
      
      # Simulate one trial:
      # 1. Sample a global gain g ~ Gamma(1/sigma_g^2, sigma_g^2); Goris et al. (2014) 
      #    -> gain variability: all neurons scaled by the same g within a trial
      #　ガンマ分布を使う以上，gainが1を超えることがありうるけどいいのかな
      # 2. Draw spikes from negative binomial (approx. Poisson when size is large)　
      #サイズ大きくして一旦ファノファクターは考えない()
      simulate <- function(stim, sigma_g, n_trials, tuning_curves) {
        do.call(rbind, lapply(1:n_trials, function(i) {
          idx <- max(1, min(180, round(stim)))
          g   <- rgamma(1, shape = 1/sigma_g^2, scale = sigma_g^2)
          mu  <- g * tuning_curves[, idx]   # gain-scaled mean firing rates
          mu[mu == 0] <- 1e-8               # avoid log(0)
          resp <- rnbinom(n_neurons, size = 1000000, mu = mu)  # approx. Poisson
          data.frame(Spikes = resp,
                     Neuron = seq_len(n_neurons),
                     Stimulus = stim,
                     Trial = i)
        }))
      }
      
      incProgress(0.3, message = "High GV...")
      df_high <- rbind(
        simulate(input$stim1, input$sigma_g_high, n_trials, tuning_curves1),
        simulate(input$stim2, input$sigma_g_high, n_trials, tuning_curves2)
      ) %>% mutate(GV = "high")
      
      incProgress(0.3, message = "Low GV...")
      df_low <- rbind(
        simulate(input$stim1, input$sigma_g_low, n_trials, tuning_curves1),
        simulate(input$stim2, input$sigma_g_low, n_trials, tuning_curves2)
      ) %>% mutate(GV = "low")
      
      incProgress(0.2, message = "Done!")
      rbind(df_high, df_low)
    })
  })
  
  # ---- lda_result ----
  # Computes LDA (Fisher's Linear Discriminant) on a simplified 2-neuron model.
  # Uses sigma_g_low as the gain variability level.
  # Returns the discriminant axis w, projections, and d'.
  lda_result <- eventReactive(input$run, {
    withProgress(message = "Computing LDA...", {
      
      sigma_g <- input$sigma_g_low
      n       <- input$n_trials
      
      # Fixed mean spike counts for the two neurons under two stimulus classes.
      # Must be positive so that gain multiplication does not produce negatives.
      mu_0 <- c(30, 29)  # class 0: neuron1 more active
      mu_1 <- c(29, 30)  # class 1: neuron2 more active
      Sigma_0 <- matrix(c(1.0, 0, 0, 1.0), nrow = 2)  # independent noise
      Sigma_1 <- matrix(c(1.0, 0, 0, 1.0), nrow = 2)
      
      # Generate base spike counts from multivariate normal
      data0 <- mvrnorm(n, mu = mu_0, Sigma = Sigma_0)
      data1 <- mvrnorm(n, mu = mu_1, Sigma = Sigma_1)
      
      # Apply gain variability: multiply each trial's responses by a shared gain factor.
      # This introduces correlated variability across neurons within each trial.
      gain0 <- rgamma(n, shape = 1/sigma_g^2, scale = sigma_g^2)
      gain1 <- rgamma(n, shape = 1/sigma_g^2, scale = sigma_g^2)
      data0[, 1] <- data0[, 1] * gain0
      data0[, 2] <- data0[, 2] * gain0
      data1[, 1] <- data1[, 1] * gain1
      data1[, 2] <- data1[, 2] * gain1
      
      data <- data.frame(
        class   = factor(c(rep(0, n), rep(1, n))),
        neuron1 = c(data0[, 1], data1[, 1]),
        neuron2 = c(data0[, 2], data1[, 2])
      )
      
      # Compute mean difference vector: df = mu1 - mu0
      neurons   <- c("neuron1", "neuron2")
      data0_sub <- data[data$class == 0, neurons]
      data1_sub <- data[data$class == 1, neurons]
      df_vec    <- colMeans(data1_sub) - colMeans(data0_sub)
      
      # Pooled within-class covariance with Tikhonov regularization.
      # Regularization (lambda * I) prevents singularity when neurons are correlated.
      lambda_reg <- 0.01
      Sigma_w    <- (cov(data0) + cov(data1)) / 2
      Sigma_w    <- Sigma_w + lambda_reg * diag(length(neurons))
      
      # Fisher's LDA discriminant axis: w = Sigma^{-1} * df
      # Maximizes between-class separation relative to within-class variance.
      w        <- solve(Sigma_w) %*% df_vec
      w_scaled <- as.numeric(w)
      mean0    <- colMeans(data0)
      mean1    <- colMeans(data1)
      
      # Linear Fisher Information: LFI = df^T * Sigma^{-1} * df
      # d' = sqrt(LFI): theoretical discriminability along the optimal axis
      lfi <- t(df_vec) %*% solve(Sigma_w) %*% df_vec
      dp  <- sqrt(lfi)
      
      # Project data onto w: scalar projection = x . w
      proj0 <- as.numeric(data0 %*% w)
      proj1 <- as.numeric(data1 %*% w)
      mu_proj0 <- mean(proj0)
      mu_proj1 <- mean(proj1)
      # Empirical d' from the projected 1D distributions
      sd_proj  <- sqrt((var(proj0) + var(proj1)) / 2)  # pooled SD
      dp_1d    <- (mu_proj1 - mu_proj0) / sd_proj
      
      list(
        data     = data,
        data0    = data0,
        data1    = data1,
        df_vec   = df_vec,
        w_scaled = w_scaled,
        mean0    = mean0,
        mean1    = mean1,
        dp       = dp,
        proj0    = proj0,
        proj1    = proj1,
        dp_1d    = dp_1d,
        mu_proj0 = mu_proj0,
        mu_proj1 = mu_proj1,
        n        = n
      )
    })
  })
  
  # ---- Naka-Rushton plot ----
  # Shows the full contrast-response function.
  # Red dots mark the peak firing rates corresponding to contrast1 (S1) and contrast2 (S2).
  output$g_nr <- renderPlot({
    req(input$contrast1, input$contrast2, input$stim1, input$stim2)
    Rmax <- 115; C50 <- 19.3; n_nr <- 2.9
    contrast_seq <- seq(0, 100, 0.1)
    nr <- data.frame(
      Contrast   = contrast_seq,
      Max_firing = Rmax * contrast_seq^n_nr / (contrast_seq^n_nr + C50^n_nr)
    )
    
    c1 <- input$contrast1; y1 <- Rmax * c1^n_nr / (c1^n_nr + C50^n_nr)
    c2 <- input$contrast2; y2 <- Rmax * c2^n_nr / (c2^n_nr + C50^n_nr)
    
    pts <- data.frame(x = c(c1, c2), y = c(y1, y2), label = c("S1", "S2"))
    
    ggplot(nr, aes(x = Contrast, y = Max_firing)) +
      geom_line(linewidth = 0.7) +
      geom_point(data = pts, aes(x = x, y = y), size = 3, color = "red") +
      geom_text(data = pts, aes(x = x, y = y, label = label),
                vjust = -1, hjust = c(-0.3, 1.3), size = 3.5, color = "red") +
      annotate("text", x = 50, y = 130, label = "Naka-Rushton",
               vjust = 1, hjust = 0.5, size = 3.5) +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      coord_cartesian(ylim = c(0, 130)) +
      scale_y_continuous(breaks = seq(0, 120, by = 30)) +
      labs(x = "Contrast (%)", y = "Peak response") +
      theme_classic(base_size = 11)
  })
  
  # ---- Tuning curve plot ----
  # Gaussian orientation tuning curves for a subset of neurons (every 18th).
  # Peak firing rate is set by contrast1.
  # Black solid/dashed lines highlight the neurons closest to stim1 and stim2.
  output$g_tuning <- renderPlot({
    req(input$contrast1, input$stim1, input$stim2)
    Rmax <- 115; C50 <- 19.3; n_nr <- 2.9
    contrast     <- input$contrast1
    max_fr       <- Rmax * contrast^n_nr / (contrast^n_nr + C50^n_nr)
    tuning_width <- 20
    n_neurons    <- 180
    orientations <- seq(1, 180)
    preferred_orientations <- seq(1, 180, length.out = n_neurons)
    
    # Color each neuron by its preferred orientation using a color wheel
    color_wheel <- function(n = 180) {
      h <- seq(0, 1, length.out = n + 1)[-(n + 1)]
      hsv(h, 1, 1)
    }
    colors <- color_wheel(n_neurons)
    
    tuning_curves <- matrix(0, nrow = n_neurons, ncol = length(orientations))
    for (i in 1:n_neurons) {
      tuning_curves[i, ] <- max_fr * exp(
        -0.5 * (pmin(abs(orientations - preferred_orientations[i]),
                     180 - abs(orientations - preferred_orientations[i]))^2
        ) / tuning_width^2
      )
    }
    
    # Build long-format data for every 18th neuron (10 curves total)
    neurons_show <- seq(1, n_neurons, by = 18)
    df_tuning <- do.call(rbind, lapply(neurons_show, function(i) {
      data.frame(Orientation = orientations,
                 FiringRate  = tuning_curves[i, ],
                 Neuron      = i,
                 Color       = colors[i])
    }))
    
    # Find neurons closest to stim1 and stim2 for highlighting
    s1   <- input$stim1; idx1 <- which.min(abs(preferred_orientations - s1))
    s2   <- input$stim2; idx2 <- which.min(abs(preferred_orientations - s2))
    
    df_highlight <- rbind(
      data.frame(Orientation = orientations, FiringRate = tuning_curves[idx1, ],
                 label = paste0("S1 (", s1, "°)")),
      data.frame(Orientation = orientations, FiringRate = tuning_curves[idx2, ],
                 label = paste0("S2 (", s2, "°)"))
    )
    
    ggplot() +
      geom_line(data = df_tuning,
                aes(x = Orientation, y = FiringRate, color = Color, group = Neuron)) +
      scale_color_identity() +
      geom_line(data = df_highlight,
                aes(x = Orientation, y = FiringRate, linetype = label, group = label),
                color = "black", linewidth = 1.2) +
      annotate("text", x = 90, y = 130,
               label = paste0("Contrast = ", contrast, "%"),
               vjust = 1, hjust = 0.5, size = 3.5) +
      scale_x_continuous(breaks = c(0, 60, 120, 180)) +
      coord_cartesian(ylim = c(0, 130)) +
      scale_y_continuous(breaks = seq(0, 120, by = 30)) +
      scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
      labs(x = "Orientation (°)", y = "Spikes") +
      theme_classic(base_size = 11) +
      theme(legend.position = "bottom")
  })
  
  # ---- Total spike count ----
  # Distribution of summed population spike counts across trials.
  # High GV (red) vs Low GV (blue): gain variability broadens the distribution,
  # impairing discrimination even when mean response is unchanged.
  output$g_density <- renderPlot({
    req(sim_result())
    sim_result() %>%
      group_by(Stimulus, GV, Trial) %>%
      summarise(Sum_spikes = sum(Spikes), .groups = "drop") %>%
      ggplot(aes(x = Sum_spikes, color = GV, fill = GV)) +
      geom_density(alpha = 0.2, linewidth = 1) +
      scale_color_manual(values = c("high" = "#E41A1C", "low" = "#377EB8")) +
      scale_fill_manual( values = c("high" = "#E41A1C", "low" = "#377EB8")) +
      labs(x = "Total spikes", y = "Density") +
      theme_classic(base_size = 13)
  })
  
  # ---- 3D scatter ----
  # Responses of 3 neurons with preferred orientations at 60, 90, and 120 degrees.
  # stim1 = cross (+, size 3), stim2 = circle (filled, size 2).
  # Red = high GV, blue = low GV.
  output$p_3d <- renderPlotly({
    req(sim_result())
    df_3d <- sim_result() %>%
      filter(Neuron %in% c(60, 90, 120)) %>%
      pivot_wider(id_cols = c(GV, Stimulus, Trial),
                  names_from = Neuron, values_from = Spikes)
    
    s1 <- input$stim1; s2 <- input$stim2
    
    plot_ly() %>%
      add_trace(data = df_3d %>% filter(Stimulus == s1),
                x = ~`60`, y = ~`90`, z = ~`120`,
                color = ~factor(GV), colors = c("#E41A1C", "#377EB8"),
                type = "scatter3d", mode = "markers",
                marker = list(size = 3, symbol = "cross"),
                name = ~paste0(GV, " stim", s1)) %>%
      add_trace(data = df_3d %>% filter(Stimulus == s2),
                x = ~`60`, y = ~`90`, z = ~`120`,
                color = ~factor(GV), colors = c("#E41A1C", "#377EB8"),
                type = "scatter3d", mode = "markers",
                marker = list(size = 2, symbol = "circle"),
                name = ~paste0(GV, " stim", s2)) %>%
      layout(scene = list(
        xaxis = list(title = "Neuron 60",  range = c(0, 200)),
        yaxis = list(title = "Neuron 90",  range = c(0, 200)),
        zaxis = list(title = "Neuron 120", range = c(0, 200)),
        aspectmode = "cube"
      ))
  })
  
  # ---- Decision boundary ----
  # Fits a logistic regression on the 3-neuron responses to classify stim1 vs stim2.
  # The navy mesh shows the decision surface where P(stim2) = 0.5.
  output$p_boundary <- renderPlotly({
    req(sim_result())
    withProgress(message = "Fitting decision boundary...", {
      df_scatter <- sim_result() %>%
        filter(Neuron %in% c(60, 90, 120)) %>%
        pivot_wider(id_cols = c(GV, Stimulus, Trial),
                    names_from = Neuron, values_from = Spikes) %>%
        mutate(stim_bin = as.numeric(as.factor(Stimulus)) - 1)  # 0 = stim1, 1 = stim2
      
      fit <- glm(stim_bin ~ `60` + `90` + `120`,
                 data = df_scatter, family = binomial)
      
      # Evaluate predicted probability on a 3D grid and extract the boundary surface
      x_seq <- seq(0, 200, length.out = 15)
      y_seq <- seq(0, 200, length.out = 15)
      z_seq <- seq(0, 200, length.out = 15)
      grid3d <- expand.grid(`60` = x_seq, `90` = y_seq, `120` = z_seq)
      grid3d$prob <- predict(fit, newdata = grid3d, type = "response")
      decision_points <- grid3d %>% filter(abs(prob - 0.5) < 0.03)
      
      s1 <- input$stim1; s2 <- input$stim2
      
      p <- plot_ly() %>%
        add_trace(data = df_scatter %>% filter(Stimulus == s1),
                  x = ~`60`, y = ~`90`, z = ~`120`,
                  color = ~factor(GV), colors = c("#E41A1C", "#377EB8"),
                  type = "scatter3d", mode = "markers",
                  marker = list(size = 3, symbol = "cross"),
                  name = ~paste0(GV, " stim", s1)) %>%
        add_trace(data = df_scatter %>% filter(Stimulus == s2),
                  x = ~`60`, y = ~`90`, z = ~`120`,
                  color = ~factor(GV), colors = c("#E41A1C", "#377EB8"),
                  type = "scatter3d", mode = "markers",
                  marker = list(size = 2, symbol = "circle"),
                  name = ~paste0(GV, " stim", s2))
      
      if (nrow(decision_points) > 0) {
        p <- p %>%
          add_trace(data = decision_points,
                    x = ~`60`, y = ~`90`, z = ~`120`,
                    type = "mesh3d", opacity = 0.4,
                    color = I("navy"), name = "Decision boundary",
                    inherit = FALSE)
      }
      p %>% layout(scene = list(
        xaxis = list(title = "Neuron 60",  range = c(0, 200)),
        yaxis = list(title = "Neuron 90",  range = c(0, 200)),
        zaxis = list(title = "Neuron 120", range = c(0, 200)),
        aspectmode = "cube"
      ))
    })
  })
  
  # ---- LDA 2D ----
  # 2D scatter of two-neuron responses.
  # Green arrow (df): mean difference vector mu1 - mu0.
  # Purple arrow (w): LDA discriminant axis = Sigma^{-1} * df.
  # With gain variability, the covariance Sigma is inflated along the sum direction,
  # rotating w away from df and reducing discriminability.
  output$g_lda_2d <- renderPlot({
    req(lda_result())
    r <- lda_result()
    
    arrows_df <- data.frame(
      x    = c(r$mean0[1], r$mean0[1]),
      y    = c(r$mean0[2], r$mean0[2]),
      xend = c(r$mean0[1] + r$df_vec[1], r$mean0[1] + r$w_scaled[1]),
      yend = c(r$mean0[2] + r$df_vec[2], r$mean0[2] + r$w_scaled[2]),
      type = c("df", "w")
    )
    
    ggplot(r$data, aes(x = neuron1, y = neuron2, color = class)) +
      geom_point(size = 2, alpha = 0.5) +
      geom_point(aes(x = r$mean0[1], y = r$mean0[2]), color = "blue", size = 3) +
      geom_point(aes(x = r$mean1[1], y = r$mean1[2]), color = "red",  size = 3) +
      geom_segment(data = arrows_df,
                   aes(x = x, y = y, xend = xend, yend = yend, color = type),
                   arrow = arrow(length = unit(0.15, "cm")), lwd = 1.2) +
      coord_fixed() +
      scale_color_manual(values = c("0" = "blue", "1" = "red",
                                    "df" = "green", "w" = "purple")) +
      labs(title = "Mean difference vector df & discriminant axis w",
           subtitle = paste("d' =", round(r$dp, 3)),
           x = "Neuron 1", y = "Neuron 2", color = "Legend") +
      theme_minimal(base_size = 13)
  })
  
  # ---- LDA 1D ----
  # Distributions of projections onto the discriminant axis w.
  # d' = (mu1_proj - mu0_proj) / pooled_SD.
  # Higher gain variability -> broader distributions -> lower d'.
  output$g_lda_1d <- renderPlot({
    req(lda_result())
    r <- lda_result()
    
    proj_df <- data.frame(
      projection = c(r$proj0, r$proj1),
      class      = factor(c(rep(0, r$n), rep(1, r$n)))
    )
    
    ggplot(proj_df, aes(x = projection, fill = class)) +
      geom_density(alpha = 0.6) +
      geom_vline(xintercept = r$mu_proj0, color = "blue", linetype = "dashed") +
      geom_vline(xintercept = r$mu_proj1, color = "red",  linetype = "dashed") +
      scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
      labs(title = "1D distribution after projection onto w",
           subtitle = paste("d' =", round(r$dp_1d, 3)),
           x = "Projection onto w", y = "Density") +
      xlim(-6, 6) +
      theme_minimal(base_size = 13)
  })
}

# ============================================================
shinyApp(ui, server)