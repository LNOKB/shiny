library(shiny)
library(plotly)
library(tidyverse)
library(patchwork)

# ============================================================
# Preset definitions
# ============================================================
presets <- list(
  manual = list(
    contrast_A = 40, contrast_B = 40,
    sigma_g_A  = 0.05, sigma_g_B  = 0.05,
    spont_A    = 2,  spont_B    = 2,
    fano_A     = 1,  fano_B     = 1
  ),
  samaha = list(
    # High vs Low spontaneous activity
    contrast_A = 40, contrast_B = 40,
    sigma_g_A  = 0.05, sigma_g_B  = 0.05,
    spont_A    = 25,  spont_B    = 0,
    fano_A     = 1,  fano_B     = 1
  ),
  blindsight = list(
    # High vs Low gain variability
    contrast_A = 40, contrast_B = 40,
    sigma_g_A  = 0.2, sigma_g_B  = 0.05,
    spont_A    = 2,  spont_B    = 2,
    fano_A     = 1,  fano_B     = 1
  ),
  subjective = list(
    # High vs Low Fano Factor
    contrast_A = 40, contrast_B = 40,
    sigma_g_A  = 0.05, sigma_g_B  = 0.05,
    spont_A    = 2,  spont_B    = 2,
    fano_A     = 3,  fano_B     = 1
  )
)

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("Neural Population Response Simulator"),
  sidebarLayout(
    sidebarPanel(
      # Preset selector
      selectInput("preset", "Preset",
                  choices = c("Manual"              = "manual",
                              "Samaha effect"        = "samaha",
                              "Blindsight"           = "blindsight",
                              "Subjective inflation" = "subjective")),
      hr(),
      # Stimuli and trials (shared across conditions)
      sliderInput("stim1",    "Stimulus 1 orientation (°)", min = 1, max = 180, value = 90,  step = 1),
      sliderInput("stim2",    "Stimulus 2 orientation (°)", min = 1, max = 180, value = 100, step = 1),
      sliderInput("n_trials", "Number of trials", min = 50, max = 500, value = 100, step = 50),
      hr(),
      # Condition A
      h5("Condition A", style = "color:#E41A1C; font-weight:bold;"),
      sliderInput("contrast_A", "Contrast A (%)",        min = 1,    max = 100,  value = 40,   step = 1),
      sliderInput("sigma_g_A",  "Gain variability A",    min = 0.01, max = 0.5,  value = 0.05, step = 0.01),
      sliderInput("spont_A",    "Spontaneous activity A",min = 0,    max = 25,   value = 2,    step = 0.5),
      sliderInput("fano_A",     "Fano Factor A",         min = 1,    max = 5,   value = 1,    step = 0.5),
      hr(),
      # Condition B
      h5("Condition B", style = "color:#377EB8; font-weight:bold;"),
      sliderInput("contrast_B", "Contrast B (%)",        min = 1,    max = 100,  value = 40,   step = 1),
      sliderInput("sigma_g_B",  "Gain variability B",    min = 0.01, max = 0.5,  value = 0.05, step = 0.01),
      sliderInput("spont_B",    "Spontaneous activity B",min = 0,    max = 25,   value = 2,    step = 0.5),
      sliderInput("fano_B",     "Fano Factor B",         min = 1,    max = 10,   value = 1,    step = 0.5),
      hr(),
      actionButton("run", "Run simulation", class = "btn-primary")
    ),
    mainPanel(
      h4("Stimuli"),
      plotOutput("g_stimuli", height = "180px"),
      hr(),
      h4("Naka-Rushton & Tuning curves"),
      fluidRow(
        column(6, plotOutput("g_nr")),
        column(6, plotOutput("g_tuning"))
      ),
      hr(),
      h4("3D scatter"),
      plotlyOutput("p_3d"),
      hr(),
      h4("Total spike count"),
      plotOutput("g_density"),
      hr(),
      h4("Decision boundary"),
      plotlyOutput("p_boundary")#,
      # hr(),
      # h4("LDA projection (2D)"),
      # plotOutput("g_lda_2d"),
      # hr(),
      # h4("LDA projection (1D)"),
      # plotOutput("g_lda_1d")
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  # ---- Preset: auto-update sliders ----
  observeEvent(input$preset, {
    p <- presets[[input$preset]]
    updateSliderInput(session, "contrast_A", value = p$contrast_A)
    updateSliderInput(session, "contrast_B", value = p$contrast_B)
    updateSliderInput(session, "sigma_g_A",  value = p$sigma_g_A)
    updateSliderInput(session, "sigma_g_B",  value = p$sigma_g_B)
    updateSliderInput(session, "spont_A",    value = p$spont_A)
    updateSliderInput(session, "spont_B",    value = p$spont_B)
    updateSliderInput(session, "fano_A",     value = p$fano_A)
    updateSliderInput(session, "fano_B",     value = p$fano_B)
  })
  
  # ---- sim_result ----
  # Generates spike responses for Condition A and B.
  # Spontaneous activity is added as an offset to all tuning curves.
  # Fano Factor controls the size parameter of the negative binomial:
  #   Var = mu + mu^2/size, so size = mu/(Fano-1) when Fano > 1
  #   size = 1e6 (approx Poisson) when Fano = 1
  sim_result <- eventReactive(input$run, {
    withProgress(message = "Simulating...", value = 0, {
      
      Rmax <- 115; C50 <- 19.3; n_nr <- 2.9
      n_neurons    <- 180
      orientations <- seq(1, 180)
      preferred_orientations <- seq(1, 180, length.out = n_neurons)
      tuning_width <- 20
      n_trials     <- input$n_trials
      
      make_tuning_curves <- function(max_fr, spont) {
        tc <- matrix(0, nrow = n_neurons, ncol = 180)
        for (i in 1:n_neurons) {
          tc[i, ] <- spont + max_fr * exp(
            -0.5 * (pmin(abs(orientations - preferred_orientations[i]),
                         180 - abs(orientations - preferred_orientations[i]))^2
            ) / tuning_width^2
          )
        }
        tc
      }
      
      # Fano Factor -> size parameter for rnbinom
      # Fano = 1 + mu/size  =>  size = mu / (Fano - 1)
      # For Fano = 1, use large size (approx Poisson)
      fano_to_size <- function(fano) {
        if (fano <= 1) return(1e6)
        # use a representative mu to derive size; mu=30 is a reasonable midpoint
        30 / (fano - 1)
      }
      
      simulate <- function(stim, sigma_g, n_trials, tuning_curves, nb_size) {
        do.call(rbind, lapply(1:n_trials, function(i) {
          idx  <- max(1, min(180, round(stim)))
          g    <- rgamma(1, shape = 1/sigma_g^2, scale = sigma_g^2)
          mu   <- g * tuning_curves[, idx]
          mu[mu <= 0] <- 1e-8
          resp <- rnbinom(n_neurons, size = nb_size, mu = mu)
          data.frame(Spikes = resp, Neuron = seq_len(n_neurons),
                     Stimulus = stim, Trial = i)
        }))
      }
      
      incProgress(0.1, message = "Building tuning curves...")
      
      # Condition A
      max_fr_A <- Rmax * input$contrast_A^n_nr / (input$contrast_A^n_nr + C50^n_nr)
      tc_A     <- make_tuning_curves(max_fr_A, input$spont_A)
      size_A   <- fano_to_size(input$fano_A)
      
      # Condition B
      max_fr_B <- Rmax * input$contrast_B^n_nr / (input$contrast_B^n_nr + C50^n_nr)
      tc_B     <- make_tuning_curves(max_fr_B, input$spont_B)
      size_B   <- fano_to_size(input$fano_B)
      
      incProgress(0.3, message = "Condition A...")
      df_A <- rbind(
        simulate(input$stim1, input$sigma_g_A, n_trials, tc_A, size_A),
        simulate(input$stim2, input$sigma_g_A, n_trials, tc_A, size_A)
      ) %>% mutate(Condition = "A")
      
      incProgress(0.3, message = "Condition B...")
      df_B <- rbind(
        simulate(input$stim1, input$sigma_g_B, n_trials, tc_B, size_B),
        simulate(input$stim2, input$sigma_g_B, n_trials, tc_B, size_B)
      ) %>% mutate(Condition = "B")
      
      incProgress(0.2, message = "Done!")
      rbind(df_A, df_B)
    })
  })
  
  # ---- lda_result ----
  lda_result <- eventReactive(input$run, {
    req(sim_result())
    withProgress(message = "Computing LDA...", {
      
      s1 <- input$stim1; s2 <- input$stim2
      
      run_lda <- function(cond_label) {
        df_lda <- sim_result() %>%
          filter(Condition == cond_label, Neuron %in% c(95, 90, 100)) %>%
          pivot_wider(id_cols = c(Stimulus, Trial),
                      names_from = Neuron, values_from = Spikes) %>%
          rename(neuron1 = `95`, neuron2 = `90`, neuron3 = `100`) %>%
          mutate(class = factor(ifelse(Stimulus == s1, 0, 1)))
        
        data0 <- as.matrix(df_lda[df_lda$class == 0, c("neuron1","neuron2","neuron3")])
        data1 <- as.matrix(df_lda[df_lda$class == 1, c("neuron1","neuron2","neuron3")])
        n     <- nrow(data0)
        
        df_vec    <- colMeans(data1) - colMeans(data0)
        lambda_reg <- 0.01
        Sigma_w   <- (cov(data0) + cov(data1)) / 2 + lambda_reg * diag(3)
        w         <- solve(Sigma_w) %*% df_vec
        w_scaled  <- as.numeric(w)
        mean0     <- colMeans(data0); mean1 <- colMeans(data1)
        lfi       <- t(df_vec) %*% solve(Sigma_w) %*% df_vec
        dp        <- sqrt(lfi)
        proj0     <- as.numeric(data0 %*% w)
        proj1     <- as.numeric(data1 %*% w)
        mu_proj0  <- mean(proj0); mu_proj1 <- mean(proj1)
        sd_proj   <- sqrt((var(proj0) + var(proj1)) / 2)
        dp_1d     <- (mu_proj1 - mu_proj0) / sd_proj
        
        list(data0=data0, data1=data1, df_vec=df_vec, w_scaled=w_scaled,
             mean0=mean0, mean1=mean1, dp=dp,
             proj0=proj0, proj1=proj1, dp_1d=dp_1d,
             mu_proj0=mu_proj0, mu_proj1=mu_proj1, n=n)
      }
      
      list(A = run_lda("A"), B = run_lda("B"))
    })
  })
  
  # ---- Stimuli: Gabor patches ----
  # Schematic illustration of 4 conditions: Condition A/B x Stimulus 1/2
  # Gabor orientation reflects stim1/stim2 angle; contrast is approximate/visual only.
  output$g_stimuli <- renderPlot({
    req(input$stim1, input$stim2, input$contrast_A, input$contrast_B)
    
    make_gabor <- function(theta_deg, contrast_scale, nx = 60) {
      theta <- theta_deg * pi / 180
      x <- seq(-1, 1, length.out = nx)
      grid <- expand.grid(x = x, y = x)
      # Rotate coordinates
      xr <-  grid$x * cos(theta) + grid$y * sin(theta)
      yr <- -grid$x * sin(theta) + grid$y * cos(theta)
      # Gabor = Gaussian envelope * sinusoidal grating
      sigma <- 0.35
      freq  <- 3.5
      val <- exp(-(xr^2 + yr^2) / (2 * sigma^2)) * cos(2 * pi * freq * xr)
      # Scale by contrast (0~1)
      val <- val * contrast_scale
      data.frame(x = grid$x, y = grid$y, val = val)
    }
    
    cA_scale <- input$contrast_A / 100
    cB_scale <- input$contrast_B / 100
    
    panels <- list(
      list(cond = "Condition A", stim_label = paste0("S1"),
           theta = input$stim1, cs = cA_scale, px = 1, py = 2),
      list(cond = "Condition A", stim_label = paste0("S2"),
           theta = input$stim2, cs = cA_scale, px = 2, py = 2),
      list(cond = "Condition B", stim_label = paste0("S1"),
           theta = input$stim1, cs = cB_scale, px = 1, py = 1),
      list(cond = "Condition B", stim_label = paste0("S2"),
           theta = input$stim2, cs = cB_scale, px = 2, py = 1)
    )
    
    # Build combined data frame with panel offsets
    df_all <- do.call(rbind, lapply(panels, function(p) {
      g <- make_gabor(p$theta, p$cs)
      g$x_off <- g$x + (p$px - 1) * 2.5
      g$y_off <- g$y + (p$py - 1) * 2.5
      g$cond  <- p$cond
      g$stim_label <- p$stim_label
      g$px <- p$px; g$py <- p$py
      g
    }))
    
    # Label positions
    labels_df <- data.frame(
      x_off = c(0, 2.5, 0, 2.5),
      y_off = c(3.3, 3.3, 0.8, 0.8),
      label = c(paste0("S1"),
                paste0("S2"),
                paste0("S1"),
                paste0("S2"))
    )
    cond_labels_df <- data.frame(
      x_off = c(-1.6, -1.6),
      y_off = c(2.5, 0),
      label = c("Cond A", "Cond B"),
      col   = c("#E41A1C", "#377EB8")
    )
    
    ggplot(df_all, aes(x = x_off, y = y_off, fill = val)) +
      geom_raster(interpolate = TRUE) +
      scale_fill_gradient2(low = "black", mid = "gray50", high = "white",
                           midpoint = 0, limits = c(-1, 1), guide = "none") +
      geom_text(data = labels_df, aes(x = x_off, y = y_off, label = label),
                inherit.aes = FALSE, size = 3.2, vjust = 0, color = "white") +
      geom_text(data = cond_labels_df,
                aes(x = x_off, y = y_off, label = label, color = col),
                inherit.aes = FALSE, size = 3.3, fontface = "bold") +
      scale_color_identity() +
      coord_fixed(xlim = c(-1.8, 3.6), ylim = c(-1.2, 3.6)) +
      theme_void()
  })
  
  # ---- Naka-Rushton plot ----
  output$g_nr <- renderPlot({
    req(input$contrast_A, input$contrast_B)
    Rmax <- 115; C50 <- 19.3; n_nr <- 2.9
    contrast_seq <- seq(0, 100, 0.1)
    nr <- data.frame(Contrast = contrast_seq,
                     Max_firing = Rmax * contrast_seq^n_nr / (contrast_seq^n_nr + C50^n_nr))
    
    cA <- input$contrast_A; yA <- Rmax * cA^n_nr / (cA^n_nr + C50^n_nr)
    cB <- input$contrast_B; yB <- Rmax * cB^n_nr / (cB^n_nr + C50^n_nr)
    pts <- data.frame(x = c(cA, cB), y = c(yA, yB),
                      label = c("A", "B"), col = c("#E41A1C", "#377EB8"))
    
    ggplot(nr, aes(x = Contrast, y = Max_firing)) +
      geom_line(linewidth = 0.7) +
      geom_point(data = pts, aes(x = x, y = y, color = col), size = 3) +
      geom_text(data = pts, aes(x = x, y = y, label = label, color = col),
                vjust = -1, hjust = 0.5, size = 4) +
      scale_color_identity() +
      annotate("text", x = 50, y = 130, label = "Naka-Rushton",
               vjust = 1, hjust = 0.5, size = 3.5) +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      coord_cartesian(ylim = c(0, 130)) +
      scale_y_continuous(breaks = seq(0, 120, by = 30)) +
      labs(x = "Contrast (%)", y = "Peak response") +
      theme_classic(base_size = 11)
  })
  
  # ---- Tuning curve plot ----
  # Shows Condition A (top) and Condition B (bottom) tuning curves,
  # illustrating how spontaneous activity raises the baseline.
  output$g_tuning <- renderPlot({
    req(input$contrast_A, input$contrast_B, input$stim1, input$stim2)
    Rmax <- 115; C50 <- 19.3; n_nr <- 2.9
    tuning_width <- 20; n_neurons <- 180
    orientations <- seq(1, 180)
    preferred_orientations <- seq(1, 180, length.out = n_neurons)
    
    color_wheel <- function(n = 180) {
      h <- seq(0, 1, length.out = n + 1)[-(n + 1)]; hsv(h, 1, 1)
    }
    colors <- color_wheel(n_neurons)
    neurons_show <- seq(1, n_neurons, by = 18)
    
    make_tc_df <- function(max_fr, spont, cond_label) {
      tc <- matrix(0, nrow = n_neurons, ncol = length(orientations))
      for (i in 1:n_neurons) {
        tc[i, ] <- spont + max_fr * exp(
          -0.5 * (pmin(abs(orientations - preferred_orientations[i]),
                       180 - abs(orientations - preferred_orientations[i]))^2
          ) / tuning_width^2
        )
      }
      do.call(rbind, lapply(neurons_show, function(i) {
        data.frame(Orientation = orientations, FiringRate = tc[i, ],
                   Neuron = i, Color = colors[i], Condition = cond_label,
                   spont = spont, max_fr = max_fr)
      }))
    }
    
    max_fr_A <- Rmax * input$contrast_A^n_nr / (input$contrast_A^n_nr + C50^n_nr)
    max_fr_B <- Rmax * input$contrast_B^n_nr / (input$contrast_B^n_nr + C50^n_nr)
    
    df_all <- rbind(
      make_tc_df(max_fr_A, input$spont_A, paste0("Condition A  (spont=", input$spont_A, ")")),
      make_tc_df(max_fr_B, input$spont_B, paste0("Condition B  (spont=", input$spont_B, ")"))
    ) %>% mutate(Condition = factor(Condition,
                                    levels = c(paste0("Condition A  (spont=", input$spont_A, ")"),
                                               paste0("Condition B  (spont=", input$spont_B, ")"))))
    
    y_max <- max(df_all$FiringRate) * 1.05
    
    ggplot(df_all, aes(x = Orientation, y = FiringRate,
                       color = Color, group = Neuron)) +
      geom_line() +
      scale_color_identity() +
      facet_wrap(~ Condition, nrow = 2) +
      scale_x_continuous(breaks = c(0, 60, 120, 180)) +
      coord_cartesian(ylim = c(0, y_max)) +
      labs(x = "Orientation (°)", y = "Spikes") +
      theme_classic(base_size = 11) +
      theme(strip.text = element_text(size = 10, face = "bold"))
  })
  
  # ---- Total spike count ----
  output$g_density <- renderPlot({
    req(sim_result())
    sim_result() %>%
      group_by(Stimulus, Condition, Trial) %>%
      summarise(Sum_spikes = sum(Spikes), .groups = "drop") %>%
      ggplot(aes(x = Sum_spikes, color = Condition, fill = Condition)) +
      geom_density(alpha = 0.2, linewidth = 1) +
      scale_color_manual(values = c("A" = "#E41A1C", "B" = "#377EB8")) +
      scale_fill_manual( values = c("A" = "#E41A1C", "B" = "#377EB8")) +
      labs(x = "Total spikes", y = "Density") +
      theme_classic(base_size = 13)
  })
  
  # ---- 3D scatter ----
  output$p_3d <- renderPlotly({
    req(sim_result())
    df_3d <- sim_result() %>%
      filter(Neuron %in% c(95, 90, 100)) %>%
      pivot_wider(id_cols = c(Condition, Stimulus, Trial),
                  names_from = Neuron, values_from = Spikes)
    s1 <- input$stim1; s2 <- input$stim2
    
    plot_ly() %>%
      add_trace(data = df_3d %>% filter(Stimulus == s1),
                x = ~`95`, y = ~`90`, z = ~`100`,
                color = ~factor(Condition), colors = c("#E41A1C", "#377EB8"),
                type = "scatter3d", mode = "markers",
                marker = list(size = 3.5, symbol = "cross"),
                name = ~paste0("Cond ", Condition, "; S1 (", s1, " deg)")) %>%
      add_trace(data = df_3d %>% filter(Stimulus == s2),
                x = ~`95`, y = ~`90`, z = ~`100`,
                color = ~factor(Condition), colors = c("#E41A1C", "#377EB8"),
                type = "scatter3d", mode = "markers",
                marker = list(size = 1.7, symbol = "circle"),
                name = ~paste0("Cond ", Condition, "; S2 (", s2, " deg)")) %>%
      layout(scene = list(
        xaxis = list(title = "Neuron 95",  range = c(0, 180)),
        yaxis = list(title = "Neuron 90",  range = c(0, 180)),
        zaxis = list(title = "Neuron 100", range = c(0, 180)),
        aspectmode = "cube"
      ))
  })
  
  # ---- Decision boundary ----
  output$p_boundary <- renderPlotly({
    req(sim_result())
    withProgress(message = "Fitting decision boundary...", {
      
      make_boundary_plot <- function(cond_label, col) {
        df_s <- sim_result() %>%
          filter(Condition == cond_label, Neuron %in% c(95, 90, 100)) %>%
          pivot_wider(id_cols = c(Stimulus, Trial),
                      names_from = Neuron, values_from = Spikes) %>%
          mutate(stim_bin = as.numeric(as.factor(Stimulus)) - 1)
        
        fit <- glm(stim_bin ~ `95` + `90` + `100`, data = df_s, family = binomial)
        x_seq <- seq(0, 180, length.out = 20)
        grid3d <- expand.grid(`95` = x_seq, `90` = x_seq, `100` = x_seq)
        grid3d$prob <- predict(fit, newdata = grid3d, type = "response")
        dp <- grid3d %>% filter(abs(prob - 0.5) < 0.07) #ここをどのくらい厳しくする？
        list(df = df_s, dp = dp, col = col, label = cond_label)
      }
      
      rA <- make_boundary_plot("A", "#E41A1C")
      rB <- make_boundary_plot("B", "#377EB8")
      s1 <- input$stim1; s2 <- input$stim2
      
      p <- plot_ly()
      for (r in list(rA, rB)) {
        p <- p %>%
          add_trace(data = r$df %>% filter(Stimulus == s1),
                    x = ~`95`, y = ~`90`, z = ~`100`,
                    type = "scatter3d", mode = "markers",
                    marker = list(size = 3, symbol = "cross", color = r$col),
                    name = paste0("Cond", r$label, " S1")) %>%
          add_trace(data = r$df %>% filter(Stimulus == s2),
                    x = ~`95`, y = ~`90`, z = ~`100`,
                    type = "scatter3d", mode = "markers",
                    marker = list(size = 1, symbol = "circle", color = r$col),
                    name = paste0("Cond", r$label, " S2"))
        if (nrow(r$dp) > 0) {
          p <- p %>%
            add_trace(data = r$dp, x = ~`95`, y = ~`90`, z = ~`100`,
                      type = "mesh3d", opacity = 0.3,
                      colorscale = list(c(0, r$col), c(1, r$col)),
                      intensity = rep(0.5, nrow(r$dp)),
                      showscale = FALSE,
                      name = paste0("Boundary Cond", r$label),
                      inherit = FALSE)
        }
      }
      p %>% layout(scene = list(
        xaxis = list(title = "Neuron 95",  range = c(0, 180)),
        yaxis = list(title = "Neuron 90",  range = c(0, 180)),
        zaxis = list(title = "Neuron 100", range = c(0, 180)),
        aspectmode = "cube"
      ))
    })
  })
  
  # # ---- LDA 2D ----
  # output$g_lda_2d <- renderPlot({
  #   req(lda_result())
  #   
  #   make_panel <- function(r, ix, iy, xlab, ylab, cond_col) {
  #     df_plot <- data.frame(
  #       x     = c(r$data0[, ix], r$data1[, ix]),
  #       y     = c(r$data0[, iy], r$data1[, iy]),
  #       class = factor(c(rep(0, r$n), rep(1, r$n)))
  #     )
  #     arrows_df <- data.frame(
  #       x    = c(r$mean0[ix], r$mean0[ix]),
  #       y    = c(r$mean0[iy], r$mean0[iy]),
  #       xend = c(r$mean0[ix] + r$df_vec[ix], r$mean0[ix] + r$w_scaled[ix]),
  #       yend = c(r$mean0[iy] + r$df_vec[iy], r$mean0[iy] + r$w_scaled[iy]),
  #       type = c("df", "w")
  #     )
  #     ggplot(df_plot, aes(x = x, y = y, color = class)) +
  #       geom_point(size = 1.5, alpha = 0.3) +
  #       annotate("point", x = r$mean0[ix], y = r$mean0[iy], color = "blue", size = 3) +
  #       annotate("point", x = r$mean1[ix], y = r$mean1[iy], color = "red",  size = 3) +
  #       geom_segment(data = arrows_df,
  #                    aes(x = x, y = y, xend = xend, yend = yend, color = type),
  #                    arrow = arrow(length = unit(0.15, "cm")), lwd = 1.2,
  #                    inherit.aes = FALSE) +
  #       coord_fixed() +
  #       scale_color_manual(values = c("0"="blue","1"="red","df"="green","w"="purple")) +
  #       labs(x = xlab, y = ylab) +
  #       theme_minimal(base_size = 11) +
  #       theme(legend.position = "none")
  #   }
  #   
  #   rA <- lda_result()$A; rB <- lda_result()$B
  #   pA1 <- make_panel(rA, 1, 2, "N95", "N90",  "#E41A1C")
  #   pA2 <- make_panel(rA, 2, 3, "N90", "N100", "#E41A1C")
  #   pB1 <- make_panel(rB, 1, 2, "N95", "N90",  "#377EB8")
  #   pB2 <- make_panel(rB, 2, 3, "N90", "N100", "#377EB8")
  #   
  #   (pA1 + pA2) / (pB1 + pB2) +
  #     plot_annotation(
  #       title = "LDA: df (green) & w (purple)",
  #       subtitle = paste0("Cond A d'=", round(rA$dp, 2),
  #                         "  |  Cond B d'=", round(rB$dp, 2))
  #     )
  # })
  # 
  # # ---- LDA 1D ----
  # output$g_lda_1d <- renderPlot({
  #   req(lda_result())
  #   rA <- lda_result()$A; rB <- lda_result()$B
  #   
  #   proj_df <- rbind(
  #     data.frame(projection = c(rA$proj0, rA$proj1),
  #                class = factor(c(rep(0, rA$n), rep(1, rA$n))),
  #                Condition = "A"),
  #     data.frame(projection = c(rB$proj0, rB$proj1),
  #                class = factor(c(rep(0, rB$n), rep(1, rB$n))),
  #                Condition = "B")
  #   )
  #   
  #   ggplot(proj_df, aes(x = projection, fill = class)) +
  #     geom_density(alpha = 0.5) +
  #     geom_vline(data = data.frame(
  #       xint = c(rA$mu_proj0, rA$mu_proj1, rB$mu_proj0, rB$mu_proj1),
  #       Condition = c("A","A","B","B"),
  #       class = factor(c(0,1,0,1))
  #     ), aes(xintercept = xint, color = class), linetype = "dashed") +
  #     scale_fill_manual(values  = c("0" = "blue", "1" = "red")) +
  #     scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  #     facet_wrap(~ Condition, nrow = 2,
  #                labeller = labeller(Condition = c(
  #                  A = paste0("Condition A  d'=", round(rA$dp_1d, 2)),
  #                  B = paste0("Condition B  d'=", round(rB$dp_1d, 2))
  #                ))) +
  #     labs(title = "1D projection onto w", x = "Projection onto w", y = "Density") +
  #     theme_minimal(base_size = 13) +
  #     theme(legend.position = "bottom")
  # })
}

# ============================================================
shinyApp(ui, server)