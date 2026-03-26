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
      sliderInput("contrast",
                  label = "Contrast (%)",
                  min = 1, max = 100, value = 40),
      sliderInput("n_trials",
                  label = "Number of trials",
                  min = 50, max = 500, value = 100, step = 50),
      sliderInput("sigma_g_low",  "Gain variability (low)",  0.01, 0.3, 0.05, 0.01),
      sliderInput("sigma_g_high", "Gain variability (high)", 0.1,  0.5, 0.2,  0.01),
      actionButton("run", "Run simulation", class = "btn-primary")
    ),
    mainPanel(
      h4("Total spike count"),
      plotOutput("g_density"),
      hr(),
      h4("3D scatter"),
      plotlyOutput("p_3d"),
      hr(),
      h4("Decision boundary"),
      plotlyOutput("p_boundary"),
      hr(),
      h4("LDA projection (2D)"),
      plotOutput("g_lda_2d"),
      hr(),
      h4("LDA projection (1D)"),
      plotOutput("g_lda_1d")#,
      #h4("LDA projection"),
      #plotlyOutput("g_lda_combined", height = "750px")
    )
  )
)


# ============================================================
# Server
# ============================================================
server <- function(input, output) {
  
  sim_result <- eventReactive(input$run, {
    withProgress(message = "Simulating...", value = 0, {
      
      Rmax <- 115; C50 <- 19.3; n_nr <- 2.9
      contrast <- input$contrast
      max_fr <- Rmax * contrast^n_nr / (contrast^n_nr + C50^n_nr)
      
      n_neurons <- 180
      orientations <- seq(1, 180)
      preferred_orientations <- seq(1, 180, length.out = n_neurons)
      tuning_width <- 20
      n_trials <- input$n_trials
      
      tuning_curves <- matrix(0, nrow = n_neurons, ncol = 180)
      for (i in 1:n_neurons) {
        tuning_curves[i, ] <- max_fr * exp(
          -0.5 * (pmin(abs(orientations - preferred_orientations[i]),
                       180 - abs(orientations - preferred_orientations[i]))^2
          ) / tuning_width^2
        )
      }
      
      incProgress(0.2, message = "Running simulation...")
      
      simulate <- function(stim, sigma_g, n_trials) {
        do.call(rbind, lapply(1:n_trials, function(i) {
          idx <- max(1, min(180, round(stim)))
          g   <- rgamma(1, shape = 1/sigma_g^2, scale = sigma_g^2)
          mu  <- g * tuning_curves[, idx]
          mu[mu == 0] <- 1e-8
          resp <- rnbinom(n_neurons, size = 1000000, mu = mu)
          data.frame(Spikes = resp,
                     Neuron = seq_len(n_neurons),
                     Stimulus = stim,
                     Trial = i)
        }))
      }
      
      incProgress(0.3, message = "High GV...")
      df_high <- rbind(
        simulate(90, input$sigma_g_high, n_trials),
        simulate(91, input$sigma_g_high, n_trials)
      ) %>% mutate(GV = "high")
      
      incProgress(0.3, message = "Low GV...")
      df_low <- rbind(
        simulate(90, input$sigma_g_low, n_trials),
        simulate(91, input$sigma_g_low, n_trials)
      ) %>% mutate(GV = "low")
      
      incProgress(0.2, message = "Done!")
      rbind(df_high, df_low)
    })
  })
  
  lda_result <- eventReactive(input$run, {
    withProgress(message = "Computing LDA...", {
      
      sigma_g <- input$sigma_g_low
      n       <- input$n_trials
      
      mu_0 <- c(30, 29)
      mu_1 <- c(29, 30)
      Sigma_0 <- matrix(c(1.0, 0, 0, 1.0), nrow = 2)
      Sigma_1 <- matrix(c(1.0, 0, 0, 1.0), nrow = 2)
      
      data0 <- mvrnorm(n, mu = mu_0, Sigma = Sigma_0)
      data1 <- mvrnorm(n, mu = mu_1, Sigma = Sigma_1)
      
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
      
      neurons   <- c("neuron1", "neuron2")
      data0_sub <- data[data$class == 0, neurons]
      data1_sub <- data[data$class == 1, neurons]
      df_vec    <- colMeans(data1_sub) - colMeans(data0_sub)
      
      lambda_reg <- 0.01
      Sigma_w    <- (cov(data0) + cov(data1)) / 2
      Sigma_w    <- Sigma_w + lambda_reg * diag(length(neurons))
      
      w        <- solve(Sigma_w) %*% df_vec
      w_scaled <- as.numeric(w)
      mean0    <- colMeans(data0)
      mean1    <- colMeans(data1)
      
      lfi <- t(df_vec) %*% solve(Sigma_w) %*% df_vec
      dp  <- sqrt(lfi)
      
      proj0 <- as.numeric(data0 %*% w)
      proj1 <- as.numeric(data1 %*% w)
      mu_proj0 <- mean(proj0)
      mu_proj1 <- mean(proj1)
      sd_proj  <- sqrt((var(proj0) + var(proj1)) / 2)
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
  
  # --- Total spike count ---
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
  
  # --- 3D scatter ---
  output$p_3d <- renderPlotly({
    req(sim_result())
    df_3d <- sim_result() %>%
      filter(Neuron %in% c(60, 90, 120)) %>%
      pivot_wider(id_cols = c(GV, Stimulus, Trial),
                  names_from = Neuron, values_from = Spikes)
    
    plot_ly() %>%
      # 90 = + (size 3)
      add_trace(data = df_3d %>% filter(Stimulus == 90),
                x = ~`60`, y = ~`90`, z = ~`120`,
                color = ~factor(GV),
                colors = c("#E41A1C", "#377EB8"),
                type = "scatter3d", mode = "markers",
                marker = list(size = 3, symbol = "cross"),
                name = ~paste0(GV, " stim90")) %>%
      # 91 = ● (size 2)
      add_trace(data = df_3d %>% filter(Stimulus == 91),
                x = ~`60`, y = ~`90`, z = ~`120`,
                color = ~factor(GV),
                colors = c("#E41A1C", "#377EB8"),
                type = "scatter3d", mode = "markers",
                marker = list(size = 2, symbol = "circle"),
                name = ~paste0(GV, " stim91")) %>%
      layout(scene = list(
        xaxis = list(title = "Neuron 60",  range = c(0, 200)),
        yaxis = list(title = "Neuron 90",  range = c(0, 200)),
        zaxis = list(title = "Neuron 120", range = c(0, 200)),
        aspectmode = "cube"
      ))
  })
  
  # --- Decision boundary ---
  output$p_boundary <- renderPlotly({
    req(sim_result())
    withProgress(message = "Fitting decision boundary...", {
      df_scatter <- sim_result() %>%
        filter(Neuron %in% c(60, 90, 120)) %>%
        pivot_wider(id_cols = c(GV, Stimulus, Trial),
                    names_from = Neuron, values_from = Spikes) %>%
        mutate(stim_bin = as.numeric(as.factor(Stimulus)) - 1)
      
      fit <- glm(stim_bin ~ `60` + `90` + `120`,
                 data = df_scatter, family = binomial)
      
      x_seq <- seq(0, 200, length.out = 15)
      y_seq <- seq(0, 200, length.out = 15)
      z_seq <- seq(0, 200, length.out = 15)
      grid3d <- expand.grid(`60` = x_seq, `90` = y_seq, `120` = z_seq)
      grid3d$prob <- predict(fit, newdata = grid3d, type = "response")
      decision_points <- grid3d %>% filter(abs(prob - 0.5) < 0.03)
      
      p <- plot_ly() %>%
        # 90 = + (size 3)
        add_trace(data = df_scatter %>% filter(Stimulus == 90),
                  x = ~`60`, y = ~`90`, z = ~`120`,
                  color = ~factor(GV),
                  colors = c("#E41A1C", "#377EB8"),
                  type = "scatter3d", mode = "markers",
                  marker = list(size = 3, symbol = "cross"),
                  name = ~paste0(GV, " stim90")) %>%
        # 91 = ● (size 2)
        add_trace(data = df_scatter %>% filter(Stimulus == 91),
                  x = ~`60`, y = ~`90`, z = ~`120`,
                  color = ~factor(GV),
                  colors = c("#E41A1C", "#377EB8"),
                  type = "scatter3d", mode = "markers",
                  marker = list(size = 2, symbol = "circle"),
                  name = ~paste0(GV, " stim91"))
      
      if (nrow(decision_points) > 0) {
        p <- p %>%
          add_trace(data = decision_points,
                    x = ~`60`, y = ~`90`, z = ~`120`,
                    type = "mesh3d",
                    opacity = 0.4,
                    color = I("navy"),
                    name = "Decision boundary",
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
  
  # --- LDA 2D ---
  output$g_lda_2d <- renderPlot({
    req(lda_result())
    r <- lda_result()
    
    arrows_df <- data.frame(
      x    = c(r$mean0[1], r$mean0[1]),
      y    = c(r$mean0[2], r$mean0[2]),
      xend = c(r$mean0[1] + r$df_vec[1],   r$mean0[1] + r$w_scaled[1]),
      yend = c(r$mean0[2] + r$df_vec[2],   r$mean0[2] + r$w_scaled[2]),
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
      labs(title = "平均値差ベクトル df & 判別軸 w",
           subtitle = paste("d' =", round(r$dp, 3)),
           x = "Neuron 1", y = "Neuron 2", color = "Legend") +
      theme_minimal(base_size = 13)
  })
  
  # --- LDA 1D ---
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
      labs(title = "判別軸 w への射影後の1次元分布",
           subtitle = paste("d' =", round(r$dp_1d, 3)),
           x = "Projection onto w", y = "Density") +
      xlim(-6, 6) +
      theme_minimal(base_size = 13)
  })
  
  # output$g_lda_combined <- renderPlotly({
  #   req(lda_result())
  #   r <- lda_result()
  #   
  #   # w軸の単位ベクトル・基点
  #   w_unit <- r$w_scaled / sqrt(sum(r$w_scaled^2))
  #   origin <- r$mean0
  #   
  #   # 全点の射影計算
  #   all_pts <- r$data %>%
  #     mutate(
  #       t     = (neuron1 - origin[1]) * w_unit[1] +
  #         (neuron2 - origin[2]) * w_unit[2],
  #       xproj = origin[1] + t * w_unit[1],
  #       yproj = origin[2] + t * w_unit[2],
  #       label = paste0("class: ", class,
  #                      "<br>neuron1: ", round(neuron1, 2),
  #                      "<br>neuron2: ", round(neuron2, 2),
  #                      "<br>projection (t): ", round(t, 3),
  #                      "<br>proj x: ", round(xproj, 2),
  #                      "<br>proj y: ", round(yproj, 2))
  #     )
  #   
  #   t_range <- range(all_pts$t)
  #   
  #   # w軸の延長線
  #   wline_x <- c(origin[1] + t_range[1] * w_unit[1],
  #                origin[1] + t_range[2] * w_unit[1])
  #   wline_y <- c(origin[2] + t_range[1] * w_unit[2],
  #                origin[2] + t_range[2] * w_unit[2])
  #   
  #   # --- 上段: 2D scatter ---
  #   p_top <- plot_ly() %>%
  #     # w軸
  #     add_trace(x = wline_x, y = wline_y,
  #               type = "scatter", mode = "lines",
  #               line = list(color = "black", width = 2),
  #               name = "w axis",
  #               hoverinfo = "none") %>%
  #     # class 0
  #     add_trace(data = all_pts %>% filter(class == 0),
  #               x = ~neuron1, y = ~neuron2,
  #               type = "scatter", mode = "markers",
  #               marker = list(color = "blue", size = 5, opacity = 0.4),
  #               name = "class 0",
  #               text = ~label,
  #               hovertemplate = "%{text}<extra></extra>") %>%
  #     # class 1
  #     add_trace(data = all_pts %>% filter(class == 1),
  #               x = ~neuron1, y = ~neuron2,
  #               type = "scatter", mode = "markers",
  #               marker = list(color = "red", size = 5, opacity = 0.4),
  #               name = "class 1",
  #               text = ~label,
  #               hovertemplate = "%{text}<extra></extra>") %>%
  #     # 射影点（w軸上）
  #     add_trace(data = all_pts %>% filter(class == 0),
  #               x = ~xproj, y = ~yproj,
  #               type = "scatter", mode = "markers",
  #               marker = list(color = "blue", size = 4,
  #                             symbol = "circle-open", opacity = 0.3),
  #               name = "proj (class 0)",
  #               hoverinfo = "none",
  #               showlegend = FALSE) %>%
  #     add_trace(data = all_pts %>% filter(class == 1),
  #               x = ~xproj, y = ~yproj,
  #               type = "scatter", mode = "markers",
  #               marker = list(color = "red", size = 4,
  #                             symbol = "circle-open", opacity = 0.3),
  #               name = "proj (class 1)",
  #               hoverinfo = "none",
  #               showlegend = FALSE) %>%
  #     layout(
  #       xaxis = list(title = "Neuron 1", scaleanchor = "y"),
  #       yaxis = list(title = "Neuron 2"),
  #       annotations = list(list(
  #         x = 0.01, y = 0.99, xref = "paper", yref = "paper",
  #         text = paste0("d' = ", round(r$dp, 3)),
  #         showarrow = FALSE, font = list(size = 13)
  #       ))
  #     )
  #   
  #   # --- 下段: 1D density ---
  #   proj_df <- data.frame(
  #     t     = c(r$proj0, r$proj1),
  #     class = factor(c(rep(0, r$n), rep(1, r$n)))
  #   )
  #   
  #   # density計算
  #   d0 <- density(r$proj0)
  #   d1 <- density(r$proj1)
  #   
  #   p_bot <- plot_ly() %>%
  #     add_trace(x = d0$x, y = d0$y,
  #               type = "scatter", mode = "lines",
  #               fill = "tozeroy", fillcolor = "rgba(0,0,255,0.3)",
  #               line = list(color = "blue"),
  #               name = "class 0 density") %>%
  #     add_trace(x = d1$x, y = d1$y,
  #               type = "scatter", mode = "lines",
  #               fill = "tozeroy", fillcolor = "rgba(255,0,0,0.3)",
  #               line = list(color = "red"),
  #               name = "class 1 density") %>%
  #     # 個別の射影点をrug plotとして
  #     add_trace(data = proj_df %>% filter(class == 0),
  #               x = ~t, y = rep(-0.01, sum(proj_df$class == 0)),
  #               type = "scatter", mode = "markers",
  #               marker = list(color = "blue", size = 4,
  #                             symbol = "line-ns", opacity = 0.3),
  #               name = "proj pts (0)",
  #               hovertemplate = "t = %{x:.3f}<extra>class 0</extra>") %>%
  #     add_trace(data = proj_df %>% filter(class == 1),
  #               x = ~t, y = rep(-0.015, sum(proj_df$class == 1)),
  #               type = "scatter", mode = "markers",
  #               marker = list(color = "red", size = 4,
  #                             symbol = "line-ns", opacity = 0.3),
  #               name = "proj pts (1)",
  #               hovertemplate = "t = %{x:.3f}<extra>class 1</extra>") %>%
  #     layout(
  #       xaxis = list(title = "Projection onto w",
  #                    range = t_range),
  #       yaxis = list(title = "Density"),
  #       annotations = list(list(
  #         x = 0.01, y = 0.99, xref = "paper", yref = "paper",
  #         text = paste0("d' = ", round(r$dp_1d, 3)),
  #         showarrow = FALSE, font = list(size = 13)
  #       ))
  #     )
  #   
  #   # 上下に並べる
  #   subplot(p_top, p_bot,
  #           nrows = 2,
  #           heights = c(0.65, 0.35),
  #           shareX = FALSE,
  #           titleX = TRUE,
  #           titleY = TRUE)
  # })
}

# ============================================================
shinyApp(ui, server)