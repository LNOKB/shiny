library(shiny)
library(plotly)
library(tidyverse)

# ui <- fluidPage(
#   titlePanel("Neural Population Response Simulator"),
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("contrast",
#                   label = "Contrast (%)",
#                   min = 1, max = 100, value = 40),
#       sliderInput("n_trials",
#                   label = "Number of trials",
#                   min = 50, max = 500, value = 100, step = 50),
#       actionButton("run", "Run simulation", class = "btn-primary")
#     ),
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Total spike count", plotOutput("g_density")),
#         tabPanel("3D scatter",        plotlyOutput("p_3d")),
#         tabPanel("Decision boundary", plotlyOutput("p_boundary"))  # ← 追加
#       )
#     )
#   )
# )
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
      actionButton("run", "Run simulation", class = "btn-primary")
    ),
    mainPanel(
      plotOutput("g_density"),
      hr(),  # 区切り線
      plotlyOutput("p_3d"),
      hr(),
      plotlyOutput("p_boundary")
    )
  )
)
server <- function(input, output) {
  
  # ボタンを押した時だけ実行
  sim_result <- eventReactive(input$run, {
    
    withProgress(message = "Simulating...", value = 0, {
      
      # Naka-Rushton
      Rmax <- 115; C50 <- 19.3; n_nr <- 2.9
      contrast <- input$contrast
      max_fr <- Rmax * contrast^n_nr / (contrast^n_nr + C50^n_nr)
      
      # チューニングカーブ
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
      
      # シミュレーション関数
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
      
      # high / low gain variability
      incProgress(0.3, message = "High GV...")
      df_high <- rbind(
        simulate(90, 0.2, n_trials),
        simulate(92, 0.2, n_trials)
      ) %>% mutate(GV = "high")
      
      incProgress(0.3, message = "Low GV...")
      df_low <- rbind(
        simulate(90, 0.01, n_trials),
        simulate(92, 0.01, n_trials)
      ) %>% mutate(GV = "low")
      
      incProgress(0.2, message = "Done!")
      rbind(df_high, df_low)
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
    sim_result() %>%
      filter(Neuron %in% c(60, 90, 120)) %>%
      pivot_wider(id_cols = c(GV, Stimulus, Trial),
                  names_from = Neuron, values_from = Spikes) %>%
      plot_ly(x = ~`60`, y = ~`90`, z = ~`120`,
              color = ~factor(GV),
              symbol = ~factor(Stimulus),
              colors = c("#E41A1C", "#377EB8"),
              type = "scatter3d", mode = "markers",
              marker = list(size = 3)) %>%
      layout(scene = list(
        xaxis = list(title = "Neuron 60",  range = c(0, 200)),
        yaxis = list(title = "Neuron 90",  range = c(0, 200)),
        zaxis = list(title = "Neuron 120", range = c(0, 200)),
        aspectmode = "cube"
      ))
  })
  
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
      
      # decision boundary点を計算
      x_seq <- seq(0, 200, length.out = 15)
      y_seq <- seq(0, 200, length.out = 15)
      z_seq <- seq(0, 200, length.out = 15)
      grid3d <- expand.grid(`60` = x_seq, `90` = y_seq, `120` = z_seq)
      grid3d$prob <- predict(fit, newdata = grid3d, type = "response")
      decision_points <- grid3d %>% filter(abs(prob - 0.5) < 0.03)
      
      # 散布図 + boundary
      p <- df_scatter %>%
        plot_ly(x = ~`60`, y = ~`90`, z = ~`120`,
                color = ~factor(GV),
                symbol = ~factor(Stimulus),
                colors = c("#E41A1C", "#377EB8"),
                type = "scatter3d", mode = "markers",
                marker = list(size = 3))
      
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
}

shinyApp(ui, server)