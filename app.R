library(shiny)
library(shinyjs)
library(shinyBS)
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
    contrast_A = 40, contrast_B = 40,
    sigma_g_A  = 0.01, sigma_g_B  = 0.01,
    spont_A    = 25,  spont_B    = 0,
    fano_A     = 1,  fano_B     = 1
  ),
  blindsight = list(
    contrast_A = 40, contrast_B = 40,
    sigma_g_A  = 0.2, sigma_g_B  = 0.05,
    spont_A    = 2,  spont_B    = 2,
    fano_A     = 1,  fano_B     = 1
  ),
  subjective = list(
    contrast_A = 40, contrast_B = 40,
    sigma_g_A  = 0.01, sigma_g_B  = 0.01,
    spont_A    = 2,  spont_B    = 2,
    fano_A     = 3,  fano_B     = 1
  )
)

active_sliders <- list(
  manual     = c("contrast_A", "contrast_B", "sigma_g_A", "sigma_g_B",
                 "spont_A", "spont_B", "fano_A", "fano_B"),
  samaha     = c("contrast_A", "contrast_B", "spont_A"),
  blindsight = c("contrast_A", "contrast_B", "sigma_g_A"),
  subjective = c("contrast_A", "contrast_B", "fano_A")
)

all_sliders <- c("contrast_A", "contrast_B", "sigma_g_A", "sigma_g_B",
                 "spont_A", "spont_B", "fano_A", "fano_B")

# Key assumption definitions per preset
key_assumptions <- list(
  manual     = NULL,
  samaha     = tagList(
    "Lower α indicates a higher excitability (feature-independent, baseline-like signal addition)",
    tags$sup(tags$a(href = "#ref1", "1")), ", ",
    tags$sup(tags$a(href = "#ref2", "2"))
  ),
  blindsight = tagList(
    "In V1-lesioned conditions, enhanced trial-to-trial shared fluctuations can be parsimoniously captured as increased shared gain variability.",
    tags$sup(tags$a(href = "#ref3", "3"))
  ),
  subjective = tagList(
    "Inattention is associated with higher spike-count variability (higher Fano factor) compared with the attended condition.",
    tags$sup(tags$a(href = "#ref4", "4")), ", ",
    tags$sup(tags$a(href = "#ref5", "5")), ", ",
    tags$sup(tags$a(href = "#ref6", "6")), ", ",
    tags$sup(tags$a(href = "#ref7", "7"))
  )
)

# Tooltip text per slider per preset (NULL = no tooltip)
slider_tooltips <- list(
  samaha = list(
    spont_A = "Under lower α (Condition A), baseline activity increases regardless of stimulus orientation — this is the key manipulation."
  ),
  blindsight = list(
    sigma_g_A = "Under inattention (Condition A), trial-by-trial gain fluctuations are larger, producing correlated variability across the population."
  ),
  subjective = list(
    fano_A = "Under high Fano Factor (Condition A), spike count variance relative to the mean is increased, broadening the population response distribution."
  ),
  manual = list()
)

# ============================================================
# Helper: collapsible note panel
# ============================================================
note_panel <- function(id, content) {
  tagList(
    bsCollapse(
      id = id,
      bsCollapsePanel(
        title = "📖",
        style = "info",
        tags$div(style = "font-size:14px; color:#888; line-height:1.7;", content)
      )
    )
  )
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML("
    .slider-disabled { opacity: 0.35; pointer-events: none; }
    .panel-info > .panel-heading { background-color: #f0f0f0; border-color: #cccccc; color: #444; font-size:13px; padding: 6px 12px; }
    .panel-info { border-color: #cccccc; margin-top: 8px; }
    .key-assumption-box {
      background: #fffbe6;
      border-left: 4px solid #F5A623;
      padding: 10px 14px;
      margin-bottom: 12px;
      font-size: 14px;
      color: #555;
      line-height: 1.6;
      border-radius: 3px;
    }
    .tooltip-inner { max-width: 400px; text-align: left; }
  ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateTooltip', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) {
        $(el).closest('.form-group').attr('title', msg.title)
             .tooltip('destroy').tooltip({placement: 'right'});
      }
    });
  ")),
  sidebarLayout(
    sidebarPanel(
      selectInput("preset", "Preset",
                  choices = c("Manual"              = "manual",
                              "Samaha effect"        = "samaha",
                              "Blindsight"           = "blindsight",
                              "Subjective inflation" = "subjective")),
      hr(),
      sliderInput("stim1",    "Stimulus 1 orientation (°)", min = 1, max = 180, value = 90,  step = 1),
      sliderInput("stim2",    "Stimulus 2 orientation (°)", min = 1, max = 180, value = 100, step = 1),
      sliderInput("n_trials", "Number of trials", min = 50, max = 500, value = 100, step = 50),
      hr(),
      h5("Condition A", style = "color:#E41A1C; font-weight:bold;"),
      div(id = "wrap_contrast_A", sliderInput("contrast_A", "Stimulus contrast (%)", min = 1,    max = 100, value = 40,   step = 1)),
      div(id = "wrap_sigma_g_A",  sliderInput("sigma_g_A",  "Gain variability",      min = 0.01, max = 0.5, value = 0.05, step = 0.01)),
      div(id = "wrap_spont_A",    sliderInput("spont_A",    "Baseline activity",     min = 0,    max = 25,  value = 2,    step = 0.5)),
      div(id = "wrap_fano_A",     sliderInput("fano_A",     "Fano Factor",           min = 1,    max = 5,   value = 1,    step = 0.5)),
      hr(),
      h5("Condition B", style = "color:#377EB8; font-weight:bold;"),
      div(id = "wrap_contrast_B", sliderInput("contrast_B", "Stimulus contrast (%)", min = 1,    max = 100, value = 40,   step = 1)),
      div(id = "wrap_sigma_g_B",  sliderInput("sigma_g_B",  "Gain variability",      min = 0.01, max = 0.5, value = 0.05, step = 0.01)),
      div(id = "wrap_spont_B",    sliderInput("spont_B",    "Baseline activity",     min = 0,    max = 25,  value = 2,    step = 0.5)),
      div(id = "wrap_fano_B",     sliderInput("fano_B",     "Fano Factor",           min = 1,    max = 10,  value = 1,    step = 0.5)),
      hr(),
      actionButton("run", "Run simulation", class = "btn-primary")
    ),
    mainPanel(
      h4("Stimuli"),
      plotOutput("g_stimuli", height = "180px"),
      hr(),
      # ---- Encoding section ----
      div(style = "background:#fffaf5; padding:15px; border-left:5px solid #F5A623; margin-bottom:16px;",
          h4("Encoding", style = "color:#F5A623; margin-top:0;"),
          uiOutput("key_assumption_box"),   # Key assumption box
          h4("Tuning curves"),
          note_panel("note_tuning", tagList(
            tags$p("Each neuron has a Gaussian orientation tuning curve centered on its preferred orientation."),
            tags$p("The Naka-Rushton function maps stimulus contrast to peak neural response (height of tuning curve):"),
            tags$p(HTML("R(c) = R<sub>max</sub> &times; c<sup>n</sup> / (c<sup>n</sup> + C<sub>50</sub><sup>n</sup>)")),
            tags$p(HTML("where R<sub>max</sub> = 115, C<sub>50</sub> = 19.3, n = 2.9 (Citation).")),
            tags$p("Baseline activity adds a constant offset to all neurons' responses."),
            tags$hr(),
            tags$p(tags$strong("Gain variability model:")),
            tags$p("On each trial, a single gain factor g is sampled from a Gamma distribution and applied multiplicatively to the mean firing rates of all 180 neurons:"),
            tags$p(HTML("<code>g ~ Gamma(shape = 1/σ_g², scale = σ_g²)</code>  →  mean = 1, variance = σ_g²")),
            tags$p("Spike counts are then drawn from a Negative Binomial distribution:"),
            tags$p(HTML("<code>r_i ~ NegBinom(mu = g · μ_i,  size = g · μ_i / (Fano − 1))</code>")),
            tags$p(HTML("where μ_i is the tuning curve value of neuron i. When Fano = 1, this reduces to a Poisson process (size → ∞)."))
          )),
          fluidRow(
            column(6, plotOutput("g_nr")),
            column(6, plotOutput("g_tuning"))
          ),
          hr(),
          h4("Trial-by-trial spike distributions"),
          note_panel("note_3d", tagList(
            tags$p("Three neurons tuned to different orientations (90, 95, and 100 degs) are shown for illustration."),
            tags$p("Each point represents one trial.")
          )),
          plotlyOutput("p_3d")
      ),
      # ---- Decoding / Read-out section ----
      div(style = "background:#f5fffe; padding:15px; border-left:5px solid #17A589; margin-bottom:16px;",
          h4("Decoding / Read-out", style = "color:#17A589; margin-top:0;"),
          h4("Total spike count"),
          note_panel("note_density", tagList(
            tags$p("The total spike count across the population is summed for each trial."),
            tags$p("Visual awareness is represented by the total spike count across the population, with the hyperplane (not shown in graph) indicating the boundary between low and high visibility ratings.")
          )),
          plotOutput("g_density"),
          uiOutput("text_density"),
          hr(),
          h4("Discrimination boundary"),
          note_panel("note_boundary", tagList(
            tags$p("A logistic regression classifier is fit to the 3-neuron spike counts to find the hyperplane that best separates S1 from S2 responses."),
            tags$p("The red / blue surface shows the decision boundary where P(S2) = 0.5 for condition A / B, independently."),
            tags$p("Orientation sensitivity is given by the discriminability between spike distributions,"),
            tags$p("with decision uncertainty corresponding to the distance of each spike point from the discrimination boundary.")
          )),
          plotlyOutput("p_boundary"),
          uiOutput("text_boundary")
      ),
      # ---- References ----
      hr(),
      tags$div(
        style = "font-size:11px; color:#999; line-height:2.0; margin-top:8px; margin-bottom:20px;",
        tags$strong("References", style = "font-size:12px; color:#777; display:block; margin-bottom:4px;"),
        tags$p(id = "ref1",
               tags$sup("1"), " ",
               "Goris RL, Movshon JA, Simoncelli EP. Partitioning neuronal variability. ",
               tags$em("Nat Neurosci."), " 2014;17(6):858–865. doi:10.1038/nn.3711"
        ),
        tags$p(id = "ref2",
               tags$sup("2"), " ",
               "Lin IC, Okun M, Carandini M, Harris KD. The Nature of Shared Cortical Variability. ",
               tags$em("Neuron."), " 2015;87(3):644–656. doi:10.1016/j.neuron.2015.06.035"
        ),
        tags$p(id = "ref3",
               tags$sup("3"), " ",
               "Shapcott K, Schmiedt J, Saunders R, et al. Correlated activity of cortical neurons survives extensive removal of feedforward sensory input. ",
               tags$em("Sci Rep."), " 2016;6:34886. doi:10.1038/srep34886"
        ),
        tags$p(id = "ref4",
               tags$sup("4"), " ",
               "Cohen MR, Maunsell JH. Attention improves performance primarily by reducing interneuronal correlations. ",
               tags$em("Nat Neurosci."), " 2009;12(12):1594–1600. doi:10.1038/nn.2439"
        ),
        tags$p(id = "ref5",
               tags$sup("5"), " ",
               "Mitchell JF, Sundberg KA, Reynolds JH. Differential Attention-Dependent Response Modulation across Cell Classes in Macaque Visual Area V4. ",
               tags$em("Neuron."), " 2007;55(1):131–141."
        ),
        tags$p(id = "ref6",
               tags$sup("6"), " ",
               "Mitchell JF, Sundberg KA, Reynolds JH. Spatial Attention Decorrelates Intrinsic Activity Fluctuations in Macaque Area V4. ",
               tags$em("Neuron."), " 2009;63(6):879–888."
        ),
        tags$p(id = "ref7",
               tags$sup("7"), " ",
               "Ghosh S, Maunsell JHR. Single trial neuronal activity dynamics of attentional intensity in monkey visual area V4. ",
               tags$em("Nat Commun."), " 2021;12:2003. doi:10.1038/s41467-021-22281-2"
        )
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  # ---- Preset: update sliders, enable/disable, update tooltips ----
  observeEvent(input$preset, {
    p      <- presets[[input$preset]]
    active <- active_sliders[[input$preset]]
    updateSliderInput(session, "contrast_A", value = p$contrast_A)
    updateSliderInput(session, "contrast_B", value = p$contrast_B)
    updateSliderInput(session, "sigma_g_A",  value = p$sigma_g_A)
    updateSliderInput(session, "sigma_g_B",  value = p$sigma_g_B)
    updateSliderInput(session, "spont_A",    value = p$spont_A)
    updateSliderInput(session, "spont_B",    value = p$spont_B)
    updateSliderInput(session, "fano_A",     value = p$fano_A)
    updateSliderInput(session, "fano_B",     value = p$fano_B)
    for (sl in all_sliders) {
      wrap_id <- paste0("wrap_", sl)
      if (sl %in% active) removeClass(wrap_id, "slider-disabled")
      else                 addClass(wrap_id,    "slider-disabled")
    }
    # Update tooltips via JavaScript
    tips <- slider_tooltips[[input$preset]]
    session$sendCustomMessage("updateTooltip", list(
      id    = "spont_A",
      title = if (!is.null(tips$spont_A))   tips$spont_A   else ""
    ))
    session$sendCustomMessage("updateTooltip", list(
      id    = "sigma_g_A",
      title = if (!is.null(tips$sigma_g_A)) tips$sigma_g_A else ""
    ))
    session$sendCustomMessage("updateTooltip", list(
      id    = "fano_A",
      title = if (!is.null(tips$fano_A))    tips$fano_A    else ""
    ))
  })
  
  # ---- Key assumption box ----
  output$key_assumption_box <- renderUI({
    txt <- key_assumptions[[input$preset]]
    if (is.null(txt)) return(NULL)
    tags$div(class = "key-assumption-box", txt)
  })
  
  # ---- sim_result ----
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
      
      simulate <- function(stim, sigma_g, n_trials, tuning_curves, fano) {
        do.call(rbind, lapply(1:n_trials, function(i) {
          idx  <- max(1, min(180, round(stim)))
          g    <- rgamma(1, shape = 1/sigma_g^2, scale = sigma_g^2)
          mu   <- g * tuning_curves[, idx]
          mu[mu <= 0] <- 1e-8
          nb_size <- if (fano <= 1) rep(1000000, length(mu)) else mu / (fano - 1)
          nb_size[nb_size <= 0] <- 1000000
          resp <- rnbinom(n_neurons, size = nb_size, mu = mu)
          data.frame(Spikes = resp, Neuron = seq_len(n_neurons),
                     Stimulus = stim, Trial = i)
        }))
      }
      
      incProgress(0.1, message = "Building tuning curves...")
      max_fr_A <- Rmax * input$contrast_A^n_nr / (input$contrast_A^n_nr + C50^n_nr)
      tc_A     <- make_tuning_curves(max_fr_A, input$spont_A)
      max_fr_B <- Rmax * input$contrast_B^n_nr / (input$contrast_B^n_nr + C50^n_nr)
      tc_B     <- make_tuning_curves(max_fr_B, input$spont_B)
      
      incProgress(0.3, message = "Condition A...")
      df_A <- rbind(
        simulate(input$stim1, input$sigma_g_A, n_trials, tc_A, input$fano_A),
        simulate(input$stim2, input$sigma_g_A, n_trials, tc_A, input$fano_A)
      ) %>% mutate(Condition = "A")
      
      incProgress(0.3, message = "Condition B...")
      df_B <- rbind(
        simulate(input$stim1, input$sigma_g_B, n_trials, tc_B, input$fano_B),
        simulate(input$stim2, input$sigma_g_B, n_trials, tc_B, input$fano_B)
      ) %>% mutate(Condition = "B")
      
      incProgress(0.2, message = "Done!")
      rbind(df_A, df_B)
    })
  })
  
  # ---- Stimuli: Gabor patches ----
  output$g_stimuli <- renderPlot({
    req(input$stim1, input$stim2, input$contrast_A, input$contrast_B)
    make_gabor <- function(theta_deg, contrast_scale, nx = 60) {
      theta <- theta_deg * pi / 180
      x <- seq(-1, 1, length.out = nx)
      grid <- expand.grid(x = x, y = x)
      xr <-  grid$x * cos(theta) + grid$y * sin(theta)
      yr <- -grid$x * sin(theta) + grid$y * cos(theta)
      sigma <- 0.35; freq <- 3.5
      val <- exp(-(xr^2 + yr^2) / (2 * sigma^2)) * cos(2 * pi * freq * xr)
      data.frame(x = grid$x, y = grid$y, val = val * contrast_scale)
    }
    cA_scale <- input$contrast_A / 100
    cB_scale <- input$contrast_B / 100
    panels <- list(
      list(theta = input$stim1, cs = cA_scale, px = 1, py = 2),
      list(theta = input$stim2, cs = cA_scale, px = 2, py = 2),
      list(theta = input$stim1, cs = cB_scale, px = 1, py = 1),
      list(theta = input$stim2, cs = cB_scale, px = 2, py = 1)
    )
    df_all <- do.call(rbind, lapply(panels, function(p) {
      g <- make_gabor(p$theta, p$cs)
      g$x_off <- g$x + (p$px - 1) * 2.5
      g$y_off <- g$y + (p$py - 1) * 2.5
      g
    }))
    labels_df <- data.frame(
      x_off = c(0, 2.5, 0, 2.5), y_off = c(3.1, 3.1, 0.6, 0.6),
      label = c("S1", "S2", "S1", "S2")
    )
    cond_labels_df <- data.frame(
      x_off = c(1.25, 1.25), y_off = c(3.7, 1.2),
      label = c("Condition A", "Condition B"), col = c("#E41A1C", "#377EB8")
    )
    ggplot(df_all, aes(x = x_off, y = y_off, fill = val)) +
      geom_raster(interpolate = TRUE) +
      scale_fill_gradient2(low = "black", mid = "gray50", high = "white",
                           midpoint = 0, limits = c(-1, 1), guide = "none") +
      geom_text(data = labels_df, aes(x = x_off, y = y_off, label = label),
                inherit.aes = FALSE, size = 5, vjust = 0, color = "white") +
      geom_text(data = cond_labels_df,
                aes(x = x_off, y = y_off, label = label, color = col),
                inherit.aes = FALSE, size = 5, fontface = "bold") +
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
               vjust = 1, hjust = 0.5, size = 7) +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      coord_cartesian(ylim = c(0, 130)) +
      scale_y_continuous(breaks = seq(0, 120, by = 30)) +
      labs(x = "Contrast (%)", y = "Peak response") +
      theme_classic(base_size = 20)
  })
  
  # ---- Tuning curve plot ----
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
                   Neuron = i, Color = colors[i], Condition = cond_label)
      }))
    }
    max_fr_A <- Rmax * input$contrast_A^n_nr / (input$contrast_A^n_nr + C50^n_nr)
    max_fr_B <- Rmax * input$contrast_B^n_nr / (input$contrast_B^n_nr + C50^n_nr)
    df_all <- rbind(
      make_tc_df(max_fr_A, input$spont_A, "Condition A"),
      make_tc_df(max_fr_B, input$spont_B, "Condition B")
    ) %>% mutate(Condition = factor(Condition,
                                    levels = c("Condition A", "Condition B")))
    y_max <- max(df_all$FiringRate) * 1.05
    strip_colors <- c("Condition A" = "#E41A1C", "Condition B" = "#377EB8")
    g <- ggplot(df_all, aes(x = Orientation, y = FiringRate, color = Color, group = Neuron)) +
      geom_line() +
      scale_color_identity() +
      facet_wrap(~ Condition, nrow = 2) +
      scale_x_continuous(breaks = c(0, 60, 120, 180)) +
      scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
      coord_cartesian(ylim = c(0, 125)) +
      labs(x = "Orientation (°)", y = "Spikes") +
      theme_classic(base_size = 20) +
      theme(strip.text = element_text(size = 13, face = "bold"))
    gb <- ggplot_build(g)
    gt <- ggplot_gtable(gb)
    strip_idx <- which(grepl("strip", gt$layout$name))
    strip_idx <- strip_idx[order(gt$layout$t[strip_idx])]
    fills <- strip_colors[levels(df_all$Condition)]
    for (i in seq_along(strip_idx)) {
      gt$grobs[[strip_idx[i]]]$grobs[[1]]$children[[1]]$gp$fill <- fills[i]
      gt$grobs[[strip_idx[i]]]$grobs[[1]]$children[[2]]$children[[1]]$gp$col  <- "white"
    }
    grid::grid.draw(gt)
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
      theme_classic(base_size = 20)
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
      layout(
        font   = list(size = 12),
        legend = list(font = list(size = 16)),
        scene  = list(
          xaxis = list(title = "Neuron 95",  range = c(0, 180)),
          yaxis = list(title = "Neuron 90",  range = c(0, 180)),
          zaxis = list(title = "Neuron 100", range = c(0, 180)),
          aspectmode = "cube"
        )
      )
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
        dp <- grid3d %>% filter(abs(prob - 0.5) < 0.07)
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
      p %>% layout(
        font   = list(size = 12),
        legend = list(font = list(size = 16)),
        scene  = list(
          xaxis = list(title = "Neuron 95",  range = c(0, 180)),
          yaxis = list(title = "Neuron 90",  range = c(0, 180)),
          zaxis = list(title = "Neuron 100", range = c(0, 180)),
          aspectmode = "cube"
        )
      )
    })
  })
  
  # ---- Subtitle texts ----
  subtitle_texts <- list(
    manual     = list(density = NULL, boundary = NULL),
    samaha     = list(
      density  = list(
        title = "Samaha effect",
        body  = "In condition A (= under lower α), increased baseline activity shifts the population response toward greater total spiking, causing higher visibility rating."
      ),
      boundary = list(
        title = "Samaha effect",
        body  = "In condition A (= under lower α), increased baseline activity shifts the population response along the direction parallel to the discrimination hyperplane, leaving orientation discrimination sensitivity unchanged."
      )
    ),
    subjective = list(
      density  = list(
        title = "Subjective inflation",
        body  = "In condition A (= under inattention), greater spike variability increases the chance of the population response exceeding the detection hyperplane."
      ),
      boundary = list(
        title = "Subjective inflation",
        body  = "In condition A (= under inattention), greater spike variability reduces manifold separability."
      )
    ),
    blindsight = list(
      density  = list(
        title = "Blindsight",
        body  = "In condition A (= under low stability), increased gain variability expands the manifold along the total spike axis, impairing yes/no detection sensitivity."
      ),
      boundary = list(
        title = "Blindsight",
        body  = "In condition A (= under low stability), increased gain variability produces response correlations parallel to the orientation discrimination hyperplane, leaving discrimination sensitivity unaffected."
      )
    )
  )
  
  make_subtitle_card <- function(item) {
    if (is.null(item)) return(NULL)
    tags$div(
      style = "border-left: 4px solid #17A589; background: #f5fffe; border-radius: 0 6px 6px 0; box-shadow: 0 1px 4px rgba(0,0,0,0.08); padding: 12px 16px; margin-top: 12px;",
      tags$div(
        style = "font-size:17px; font-weight:bold; color:#17A589; margin-bottom:6px;",
        tags$span("💡 "), item$title
      ),
      tags$span(item$body, style = "font-size:15px; color:#444; line-height:1.7;")
    )
  }
  
  output$text_density <- renderUI({
    make_subtitle_card(subtitle_texts[[input$preset]]$density)
  })
  
  output$text_boundary <- renderUI({
    make_subtitle_card(subtitle_texts[[input$preset]]$boundary)
  })
}

# ============================================================
shinyApp(ui, server)