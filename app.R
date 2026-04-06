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

# Condition labels per preset
cond_labels <- list(
  manual     = list(A = "Condition A", B = "Condition B"),
  samaha     = list(A = "Low α",   B = "High α"),
  blindsight = list(A = "High gain fluctuations", B = "Low gain fluctuations"),
  subjective = list(A = "Low attention", B = "High attention")
)

# Phenomenon intro per preset
phenomenon_intro_content <- list(
  manual     = NULL,
  samaha     = list(
    title = "Samaha effect",
    body  = "Low pre-stimulus α power predicts increased visibility while leaving orientation discrimination sensitivity unchanged.",
    body2 = tagList("Our simulations below demonstrate that increased baseline neural activity associated with low pre-stimulus α power", tags$sup(tags$a(href="#ref9","9")), " is sufficient to account for these observations."),
    refs  = tagList(tags$sup(tags$a(href="#ref11","11")), ", ", tags$sup(tags$a(href="#ref12","12"))),
    img   = "samaha.png"
  ),
  blindsight = list(
    title = "Blindsight",
    body  = "Lesions in the primary visual cortex impair awareness (yes/no detection sensitivity) while leaving orientation discrimination sensitivity largely intact.",
    body2 = tagList("Our simulations below demonstrate that elevating neural gain fluctuations following V1 lesions is sufficient to account for these observations."),
    refs  = tagList(tags$sup(tags$a(href="#ref4","4")), ", ", tags$sup(tags$a(href="#ref5","5"))),
    img   = "blindsight.png"
  ),
  subjective = list(
    title = "Subjective inflation",
    body  = "Inattention leads to lower discrimination sensitivity but paradoxically increases subjective awareness.",
    body2 = tagList("Our simulations below demonstrate that increased spike variability by inattention", tags$sup(tags$a(href="#ref10","10")), " is sufficient to account for these observations."),
    refs  = tagList(tags$sup(tags$a(href="#ref6","6")), ", ", tags$sup(tags$a(href="#ref7","7"))),
    img   = "subjective.png"
  )
)

# ============================================================
# Helper: collapsible note panel
# ============================================================
note_panel <- function(id, content) {
  tagList(
    bsCollapse(
      id = id,
      bsCollapsePanel(
        title = "📖 notes",
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
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"),
    tags$script(HTML("
      MathJax = {
        tex: { inlineMath: [['\\\\(', '\\\\)']], displayMath: [['$$', '$$']] }
      };
    "))
  ),
  tags$style(HTML("
    .slider-disabled { opacity: 0.35; pointer-events: none; }
    .panel-info > .panel-heading { background-color: #f7f7f7; border-color: #e0e0e0; color: #aaa; font-size:11px; padding: 4px 10px; }
    .panel-info { border-color: #e0e0e0; margin-top: 6px; }
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
    .how-to-use-box {
      background: #f9f9f9;
      border: 1px solid #ddd;
      border-radius: 5px;
      padding: 10px 14px;
      margin-bottom: 10px;
      font-size: 12px;
      color: #666;
      line-height: 1.8;
    }
  ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateTooltip', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) {
        $(el).closest('.form-group').attr('title', msg.title)
             .tooltip('destroy').tooltip({placement: 'right'});
      }
    });
    $(document).on('shown.bs.collapse', function() {
      if (window.MathJax && MathJax.typesetPromise) {
        MathJax.typesetPromise();
      }
    });
  ")),
  sidebarLayout(
    sidebarPanel(
      tags$div(class = "how-to-use-box",
               tags$strong("How to use", style = "font-size:13px; color:#444;"),
               tags$ol(style = "padding-left:16px; margin-top:4px;",
                       tags$li("Select a preset scenario from the dropdown."),
                       tags$li("Click ", tags$strong("Run simulation"), " to generate results."),
                       tags$li("Adjust sliders if needed (greyed-out sliders are fixed for the selected preset)."),
                       tags$li("Explore the Encoding and Decoding sections below.")
               )
      ),
      hr(),
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
      uiOutput("label_cond_A"),
      div(id = "wrap_contrast_A", sliderInput("contrast_A", "Stimulus contrast (%)", min = 1,    max = 100, value = 40,   step = 1)),
      div(id = "wrap_sigma_g_A",  sliderInput("sigma_g_A",  "Gain fluctuations",     min = 0.01, max = 0.5, value = 0.05, step = 0.01)),
      div(id = "wrap_spont_A",    sliderInput("spont_A",    "Baseline activity",     min = 0,    max = 25,  value = 2,    step = 0.5)),
      div(id = "wrap_fano_A",     sliderInput("fano_A",     "Fano factor",           min = 1,    max = 5,   value = 1,    step = 0.5)),
      hr(),
      uiOutput("label_cond_B"),
      div(id = "wrap_contrast_B", sliderInput("contrast_B", "Stimulus contrast (%)", min = 1,    max = 100, value = 40,   step = 1)),
      div(id = "wrap_sigma_g_B",  sliderInput("sigma_g_B",  "Gain fluctuations",     min = 0.01, max = 0.5, value = 0.05, step = 0.01)),
      div(id = "wrap_spont_B",    sliderInput("spont_B",    "Baseline activity",     min = 0,    max = 25,  value = 2,    step = 0.5)),
      div(id = "wrap_fano_B",     sliderInput("fano_B",     "Fano factor",           min = 1,    max = 10,  value = 1,    step = 0.5)),
      hr(),
      actionButton("run", "Run simulation", class = "btn-primary")
    ),
    mainPanel(
      uiOutput("phenomenon_intro"),
      h4("Stimuli"),
      plotOutput("g_stimuli", height = "180px"),
      hr(),
      # ---- Encoding section ----
      div(style = "background:#fffaf5; padding:15px; border-left:5px solid #F5A623; margin-bottom:16px;",
          h4("Encoding", style = "color:#F5A623; margin-top:0;"),
          uiOutput("key_assumption_box"),
          h4("Tuning curves"),
          note_panel("note_tuning", tagList(
            tags$p("Spike count \\(r_{ij}\\) is drawn from a Negative Binomial distribution:"),
            tags$p(HTML("$$r_{ij} \\sim \\text{NegBinom}\\!\\left(mean = g\\,\\mu_{ij},\\; \\text{size} = \\frac{g\\,\\mu_{ij}}{F - 1}\\right)$$")),
            tags$p(HTML("where \\(\\mu_{ij}\\) is a spike count for a neuron tuned to orientation \\(j\\) under a stimulus orientation \\(i\\) ,  \\(g\\) is a multiplicative gain, and \\(F\\) is a Fano Factor. "), tags$sup(tags$a(href="#ref2","2"))),            
            tags$hr(),
            tags$p(tags$strong("Stimulus contrast", ":")),
            tags$p("The model comprises a population of 180 neurons, each selectively tuned to a unique orientation spanning the full range of orientations."),
            tags$p("Each of neurons has a circular Gaussian orientation tuning curve. The peak response amplitude is determined by the Naka-Rushton contrast-response function:"),
            tags$p(HTML("$$R(C) = R_{\\max} \\cdot \\frac{C^n}{C^n + C_{50}^n}$$")),
            tags$p(HTML("Parameters are set to \\(R_{\\max} = 115\\) spikes/s, \\(C_{50} = 19.3\\%\\), \\(n = 2.9\\), based on physiological measurements in V1."), tags$sup(tags$a(href="#ref1","1"))),
            tags$p(tags$strong("Baseline activity", ":")),
            tags$p("Baseline activity adds an orientation-independent offset \\(K\\) to all neurons, capturing responses unrelated to the stimulus."),
            tags$p(HTML("$$\\mu_{ij} = R(C) + K$$")),
            tags$p(tags$strong("Gain fluctuations", ":")),
            tags$p(HTML("On each trial, a single scalar gain \\(g\\) is sampled from a Gamma distribution and applied multiplicatively to \\(\\mu_{ij}\\). The slider value is the parameter controlling the variance of the Gamma distribution. Gain fluctuations introduce trial-by-trial spike count correlation across the population (often conceptualized as noise correlation)."), tags$sup(tags$a(href="#ref2","2")), tags$sup(tags$a(href="#ref3",", 3"))),            tags$p(HTML("$$g \\sim \\text{Gamma}\\!\\left(\\frac{1}{\\sigma_g^2},\\; \\sigma_g^2\\right), \\quad \\mathbb{E}[g] = 1, \\quad \\text{Var}[g] = \\sigma_g^2$$")),
            tags$p(tags$strong("Fano factor", ":")),
            tags$p(HTML("When \\(F = 1\\) and \\(g = 1\\), the distribution reduces to Poisson."))
          )),
          fluidRow(
            column(6, plotOutput("g_nr")),
            column(6, plotOutput("g_tuning"))
          ),
          hr(),
          h4("Trial-by-trial spike distributions"),
          note_panel("note_3d", tagList(
            tags$p("The response space is high-dimensional (180 neurons), but here we visualise three example neurons. Each point is one simulated trial."),
            tags$p(tags$strong("Gain fluctuations and Fano factor", ":")),
            tags$p("Both gain fluctuations and Fano Factor increase the variance of individual neurons' spike counts. However, they differ in their effect on the covariance of population responses. Gain fluctuations introduce shared variability across neurons: when Neuron 90 fires more, Neuron 100 tends to fire more as well. Fano factor, in contrast, inflates each neuron's variance independently, leaving the covariance structure unchanged.")
          )),
          plotlyOutput("p_3d")
      ),
      # ---- Decoding / Read-out section ----
      div(style = "background:#f5fffe; padding:15px; border-left:5px solid #17A589; margin-bottom:16px;",
          h4("Decoding / Read-out", style = "color:#17A589; margin-top:0;"),
          h4("Total spike count"),
          note_panel("note_density", tagList(
            tags$p("A total spike summed across all 180 neurons is computed for each trial. This scalar summary projects the 180-dimensional population response onto a single axis — the awareness read-out axis. According to the proposed framework, the probability of a stimulus being consciously detected corresponds to the density of this total spike count distribution over the detection threshold. Visiblity can also be explained by setting rating criteria along this axis."),
            tags$p("Since total spike count is agnostic of activity patterns of individual neurons, awareness can be separated from discrimination sensitivity and uncertainty.")
          )),
          plotOutput("g_density"),
          uiOutput("text_density"),
          hr(),
          h4("Discrimination boundary"),
          note_panel("note_boundary", tagList(
            tags$p("Within this 3D space, the mech surface shows orientation discrimination hyperplane that best separates S1 from S2."),
            tags$p("Decision uncertainty corresponds to the distance of each spike point from the this discrimination boundary."),
            tags$p("In this manner, sensitivity, uncertainty and awareness can be read out as separate constructs.")
          )),
          plotlyOutput("p_boundary"),
          uiOutput("text_boundary")
      ),
      # ---- References ----
      hr(),
      tags$div(
        style = "font-size:11px; color:#999; line-height:2.0; margin-top:8px; margin-bottom:20px;",
        tags$strong("References", style = "font-size:12px; color:#777; display:block; margin-bottom:4px;"),
        tags$p(id = "ref1", tags$sup("1"), " ",
               "Albrecht DG, Hamilton DB. Striate cortex of monkey and cat: contrast response function. ",
               tags$em("J Neurophysiol."), " 1982;48(1):217–237."),
        tags$p(id = "ref2", tags$sup("2"), " ",
               "Goris RL, Movshon JA, Simoncelli EP. Partitioning neuronal variability. ",
               tags$em("Nat Neurosci."), " 2014;17(6):858–865."),
        tags$p(id = "ref3", tags$sup("3"), " ",
               "Lin IC, Okun M, Carandini M, Harris KD. The Nature of Shared Cortical Variability. ",
               tags$em("Neuron."), " 2015;87(3):644–656."),
        tags$p(id = "ref4", tags$sup("4"), " ",
               "Miyoshi K, and Lau H. A decision-congruent heuristic gives superior metacognitive sensitivity under realistic variance assumptions. ",
               tags$em("Psychol. Rev."), " 2020;127:655–671."),
        tags$p(id = "ref5", tags$sup("5"), " ",
               "Phillips I. Blindsight is qualitatively degraded conscious vision. ",
               tags$em("Psychol. Rev."), " 2021;128:558–584."),
        tags$p(id = "ref6", tags$sup("6"), " ",
               "Rahnev D, Maniscalco B, Graves T, Huang E, de Lange FP, and Lau H. Attention induces conservative subjective biases in visual perception. ",
               tags$em("Nat. Neurosci."), " 2011;14:1513–1515."),
        tags$p(id = "ref7", tags$sup("7"), " ",
               "Okubo L, Miyoshi K, Yokosawa K, and Nishida S. Inattentional noise leads to subjective color uniformity across the visual field. ",
               tags$em("Cognition."), " 2026;266:106293."),
        tags$p(id = "ref8", tags$sup("8"), " ",
               "Cohen MR, Maunsell JH. Attention improves performance primarily by reducing interneuronal correlations. ",
               tags$em("Nat. Neurosci."), " 2009;12(12):1594–1600."),
        tags$p(id = "ref9", tags$sup("9"), " ",
               "Mitchell JF, Sundberg KA, Reynolds JH. Differential Attention-Dependent Response Modulation across Cell Classes in Macaque Visual Area V4. ",
               tags$em("Neuron."), " 2007;55(1):131–141."),
        tags$p(id = "ref10", tags$sup("10"), " ",
               "Mitchell JF, Sundberg KA, Reynolds JH. Spatial Attention Decorrelates Intrinsic Activity Fluctuations in Macaque Area V4. ",
               tags$em("Neuron."), " 2009;63(6):879–888."),
        tags$p(id = "ref11", tags$sup("11"), " ",
               "Samaha J, LaRocque JJ, and Postle BR. Spontaneous alpha-band amplitude predicts subjective visibility but not discrimination accuracy during high-level perception. ",
               tags$em("Conscious. Cogn."), " 2022;102:103337."),
        tags$p(id = "ref12", tags$sup("12"), " ",
               "Iemi L, and Busch NA. Moment-to-moment fluctuations in neuronal excitability bias subjective perception rather than strategic decision-making. ",
               tags$em("eNeuro."), " 2018;5:ENEURO.0430–17.2018.")
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  # ---- Phenomenon intro ----
  output$phenomenon_intro <- renderUI({
    item <- phenomenon_intro_content[[input$preset]]
    if (is.null(item)) return(NULL)
    tags$div(
      style = "border: 2.5px solid #222; border-radius: 8px; overflow: hidden; margin-bottom: 20px; box-shadow: 0 3px 10px rgba(0,0,0,0.15);",
      tags$div(
        style = "background: #222; padding: 10px 20px;",
        tags$h4(item$title, style = "color: white; margin: 0; font-size: 20px; letter-spacing: 0.5px;")
      ),
      fluidRow(
        style = "margin: 0; padding: 16px 20px; background: white;",
        column(8,
               tags$p(item$body, item$refs, style = "font-size:15px; color:#333; line-height:1.9; margin:0;"),
               if (!is.null(item$body2)) tagList(
                 tags$hr(style = "margin: 10px 0;"),
                 tags$p(item$body2, style = "font-size:14px; color:#555; line-height:1.8; margin:0;")
               )
        ),
        column(4,
               tags$img(
                 src     = item$img,
                 style   = "width:100%; border-radius:4px; object-fit:cover;",
                 onerror = "this.style.display='none'"
               )
        )
      )
    )
  })
  
  # ---- Condition labels ----
  output$label_cond_A <- renderUI({
    lbl <- cond_labels[[input$preset]]$A
    h5(lbl, style = "color:#377EB8; font-weight:bold;")
  })
  output$label_cond_B <- renderUI({
    lbl <- cond_labels[[input$preset]]$B
    h5(lbl, style = "color:#E41A1C; font-weight:bold;")
  })
  
  # ---- Preset: update sliders, enable/disable ----
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
    lbl_A <- cond_labels[[input$preset]]$A
    lbl_B <- cond_labels[[input$preset]]$B
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
      label = c(lbl_A, lbl_B), col = c("#377EB8", "#E41A1C")
    )
    ggplot(df_all, aes(x = x_off, y = y_off, fill = val)) +
      geom_raster(interpolate = TRUE) +
      scale_fill_gradient2(low = "black", mid = "gray50", high = "white",
                           midpoint = 0, limits = c(-1, 1), guide = "none") +
      geom_text(data = labels_df, aes(x = x_off, y = y_off, label = label),
                inherit.aes = FALSE, size = 5, vjust = 0, color = "white") +
      geom_text(data = cond_labels_df,
                aes(x = x_off, y = y_off, label = label, color = col),
                inherit.aes = FALSE, size = 3.5, fontface = "bold") +
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
                      label = c("A", "B"), col = c("#377EB8", "#E41A1C"))
    g <- ggplot(nr, aes(x = Contrast, y = Max_firing)) +
      geom_line(linewidth = 0.7) +
      geom_point(data = pts, aes(x = x, y = y, color = col), size = 3) +
      scale_color_identity() +
      annotate("text", x = 50, y = 130, label = "Naka-Rushton function",
               vjust = 1, hjust = 0.5, size = 7) +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      coord_cartesian(ylim = c(0, 130)) +
      scale_y_continuous(breaks = seq(0, 120, by = 30)) +
      labs(x = "Contrast (%)", y = "Peak response") +
      theme_classic(base_size = 20)
    if (input$preset == "manual") {
      g <- g + geom_text(data = pts, aes(x = x, y = y, label = label, color = col),
                         vjust = -1, hjust = 0.5, size = 4)
    }
    g
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
    lbl_A <- cond_labels[[input$preset]]$A
    lbl_B <- cond_labels[[input$preset]]$B
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
      make_tc_df(max_fr_A, input$spont_A, lbl_A),
      make_tc_df(max_fr_B, input$spont_B, lbl_B)
    ) %>% mutate(Condition = factor(Condition, levels = c(lbl_A, lbl_B)))
    strip_colors <- setNames(c("#377EB8", "#E41A1C"), c(lbl_A, lbl_B))
    g <- ggplot(df_all, aes(x = Orientation, y = FiringRate, color = Color, group = Neuron)) +
      geom_line() +
      scale_color_identity() +
      facet_wrap(~ Condition, nrow = 2) +
      scale_x_continuous(breaks = c(0, 60, 120, 180)) +
      scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
      coord_cartesian(ylim = c(0, 125)) +
      labs(x = "Orientation (°)", y = "Spikes") +
      theme_classic(base_size = 20) +
      theme(strip.text = element_text(size = 11, face = "bold"))
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
    lbl_A <- cond_labels[[input$preset]]$A
    lbl_B <- cond_labels[[input$preset]]$B
    sim_result() %>%
      group_by(Stimulus, Condition, Trial) %>%
      summarise(Sum_spikes = sum(Spikes), .groups = "drop") %>%
      mutate(Condition = ifelse(Condition == "A", lbl_A, lbl_B)) %>%
      ggplot(aes(x = Sum_spikes, color = Condition, fill = Condition)) +
      geom_density(alpha = 0.2, linewidth = 1) +
      scale_color_manual(values = setNames(c("#377EB8", "#E41A1C"), c(lbl_A, lbl_B))) +
      scale_fill_manual( values = setNames(c("#377EB8", "#E41A1C"), c(lbl_A, lbl_B))) +
      labs(x = "Total spikes", y = "Density") +
      theme_classic(base_size = 20)
  })
  
  # ---- 3D scatter ----
  output$p_3d <- renderPlotly({
    req(sim_result())
    lbl_A <- cond_labels[[input$preset]]$A
    lbl_B <- cond_labels[[input$preset]]$B
    s1 <- min(input$stim1, input$stim2)
    s2 <- max(input$stim1, input$stim2)
    n1 <- max(1,   round(s1 - 10))
    n2 <- max(1,   min(180, round((s1 + s2) / 2)))
    n3 <- min(180, round(s2 + 10))
    nn1 <- as.character(n1); nn2 <- as.character(n2); nn3 <- as.character(n3)
    df_3d <- sim_result() %>%
      filter(Neuron %in% c(n1, n2, n3)) %>%
      pivot_wider(id_cols = c(Condition, Stimulus, Trial),
                  names_from = Neuron, values_from = Spikes)
    get_xyz <- function(df_sub) list(
      x = df_sub[[nn1]], y = df_sub[[nn2]], z = df_sub[[nn3]]
    )
    dA1 <- df_3d %>% filter(Condition == "A", Stimulus == input$stim1)
    dA2 <- df_3d %>% filter(Condition == "A", Stimulus == input$stim2)
    dB1 <- df_3d %>% filter(Condition == "B", Stimulus == input$stim1)
    dB2 <- df_3d %>% filter(Condition == "B", Stimulus == input$stim2)
    plot_ly() %>%
      add_trace(x = dA1[[nn1]], y = dA1[[nn2]], z = dA1[[nn3]],
                type = "scatter3d", mode = "markers",
                marker = list(size = 3.5, symbol = "cross", color = "#377EB8"),
                name = paste0(lbl_A, "; S1"), legendrank = 1) %>%
      add_trace(x = dA2[[nn1]], y = dA2[[nn2]], z = dA2[[nn3]],
                type = "scatter3d", mode = "markers",
                marker = list(size = 1.7, symbol = "circle", color = "#377EB8"),
                name = paste0(lbl_A, "; S2"), legendrank = 2) %>%
      add_trace(x = dB1[[nn1]], y = dB1[[nn2]], z = dB1[[nn3]],
                type = "scatter3d", mode = "markers",
                marker = list(size = 3.5, symbol = "cross", color = "#E41A1C"),
                name = paste0(lbl_B, "; S1"), legendrank = 3) %>%
      add_trace(x = dB2[[nn1]], y = dB2[[nn2]], z = dB2[[nn3]],
                type = "scatter3d", mode = "markers",
                marker = list(size = 1.7, symbol = "circle", color = "#E41A1C"),
                name = paste0(lbl_B, "; S2"), legendrank = 4) %>%
      layout(
        font   = list(size = 12),
        legend = list(font = list(size = 16)),
        scene  = list(
          xaxis = list(title = paste0("Neuron ", n1), range = c(0, 180)),
          yaxis = list(title = paste0("Neuron ", n2), range = c(0, 180)),
          zaxis = list(title = paste0("Neuron ", n3), range = c(0, 180)),
          aspectmode = "cube"
        )
      )
  })
  
  # ---- Decision boundary ----
  output$p_boundary <- renderPlotly({
    req(sim_result())
    lbl_A <- cond_labels[[input$preset]]$A
    lbl_B <- cond_labels[[input$preset]]$B
    s1 <- min(input$stim1, input$stim2)
    s2 <- max(input$stim1, input$stim2)
    n1 <- max(1,   round(s1 - 10))
    n2 <- max(1,   min(180, round((s1 + s2) / 2)))
    n3 <- min(180, round(s2 + 10))
    nn1 <- as.character(n1); nn2 <- as.character(n2); nn3 <- as.character(n3)
    withProgress(message = "Fitting decision boundary...", {
      make_boundary_plot <- function(cond_label, col, cond_key) {
        df_s <- sim_result() %>%
          filter(Condition == cond_key, Neuron %in% c(n1, n2, n3)) %>%
          pivot_wider(id_cols = c(Stimulus, Trial),
                      names_from = Neuron, values_from = Spikes) %>%
          mutate(stim_bin = as.numeric(as.factor(Stimulus)) - 1)
        fml <- as.formula(paste("stim_bin ~", paste0("`", c(nn1, nn2, nn3), "`", collapse = " + ")))
        fit <- glm(fml, data = df_s, family = binomial)
        x_seq <- seq(0, 180, length.out = 20)
        grid3d <- expand.grid(x_seq, x_seq, x_seq)
        names(grid3d) <- c(nn1, nn2, nn3)
        grid3d$prob <- predict(fit, newdata = grid3d, type = "response")
        dp <- grid3d %>% filter(abs(prob - 0.5) < 0.07)
        list(df = df_s, dp = dp, col = col, label = cond_label)
      }
      rA <- make_boundary_plot(lbl_A, "#377EB8", "A")
      rB <- make_boundary_plot(lbl_B, "#E41A1C", "B")
      p <- plot_ly()
      for (r in list(rA, rB)) {
        ds1 <- r$df %>% filter(Stimulus == input$stim1)
        ds2 <- r$df %>% filter(Stimulus == input$stim2)
        p <- p %>%
          add_trace(x = ds1[[nn1]], y = ds1[[nn2]], z = ds1[[nn3]],
                    type = "scatter3d", mode = "markers",
                    marker = list(size = 3, symbol = "cross", color = r$col),
                    name = paste0(r$label, " S1")) %>%
          add_trace(x = ds2[[nn1]], y = ds2[[nn2]], z = ds2[[nn3]],
                    type = "scatter3d", mode = "markers",
                    marker = list(size = 1, symbol = "circle", color = r$col),
                    name = paste0(r$label, " S2"))
        if (nrow(r$dp) > 0) {
          p <- p %>%
            add_trace(x = r$dp[[nn1]], y = r$dp[[nn2]], z = r$dp[[nn3]],
                      type = "mesh3d", opacity = 0.3,
                      colorscale = list(c(0, r$col), c(1, r$col)),
                      intensity = rep(0.5, nrow(r$dp)),
                      showscale = FALSE,
                      name = paste0("Boundary: ", r$label),
                      inherit = FALSE)
        }
      }
      p %>% layout(
        font   = list(size = 12),
        legend = list(font = list(size = 16)),
        scene  = list(
          xaxis = list(title = paste0("Neuron ", n1), range = c(0, 180)),
          yaxis = list(title = paste0("Neuron ", n2), range = c(0, 180)),
          zaxis = list(title = paste0("Neuron ", n3), range = c(0, 180)),
          aspectmode = "cube"
        )
      )
    })
  })
  
  # ---- Subtitle texts ----
  subtitle_texts <- list(
    manual     = list(density = NULL, boundary = NULL),
    samaha     = list(
      density  = list(title = "Explanation for Samaha effect",
                      body  = "Under lower α, increased baseline activity shifts the population response toward greater total spiking, causing higher visibility rating."),
      boundary = list(title = "Explanation for  Samaha effect",
                      body  = "Under lower α, increased baseline activity shifts the population response along the direction parallel to the discrimination hyperplane, leaving orientation discrimination sensitivity unchanged.")
    ),
    subjective = list(
      density  = list(title = "Explanation for Subjective inflation",
                      body  = "Under low attention, greater spike variability increases the chance of the population response exceeding the detection criterion"),
      boundary = list(title = "Explanation for Subjective inflation",
                      body  = "Under low attention, greater spike variability reduces manifold separability.")
    ),
    blindsight = list(
      density  = list(title = "Explanation for Blindsight",
                      body  = "Increased gain fluctuations expand the distribution along the total spike axis, impairing yes/no detection sensitivity."),
      boundary = list(title = "Explanation for Blindsight",
                      body  = "Increased gain fluctuations produce response correlations parallel to the orientation discrimination hyperplane, leaving discrimination sensitivity unaffected.")
    )
  )
  
  make_subtitle_card <- function(item) {
    if (is.null(item)) return(NULL)
    tags$div(
      style = "border-left: 4px solid #17A589; background: #f5fffe; border-radius: 0 6px 6px 0; box-shadow: 0 1px 4px rgba(0,0,0,0.08); padding: 12px 16px; margin-top: 12px;",
      tags$div(style = "font-size:17px; font-weight:bold; color:#17A589; margin-bottom:6px;",
               tags$span("💡 "), item$title),
      tags$span(item$body, style = "font-size:15px; color:#444; line-height:1.7;")
    )
  }
  
  output$text_density  <- renderUI({ make_subtitle_card(subtitle_texts[[input$preset]]$density)  })
  output$text_boundary <- renderUI({ make_subtitle_card(subtitle_texts[[input$preset]]$boundary) })
}

# ============================================================
shinyApp(ui, server)