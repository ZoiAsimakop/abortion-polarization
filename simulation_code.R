# ======================================================================
# ABORTION-OPINION SIMULATION WITH INTERACTIVE OPINION MOTION
# ======================================================================
# SETUP IN RSTUDIO
# 1. Save this entire script as app.R in an RStudio project.
# 2. Install once in the Console:
#    install.packages(c("shiny", "plotly"))
# 3. Open app.R, click Run App, then Run simulation inside the app.
#
# MODEL: one simplified legal-abortion-access opinion per person.
# -1 = restrictive/pro-life policy endpoint
# +1 = permissive/pro-choice policy endpoint
# These labels do not capture all moral views or policy exceptions.
# This is a conceptual model, not a calibrated prediction. No regression.
#
# NEW VISUALIZATION: Opinion motion
# Horizontal position = opinion; vertical position = fixed display lane.
# People move horizontally, not through a modeled physical/social space.
# Trails are opinion histories, NOT social-network edges.
# Play the time slider to animate a saved run; this does not change the model.
#
# R BASICS: <- assigns; $ selects a table column or list element.
# A data.frame is a table, here one row per agent.
# Vector operations calculate for many agents at once.
# TRUE/FALSE act as 1/0 in multiplication.
# ======================================================================
install.packages("shiny")
install.packages("plotly")
library(shiny)
library(plotly) # Adds interactive hover, zoom, and click behavior to plots.

# ----------------------------------------------------------------------
# 1. HELPERS
# ----------------------------------------------------------------------
# 'Clip' means trim values to allowed limits. It is not an acronym.
clip <- function(x, lower = 0, upper = 1) {
  pmin(upper, pmax(lower, x))
}

# Color is based on CURRENT opinion, not fixed orientation or agent role.
# Restrictive opinions are orange, neutral opinions light, permissive purple.
# A palette of 201 colors maps the -1 to +1 scale into integer indices.
opinion_colors <- function(opinion) {
  palette <- colorRampPalette(c("#C65D24", "#E1E5EB", "#6550C8"))(201)
  index <- floor((clip(opinion, -1, 1) + 1) * 100) + 1L
  palette[index]
}

# ----------------------------------------------------------------------
# 2. CREATE AGENTS
# ----------------------------------------------------------------------
initialize_population <- function(p) {
  set.seed(p$seed) # Reusing settings and seed reproduces a run.
  n <- p$n
  
  # A fixed orientation trait, not a real demographic classification.
  # The two orientations are equally likely, with overlapping initial views.
  restrictive <- rbinom(n, size = 1, prob = 0.5)
  opinion <- clip(rnorm(n, mean = ifelse(restrictive == 1, -0.35, 0.35),
                        sd = 0.3), -1, 1)
  
  # Conviction controls update SIZE; bound controls message ACCEPTANCE.
  # Strong conviction and openness can coexist in an individual.
  conviction <- clip(rnorm(n, p$conviction, 0.1), 0, 0.95)
  open_minded <- runif(n) < p$open_share
  bound <- ifelse(open_minded, pmin(2, 2 * p$bound), p$bound)
  
  zealot <- rep(FALSE, n)
  number_zealots <- round(n * p$zealot_share)
  if (number_zealots > 0) {
    ids <- sample.int(n, number_zealots)
    zealot[ids] <- TRUE
    # Fixed endpoints balanced as closely as possible.
    endpoints <- rep(c(-1, 1), length.out = number_zealots)
    endpoints <- endpoints[sample.int(number_zealots)]
    opinion[ids] <- endpoints
    restrictive[ids] <- as.integer(endpoints == -1)
  }
  
  # One campaign at a time. Activists initially support its message.
  # Their role persists, but non-zealot activists can change personal views.
  eligible <- which(opinion * p$campaign_side > 0)
  number_activists <- min(round(n * p$activist_share), length(eligible))
  activist <- rep(FALSE, n)
  if (number_activists > 0) {
    ids <- eligible[sample.int(length(eligible), number_activists)]
    activist[ids] <- TRUE
  }
  
  # Persistent differences in baseline attention preserve heterogeneity.
  baseline_attention <- runif(n, 0.1, 0.3)
  data.frame(
    id = seq_len(n), restrictive = restrictive,
    initial_opinion = opinion, opinion = opinion,
    conviction = conviction, bound = bound,
    open_minded = open_minded, zealot = zealot, activist = activist,
    baseline_attention = baseline_attention, attention = baseline_attention,
    involvement = runif(n, 0.1, 0.3),
    campaign_exposure = 0L, information_exposure = 0L
  )
}

# ----------------------------------------------------------------------
# 3. UPDATE ONE TIME STEP
# ----------------------------------------------------------------------
simulation_step <- function(a, p) {
  old <- a
  n <- nrow(old)
  # All changes use the OLD state: synchronous updating.
  
  # Visibility depends on actual activist prevalence, not requested prevalence.
  # The factor 4 is an arbitrary prototype scale, not empirical calibration.
  visibility <- clip(mean(old$activist) * p$intensity * 4)
  # Extra restrictive-group attention is optional and an explicit assumption.
  target_attention <- clip(old$baseline_attention + visibility *
                             (1 + p$extra_attention * old$restrictive))
  a$attention <- clip(old$attention + 0.2 *
                        (target_attention - old$attention))
  # Involvement follows old attention more slowly than attention changes.
  a$involvement <- clip(old$involvement + 0.05 *
                          (old$attention - old$involvement))
  
  # Stronger conviction reduces movement. Involvement increases engagement.
  # Conviction and confidence bounds are fixed in this initial model.
  responsiveness <- 0.15 * (1 - old$conviction) *
    (0.5 + 0.5 * old$involvement)
  
  # A. Ordinary social contact: one distinct random partner per receiver.
  # Modulo (%%) wraps indices; nonzero offsets exclude self-contact.
  partner <- ((seq_len(n) - 1L +
                 sample.int(n - 1L, n, replace = TRUE)) %% n) + 1L
  difference <- old$opinion[partner] - old$opinion
  noticed <- runif(n) < old$attention
  accepted <- abs(difference) <= old$bound
  social_change <- responsiveness * difference * noticed * accepted
  # Rejected ordinary contact has no effect; no repulsion is implied.
  
  # B. Fixed campaign message, separate from activists' personal opinions.
  saw_campaign <- runif(n) < old$attention * visibility
  campaign_difference <- p$campaign_side - old$opinion
  accepted_campaign <- abs(campaign_difference) <= old$bound
  campaign_change <- responsiveness * campaign_difference *
    saw_campaign * accepted_campaign
  
  # Optional backlash means movement AWAY from a rejected campaign message.
  # Either campaign side can provoke it; this is a modeling choice.
  backlash_change <- rep(0, n)
  if (p$backlash) {
    backlash_change <- -responsiveness * p$backlash_strength *
      campaign_difference * saw_campaign * (!accepted_campaign)
  }
  
  # C. External message position is a policy signal, NOT a truth score.
  saw_information <- runif(n) < old$attention * p$information_rate
  information_difference <- p$information_position - old$opinion
  accepted_information <- abs(information_difference) <= old$bound
  information_change <- responsiveness * p$information_uptake *
    information_difference * saw_information * accepted_information
  
  # Sum simultaneous influences, freeze zealots, enforce opinion limits.
  total_change <- social_change + campaign_change +
    backlash_change + information_change
  total_change[old$zealot] <- 0
  a$opinion <- clip(old$opinion + total_change, -1, 1)
  # Counters include noticed messages even if subsequently rejected.
  a$campaign_exposure <- old$campaign_exposure + as.integer(saw_campaign)
  a$information_exposure <- old$information_exposure + as.integer(saw_information)
  a
}

# ----------------------------------------------------------------------
# 4. POPULATION MEASURES AND COMPLETE RUN
# ----------------------------------------------------------------------
population_measures <- function(a, time) {
  group <- a$restrictive == 1
  gap <- if (any(group) && any(!group)) {
    abs(mean(a$opinion[group]) - mean(a$opinion[!group]))
  } else NA_real_ # Safe missing value when one group is empty.
  data.frame(time = time,
             mean_opinion = mean(a$opinion), # Overall direction of policy support.
             dispersion = sd(a$opinion),    # Spread, not proof of two opposing camps.
             extreme_share = mean(abs(a$opinion) >= 0.8), # Explicit chosen threshold.
             group_gap = gap, attention = mean(a$attention),
             involvement = mean(a$involvement))
}

run_simulation <- function(p) {
  a <- initialize_population(p)
  # Store every snapshot so animation simply replays the saved model history.
  history <- vector("list", p$steps + 1L)
  measures <- vector("list", p$steps + 1L)
  history[[1]] <- a
  measures[[1]] <- population_measures(a, 0)
  for (t in seq_len(p$steps)) {
    a <- simulation_step(a, p)
    history[[t + 1L]] <- a
    measures[[t + 1L]] <- population_measures(a, t)
  }
  # Basic safeguards; these are implementation checks, not model validation.
  stopifnot(all(is.finite(a$opinion)), all(abs(a$opinion) <= 1),
            all(a$opinion[a$zealot] == a$initial_opinion[a$zealot]),
            all(a$attention >= 0 & a$attention <= 1),
            all(a$involvement >= 0 & a$involvement <= 1))
  list(history = history, measures = do.call(rbind, measures), settings = p)
}

# ----------------------------------------------------------------------
# 5. BUILD THE INTERACTIVE OPINION DISPLAY
# ----------------------------------------------------------------------
# This is a DISPLAY function only; it never updates agents or draws random
# numbers. Changing visualization options cannot change simulation results.
build_motion_plot <- function(r, time, show_trails, trail_steps, point_size) {
  a <- r$history[[time + 1L]]
  n <- nrow(a)
  
  # Fixed, evenly dispersed display lanes, determined only by agent ID.
  # The irrational multiplier scatters IDs without consuming random numbers.
  # Vertical distance has NO substantive meaning; horizontal distance does.
  lane <- 0.04 + 0.92 * ((a$id * 0.61803398875) %% 1)
  first_time <- max(0L, time - as.integer(trail_steps))
  past <- r$history[[first_time + 1L]]
  
  # Symbol priority preserves overlapping roles rather than hiding them.
  role <- ifelse(a$zealot & a$activist, "Activist + zealot",
                 ifelse(a$zealot, "Zealot", ifelse(a$activist, "Activist", "Other agent")))
  symbols <- c("Other agent" = "circle", "Activist" = "diamond",
               "Zealot" = "square", "Activist + zealot" = "star")
  
  # Start an interactive figure with a source ID for click events.
  fig <- plot_ly(source = "opinion_motion")
  
  if (show_trails && time > 0) {
    # Stack each agent's recent x positions with a missing-value separator.
    # Separators stop Plotly connecting DIFFERENT people's trails.
    times <- seq.int(first_time, time)
    opinions <- vapply(times, function(t) r$history[[t + 1L]]$opinion,
                       numeric(n))
    trail_x <- as.vector(rbind(t(opinions), rep(NA_real_, n)))
    trail_y <- as.vector(rbind(matrix(lane, nrow = length(times), ncol = n,
                                      byrow = TRUE), rep(NA_real_, n)))
    fig <- add_trace(fig, x = trail_x, y = trail_y,
                     type = "scatter", mode = "lines", connectgaps = FALSE,
                     line = list(color = "rgba(90,110,140,0.22)", width = 2),
                     hoverinfo = "skip", showlegend = FALSE, name = "Recent opinion paths")
  }
  
  # One trace per role makes a useful legend. Hover explains each individual.
  # Colors track opinion, marker shape tracks role, y is just a display lane.
  for (label in names(symbols)) {
    ids <- which(role == label)
    if (!length(ids)) next
    change <- a$opinion[ids] - past$opinion[ids]
    hover <- paste0(
      "Agent ", a$id[ids], "<br>Role: ", label,
      "<br>Opinion: ", sprintf("%.3f", a$opinion[ids]),
      "<br>Change over last ", time - first_time, " steps: ", sprintf("%+.3f", change),
      "<br>Conviction: ", sprintf("%.2f", a$conviction[ids]),
      "<br>Confidence bound: ", sprintf("%.2f", a$bound[ids]),
      "<br>Open-minded: ", ifelse(a$open_minded[ids], "yes", "no"),
      "<br>Attention: ", sprintf("%.2f", a$attention[ids]),
      "<br>Involvement: ", sprintf("%.2f", a$involvement[ids]))
    fig <- add_trace(fig, x = a$opinion[ids], y = lane[ids],
                     type = "scatter", mode = "markers", name = label,
                     customdata = a$id[ids], text = hover, hoverinfo = "text",
                     marker = list(symbol = unname(symbols[label]),
                                   size = point_size + ifelse(label == "Other agent", 0, 2),
                                   color = opinion_colors(a$opinion[ids]), opacity = 0.85,
                                   line = list(color = "#344054", width = 0.5)))
  }
  
  # Fixed axes prevent apparent motion caused by automatic axis rescaling.
  # uirevision keeps the user's zoom stable across animation frames.
  fig <- layout(fig,
                title = list(text = paste("Opinion motion — step", time), x = 0.02),
                paper_bgcolor = "#FFFFFF", plot_bgcolor = "#F7F9FC",
                xaxis = list(title = "Opinion: restrictive / pro-life ← → permissive / pro-choice",
                             range = c(-1.08, 1.08), tickvals = c(-1, -0.5, 0, 0.5, 1),
                             zeroline = TRUE, zerolinecolor = "#98A2B3"),
                yaxis = list(title = "", range = c(0, 1), showticklabels = FALSE,
                             showgrid = FALSE, zeroline = FALSE),
                legend = list(orientation = "h", x = 0, y = -0.2),
                margin = list(l = 25, r = 25, b = 110, t = 65),
                uirevision = paste(r$settings$seed, r$settings$n, sep = "_"))
  # Plotly's toolbar supports zoom, pan, reset, and saving a plot image.
  config(fig, displaylogo = FALSE, scrollZoom = TRUE)
}

# ----------------------------------------------------------------------
# 6. INTERFACE
# ----------------------------------------------------------------------
# IDs connect controls to input$... and output placeholders to output$....
ui <- fluidPage(
  titlePanel("Abortion opinions: a polarization simulation"),
  p("Simplified legal-access scale: −1 = restrictive/pro-life; +1 = permissive/pro-choice. An illustrative model, not a prediction."),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of agents", 200, min = 50, max = 1000, step = 50),
      numericInput("steps", "Time steps (not real days)", 100, min = 10, max = 300, step = 10),
      numericInput("seed", "Random seed", 42, min = 1, max = 1000000),
      sliderInput("conviction", "Typical conviction", 0, 0.9, 0.5, step = 0.05),
      sliderInput("bound", "Ordinary confidence bound", 0.05, 2, 0.6, step = 0.05),
      sliderInput("open_share", "Open-minded share (double bound, capped at 2)", 0, 1, 0.25, step = 0.05),
      sliderInput("zealot_share", "Zealot share (balanced endpoints)", 0, 0.4, 0.1, step = 0.05),
      selectInput("campaign_side", "Campaign position", c("Pro-choice / permissive" = "1", "Pro-life / restrictive" = "-1")),
      sliderInput("activist_share", "Requested activist share", 0, 0.4, 0.1, step = 0.05),
      sliderInput("intensity", "Campaign intensity", 0, 1, 0.5, step = 0.05),
      sliderInput("extra_attention", "Extra restrictive-group attention (assumption)", 0, 2, 0, step = 0.1),
      checkboxInput("backlash", "Enable backlash to rejected campaign messages", FALSE),
      sliderInput("backlash_strength", "Backlash strength", 0, 1, 0.5, step = 0.05),
      sliderInput("information_rate", "External information exposure rate", 0, 1, 0.2, step = 0.05),
      sliderInput("information_position", "External message position (not accuracy)", -1, 1, 0, step = 0.1),
      sliderInput("information_uptake", "External information uptake", 0, 1, 0.5, step = 0.05),
      actionButton("run", "Run simulation", class = "btn-primary"),
      p("Settings take effect when you click Run simulation.")
    ),
    mainPanel(
      verbatimTextOutput("run_info"),
      # Shiny adds a play/pause button when animate is supplied.
      # A 350 ms interval gives the plot time to redraw. It is playback speed,
      # not a model parameter or a claim about real-world time.
      sliderInput("time", "Displayed time point — use play to animate",
                  min = 0, max = 100, value = 0, step = 1,
                  animate = animationOptions(interval = 350, loop = FALSE)),
      tabsetPanel(
        tabPanel("Opinion motion",
                 p("Move the time slider or press play. Vertical bands of agents indicate similar opinions; two separated bands can show opposing camps."),
                 fluidRow(
                   column(4, checkboxInput("trails", "Show recent opinion trails", TRUE)),
                   column(4, sliderInput("trail_steps", "Trail length (steps)", 1, 30, 10, step = 1)),
                   column(4, sliderInput("point_size", "Agent size", 4, 14, 8, step = 1))),
                 plotlyOutput("motion", height = "550px"),
                 p("Orange = restrictive, purple = permissive. Shape shows role. Hover for traits; click an agent for details. Zoom with the mouse or toolbar; double-click to reset."),
                 p("Vertical position is only a fixed display lane. Trails show each person's opinion history, NOT social ties. This is not a network layout."),
                 verbatimTextOutput("selected_agent")),
        tabPanel("Opinions", verbatimTextOutput("summary"), plotOutput("distribution")),
        tabPanel("Dynamics", plotOutput("dynamics", height = "650px")),
        tabPanel("Agents", tableOutput("agents"),
                 downloadButton("download", "Download selected agent snapshot")),
        tabPanel("Model rules", tags$ul(
          tags$li("Random social partners; accepted contacts produce convergence."),
          tags$li("Conviction reduces movement; confidence bounds determine acceptance."),
          tags$li("Open-minded agents have wider bounds; zealots' opinions stay fixed."),
          tags$li("A single campaign supports either endpoint. Its activists generate visibility."),
          tags$li("Attention responds to visibility; involvement follows attention slowly."),
          tags$li("Extra restrictive-group attention and backlash are optional assumptions, not established facts."),
          tags$li("Rejection ordinarily has no effect. Optional campaign backlash produces repulsion."),
          tags$li("External information is a policy signal, not a factual-accuracy score."),
          tags$li("Initial differences across fixed orientation groups are programmed, not calibrated."),
          tags$li("Dispersion and extremity alone do not establish opposing camps."),
          tags$li("Opinion motion displays the existing model history; display settings do not affect the model."),
          tags$li("No fixed social network, regression, or hysteresis test is included.")))
      )
    )
  )
)

# ----------------------------------------------------------------------
# 7. SERVER: CONNECT MODEL AND DISPLAY
# ----------------------------------------------------------------------
server <- function(input, output, session) {
  result <- reactiveVal(NULL)
  selected_id <- reactiveVal(NULL) # Remembers an agent clicked in the plot.
  
  observeEvent(input$run, {
    # Numeric input bounds are suggestions: verify manually typed values.
    values <- c(input$n, input$steps, input$seed)
    valid <- length(values) == 3 && all(is.finite(values)) &&
      input$n >= 50 && input$n <= 1000 && input$n == floor(input$n) &&
      input$steps >= 10 && input$steps <= 300 && input$steps == floor(input$steps) &&
      input$seed >= 1 && input$seed <= 1000000 && input$seed == floor(input$seed)
    if (!valid) {
      showNotification("Use whole numbers: agents 50–1000, steps 10–300, seed 1–1000000.", type = "error")
      return(invisible(NULL))
    }
    # Capture run settings once. Selection inputs return text; convert side.
    p <- list(n = as.integer(input$n), steps = as.integer(input$steps),
              seed = as.integer(input$seed), conviction = input$conviction,
              bound = input$bound, open_share = input$open_share,
              zealot_share = input$zealot_share, campaign_side = as.numeric(input$campaign_side),
              activist_share = input$activist_share, intensity = input$intensity,
              extra_attention = input$extra_attention, backlash = input$backlash,
              backlash_strength = input$backlash_strength, information_rate = input$information_rate,
              information_position = input$information_position,
              information_uptake = input$information_uptake)
    withProgress(message = "Running simulation", value = 0, {
      result(run_simulation(p))
      incProgress(1)
    })
    selected_id(NULL)
    # Start at time zero so the user can watch the full development.
    updateSliderInput(session, "time", max = p$steps, value = 0)
  }, ignoreInit = TRUE)
  
  selected_time <- reactive({
    req(result(), !is.null(input$time))
    min(as.integer(input$time), result()$settings$steps)
  })
  snapshot <- reactive({
    req(result())
    result()$history[[selected_time() + 1L]] # R starts indexing at 1.
  })
  
  output$run_info <- renderText({
    if (is.null(result())) return("Click Run simulation, then press play above the Opinion motion tab.")
    r <- result()
    sprintf("Stored run: seed %d | %d agents | %d steps | actual activists %.1f%% | campaign %s | backlash %s",
            r$settings$seed, r$settings$n, r$settings$steps,
            100 * mean(r$history[[1]]$activist),
            ifelse(r$settings$campaign_side == 1, "pro-choice", "pro-life"),
            ifelse(r$settings$backlash, "on", "off"))
  })
  
  # Interactive plot recalculates with time or display-option changes.
  # It reads saved data; it does not rerun or modify the simulation.
  output$motion <- renderPlotly({
    req(result())
    build_motion_plot(result(), selected_time(), input$trails,
                      input$trail_steps, input$point_size)
  })
  
  # Plotly click events carry the agent ID via customdata.
  # A trail click has no agent customdata and is ignored.
  observeEvent(event_data("plotly_click", source = "opinion_motion"), {
    event <- event_data("plotly_click", source = "opinion_motion")
    if (!is.null(event$customdata)) {
      id <- suppressWarnings(as.integer(event$customdata[1]))
      if (length(id) == 1 && !is.na(id)) selected_id(id)
    }
  })
  
  # Details follow the selected person as the time slider advances.
  output$selected_agent <- renderPrint({
    req(result())
    if (is.null(selected_id())) {
      cat("Click an agent to follow their details through time.")
      return(invisible(NULL))
    }
    a <- snapshot()
    person <- a[a$id == selected_id(), , drop = FALSE]
    if (!nrow(person)) return(invisible(NULL))
    cat("Agent:", person$id, " | Time:", selected_time(),
        "\nInitial opinion:", round(person$initial_opinion, 3),
        " | Current opinion:", round(person$opinion, 3),
        "\nConviction:", round(person$conviction, 2), " | Bound:", round(person$bound, 2),
        "\nAttention:", round(person$attention, 2), " | Involvement:", round(person$involvement, 2),
        "\nOpen-minded:", person$open_minded, " | Zealot:", person$zealot,
        " | Activist:", person$activist,
        "\nCampaign messages noticed:", person$campaign_exposure,
        " | Information messages noticed:", person$information_exposure, "\n")
  })
  
  output$summary <- renderPrint({
    m <- population_measures(snapshot(), selected_time())
    cat("Time:", m$time, "\nMean opinion:", round(m$mean_opinion, 3),
        "\nOpinion dispersion (SD):", round(m$dispersion, 3),
        "\nExtreme share (absolute opinion >= 0.8):", round(100 * m$extreme_share, 1), "%",
        "\nFixed-orientation group gap:", round(m$group_gap, 3),
        "\nMean attention:", round(m$attention, 3),
        "\nMean involvement:", round(m$involvement, 3), "\n")
  })
  output$distribution <- renderPlot({
    hist(snapshot()$opinion, breaks = seq(-1, 1, length.out = 21),
         xlim = c(-1, 1), col = "steelblue", border = "white",
         main = paste("Opinion distribution: time", selected_time()),
         xlab = "Restrictive / pro-life (-1) to permissive / pro-choice (+1)")
    abline(v = 0, lty = 2)
  })
  
  output$dynamics <- renderPlot({
    req(result())
    m <- result()$measures
    previous <- par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
    on.exit(par(previous))
    plot(m$time, m$mean_opinion, type = "l", ylim = c(-1, 1),
         xlab = "Time step", ylab = "Mean opinion", main = "Overall policy opinion")
    abline(h = 0, lty = 2, col = "gray")
    abline(v = selected_time(), lty = 3, col = "#6550C8")
    plot(m$time, m$dispersion, type = "l", ylim = c(0, 1.1),
         xlab = "Time step", ylab = "Opinion SD", main = "Opinion dispersion")
    abline(v = selected_time(), lty = 3, col = "#6550C8")
    matplot(m$time, m[c("attention", "involvement")], type = "l", lty = 1,
            col = c("steelblue", "darkorange"), ylim = c(0, 1),
            xlab = "Time step", ylab = "Population mean")
    abline(v = selected_time(), lty = 3, col = "#6550C8")
    legend("topright", c("Attention", "Involvement"), lty = 1,
           col = c("steelblue", "darkorange"), bty = "n")
  })
  output$agents <- renderTable({ head(snapshot(), 12) }, digits = 3)
  
  # Export every agent at the selected time, with all stored run settings.
  output$download <- downloadHandler(
    filename = function() paste0("agents_time_", selected_time(), ".csv"),
    content = function(file) {
      a <- snapshot()
      a$time <- selected_time()
      for (name in names(result()$settings)) {
        a[[paste0("setting_", name)]] <- result()$settings[[name]]
      }
      write.csv(a, file, row.names = FALSE)
    })
}

# Launch the interface and server together.
shinyApp(ui, server)
