# ======================================================================
# ABORTION-OPINION SIMULATION: INITIAL SHINY APP
# ======================================================================
# SETUP IN RSTUDIO
# 1. Create an RStudio project and save this script as app.R.
# 2. In the Console, install Shiny once: install.packages("shiny")
# 3. Open app.R, click Run App, then click Run simulation in the app.
#
# WHAT THIS MODEL DOES
# Simulates opinions about legal abortion access on a simplified scale:
# -1 = highly restrictive / pro-life policy endpoint
# +1 = highly permissive / pro-choice policy endpoint
# These labels do not capture all abortion attitudes or policy exceptions.
# The model is conceptual, not calibrated to real people or real time.
# No regression analysis or formal experimental-design tools are included.
#
# FILE ORGANIZATION
# 1. Helper functions
# 2. Create the initial population
# 3. Update the population by one step
# 4. Run the simulation and summarize results
# 5. Define the Shiny interface
# 6. Connect the interface to the simulation
#
# A FEW R CONVENTIONS
# <- assigns a value. $ selects a named list element or table column.
# A data.frame is a table: here, one row per simulated person (agent).
# Vector calculations operate on every person at once.
# TRUE and FALSE become 1 and 0 when multiplied.
# ======================================================================

library(shiny) # The only extra package needed for this prototype.

# ----------------------------------------------------------------------
# 1. HELPERS
# ----------------------------------------------------------------------
# Keep a vector of values between a lower and upper limit.
# Attention and involvement use 0–1; opinions use -1–1.
clip <- function(x, lower = 0, upper = 1) {
  pmin(upper, pmax(lower, x))
}

# ----------------------------------------------------------------------
# 2. INITIAL POPULATION
# ----------------------------------------------------------------------
# p contains all settings collected from the interface.
initialize_population <- function(p) {
  set.seed(p$seed) # Identical settings and seed reproduce a run.
  n <- p$n
  
  # A fixed modeling trait: 1 = restrictive orientation, 0 = permissive.
  # This is not a classification of all real-world conservatives.
  # Each agent has a 50% chance of belonging to either orientation group.
  restrictive <- rbinom(n, size = 1, prob = 0.5)
  
  # Give the two groups somewhat different starting opinions, with overlap.
  # These initial differences are assumptions, not empirical estimates.
  # rnorm draws values around a mean, with standard deviation 0.3.
  opinion <- clip(rnorm(n, mean = ifelse(restrictive == 1, -0.35, 0.35),
                        sd = 0.3), -1, 1)
  
  # Confidence/conviction and bounded confidence are DIFFERENT:
  # conviction reduces how far a person moves after accepting influence;
  # the confidence bound determines which opinion distances are accepted.
  conviction <- clip(rnorm(n, p$conviction, 0.1), 0, 0.95)
  open_minded <- runif(n) < p$open_share
  bound <- ifelse(open_minded, pmin(2, 2 * p$bound), p$bound)
  # Maximum distance is 2: the distance from -1 to +1.
  
  # Randomly select zealots, whose opinions will never change.
  zealot <- rep(FALSE, n)
  number_zealots <- round(n * p$zealot_share)
  if (number_zealots > 0) {
    ids <- sample.int(n, number_zealots)
    zealot[ids] <- TRUE
    # Balance the fixed endpoints as closely as possible.
    # Sampling indices avoids R's special sample() behavior for one number.
    endpoints <- rep(c(-1, 1), length.out = number_zealots)
    endpoints <- endpoints[sample.int(number_zealots)]
    opinion[ids] <- endpoints
    restrictive[ids] <- as.integer(endpoints == -1)
  }
  
  # One campaign is active at a time; it may advocate either endpoint.
  # Select activists among its initial supporters. Roles can overlap:
  # an activist can also be open-minded or a zealot.
  eligible <- which(opinion * p$campaign_side > 0)
  number_activists <- min(round(n * p$activist_share), length(eligible))
  activist <- rep(FALSE, n)
  if (number_activists > 0) {
    ids <- eligible[sample.int(length(eligible), number_activists)]
    activist[ids] <- TRUE
  }
  
  # Personal baseline attention varies and stays fixed. This preserves some
  # heterogeneity rather than forcing everyone's attention toward one value.
  baseline_attention <- runif(n, 0.1, 0.3)
  
  data.frame(
    id = seq_len(n),
    restrictive = restrictive,
    initial_opinion = opinion,
    opinion = opinion,
    conviction = conviction,
    bound = bound,
    open_minded = open_minded,
    zealot = zealot,
    activist = activist,
    baseline_attention = baseline_attention,
    attention = baseline_attention,
    involvement = runif(n, 0.1, 0.3),
    campaign_exposure = 0L,
    information_exposure = 0L
  )
}

# ----------------------------------------------------------------------
# 3. ONE TIME STEP
# ----------------------------------------------------------------------
simulation_step <- function(a, p) {
  old <- a
  n <- nrow(old)
  # All calculations use the OLD state: synchronous updating.
  # Thus the result does not depend on which agent row we process first.
  
  # Activists generate visibility. Actual activist share may be smaller
  # than requested when there are too few eligible initial supporters.
  # The factor 4 is an arbitrary prototype scaling constant.
  visibility <- clip(mean(old$activist) * p$intensity * 4)
  
  # Extra attention among restrictive-oriented agents is an OPTIONAL rule.
  # It does not assert that real pro-life people respond in this way.
  target_attention <- clip(old$baseline_attention + visibility *
                             (1 + p$extra_attention * old$restrictive))
  # Close 20% of the gap to target attention on each step.
  # Attention can decrease if visibility decreases in a future extension.
  a$attention <- clip(old$attention + 0.2 *
                        (target_attention - old$attention))
  # Involvement follows attention more slowly, at 5% of the gap per step.
  a$involvement <- clip(old$involvement + 0.05 *
                          (old$attention - old$involvement))
  
  # Higher conviction reduces movement; involvement increases responsiveness.
  # Conviction itself stays fixed in this initial version.
  # These constants are explicit assumptions, not fitted parameters.
  responsiveness <- 0.15 * (1 - old$conviction) *
    (0.5 + 0.5 * old$involvement)
  
  # A. ORDINARY SOCIAL INFLUENCE
  # Each agent gets one random, distinct partner per step.
  # Offsets 1 through n-1 and modulo (%%) exclude self-contact.
  partner <- ((seq_len(n) - 1L +
                 sample.int(n - 1L, n, replace = TRUE)) %% n) + 1L
  difference <- old$opinion[partner] - old$opinion
  noticed <- runif(n) < old$attention
  accepted <- abs(difference) <= old$bound
  social_change <- responsiveness * difference * noticed * accepted
  # A rejected ordinary contact causes NO change: rejection is not backlash.
  
  # B. ACTIVIST CAMPAIGN
  # The campaign message is fixed at the chosen endpoint. Non-zealot
  # activists may change personal opinions, but their campaign role stays fixed.
  saw_campaign <- runif(n) < old$attention * visibility
  campaign_difference <- p$campaign_side - old$opinion
  accepted_campaign <- abs(campaign_difference) <= old$bound
  campaign_change <- responsiveness * campaign_difference *
    saw_campaign * accepted_campaign
  
  # Optional reactance: rejected campaign messages move recipients AWAY
  # from the message. Both campaign sides can provoke this response.
  # Without this explicit extension, bounded-confidence rejection is inert.
  backlash_change <- rep(0, n)
  if (p$backlash) {
    backlash_change <- -responsiveness * p$backlash_strength *
      campaign_difference * saw_campaign * (!accepted_campaign)
  }
  
  # C. EXTERNAL INFORMATION
  # Message position is an opinion/policy signal, NOT a truth score.
  # This channel is separate from activist campaigning.
  saw_information <- runif(n) < old$attention * p$information_rate
  information_difference <- p$information_position - old$opinion
  accepted_information <- abs(information_difference) <= old$bound
  information_change <- responsiveness * p$information_uptake *
    information_difference * saw_information * accepted_information
  
  # Combine influences, keep zealots fixed, enforce the opinion limits.
  total_change <- social_change + campaign_change +
    backlash_change + information_change
  total_change[old$zealot] <- 0
  a$opinion <- clip(old$opinion + total_change, -1, 1)
  
  # Count messages noticed, including messages subsequently rejected.
  a$campaign_exposure <- old$campaign_exposure + as.integer(saw_campaign)
  a$information_exposure <- old$information_exposure + as.integer(saw_information)
  a
}

# ----------------------------------------------------------------------
# 4. SUMMARY MEASURES AND COMPLETE RUN
# ----------------------------------------------------------------------
population_measures <- function(a, time) {
  group <- a$restrictive == 1
  # Handle the unlikely case of an empty orientation group safely.
  gap <- if (any(group) && any(!group)) {
    abs(mean(a$opinion[group]) - mean(a$opinion[!group]))
  } else NA_real_
  
  data.frame(
    time = time,
    mean_opinion = mean(a$opinion), # Direction of overall policy support.
    dispersion = sd(a$opinion),    # Spread, not proof of two opposing camps.
    extreme_share = mean(abs(a$opinion) >= 0.8), # Chosen threshold.
    group_gap = gap,
    attention = mean(a$attention),
    involvement = mean(a$involvement)
  )
  # A unanimous population at +1 is extreme but not split into opposing camps.
  # Always read these measures alongside the full opinion distribution.
}

run_simulation <- function(p) {
  a <- initialize_population(p)
  # Preallocate storage for time zero and all subsequent snapshots.
  history <- vector("list", p$steps + 1L)
  measures <- vector("list", p$steps + 1L)
  history[[1]] <- a
  measures[[1]] <- population_measures(a, 0)
  for (t in seq_len(p$steps)) {
    a <- simulation_step(a, p)
    history[[t + 1L]] <- a
    measures[[t + 1L]] <- population_measures(a, t)
  }
  # Basic implementation checks: fail clearly if a core rule was broken.
  stopifnot(all(is.finite(a$opinion)), all(abs(a$opinion) <= 1),
            all(a$opinion[a$zealot] == a$initial_opinion[a$zealot]),
            all(a$attention >= 0 & a$attention <= 1),
            all(a$involvement >= 0 & a$involvement <= 1))
  # Return snapshots, the joined summary table, and actual run settings.
  list(history = history, measures = do.call(rbind, measures), settings = p)
}

# ----------------------------------------------------------------------
# 5. SHINY INTERFACE: WHAT USERS SEE
# ----------------------------------------------------------------------
# Controls have IDs: the control "n" is read in the server as input$n.
# Output IDs link display locations to server functions, e.g. "distribution".
ui <- fluidPage(
  titlePanel("Abortion opinions: a polarization simulation"),
  p("Simplified policy scale: −1 = restrictive/pro-life; +1 = permissive/pro-choice. This is an illustrative model, not a prediction."),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of agents", 200, min = 50, max = 1000, step = 50),
      numericInput("steps", "Time steps (not real days)", 100, min = 10, max = 300, step = 10),
      numericInput("seed", "Random seed", 42, min = 1, max = 1000000),
      # Confidence bound is an opinion distance, not a probability.
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
      # Select an already-saved time point; this does not rerun the model.
      sliderInput("time", "Displayed time point", 0, 100, 100, step = 1),
      tabsetPanel(
        tabPanel("Opinions", verbatimTextOutput("summary"), plotOutput("distribution")),
        tabPanel("Dynamics", plotOutput("dynamics", height = "650px")),
        tabPanel("Agents", tableOutput("agents"),
                 downloadButton("download", "Download selected agent snapshot")),
        tabPanel("Model rules",
                 tags$ul(
                   tags$li("Random social partners; accepted contacts move opinions closer together."),
                   tags$li("Conviction reduces update size; confidence bounds determine acceptance."),
                   tags$li("Open-minded agents have wider bounds. Zealots' opinions stay fixed."),
                   tags$li("A single campaign supports either endpoint. Activists generate visibility."),
                   tags$li("Visibility raises attention; involvement follows attention more slowly."),
                   tags$li("Extra restrictive-group attention and backlash are optional assumptions, not established facts."),
                   tags$li("Rejected messages ordinarily have no effect. Enabled campaign backlash causes movement away."),
                   tags$li("External information is a separate policy message, not a factual-accuracy measure."),
                   tags$li("Initial opinions overlap across fixed orientation groups. This setup is not empirical calibration."),
                   tags$li("Dispersion and extremity alone do not establish two opposing camps."),
                   tags$li("No regression analysis is included. Tipping points and hysteresis are not assumed or tested in this prototype.")))
      )
    )
  )
)

# ----------------------------------------------------------------------
# 6. SERVER: CONNECT CONTROLS, SIMULATION, AND OUTPUTS
# ----------------------------------------------------------------------
# Reactive objects update when their dependencies change.
# The expensive simulation runs only when the button is clicked.
server <- function(input, output, session) {
  result <- reactiveVal(NULL) # Holds the most recently completed run.
  
  observeEvent(input$run, {
    # Numeric fields can be typed outside their suggested bounds.
    # Check inputs explicitly and show a message instead of trying a bad run.
    values <- c(input$n, input$steps, input$seed)
    valid <- length(values) == 3 && all(is.finite(values)) &&
      input$n >= 50 && input$n <= 1000 && input$n == floor(input$n) &&
      input$steps >= 10 && input$steps <= 300 && input$steps == floor(input$steps) &&
      input$seed >= 1 && input$seed <= 1000000 && input$seed == floor(input$seed)
    if (!valid) {
      showNotification("Use whole numbers: agents 50–1000, steps 10–300, seed 1–1000000.", type = "error")
      return(invisible(NULL))
    }
    # Capture all settings once. A selection input returns text, so the
    # campaign side is converted to a number before simulation calculations.
    p <- list(n = as.integer(input$n), steps = as.integer(input$steps),
              seed = as.integer(input$seed), conviction = input$conviction,
              bound = input$bound, open_share = input$open_share,
              zealot_share = input$zealot_share, campaign_side = as.numeric(input$campaign_side),
              activist_share = input$activist_share, intensity = input$intensity,
              extra_attention = input$extra_attention, backlash = input$backlash,
              backlash_strength = input$backlash_strength,
              information_rate = input$information_rate,
              information_position = input$information_position,
              information_uptake = input$information_uptake)
    withProgress(message = "Running simulation", value = 0, {
      result(run_simulation(p))
      incProgress(1)
    })
    # Update the slider duration and select the last saved time point.
    updateSliderInput(session, "time", max = p$steps, value = p$steps)
  }, ignoreInit = TRUE)
  
  # req silently waits until a simulation exists.
  selected_time <- reactive({
    req(result(), !is.null(input$time))
    min(as.integer(input$time), result()$settings$steps)
  })
  snapshot <- reactive({
    req(result())
    # Time zero is list entry 1 because R indexing starts at 1.
    result()$history[[selected_time() + 1L]]
  })
  
  # Show stored run settings, not any slider changes awaiting another run.
  output$run_info <- renderText({
    if (is.null(result())) return("Click Run simulation to begin.")
    r <- result()
    sprintf("Stored run: seed %d | %d agents | %d steps | actual activists %.1f%% | campaign %s | backlash %s",
            r$settings$seed, r$settings$n, r$settings$steps,
            100 * mean(r$history[[1]]$activist),
            ifelse(r$settings$campaign_side == 1, "pro-choice", "pro-life"),
            ifelse(r$settings$backlash, "on", "off"))
  })
  # renderPrint captures printed output; cat joins labels and values.
  output$summary <- renderPrint({
    m <- population_measures(snapshot(), selected_time())
    cat("Time:", m$time,
        "\nMean opinion:", round(m$mean_opinion, 3),
        "\nOpinion dispersion (SD):", round(m$dispersion, 3),
        "\nExtreme share (absolute opinion >= 0.8):", round(100 * m$extreme_share, 1), "%",
        "\nFixed-orientation group gap:", round(m$group_gap, 3),
        "\nMean attention:", round(m$attention, 3),
        "\nMean involvement:", round(m$involvement, 3), "\n")
  })
  # Fixed bins and axis limits make snapshots easier to compare.
  output$distribution <- renderPlot({
    hist(snapshot()$opinion, breaks = seq(-1, 1, length.out = 21),
         xlim = c(-1, 1), col = "steelblue", border = "white",
         main = paste("Opinion distribution: time", selected_time()),
         xlab = "Restrictive / pro-life (-1) to permissive / pro-choice (+1)")
    abline(v = 0, lty = 2) # Dashed neutral-position reference line.
  })
  # Three panels show the full run. Restore graphics settings afterward.
  output$dynamics <- renderPlot({
    req(result())
    m <- result()$measures
    previous <- par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
    on.exit(par(previous))
    plot(m$time, m$mean_opinion, type = "l", ylim = c(-1, 1),
         xlab = "Time step", ylab = "Mean opinion", main = "Overall policy opinion")
    abline(h = 0, lty = 2, col = "gray")
    plot(m$time, m$dispersion, type = "l", ylim = c(0, 1.1),
         xlab = "Time step", ylab = "Opinion SD", main = "Opinion dispersion")
    # matplot draws attention and involvement columns together.
    matplot(m$time, m[c("attention", "involvement")], type = "l", lty = 1,
            col = c("steelblue", "darkorange"), ylim = c(0, 1),
            xlab = "Time step", ylab = "Population mean")
    legend("topright", c("Attention", "Involvement"), lty = 1,
           col = c("steelblue", "darkorange"), bty = "n")
  })
  # Show the first 12 agents only; the CSV includes the entire snapshot.
  output$agents <- renderTable({ head(snapshot(), 12) }, digits = 3)
  output$download <- downloadHandler(
    filename = function() paste0("agents_time_", selected_time(), ".csv"),
    content = function(file) {
      a <- snapshot()
      a$time <- selected_time()
      # Add every stored setting as a column so this exported run is identifiable.
      for (name in names(result()$settings)) {
        a[[paste0("setting_", name)]] <- result()$settings[[name]]
      }
      write.csv(a, file, row.names = FALSE)
    })
}

# Combine the interface and server into the application.
shinyApp(ui, server)
