# ======================================================================
# Example runs: one simulation with and without backlash, for a visual
# impression of what polarization looks like in the model.
# Requires the model functions (load_model()) and src/experiment.R.
# ======================================================================

#' Run the model once with backlash off and once with backlash on.
#'
#' Both runs use the same seed and the same settings as the experiment
#' (default_base_params()), so they differ only in backlash.
#' @param intensity campaign intensity (0 = no campaign, 1 = maximum).
#' @param seed random seed shared by both runs.
#' @param steps number of time steps.
#' @return data.frame with one row per agent, run and moment
#'   (columns: backlash, moment, opinion, zealot).
run_example_pair <- function(intensity = 0.75, seed = 1, steps = 300) {
  if (!exists("run_simulation", mode = "function")) load_model(find_model_file())
  one_run <- function(backlash) {
    p <- modifyList(default_base_params(steps),
                    list(intensity = intensity, backlash = backlash, seed = seed))
    history <- run_simulation(p)$history
    snapshot <- function(a, moment) {
      data.frame(backlash = if (backlash) "Backlash on" else "Backlash off",
                 moment = moment, opinion = a$opinion, zealot = a$zealot)
    }
    rbind(snapshot(history[[1]], "Start"),
          snapshot(history[[length(history)]], paste("After", steps, "steps")))
  }
  out <- rbind(one_run(FALSE), one_run(TRUE))
  out$moment <- factor(out$moment, levels = unique(out$moment))
  out
}

#' Histograms of opinions at the start and the end of each example run.
#' @param example output of run_example_pair().
#' @param include_zealots show zealots (always at -1 or +1) as well?
plot_opinion_distributions <- function(example, include_zealots = FALSE) {
  if (!include_zealots) example <- example[!example$zealot, ]
  ggplot(example, aes(x = opinion, fill = moment)) +
    geom_histogram(breaks = seq(-1, 1, by = 0.1), colour = "white",
                   position = "identity", alpha = 0.75) +
    facet_grid(moment ~ backlash) +
    scale_fill_manual(values = c("#9AA5B1", "#D6457F"), guide = "none") +
    scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1)) +
    labs(x = "Opinion (\u22121 = pro-life, +1 = pro-choice)",
         y = "Number of agents",
         title = "What polarization looks like in one run",
         subtitle = "Strong pro-choice campaign, same seed in both runs (zealots not shown)") +
    theme_study()
}
