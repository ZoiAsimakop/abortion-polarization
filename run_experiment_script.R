# Runs the experiment with the simulation model and analyses it.
# Open the project folder in RStudio (it contains the model file and src/) and
# run this script from top to bottom.
library(ggplot2)
for (f in c("src/experiment.R", "src/analysis.R", "src/plots.R", "src/results_report.R")) source(f)

# --- 1. Settings --------------------------------------------------------
n_runs <- 100             # runs per condition (5 for a quick test)
steps  <- 300             # time steps per run (30 for a quick test)
run_sensitivity <- TRUE   # also repeat with another confidence bound (about 4 extra minutes)

# --- 2. Run the experiment and save the results in data/ ---------------
# Uses the model file in this folder (see model_candidates in src/experiment.R).
results <- run_full_experiment(n_runs = n_runs, steps = steps,
                               run_sensitivity = run_sensitivity)

# --- 3. Analyse: campaign effect compared with the no-campaign control --
outcomes <- c(shift   = "shift_toward_campaign_no_zealots",
              spread  = "dispersion_no_zealots",
              extreme = "extreme_share_no_zealots")
analyses <- list()
for (name in names(outcomes)) {
  o <- outcomes[[name]]
  fin <- label_conditions(campaign_effect(results$final, o))
  analyses[[name]] <- run_analysis(fin, paste0(o, "_vs_control"))
  cat("\n=====", o, "(campaign effect vs control) =====\n")
  print(analyses[[name]]$descriptives)
  cat("\nBacklash on minus off, within each campaign intensity:\n")
  print(analyses[[name]]$backlash_effects, digits = 3)
}

# --- 4. Plots -----------------------------------------------------------
shift_final <- label_conditions(campaign_effect(results$final, outcomes[["shift"]]))
shift_traj  <- label_conditions(campaign_effect(results$trajectories, outcomes[["shift"]]))
shift_name  <- paste0(outcomes[["shift"]], "_vs_control")

print(plot_outcomes(shift_final, shift_name))                      # final step
print(plot_trajectories(shift_traj, shift_name))                   # over time
print(plot_effects(analyses$shift$coefficients, "opinion shift",
                   outcome_is_opinion = TRUE))                     # model effects

spread_final <- label_conditions(campaign_effect(results$final, outcomes[["spread"]]))
print(plot_outcomes(spread_final, paste0(outcomes[["spread"]], "_vs_control"),
                    "Opinion SD vs control (non-zealots)"))

# --- 5. Plain-text key findings (for a results tab in the app) ----------
writeLines(build_results("data")$key_findings, "data/key_findings.txt")
