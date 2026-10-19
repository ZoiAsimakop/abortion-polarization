# ======================================================================
# Plots for the batch experiment. Every function takes the outcome column
# name, a label and colours as arguments, and returns a ggplot object.
# Requires: library(ggplot2)  (list it in the README)
# ======================================================================

default_colours <- c(Narrow = "#2A9D8F", Broad = "#6550C8")

#' Final outcome per condition: boxplot + one point per run.
#' @param data `final` table after label_conditions().
plot_outcomes <- function(data, outcome = "dispersion_no_zealots",
                          outcome_label = "Opinion SD (non-zealots), final step",
                          colours = default_colours) {
  ggplot(data, aes(x = campaign, y = .data[[outcome]], colour = campaign)) +
    geom_jitter(width = 0.15, alpha = 0.35, size = 1.2) +
    geom_boxplot(fill = NA, outlier.shape = NA, width = 0.5, colour = "grey25") +
    facet_wrap(~ backlash) +
    scale_colour_manual(values = colours, guide = "none") +
    labs(x = "Campaign", y = outcome_label,
         title = "Final polarisation by condition",
         subtitle = "Each point is one simulation run") +
    theme_minimal(base_size = 12)
}

#' Mean outcome over time (+/- 1 SD across runs) per condition.
#' @param trajectories `trajectories` table after label_conditions().
plot_trajectories <- function(trajectories, outcome = "dispersion_no_zealots",
                              outcome_label = "Opinion SD (non-zealots)",
                              colours = default_colours) {
  keys <- trajectories[c("time", "campaign", "backlash")]
  summ <- aggregate(trajectories[[outcome]], by = keys, FUN = mean)
  names(summ)[4] <- "mean"
  summ$sd <- aggregate(trajectories[[outcome]], by = keys, FUN = sd)$x

  ggplot(summ, aes(x = time, y = mean, colour = campaign, fill = campaign)) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ backlash) +
    scale_colour_manual(values = colours, name = "Campaign") +
    scale_fill_manual(values = colours, name = "Campaign") +
    labs(x = "Time step", y = outcome_label,
         title = "Polarisation over time",
         subtitle = "Line = mean across runs; band = +/- 1 SD") +
    theme_minimal(base_size = 12)
}

#' Regression coefficients with 95% CIs (intercept omitted).
#' @param coefficients output of tidy_coefficients().
plot_effects <- function(coefficients, outcome_label = "Opinion SD (non-zealots)") {
  d <- coefficients[coefficients$term != "(Intercept)", ]
  d$term <- gsub(":", " x ", gsub("campaign|backlash", "", d$term))
  ggplot(d, aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high)) +
    labs(x = paste("Effect on", outcome_label, "(95% CI)"), y = NULL,
         title = "Model coefficients",
         subtitle = "Reference: narrow campaign, backlash off") +
    theme_minimal(base_size = 12)
}
