# Plots for the batch experiment. Each function takes the outcome column and a
# label and returns a ggplot object. Needs ggplot2.
#
# Colours follow the Shiny app: orange = pro-life and purple = pro-choice, used
# only when the plotted value is an opinion. Backlash off/on use pink and blue
# so they cannot be mistaken for opinion direction.

app_colours <- c(pro_life = "#C65D24", neutral = "#98A2B3", pro_choice = "#6550C8")

# Backlash off, backlash on
default_colours <- c("#D6457F", "#2A7AB8")

# TRUE for outcomes on the app's opinion scale (-1 pro-life, +1 pro-choice)
is_opinion_outcome <- function(outcome) grepl("mean_opinion|shift_toward", outcome)

# Axis title for such outcomes.
opinion_axis_label <- function(outcome) {
  if (grepl("shift_toward", outcome)) {
    "Opinion shift towards campaign side\n(non-zealots; + = towards pro-choice)"
  } else {
    "Mean opinion (non-zealots)\n-1 = pro-life, +1 = pro-choice"
  }
}

theme_study <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "#FFF7FA", colour = NA),
      panel.grid.major = element_line(colour = "#F1DCE5", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(colour = "#4A1D3B", face = "bold"),
      plot.subtitle    = element_text(colour = "#7A5A6D"),
      axis.title       = element_text(colour = "#4A1D3B"),
      axis.text        = element_text(colour = "#5E4455"),
      strip.text       = element_text(colour = "#4A1D3B", face = "bold"),
      legend.text      = element_text(colour = "#5E4455")
    )
}

#' Final outcome per condition: a boxplot with one point per run, backlash on
#' the x axis and one panel per campaign intensity. Opinion outcomes use the
#' app's opinion colours and a line at zero; other outcomes are coloured by
#' backlash.
#' @param data `final` table after label_conditions().
plot_outcomes <- function(data, outcome = "dispersion_no_zealots",
                          outcome_label = "Opinion SD (non-zealots)",
                          colours = default_colours) {
  opinion <- is_opinion_outcome(outcome)
  p <- ggplot(data, aes(x = backlash, y = .data[[outcome]]))
  if (opinion) {
    p <- p + geom_hline(yintercept = 0, colour = app_colours[["neutral"]],
                        linetype = "dashed") +
      geom_jitter(aes(colour = .data[[outcome]]), width = 0.15, alpha = 0.8,
                  size = 1.4) +
      scale_colour_gradient2(low = app_colours[["pro_life"]],
                             mid = app_colours[["neutral"]],
                             high = app_colours[["pro_choice"]],
                             midpoint = 0, limits = c(-1, 1), oob = scales::squish,
                             guide = "none")
    outcome_label <- opinion_axis_label(outcome)
  } else {
    p <- p + geom_jitter(aes(colour = backlash), width = 0.15, alpha = 0.4,
                         size = 1.4) +
      scale_colour_manual(values = colours, guide = "none")
  }
  p +
    geom_boxplot(fill = NA, outlier.shape = NA, width = 0.5, colour = "#4A1D3B") +
    facet_wrap(~ intensity) +
    labs(x = NULL, y = outcome_label,
         title = "Final step by condition",
         subtitle = "Each point is one simulation run") +
    theme_study()
}

#' Mean outcome over time (band = +/- 1 SD across runs) per condition.
#' @param trajectories `trajectories` table after label_conditions().
plot_trajectories <- function(trajectories, outcome = "dispersion_no_zealots",
                              outcome_label = "Opinion SD (non-zealots)",
                              colours = default_colours) {
  keys <- trajectories[c("time", "intensity", "backlash")]
  summ <- aggregate(trajectories[[outcome]], by = keys, FUN = mean)
  names(summ)[4] <- "mean"
  summ$sd <- aggregate(trajectories[[outcome]], by = keys, FUN = sd)$x

  p <- ggplot(summ, aes(x = time, y = mean, colour = backlash, fill = backlash))
  if (is_opinion_outcome(outcome)) {
    p <- p + geom_hline(yintercept = 0, colour = app_colours[["neutral"]],
                        linetype = "dashed")
    outcome_label <- opinion_axis_label(outcome)
  }
  p +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ intensity) +
    scale_colour_manual(values = colours, name = NULL) +
    scale_fill_manual(values = colours, name = NULL) +
    labs(x = "Time step", y = outcome_label,
         title = "Development over time",
         subtitle = "Line = mean across runs; band = +/- 1 SD") +
    theme_study()
}

#' Regression coefficients with 95% CIs (intercept omitted).
#' @param coefficients output of tidy_coefficients().
#' @param outcome_is_opinion TRUE if the outcome is an opinion (mean opinion):
#'   effects are then coloured by direction, orange = towards pro-life,
#'   purple = towards pro-choice.
plot_effects <- function(coefficients, outcome_label = "Opinion SD (non-zealots)",
                         outcome_is_opinion = FALSE) {
  d <- coefficients[coefficients$term != "(Intercept)", ]
  d$term <- gsub(":", " x ", gsub("intensity|backlash", "", d$term))
  d$direction <- ifelse(d$estimate < 0, "pro_life", "pro_choice")
  axis_note <- ""
  p <- ggplot(d, aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50")
  if (outcome_is_opinion) {
    p <- p + geom_pointrange(aes(xmin = conf_low, xmax = conf_high,
                                 colour = direction)) +
      scale_colour_manual(values = c(pro_life = app_colours[["pro_life"]],
                                     pro_choice = app_colours[["pro_choice"]]),
                          guide = "none")
    outcome_label <- "opinion shift"
    axis_note <- "\nnegative = towards pro-life, positive = towards pro-choice"
  } else {
    p <- p + geom_pointrange(aes(xmin = conf_low, xmax = conf_high),
                             colour = "#4A1D3B")
  }
  p +
    labs(x = paste0("Effect on ", outcome_label, " (95% CI)", axis_note), y = NULL,
         title = "Model coefficients",
         subtitle = "Reference: weak campaign, backlash off") +
    theme_study()
}
