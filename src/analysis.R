# Statistical analysis of the batch experiment: campaign intensity (weak vs.
# strong) x backlash (off vs. on).
#
# Research question: how do campaign intensity and backlash affect the
# campaign's effect on opinions and the polarisation of opinions?
#
# Outcomes, all among non-zealots at the last step:
#   shift_toward_campaign_no_zealots  mean change from each person's own starting
#                                     opinion, positive = towards the campaign
#   dispersion_no_zealots             SD of opinions
#   extreme_share_no_zealots          share with |opinion| >= 0.8
#
# Opinions also drift without a campaign (social contact), so every outcome is
# compared with the control run (intensity 0) that has the same seed. See
# campaign_effect().

#' Campaign effect = outcome minus the same run's no-campaign control.
#'
#' The control run has the same seed (same starting population) and intensity 0.
#' Works on the `final` and the `trajectories` table (before
#' label_conditions()). Control rows are dropped and a column
#' `<outcome>_vs_control` is added.
#' @param control_intensity the `intensity` value that means "no campaign".
campaign_effect <- function(data, outcome, control_intensity = 0) {
  stopifnot(outcome %in% names(data), all(c("run", "intensity") %in% names(data)))
  key <- function(d) if ("time" %in% names(d)) paste(d$run, d$time) else d$run
  is_control <- data$intensity == control_intensity
  if (!any(is_control)) {
    stop("No control runs found: add a condition with intensity = ",
         control_intensity, " to the experiment.")
  }
  control <- data[is_control, ]
  control <- control[!duplicated(key(control)), ]  # backlash is irrelevant here
  baseline <- control[[outcome]][match(key(data), key(control))]
  if (anyNA(baseline)) stop("Some runs have no matching control run.")
  data[[paste0(outcome, "_vs_control")]] <- data[[outcome]] - baseline
  data[!is_control, ]
}

#' Turn raw condition columns into labelled factors.
#' Works on both the `final` and the `trajectories` table.
#' The first label is the reference level in the linear model.
#' @param intensity_levels the two `intensity` values that were run.
label_conditions <- function(data,
                             intensity_levels = c(0.25, 0.75),
                             intensity_labels = c("Weak campaign", "Strong campaign"),
                             backlash_labels = c("Backlash off", "Backlash on")) {
  stopifnot("intensity" %in% names(data), "backlash" %in% names(data))
  data$intensity <- factor(data$intensity,
                           levels = intensity_levels, labels = intensity_labels)
  if (anyNA(data$intensity)) {
    stop("intensity contains values not in intensity_levels.")
  }
  if (!is.factor(data$backlash)) {
    data$backlash <- factor(data$backlash, levels = c(FALSE, TRUE),
                            labels = backlash_labels)
  }
  data
}

#' Cohen's d for paired samples (d_z): mean of the within-run differences
#' divided by the SD of those differences.
cohens_d_paired <- function(x, y) {
  diffs <- x - y
  if (sd(diffs) < 1e-12) return(NA_real_)
  mean(diffs) / sd(diffs)
}

#' Mean, SD and n of the outcome per condition.
describe_outcome <- function(data, outcome) {
  groups <- data[c("intensity", "backlash")]
  out <- aggregate(data[[outcome]], by = groups, FUN = mean)
  names(out)[3] <- "mean"
  out$sd <- aggregate(data[[outcome]], by = groups, FUN = sd)$x
  out$n <- aggregate(data[[outcome]], by = groups, FUN = length)$x
  out
}

#' Linear model: outcome ~ intensity * backlash + run.
#' Runs share seeds across conditions, so `run` is a blocking factor: it removes
#' the noise shared between conditions and narrows the confidence intervals.
fit_model <- function(data, outcome) {
  stopifnot(outcome %in% names(data), "run" %in% names(data))
  lm(as.formula(paste(outcome, "~ intensity * backlash + factor(run)")),
     data = data)
}

#' Coefficients with 95% confidence intervals as a data.frame.
#' The run (blocking) terms are left out; they are not of interest.
tidy_coefficients <- function(fit) {
  ci <- confint(fit)
  out <- data.frame(term = rownames(ci), estimate = unname(coef(fit)),
                    conf_low = ci[, 1], conf_high = ci[, 2], row.names = NULL)
  out[!grepl("^factor\\(run\\)", out$term), ]
}

#' Paired comparison of the second vs the first level of `over`, separately
#' within each level of `by`. Runs are matched by seed. Reports the mean
#' difference with 95% CI, paired Cohen's d (d_z) and a Holm-adjusted p.
#' If a difference is exactly zero in every run, the CI is the single value
#' and p / d are NA (a t-test is not defined).
paired_contrast <- function(data, outcome, over, by) {
  rows <- lapply(levels(data[[by]]), function(b) {
    d <- data[data[[by]] == b, ]
    d <- d[order(d$run), ]
    lv <- levels(d[[over]])
    reference <- d[d[[over]] == lv[1], ]
    comparison <- d[d[[over]] == lv[2], ]
    stopifnot(identical(reference$run, comparison$run))  # pairs must match
    x <- comparison[[outcome]]
    y <- reference[[outcome]]
    constant <- sd(x - y) < 1e-12   # (numerically) identical in every run
    tt <- if (constant) NULL else t.test(x, y, paired = TRUE)
    data.frame(level = b, mean_reference = mean(y), mean_comparison = mean(x),
               difference = mean(x) - mean(y),
               ci_low = if (constant) mean(x - y) else tt$conf.int[1],
               ci_high = if (constant) mean(x - y) else tt$conf.int[2],
               cohens_d = cohens_d_paired(x, y),
               p_value = if (constant) NA_real_ else tt$p.value)
  })
  out <- do.call(rbind, rows)
  names(out)[1] <- by
  out$p_holm <- p.adjust(out$p_value, method = "holm")
  out
}

#' Strong minus weak campaign, within each backlash setting.
compare_intensity <- function(data, outcome) {
  paired_contrast(data, outcome, over = "intensity", by = "backlash")
}

#' Backlash on minus off, within each campaign intensity.
compare_backlash <- function(data, outcome) {
  paired_contrast(data, outcome, over = "backlash", by = "intensity")
}

#' The four standard residual plots of the model.
check_residuals <- function(fit) {
  op <- par(mfrow = c(2, 2))
  on.exit(par(op))
  plot(fit)
}

#' Full analysis of one outcome in one call.
#' @return list(descriptives, model, coefficients, intensity_effects,
#'   backlash_effects)
run_analysis <- function(data, outcome = "dispersion_no_zealots") {
  fit <- fit_model(data, outcome)
  list(descriptives = describe_outcome(data, outcome),
       model = fit,
       coefficients = tidy_coefficients(fit),
       intensity_effects = compare_intensity(data, outcome),
       backlash_effects = compare_backlash(data, outcome))
}
