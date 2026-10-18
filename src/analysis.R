# ======================================================================
# Statistical analysis of the batch experiment (2 x 2 design):
#   campaign (narrow vs broad) x backlash (off vs on)
#
# Pre-specified (before looking at results):
#   primary outcome   = opinion SD among non-zealots at the final step
#   secondary         = between-group gap, share of extreme opinions
#   hypothesis        = a broad campaign polarises more than a narrow one,
#                       mainly when backlash is on (interaction).
# ======================================================================

#' Turn raw condition columns into labelled factors.
#' Works on both the `final` and the `trajectories` table.
#' The first label is the reference level in the linear model.
label_conditions <- function(data,
                             campaign_levels = c(0.3, 1),
                             campaign_labels = c("Narrow", "Broad"),
                             backlash_labels = c("Backlash off", "Backlash on")) {
  data$campaign <- factor(data$campaign_position,
                          levels = campaign_levels, labels = campaign_labels)
  if (anyNA(data$campaign)) {
    stop("campaign_position contains values not in campaign_levels.")
  }
  if (!is.factor(data$backlash)) {
    data$backlash <- factor(data$backlash, levels = c(FALSE, TRUE),
                            labels = backlash_labels)
  }
  data
}

#' Cohen's d for two independent samples (pooled SD).
cohens_d <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  pooled <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  (mean(x) - mean(y)) / pooled
}

#' Mean, SD and n of the outcome per condition.
describe_outcome <- function(data, outcome) {
  groups <- data[c("campaign", "backlash")]
  out <- aggregate(data[[outcome]], by = groups, FUN = mean)
  names(out)[3] <- "mean"
  out$sd <- aggregate(data[[outcome]], by = groups, FUN = sd)$x
  out$n <- aggregate(data[[outcome]], by = groups, FUN = length)$x
  out
}

#' Linear model: outcome ~ campaign * backlash.
fit_model <- function(data, outcome) {
  stopifnot(outcome %in% names(data))
  lm(as.formula(paste(outcome, "~ campaign * backlash")), data = data)
}

#' Coefficients with 95% confidence intervals as a data.frame.
tidy_coefficients <- function(fit) {
  ci <- confint(fit)
  data.frame(term = rownames(ci), estimate = unname(coef(fit)),
             conf_low = ci[, 1], conf_high = ci[, 2], row.names = NULL)
}

#' Broad minus narrow within each backlash setting: Welch t-test,
#' mean difference with 95% CI, Cohen's d, Holm-adjusted p-value.
compare_campaigns <- function(data, outcome) {
  rows <- lapply(levels(data$backlash), function(b) {
    d <- data[data$backlash == b, ]
    narrow <- d[[outcome]][d$campaign == levels(d$campaign)[1]]
    broad <- d[[outcome]][d$campaign == levels(d$campaign)[2]]
    tt <- t.test(broad, narrow)
    data.frame(backlash = b, mean_narrow = mean(narrow),
               mean_broad = mean(broad), difference = mean(broad) - mean(narrow),
               ci_low = tt$conf.int[1], ci_high = tt$conf.int[2],
               cohens_d = cohens_d(broad, narrow), p_value = tt$p.value)
  })
  out <- do.call(rbind, rows)
  out$p_holm <- p.adjust(out$p_value, method = "holm")
  out
}

#' Residual diagnostics (4 standard lm panels). Look at them and
#' describe what you see in main.qmd.
check_residuals <- function(fit) {
  op <- par(mfrow = c(2, 2))
  on.exit(par(op))
  plot(fit)
}

#' Full analysis of one outcome in one call.
#' @return list(descriptives, model, coefficients, comparisons)
run_analysis <- function(data, outcome = "dispersion_no_zealots") {
  fit <- fit_model(data, outcome)
  list(descriptives = describe_outcome(data, outcome),
       model = fit,
       coefficients = tidy_coefficients(fit),
       comparisons = compare_campaigns(data, outcome))
}
