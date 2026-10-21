# Builds the results section (tables, figures and APA-style text) from the saved
# experiment data. Every number in the text is computed from the data and the
# wording follows the numbers (reversed vs. reduced, polarization up vs. down,
# run long enough or not), so after a model change it is enough to rerun the
# experiment and build this again.
#
# In main.qmd, after loading the files in src/:
#   res <- build_results("data")
#   cat(res$text)                     # chunk option: results: asis
#   res$tables$campaign_effects, res$plots$shift, ...

build_results <- function(data_dir = "data",
                          sensitivity_file = "sensitivity_final.csv",
                          sensitivity_bound = 0.6) {
  results <- load_experiment(data_dir)
  levels_run <- sort(unique(results$final$intensity[results$final$intensity > 0]))
  if (length(levels_run) != 2) stop("Expected exactly two campaign intensities besides the control.")
  lab <- function(d) label_conditions(d, intensity_levels = levels_run)
  outcomes <- c(shift   = "shift_toward_campaign_no_zealots",
                spread  = "dispersion_no_zealots",
                extreme = "extreme_share_no_zealots")

  # ---- analysis of the campaign effect (difference from the no-campaign control)
  analyses <- lapply(outcomes, function(o) {
    fin <- lab(campaign_effect(results$final, o))
    run_analysis(fin, paste0(o, "_vs_control"))
  })
  names(analyses) <- names(outcomes)
  weak <- "Weak campaign"; strong <- "Strong campaign"
  off <- "Backlash off"; on <- "Backlash on"
  cell <- function(key, int, bl) {
    d <- analyses[[key]]$descriptives
    d$mean[d$intensity == int & d$backlash == bl]
  }
  bl_eff <- function(key, int) {
    b <- analyses[[key]]$backlash_effects
    b[b$intensity == int, ]
  }
  f  <- function(x, d = 2) sprintf(paste0("%+.", d, "f"), round(x, d) + 0)   # signed (no "-0.00")
  u  <- function(x, d = 2) sprintf(paste0("%.", d, "f"), round(x, d) + 0)    # unsigned
  pc <- function(x) sprintf("%.0f%%", 100 * x)

  # ---- control (no campaign): absolute values
  ctrl <- results$final[results$final$intensity == 0, ]
  n_runs <- length(unique(results$final$run))
  steps <- max(results$final$time)

  # ---- tables
  rows <- expand.grid(backlash = c(off, on), intensity = c(weak, strong),
                      stringsAsFactors = FALSE)
  tab <- data.frame(
    Campaign = rows$intensity, Backlash = rows$backlash,
    `Shift towards campaign` = mapply(function(i, b) f(cell("shift", i, b)), rows$intensity, rows$backlash),
    `Opinion SD` = mapply(function(i, b) f(cell("spread", i, b)), rows$intensity, rows$backlash),
    `Share extreme` = mapply(function(i, b) f(cell("extreme", i, b), 3), rows$intensity, rows$backlash),
    check.names = FALSE)
  contrast <- do.call(rbind, lapply(names(outcomes), function(k) {
    b <- analyses[[k]]$backlash_effects
    data.frame(Outcome = c(shift = "Shift towards campaign", spread = "Opinion SD",
                           extreme = "Share extreme")[[k]],
               Campaign = b$intensity,
               `Backlash on minus off` = f(b$difference, 3),
               `95% CI` = sprintf("[%s, %s]", f(b$ci_low, 3), f(b$ci_high, 3)),
               `Cohen's d (paired)` = u(b$cohens_d, 1), check.names = FALSE)
  }))
  coef_tab <- analyses$shift$coefficients
  coef_tab[-1] <- lapply(coef_tab[-1], function(x) round(x, 3))

  # ---- run length: absolute opinion SD over time (control included)
  tr <- results$trajectories
  cond_lab <- ifelse(tr$intensity == 0, "No campaign (control)",
                paste0(ifelse(tr$intensity == max(tr$intensity), strong, weak), ", ",
                       ifelse(tr$backlash, tolower(on), tolower(off))))
  tr$condition <- factor(cond_lab)
  sd_t <- aggregate(dispersion_no_zealots ~ condition + time, tr, mean)
  at <- function(cond, t) sd_t$dispersion_no_zealots[sd_t$condition == cond & sd_t$time == t]
  late <- function(cond) at(cond, steps) - at(cond, steps - 20)
  cond_names <- levels(tr$condition)
  late_change <- vapply(cond_names, late, numeric(1))
  still_moving <- cond_names[abs(late_change) > 0.005]

  # ---- sensitivity: another confidence bound
  sens <- NULL
  sens_path <- file.path(data_dir, sensitivity_file)
  if (file.exists(sens_path)) {
    sf <- read.csv(sens_path)
    if ("sensitivity_bound" %in% names(sf)) sensitivity_bound <- sf$sensitivity_bound[1]
    sens <- lapply(outcomes[c("shift", "extreme")], function(o) {
      run_analysis(lab(campaign_effect(sf, o)), paste0(o, "_vs_control"))
    })
  }
  sens_eff <- function(key, int) {
    b <- sens[[key]]$backlash_effects; b[b$intensity == int, ]
  }

  # ---- plots
  fin_shift <- lab(campaign_effect(results$final, outcomes[["shift"]]))
  fin_spread <- lab(campaign_effect(results$final, outcomes[["spread"]]))
  fin_ext <- lab(campaign_effect(results$final, outcomes[["extreme"]]))
  tr_shift <- lab(campaign_effect(results$trajectories, outcomes[["shift"]]))
  plots <- list(
    shift = plot_outcomes(fin_shift, "shift_toward_campaign_no_zealots_vs_control"),
    spread = plot_outcomes(fin_spread, "dispersion_no_zealots_vs_control",
                           "Opinion SD vs control (non-zealots)"),
    extreme = plot_outcomes(fin_ext, "extreme_share_no_zealots_vs_control",
                            "Share extreme vs control (non-zealots)"),
    shift_time = plot_trajectories(tr_shift, "shift_toward_campaign_no_zealots_vs_control"),
    effects = plot_effects(analyses$shift$coefficients, "opinion shift", outcome_is_opinion = TRUE),
    run_length = ggplot(sd_t, aes(time, dispersion_no_zealots, colour = condition)) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = c("#7A5A6D", "#D6457F", "#2A7AB8", "#E58AB4", "#8FBDE0")[seq_along(cond_names)],
                          name = NULL) +
      labs(x = "Time step", y = "Opinion SD (non-zealots)",
           title = "Does the spread level off?",
           subtitle = "Mean across runs; absolute values, control included") +
      theme_study()
  )

  # Text in APA style: true minus sign, no leading zero for proportions,
  # italic statistics, American spelling.
  mi <- "−"
  fn <- function(x, d = 2) {                       # number, e.g. -0.72 -> "−0.72"
    x <- round(x, d) + 0
    paste0(ifelse(x < 0, mi, ""), sprintf(paste0("%.", d, "f"), abs(x)))
  }
  fp <- function(x, d = 2) {                       # proportion: no leading zero (".04")
    x <- round(x, d) + 0
    paste0(ifelse(x < 0, mi, ""), sub("^0", "", sprintf(paste0("%.", d, "f"), abs(x))))
  }
  ci <- function(lo, hi, g = fn, d = 2) sprintf("95%% CI [%s, %s]", g(lo, d), g(hi, d))
  dz <- function(x) sprintf("*d*~z~ = %s", fn(x, 1))
  pc <- function(x) sprintf("%.0f%%", 100 * x)

  s_off <- cell("shift", strong, off); s_on <- cell("shift", strong, on)
  w_off <- cell("shift", weak, off); w_on <- cell("shift", weak, on)
  shift_word <- if (s_on < 0) "reversed" else if (s_on < s_off) "reduced" else "not reduced"
  b_strong <- bl_eff("shift", strong); b_weak <- bl_eff("shift", weak)
  e_strong <- bl_eff("extreme", strong); sd_strong <- bl_eff("spread", strong)
  e_on <- cell("extreme", strong, on); sd_on <- cell("spread", strong, on)
  ctrl_ext <- mean(ctrl$extreme_share_no_zealots)
  low <- levels_run[1]

  bigger <- if (abs(s_on) > abs(w_on)) "larger" else "smaller"
  # polarization: backlash on vs. off, strong campaign
  more_polar <- e_strong$ci_low > 0 && sd_strong$ci_low > 0
  less_polar <- e_strong$ci_high < 0 && sd_strong$ci_high < 0
  pol_word <- if (more_polar) "increased" else if (less_polar) "decreased" else "did not clearly change"
  pol_cmp <- if (more_polar) "higher" else if (less_polar) "lower" else "different"
  small_off <- abs(s_off) < 0.1
  mostly_backlash <- abs(e_strong$difference) > abs(cell("extreme", strong, off))
  big_d <- abs(b_strong$cohens_d) > 2
  key_findings <- c(
    sprintf("Without backlash, the mean shift toward the campaign's side was %s*M* = %s for the strong campaign. With backlash, the effect was %s (*M* = %s).",
            if (small_off) "only " else "", fn(s_off), shift_word, fn(s_on)),
    sprintf("Compared with backlash off, backlash %s polarization: with a strong campaign, %s of non-zealot agents held extreme opinions with backlash, compared with %s without backlash and %s in the control condition.",
            pol_word, pc(e_on + ctrl_ext), pc(cell("extreme", strong, off) + ctrl_ext), pc(ctrl_ext)),
    sprintf("The strong campaign had a %s backlash effect than the weak campaign (*M* = %s vs. *M* = %s).", bigger, fn(s_on), fn(w_on)),
    "These are results from a simulation model, not evidence about real campaigns.")
  text <- c(
    '::: {.callout-note title="Key findings"}',
    "",
    paste0("- ", key_findings),
    "",
    ":::",
    "",
    sprintf(paste0("*How to read the numbers.* *M* is a mean over %d runs. A shift below zero means opinions moved away from the campaign's side, ",
                   "above zero toward it. The 95%% CI shows how precisely a difference was measured, and *d*~z~ expresses the difference ",
                   "relative to how much it varies between runs (values above 0.8 are usually called large)."), n_runs),
    "",
    "### Design and Outcomes",
    paste0("The research question was how campaign intensity (weak vs. strong) and backlash (off vs. on) affect the campaign's effect on opinions and the polarization of opinions. "),
    "",
    sprintf(paste0("Each condition was simulated %d times for %d time steps. The same %d random seeds were used in every ",
                   "condition, so that each run could be compared with its matching control run (paired design). The conditions ",
                   "were no campaign (control), a weak campaign (intensity = %s), and a strong campaign (intensity = %s), each ",
                   "with backlash off and on. All outcomes were measured among non-zealot agents at the final time step and are ",
                   "reported as the difference from the matching no-campaign run, because opinions also drift in the absence of a ",
                   "campaign. In the control condition, the mean shift was *M* = %s, the standard deviation of opinions was ",
                   "*M* = %s, and the share of extreme opinions (absolute opinion ≥ 0.8) was %s."),
            n_runs, steps, n_runs, u(low), u(max(results$final$intensity)),
            fn(mean(ctrl$shift_toward_campaign_no_zealots)), fn(mean(ctrl$dispersion_no_zealots)), fp(ctrl_ext)),
    "",
    "### Campaign Effect",
    sprintf(paste0("Without backlash, the mean shift toward the campaign's side was *M* = %s for the strong campaign and ",
                   "*M* = %s for the weak campaign (opinion scale from %s1 to 1; Table 1, Figure 1). With backlash, the effect of the strong campaign was %s, with a mean ",
                   "shift of *M* = %s. The difference between backlash on and backlash off was *M*~diff~ = %s, %s, %s. ",
                   "For the weak campaign with backlash, the mean shift was *M* = %s (*M*~diff~ = %s, %s, %s)."),
            fn(s_off), fn(w_off), mi, shift_word, fn(s_on),
            fn(b_strong$difference, 3), ci(b_strong$ci_low, b_strong$ci_high, fn, 3), dz(b_strong$cohens_d),
            fn(w_on), fn(b_weak$difference, 3), ci(b_weak$ci_low, b_weak$ci_high, fn, 3), dz(b_weak$cohens_d)),
    "",
    "### Polarization",
    sprintf(paste0("With backlash on, the strong campaign changed the standard deviation of opinions by *M* = %s and the share of ",
                   "extreme opinions by %s relative to the control. Compared with backlash off, the standard deviation was %s by ",
                   "*M*~diff~ = %s, %s, %s, and the share of extreme opinions was %s by *M*~diff~ = %s, %s, %s (Table 2, Figure 2). ",
                   "Without backlash, the strong campaign changed the standard deviation by *M* = %s and the share of extreme ",
                   "opinions by %s. %s"),
            fn(sd_on), fp(e_on), pol_cmp, fn(sd_strong$difference), ci(sd_strong$ci_low, sd_strong$ci_high, fn, 2), dz(sd_strong$cohens_d),
            pol_cmp, fp(e_strong$difference, 3), ci(e_strong$ci_low, e_strong$ci_high, fp, 3), dz(e_strong$cohens_d),
            fn(cell("spread", strong, off)), fp(cell("extreme", strong, off)),
            if (mostly_backlash) "Most of the change in polarization in the model was therefore associated with backlash and not with the campaign by itself."
            else "Backlash therefore did not account for most of the change in polarization."),
    "",
    "### Run Length",
    if (length(still_moving) == 0) {
      sprintf("The standard deviation of opinions changed by less than 0.005 over the last 20 of %d time steps in every condition, indicating that the spread had settled.", steps)
    } else {
      sprintf(paste0("The standard deviation of opinions was still changing (by more than 0.005 over the last 20 of %d time steps) ",
                     "in the following condition(s): %s. The results therefore describe the situation after %d time steps and may ",
                     "differ for longer runs."),
              steps, paste(tolower(still_moving), collapse = "; "), steps)
    },
    "",
    if (!is.null(sens)) c(
      "### Sensitivity to the Confidence Bound",
      sprintf(paste0("The experiment was repeated with a different confidence bound (%s instead of the default). For the strong campaign, the ",
                     "mean shift with backlash on minus the shift with backlash off was *M*~diff~ = %s, %s, and the share of extreme ",
                     "opinions changed by %s. %s"),
              u(sensitivity_bound), fn(sens_eff("shift", strong)$difference),
              ci(sens_eff("shift", strong)$ci_low, sens_eff("shift", strong)$ci_high, fn, 2),
              fp(sens_eff("extreme", strong)$difference, 3),
              if (sign(sens_eff("shift", strong)$difference) == sign(b_strong$difference)) {
                "The direction of the backlash effect was the same as with the default bound."
              } else "The direction of the backlash effect differed from the default bound, so the conclusion depends on this setting."),
      "") else NULL,
    "### Discussion",
    sprintf(paste0("In the model, a campaign changed opinions only to the extent that agents accepted its message. With backlash, the effect of the ",
                   "strong campaign was %s, and polarization %s compared with backlash off: %s of non-zealot agents ended at an extreme position, compared with %s without backlash and %s in the ",
                   "control condition. The backlash effect was %s for the weak campaign than for the strong campaign (*M* = %s vs. *M* = %s). ",
                   "The direction of the backlash effect follows from the backlash rule built into the model%s; what the simulation adds is the ",
                   "size of the effect, its dependence on campaign intensity, and its consequences for polarization. These findings describe the ",
                   "behavior of the model under its assumptions and should not be read as evidence about real campaigns. Backlash is a modeling ",
                   "assumption, and the confidence bound and the share of open-minded agents were set by the authors and not estimated from data. ",
                   "%s Effect sizes and confidence intervals are reported instead of significance tests, because *p* values in simulation studies ",
                   "shrink as the number of runs increases."),
            shift_word, pol_word, pc(e_on + ctrl_ext), pc(cell("extreme", strong, off) + ctrl_ext), pc(ctrl_ext),
            if (abs(w_on) < abs(s_on)) "smaller" else "larger", fn(w_on), fn(s_on),
            if (s_on < 0) ", in which agents who reject a message move away from it" else "",
            if (big_d) "Effect sizes were large because simulation runs vary little compared with empirical data." else "")
  )

  plain <- function(x) gsub("~diff~", "", gsub("\\*", "", x))
  list(text = paste(text, collapse = "\n"), key_findings = plain(key_findings), tables = list(campaign_effects = tab, backlash_contrasts = contrast,
                                                         coefficients = coef_tab),
       plots = plots, analyses = analyses, late_change = late_change, results = results)
}
