library(testthat)
library(ggplot2)
source(file.path("..", "..", "src", "experiment.R"), local = TRUE)
source(file.path("..", "..", "src", "analysis.R"), local = TRUE)
source(file.path("..", "..", "src", "plots.R"), local = TRUE)

# Tiny stand-in for the real model (defined here, not shipped in src/).
# Same interface as the real one: run_simulation(p) -> list(history, measures).
# Every seed has its own starting opinions (shared across conditions, like a
# shared starting population); the effect adds a little noise. Two zealots sit at -1 and +1. The known effect:
# a strong campaign with backlash lowers non-zealot opinions by 0.2.
run_simulation <- function(p) {
  set.seed(p$seed)
  start <- runif(20, -0.5, 0.5)
  effect <- if (p$intensity > 0.5 && p$backlash) -0.2 else 0
  noise <- if (effect != 0) rnorm(20, 0, 0.01) else 0   # only where it acts
  history <- lapply(0:p$steps, function(t) {
    opinion <- start + noise * t / p$steps + effect * t / p$steps
    zealot <- c(TRUE, TRUE, rep(FALSE, 18))
    opinion[1:2] <- c(-1, 1)
    data.frame(opinion = opinion, initial_opinion = start, zealot = zealot,
               restrictive = as.integer(start < 0))
  })
  measures <- data.frame(time = 0:p$steps, mean_opinion = 0)
  list(history = history, measures = measures, settings = p)
}

base <- list(steps = 4)
conds <- expand.grid(intensity = c(0.25, 0.75), backlash = c(FALSE, TRUE))

test_that("no_zealot_measures leaves zealots out", {
  agents <- data.frame(opinion = c(-1, 1, 0.1, 0.3), initial_opinion = c(-1, 1, 0, 0),
                       zealot = c(TRUE, TRUE, FALSE, FALSE),
                       restrictive = c(1, 0, 0, 0))
  m <- no_zealot_measures(list(agents))
  expect_equal(m$mean_opinion_no_zealots, 0.2)
  expect_equal(m$dispersion_no_zealots, sd(c(0.1, 0.3)))
  expect_equal(m$extreme_share_no_zealots, 0)
  expect_equal(m$time, 0)
})

test_that("shift is measured from each person's own start and follows the campaign side", {
  agents <- data.frame(opinion = c(1, 0.4, -0.2), initial_opinion = c(1, 0.2, 0),
                       zealot = c(TRUE, FALSE, FALSE), restrictive = c(0, 1, 0))
  expect_equal(no_zealot_measures(list(agents))$shift_toward_campaign_no_zealots,
               mean(c(0.2, -0.2)))                       # = 0, zealot ignored
  agents$opinion[2:3] <- c(0.5, 0.1)                       # both moved up by 0.3 / 0.1
  expect_equal(no_zealot_measures(list(agents))$shift_toward_campaign_no_zealots, 0.2)
  expect_equal(no_zealot_measures(list(agents), campaign_side = -1)$shift_toward_campaign_no_zealots, -0.2)
})

test_that("run_experiment returns one final row per condition and run", {
  res <- run_experiment(conds, base, n_runs = 5, progress = FALSE)
  expect_equal(nrow(res$final), 4 * 5)
  expect_true(all(res$final$time == 4))
  expect_equal(nrow(res$trajectories), 4 * 5 * 5)
  expect_true("dispersion_no_zealots" %in% names(res$final))
})

test_that("same inputs give identical output", {
  a <- run_experiment(conds, base, n_runs = 5, progress = FALSE)
  b <- run_experiment(conds, base, n_runs = 5, progress = FALSE)
  expect_identical(a, b)
})

test_that("run_experiment rejects a seed column in the conditions", {
  expect_error(run_experiment(cbind(conds, seed = 1), base, n_runs = 2,
                              progress = FALSE))
})

test_that("label_conditions errors on unknown intensities", {
  d <- data.frame(intensity = c(0.25, 0.5), backlash = c(FALSE, TRUE))
  expect_error(label_conditions(d), "not in intensity_levels")
})

test_that("shift outcome recovers the known effect", {
  res <- run_experiment(conds, base, n_runs = 20, progress = FALSE)
  d <- res$final
  expect_equal(mean(d$shift_toward_campaign_no_zealots[d$intensity == 0.75 & d$backlash]),
               -0.2, tolerance = 0.02)
  expect_equal(mean(d$shift_toward_campaign_no_zealots[d$intensity == 0.25]), 0)
})

test_that("analysis recovers the known interaction", {
  res <- run_experiment(conds, base, n_runs = 30, progress = FALSE)
  a <- run_analysis(label_conditions(res$final), "mean_opinion_no_zealots")
  est <- function(term) a$coefficients$estimate[a$coefficients$term == term]
  expect_equal(est("intensityStrong campaign:backlashBacklash on"), -0.2,
               tolerance = 0.02)
  expect_equal(est("backlashBacklash on"), 0, tolerance = 0.02)
})

test_that("backlash contrast is found only for the strong campaign", {
  res <- run_experiment(conds, base, n_runs = 30, progress = FALSE)
  a <- run_analysis(label_conditions(res$final), "mean_opinion_no_zealots")
  b <- a$backlash_effects
  expect_equal(b$difference[b$intensity == "Strong campaign"], -0.2, tolerance = 0.02)
  weak <- b[b$intensity == "Weak campaign", ]
  expect_equal(weak$difference, 0)
  expect_true(is.na(weak$p_value))   # exactly zero in every run: no t-test
})

test_that("run terms are hidden from the coefficient table", {
  res <- run_experiment(conds, base, n_runs = 10, progress = FALSE)
  a <- run_analysis(label_conditions(res$final))
  expect_false(any(grepl("factor\\(run\\)", a$coefficients$term)))
  expect_equal(nrow(a$coefficients), 4)
})

test_that("pairing gives narrower intervals than ignoring the runs", {
  res <- run_experiment(conds, base, n_runs = 30, progress = FALSE)
  fin <- label_conditions(res$final)
  paired <- tidy_coefficients(fit_model(fin, "mean_opinion_no_zealots"))
  unpaired <- tidy_coefficients(
    lm(mean_opinion_no_zealots ~ intensity * backlash, data = fin))
  w <- function(x) with(x[x$term == "backlashBacklash on", ], conf_high - conf_low)
  expect_lt(w(paired), w(unpaired))
})

test_that("contrasts need every run in both groups", {
  res <- run_experiment(conds, base, n_runs = 5, progress = FALSE)
  fin <- label_conditions(res$final)
  fin <- fin[!(fin$run == 3 & fin$backlash == "Backlash on" &
                 fin$intensity == "Strong campaign"), ]
  expect_error(compare_backlash(fin, "mean_opinion_no_zealots"))
})

test_that("plots build", {
  res <- run_experiment(conds, base, n_runs = 5, progress = FALSE)
  fin <- label_conditions(res$final)
  tr <- label_conditions(res$trajectories)
  a <- run_analysis(fin)
  expect_s3_class(plot_outcomes(fin), "ggplot")
  expect_s3_class(plot_trajectories(tr), "ggplot")
  p <- plot_effects(a$coefficients)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$x, "Opinion SD")
  # opinion outcomes use the app's orange/purple scale
  a2 <- run_analysis(fin, "mean_opinion_no_zealots")
  expect_s3_class(plot_outcomes(fin, "mean_opinion_no_zealots"), "ggplot")
  expect_s3_class(plot_trajectories(tr, "mean_opinion_no_zealots"), "ggplot")
  expect_s3_class(plot_effects(a2$coefficients, outcome_is_opinion = TRUE), "ggplot")
})

test_that("campaign_effect subtracts the same run's no-campaign control", {
  d <- data.frame(intensity = rep(c(0, 0.25), each = 2), backlash = c(FALSE, FALSE, FALSE, TRUE),
                  run = c(1, 2, 1, 1), shift = c(0.1, 0.3, 0.5, 0.2))
  d <- d[-4, ]                                            # runs 1, 2 control; run 1 at 0.25
  out <- campaign_effect(d, "shift")
  expect_equal(nrow(out), 1)
  expect_equal(out$shift_vs_control, 0.5 - 0.1)
})

test_that("campaign_effect needs control runs and matching runs", {
  d <- data.frame(intensity = c(0.25, 0.75), backlash = FALSE, run = 1, shift = c(1, 2))
  expect_error(campaign_effect(d, "shift"), "No control runs")
  d2 <- data.frame(intensity = c(0, 0.25), backlash = FALSE, run = c(1, 2), shift = c(1, 2))
  expect_error(campaign_effect(d2, "shift"), "matching control")
})

test_that("effects against the control are recovered end to end", {
  conds0 <- rbind(data.frame(intensity = 0, backlash = FALSE), conds)
  res <- run_experiment(conds0, base, n_runs = 20, progress = FALSE)
  fin <- campaign_effect(res$final, "shift_toward_campaign_no_zealots")
  expect_false(any(fin$intensity == 0))
  a <- run_analysis(label_conditions(fin), "shift_toward_campaign_no_zealots_vs_control")
  b <- a$backlash_effects
  expect_equal(b$difference[b$intensity == "Strong campaign"], -0.2, tolerance = 0.02)
  tr <- campaign_effect(res$trajectories, "shift_toward_campaign_no_zealots")
  expect_equal(nrow(tr), 4 * 20 * 5)
})

# ---- pipeline: loading the model and knowing when results are out of date ----

write_fake_model <- function(path, marker = "1") {
  writeLines(c("# header with the app setup",
               "# 1. HELPERS",
               paste0("model_loaded_marker <- ", marker),
               "# 4. END OF MODEL",
               "# 5. BUILD THE INTERACTIVE DISPLAY",
               "stop('the app part must not run')"), path)
}

test_that("find_model_file and load_model handle the file and its absence", {
  dir <- tempfile(); dir.create(dir)
  expect_error(find_model_file(file.path(dir, "nothing.R")), "not found")
  model <- file.path(dir, "m.R"); write_fake_model(model)
  expect_identical(find_model_file(model), model)
  load_model(model)                         # must not run the app part
  expect_true(exists("model_loaded_marker", envir = globalenv()))
  rm("model_loaded_marker", envir = globalenv())
  bad <- file.path(dir, "bad.R"); writeLines("x <- 1", bad)
  expect_error(load_model(bad), "model section")
})

test_that("saved results are rerun when the model or the settings change", {
  dir <- tempfile(); dir.create(dir)
  model <- file.path(dir, "m.R"); write_fake_model(model)
  data_dir <- file.path(dir, "data")
  expect_false(results_are_current(model, n_runs = 2, steps = 5, data_dir = data_dir))
  run_full_experiment(model, n_runs = 2, steps = 5, data_dir = data_dir)
  expect_true(all(file.exists(file.path(data_dir,
    c("experiment_final.csv", "experiment_trajectories.csv",
      "sensitivity_final.csv", "model_used.txt")))))
  expect_true(results_are_current(model, n_runs = 2, steps = 5, data_dir = data_dir))
  expect_false(results_are_current(model, n_runs = 3, steps = 5, data_dir = data_dir))
  write_fake_model(model, marker = "2")     # the model changed
  expect_false(results_are_current(model, n_runs = 2, steps = 5, data_dir = data_dir))
  rm("model_loaded_marker", envir = globalenv())
})
