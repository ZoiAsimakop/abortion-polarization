# Runs the opinion model many times per condition, collects the results, and
# runs the whole experiment from the simulation model file.
#
# Needs run_simulation(p) from the model file. It returns list(history, measures,
# settings); `history` holds one agent table per time step (columns opinion,
# initial_opinion, zealot, restrictive). Measures for non-zealots are computed
# here from `history`, so the model does not have to provide them.
#
# Main entry point: run_full_experiment() (at the end of this file).

#' Measures among non-zealots only, for every time step of one run.
#'
#' Zealots never move and sit at the extremes, so they would inflate the
#' spread and the extreme share.
#' @param history list of agent tables (one per time step, first = step 0).
#' @param extreme_threshold |opinion| at or above this counts as extreme.
#' @param campaign_side +1 if the campaign pushes towards pro-choice, -1 if
#'   towards pro-life (the model's `campaign_side`).
#' @return data.frame with one row per time step.
no_zealot_measures <- function(history, extreme_threshold = 0.8,
                               campaign_side = 1) {
  rows <- lapply(seq_along(history), function(i) {
    agents <- history[[i]]
    free <- agents[!agents$zealot, , drop = FALSE]
    side <- free$restrictive == 1
    gap <- if (any(side) && any(!side)) {
      abs(mean(free$opinion[side]) - mean(free$opinion[!side]))
    } else NA_real_
    data.frame(time = i - 1L,
               mean_opinion_no_zealots = mean(free$opinion),
               # Movement away from each person's own starting opinion, positive
               # when towards the campaign's side.
               shift_toward_campaign_no_zealots =
                 campaign_side * mean(free$opinion - free$initial_opinion),
               dispersion_no_zealots = sd(free$opinion),
               extreme_share_no_zealots =
                 mean(abs(free$opinion) >= extreme_threshold),
               group_gap_no_zealots = gap)
  })
  do.call(rbind, rows)
}

#' Run the model repeatedly for every condition.
#'
#' @param conditions data.frame, one row per condition (e.g. from
#'   expand.grid). Column names must be model parameters. Must not
#'   contain `seed`.
#' @param base_params list of model parameters shared by all conditions.
#' @param n_runs number of runs (seeds 1..n_runs) per condition. The same
#'   seeds are used in every condition, so run k is matched across
#'   conditions (analysed as a paired comparison in analysis.R).
#' @param progress print a progress line per condition?
#' @return list(final, trajectories):
#'   final        one row per run: condition settings + last time step
#'   trajectories one row per run and time step
run_experiment <- function(conditions, base_params, n_runs = 100,
                           progress = TRUE) {
  stopifnot(is.data.frame(conditions), nrow(conditions) > 0,
            is.list(base_params), n_runs >= 1,
            !"seed" %in% names(conditions))

  finals <- list()
  trajectories <- list()
  k <- 0L

  for (i in seq_len(nrow(conditions))) {
    cond <- conditions[i, , drop = FALSE]
    rownames(cond) <- NULL
    label <- paste(names(cond), vapply(cond, as.character, character(1)),
                   sep = "=", collapse = ", ")
    if (progress) message(sprintf("Condition %d/%d: %s",
                                  i, nrow(conditions), label))

    for (s in seq_len(n_runs)) {
      p <- modifyList(base_params, c(as.list(cond), list(seed = s)))
      sim <- run_simulation(p)
      m <- sim$measures
      side <- if (is.null(p$campaign_side)) 1 else p$campaign_side
      extra <- no_zealot_measures(sim$history, campaign_side = side)
      m <- cbind(m, extra[setdiff(names(extra), names(m))])
      k <- k + 1L
      finals[[k]] <- cbind(cond, run = s, m[nrow(m), , drop = FALSE])
      trajectories[[k]] <- cbind(cond, run = s, m)
    }
  }

  final <- do.call(rbind, finals)
  trajectories <- do.call(rbind, trajectories)
  rownames(final) <- NULL
  rownames(trajectories) <- NULL
  list(final = final, trajectories = trajectories)
}

#' Save experiment output as CSV files so analysis can be rerun
#' without resimulating.
#'
#' The trajectories file only keeps the columns used for the plots and the
#' run-length check (the non-zealot measures), rounded to 4 decimals, so it is
#' small enough to commit (about 5 MB instead of about 28 MB).
save_experiment <- function(results, dir = "data") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(results$final, file.path(dir, "experiment_final.csv"),
            row.names = FALSE)
  keep <- c("intensity", "backlash", "run", "time",
            "mean_opinion_no_zealots", "shift_toward_campaign_no_zealots",
            "dispersion_no_zealots", "extreme_share_no_zealots")
  keep <- intersect(keep, names(results$trajectories))
  compact <- results$trajectories[keep]
  num <- vapply(compact, is.numeric, logical(1))
  compact[num] <- lapply(compact[num], round, 4)
  write.csv(compact, file.path(dir, "experiment_trajectories.csv"),
            row.names = FALSE)
  invisible(dir)
}

#' Load experiment output saved by save_experiment().
load_experiment <- function(dir = "data") {
  paths <- file.path(dir, c("experiment_final.csv",
                            "experiment_trajectories.csv"))
  if (!all(file.exists(paths))) {
    stop("Saved results not found in '", dir, "'. Run run_experiment() first.")
  }
  list(final = read.csv(paths[1]), trajectories = read.csv(paths[2]))
}

# ---- Running the whole experiment from the model file ----------------------

# Model files are looked up in this order; the first one that exists is used.
model_candidates <- c("simulation_code.R", "app.R")

#' Settings shared by all conditions. They should match the app's defaults;
#' if a default in the model file changes, change it here too.
default_base_params <- function(steps = 300) {
  list(n = 200, steps = steps, conviction = 0.5, bound = 0.4, open_share = 0.1,
       zealot_share = 0.1, activist_share = 0.1, backlash_strength = 0.5,
       information_rate = 0, information_position = 0, information_uptake = 0.5,
       campaign_side = 1)
}

#' Path of the model file: `model_file` if given, otherwise the first of
#' `model_candidates` found in the working directory.
find_model_file <- function(model_file = NULL) {
  if (is.null(model_file)) model_file <- Filter(file.exists, model_candidates)[1]
  if (is.na(model_file) || !file.exists(model_file)) {
    stop("Model file not found. Put it in the working directory (", getwd(),
         ") or pass model_file.")
  }
  model_file
}

#' Load the model part of the model file (sections 1-4) without starting the app.
#' Defines run_simulation() and its helpers in the global environment.
load_model <- function(model_file) {
  lines <- readLines(model_file, warn = FALSE)
  start <- grep("# 1. HELPERS", lines, fixed = TRUE)[1] - 1
  end   <- grep("# 5. BUILD THE INTERACTIVE", lines, fixed = TRUE)[1] - 2
  if (anyNA(c(start, end)) || end <= start) {
    stop("Could not find the model section in ", model_file,
         " (looked for the headings '# 1. HELPERS' and '# 5. BUILD THE INTERACTIVE').")
  }
  eval(parse(text = lines[start:end]), envir = globalenv())
  invisible(model_file)
}

# What the saved results were made from: model file, run settings, parameters.
run_info <- function(model_file, n_runs, steps, run_sensitivity,
                     sensitivity_bound, base_params) {
  c(paste("model_file:", basename(model_file)),
    paste("model_md5:", unname(tools::md5sum(model_file))),
    paste("n_runs:", n_runs), paste("steps:", steps),
    paste("sensitivity_bound:", if (run_sensitivity) sensitivity_bound else "none"),
    paste("base_params:", paste(deparse(base_params), collapse = " ")))
}

#' TRUE if the results in `data_dir` were made with this model file and these
#' settings, so they do not need to be rerun.
results_are_current <- function(model_file = NULL, n_runs = 100, steps = 300,
                                run_sensitivity = TRUE, sensitivity_bound = 0.6,
                                base_params = default_base_params(steps),
                                data_dir = "data") {
  model_file <- find_model_file(model_file)
  needed <- c("experiment_final.csv", "experiment_trajectories.csv", "model_used.txt",
              if (run_sensitivity) "sensitivity_final.csv")
  if (!all(file.exists(file.path(data_dir, needed)))) return(FALSE)
  identical(readLines(file.path(data_dir, "model_used.txt")),
            run_info(model_file, n_runs, steps, run_sensitivity,
                     sensitivity_bound, base_params))
}

#' Run the experiment and save the results in `data_dir`.
#'
#' Conditions: a no-campaign control plus weak/strong campaign x backlash off/on.
#' With run_sensitivity = TRUE the same experiment is repeated with another
#' confidence bound (robustness check in the results text).
#' Takes about 4 minutes for 100 runs (about 8 with the sensitivity run).
#' @return the results of the main experiment, invisibly.
run_full_experiment <- function(model_file = NULL, n_runs = 100, steps = 300,
                                run_sensitivity = TRUE, sensitivity_bound = 0.6,
                                intensities = c(0.25, 0.75),
                                base_params = default_base_params(steps),
                                data_dir = "data") {
  model_file <- find_model_file(model_file)
  message("Using model file: ", model_file)
  load_model(model_file)

  conditions <- rbind(
    data.frame(intensity = 0, backlash = FALSE),
    expand.grid(intensity = intensities, backlash = c(FALSE, TRUE))
  )
  results <- run_experiment(conditions, base_params, n_runs = n_runs)
  save_experiment(results, data_dir)

  sens_path <- file.path(data_dir, "sensitivity_final.csv")
  if (run_sensitivity) {
    sens <- run_experiment(conditions,
                           modifyList(base_params, list(bound = sensitivity_bound)),
                           n_runs = n_runs, progress = FALSE)
    sens$final$sensitivity_bound <- sensitivity_bound
    write.csv(sens$final, sens_path, row.names = FALSE)
  } else if (file.exists(sens_path)) {
    file.remove(sens_path)     # an old file would not match these results
  }
  writeLines(run_info(model_file, n_runs, steps, run_sensitivity,
                      sensitivity_bound, base_params),
             file.path(data_dir, "model_used.txt"))
  invisible(results)
}
