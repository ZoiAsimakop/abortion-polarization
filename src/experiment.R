# ======================================================================
# Batch experiment: run the opinion model many times per condition.
#
# Expects these functions from the model files in src/ (loaded before this):
#   run_simulation(p, keep_history = FALSE) -> list(measures, settings)
# where `measures` has one row per time step, with (at least) the columns
#   time, dispersion_no_zealots, extreme_share_no_zealots,
#   group_gap_no_zealots
# and `p` accepts `campaign_position` (number in [-1, 1]) and `backlash`.
# ======================================================================

#' Run the model repeatedly for every condition.
#'
#' @param conditions data.frame, one row per condition (e.g. from
#'   expand.grid). Column names must be model parameters. Must not
#'   contain `seed`.
#' @param base_params list of model parameters shared by all conditions.
#' @param n_runs number of runs (seeds 1..n_runs) per condition. The same
#'   seeds are used in every condition (paired comparison).
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
      m <- run_simulation(p, keep_history = FALSE)$measures
      k <- k + 1L
      finals[[k]] <- data.frame(cond, run = s, m[nrow(m), , drop = FALSE],
                                row.names = NULL, check.names = FALSE)
      trajectories[[k]] <- data.frame(cond, run = s, m,
                                      row.names = NULL, check.names = FALSE)
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
save_experiment <- function(results, dir = "data") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(results$final, file.path(dir, "experiment_final.csv"),
            row.names = FALSE)
  write.csv(results$trajectories, file.path(dir, "experiment_trajectories.csv"),
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
