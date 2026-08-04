#' Capture an SCI Snapshot
#'
#' Readiness is a trajectory, not a single moment. A snapshot records the SCI,
#' the band, and the underlying pillar and indicator scores at one point in time
#' (a dry run, a database lock) under a label, so later runs can be compared with
#' [sci_diff()] and tracked with [sci_snapshot_history()].
#'
#' @param evidence A validated evidence data.frame for this run.
#' @param label A short label for the run, for example `"DB lock minus 8 weeks"`.
#' @param config An `sci_config` from [sci_config_default()].
#' @param run_context Optional [r4subcore::r4sub_run_context] to record alongside
#'   the scores.
#'
#' @return An object of class `"sci_snapshot"`.
#'
#' @seealso [sci_diff()], [sci_snapshot_history()].
#'
#' @examples
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("S1", "DEV"))
#' ev <- suppressMessages(r4subcore::as_evidence(
#'   data.frame(
#'     asset_type = "dataset", asset_id = "ADSL", source_name = "p21",
#'     indicator_id = c("Q1", "T1"), indicator_name = c("Q1", "T1"),
#'     indicator_domain = c("quality", "trace"),
#'     severity = c("low", "high"), result = c("pass", "fail"),
#'     stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' ))
#' snap <- sci_snapshot(ev, "dry run 1")
#' snap
#'
#' @export
sci_snapshot <- function(evidence, label, config = sci_config_default(),
                         run_context = NULL) {
  if (!is.character(label) || length(label) != 1L || !nzchar(label)) {
    cli::cli_abort("{.arg label} must be a single non-empty string.")
  }

  ind <- compute_indicator_scores(evidence)
  ps  <- compute_pillar_scores(evidence, config = config)
  res <- compute_sci(ps, config = config)

  structure(
    list(
      label       = label,
      run_context = run_context,
      sci         = res$SCI,
      band        = res$band,
      n_critical  = res$n_critical,
      gated       = res$gated,
      pillars     = ps[, c("pillar", "pillar_score", "weight")],
      indicators  = ind[, c("indicator_id", "indicator_domain",
                            "indicator_score")]
    ),
    class = "sci_snapshot"
  )
}

#' @param x An `sci_snapshot`.
#' @param ... Ignored.
#' @rdname sci_snapshot
#' @export
print.sci_snapshot <- function(x, ...) {
  cli::cli_alert_info("SCI snapshot: {.val {x$label}}")
  cli::cli_alert_info("  SCI {.val {x$sci}} ({x$band}){if (isTRUE(x$gated)) ' [band capped by critical finding]' else ''}")
  invisible(x)
}


#' Diff Two SCI Snapshots
#'
#' Reports what moved between two snapshots: the overall SCI change, any band
#' change, per-pillar deltas, and which indicators improved, regressed, were
#' added, or were removed. This answers "what changed and why since the last
#' run", which is the question a readiness review actually asks.
#'
#' @param before,after `sci_snapshot` objects from [sci_snapshot()], in
#'   chronological order.
#'
#' @return An object of class `"sci_diff"` with `sci_delta`, `band_from`,
#'   `band_to`, `band_changed`, a `pillars` tibble of per-pillar deltas, and an
#'   `indicators` tibble of per-indicator changes ordered by the size of the
#'   move.
#'
#' @seealso [sci_snapshot()].
#'
#' @examples
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("S1", "DEV"))
#' mk <- function(res) suppressMessages(r4subcore::as_evidence(
#'   data.frame(
#'     asset_type = "dataset", asset_id = "ADSL", source_name = "p21",
#'     indicator_id = c("Q1", "T1"), indicator_name = c("Q1", "T1"),
#'     indicator_domain = c("quality", "trace"),
#'     severity = c("low", "high"), result = res,
#'     stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' ))
#' before <- sci_snapshot(mk(c("pass", "fail")), "before")
#' after  <- sci_snapshot(mk(c("pass", "pass")), "after")
#' sci_diff(before, after)
#'
#' @export
sci_diff <- function(before, after) {
  if (!inherits(before, "sci_snapshot") || !inherits(after, "sci_snapshot")) {
    cli::cli_abort("{.arg before} and {.arg after} must be {.cls sci_snapshot} objects.")
  }

  # Pillar deltas
  pill <- merge(
    before$pillars[, c("pillar", "pillar_score")],
    after$pillars[, c("pillar", "pillar_score")],
    by = "pillar", all = TRUE, suffixes = c("_before", "_after")
  )
  pill$delta <- round(pill$pillar_score_after - pill$pillar_score_before, 4)
  pill <- pill[order(-abs(pill$delta), na.last = TRUE), ]

  # Indicator changes
  ind <- merge(
    before$indicators, after$indicators,
    by = c("indicator_id", "indicator_domain"),
    all = TRUE, suffixes = c("_before", "_after")
  )
  b <- ind$indicator_score_before
  a <- ind$indicator_score_after
  ind$status <- ifelse(
    is.na(b) & !is.na(a), "added",
    ifelse(!is.na(b) & is.na(a), "removed",
      ifelse(is.na(a) & is.na(b), "unknown",
        ifelse(a > b, "improved",
          ifelse(a < b, "regressed", "unchanged"))))
  )
  ind$delta <- round(a - b, 4)
  ind <- ind[order(-abs(ind$delta), na.last = TRUE), ]

  structure(
    list(
      sci_delta    = round(after$sci - before$sci, 1),
      band_from    = before$band,
      band_to      = after$band,
      band_changed = !identical(before$band, after$band),
      pillars      = tibble::as_tibble(pill),
      indicators   = tibble::as_tibble(ind)
    ),
    class = "sci_diff"
  )
}

#' @param x An `sci_diff`.
#' @param ... Ignored.
#' @rdname sci_diff
#' @export
print.sci_diff <- function(x, ...) {
  arrow <- if (x$sci_delta > 0) "up" else if (x$sci_delta < 0) "down" else "flat"
  cli::cli_alert_info("SCI change: {.val {x$sci_delta}} ({arrow})")
  if (x$band_changed) {
    cli::cli_alert_info("Band: {x$band_from} -> {x$band_to}")
  } else {
    cli::cli_alert_info("Band: {x$band_to} (unchanged)")
  }
  moved <- x$indicators[x$indicators$status %in%
                          c("improved", "regressed", "added", "removed"), ]
  cli::cli_alert_info("Indicators moved: {nrow(moved)}")
  invisible(x)
}


#' Build a History Table from SCI Snapshots
#'
#' Binds a list of snapshots into one tidy row per snapshot, with the SCI, band,
#' critical-finding count, and each pillar score in its own column, ready for a
#' trend plot.
#'
#' @param snapshots A list of `sci_snapshot` objects, in chronological order.
#'
#' @return A tibble with one row per snapshot.
#'
#' @seealso [sci_snapshot()].
#'
#' @examples
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("S1", "DEV"))
#' mk <- function(res) suppressMessages(r4subcore::as_evidence(
#'   data.frame(
#'     asset_type = "dataset", asset_id = "ADSL", source_name = "p21",
#'     indicator_id = c("Q1", "T1"), indicator_name = c("Q1", "T1"),
#'     indicator_domain = c("quality", "trace"),
#'     severity = c("low", "high"), result = res,
#'     stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' ))
#' s1 <- sci_snapshot(mk(c("pass", "fail")), "week 1")
#' s2 <- sci_snapshot(mk(c("pass", "pass")), "week 2")
#' sci_snapshot_history(list(s1, s2))
#'
#' @export
sci_snapshot_history <- function(snapshots) {
  if (!is.list(snapshots) ||
      !all(vapply(snapshots, inherits, logical(1), "sci_snapshot"))) {
    cli::cli_abort("{.arg snapshots} must be a list of {.cls sci_snapshot} objects.")
  }

  rows <- lapply(snapshots, function(s) {
    base <- list(
      label      = s$label,
      sci        = s$sci,
      band       = s$band,
      n_critical = s$n_critical
    )
    pillars <- stats::setNames(
      as.list(s$pillars$pillar_score),
      s$pillars$pillar
    )
    tibble::as_tibble(c(base, pillars))
  })

  dplyr::bind_rows(rows)
}
