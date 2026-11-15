#' SCI Scenario / What-If Analyzer
#'
#' Recomputes the Submission Confidence Index for one or more caller-specified
#' hypothetical changes to the evidence and returns a tidy comparison against
#' the baseline: the baseline SCI, each scenario's SCI, the delta, the band
#' movement, and which indicators drove the change. Use it to answer "what is
#' the highest-leverage fix to raise our readiness score?" purely mechanically.
#'
#' @details
#' Each scenario describes hypothetical edits to the evidence - for example "if
#' indicator `Q1` flips from `fail` to `pass`" or "if these findings were
#' resolved". Every edit is **caller-supplied**: this function invents no
#' weights, thresholds, or scores. It only applies the requested edits to a copy
#' of the evidence and re-runs the existing scoring engine
#' ([compute_pillar_scores()] then [compute_sci()]) on the modified table, so
#' the scenario SCI is produced by exactly the same math as the baseline.
#'
#' A scenario is a data frame of edits with a required `indicator_id` column and
#' at least one of the scoring-input columns `result` and `severity`. For each
#' edit row, every evidence row carrying that `indicator_id` has the supplied
#' `result` and/or `severity` overwritten (an `NA` leaves that field unchanged).
#' The modified evidence is re-validated with [r4subcore::validate_evidence()],
#' so an invalid `result` or `severity` value raises an error rather than being
#' scored silently.
#'
#' `scenarios` may be a single edit data frame (treated as one scenario named
#' `"scenario"`) or a named list of edit data frames (one named comparison per
#' element; unnamed elements are labelled `scenario_1`, `scenario_2`, ...).
#'
#' The drivers reuse [sci_decompose()]: each indicator's change in *earned* SCI
#' points (`scenario_earned - baseline_earned`) is reported, so the drivers of a
#' scenario sum to its `delta_sci` up to the usual one-decimal rounding. This
#' makes "which items drove it" an exact accounting of the move, not a heuristic
#' ranking.
#'
#' @param evidence A validated evidence data.frame (from `r4subcore`).
#' @param scenarios A single edit data frame, or a (optionally named) list of
#'   edit data frames. See Details for the edit-frame contract.
#' @param config An `sci_config` from [sci_config_default()]. Pass a profile
#'   configuration here for authority-calibrated weights and bands.
#'
#' @return A list of class `"sci_scenario"` with:
#'   - `baseline`: a list with `sci`, `band`, `gated`, `gate_reason`,
#'     `n_critical`
#'   - `comparison`: a tibble, one row per scenario, with `scenario`,
#'     `baseline_sci`, `scenario_sci`, `delta_sci`, `baseline_band`,
#'     `scenario_band`, `band_changed`, `gated`, `n_indicators_changed`, and
#'     `n_rows_changed`
#'   - `drivers`: a tidy tibble of the indicators whose earned points moved,
#'     with `scenario`, `indicator_id`, `indicator_name`, `indicator_domain`,
#'     `baseline_earned`, `scenario_earned`, and `delta_earned`, ordered within
#'     each scenario by `delta_earned` descending
#'   - `config`: the configuration used
#'
#' @seealso [sci_what_if()] for a single-indicator score override,
#'   [sci_decompose()] for the earned-points accounting the drivers reuse, and
#'   [compute_sci()] for the underlying score.
#'
#' @examples
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY1", "DEV"))
#' ev <- suppressMessages(r4subcore::as_evidence(
#'   data.frame(
#'     asset_type = "validation", asset_id = "ADSL",
#'     source_name = "pinnacle21",
#'     indicator_id = c("Q1", "Q2", "T1", "R1", "U1"),
#'     indicator_name = c("Missing var", "Label", "Trace", "Risk", "Guide"),
#'     indicator_domain = c("quality", "quality", "trace", "risk", "usability"),
#'     severity = c("high", "low", "medium", "high", "low"),
#'     result = c("fail", "pass", "warn", "fail", "pass"),
#'     stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' ))
#'
#' sc <- sci_scenario(
#'   ev,
#'   scenarios = list(
#'     fix_quality = data.frame(indicator_id = "Q1", result = "pass"),
#'     fix_both    = data.frame(
#'       indicator_id = c("Q1", "R1"), result = c("pass", "pass")
#'     )
#'   )
#' )
#' sc
#' sc$comparison
#'
#' @export
sci_scenario <- function(evidence, scenarios, config = sci_config_default()) {
  r4subcore::validate_evidence(evidence)
  if (!inherits(config, "sci_config")) {
    cli::cli_abort("{.arg config} must be an {.cls sci_config}.")
  }

  specs <- normalize_scenarios(scenarios)

  # Baseline score and decomposition, computed once.
  ps0  <- compute_pillar_scores(evidence, config = config)
  res0 <- compute_sci(ps0, config = config)
  dec0 <- sci_decompose(evidence, config = config)
  base_earned <- earned_by_indicator(dec0)

  comp_rows   <- vector("list", length(specs))
  driver_rows <- list()

  for (j in seq_along(specs)) {
    nm   <- names(specs)[j]
    spec <- specs[[j]]

    applied <- apply_scenario_edits(evidence, spec)
    ev_s <- applied$evidence
    r4subcore::validate_evidence(ev_s)

    ps_s  <- compute_pillar_scores(ev_s, config = config)
    res_s <- compute_sci(ps_s, config = config)
    dec_s <- sci_decompose(ev_s, config = config)
    scen_earned <- earned_by_indicator(dec_s)

    delta_sci <- if (is.na(res_s$SCI) || is.na(res0$SCI)) NA_real_ else
      round(res_s$SCI - res0$SCI, 1)

    comp_rows[[j]] <- tibble::tibble(
      scenario             = nm,
      baseline_sci         = res0$SCI,
      scenario_sci         = res_s$SCI,
      delta_sci            = delta_sci,
      baseline_band        = res0$band,
      scenario_band        = res_s$band,
      band_changed         = !identical(res0$band, res_s$band),
      gated                = isTRUE(res_s$gated),
      n_indicators_changed = applied$n_indicators_changed,
      n_rows_changed       = applied$n_rows_changed
    )

    # Drivers: indicators whose earned points moved, reusing sci_decompose().
    keys <- union(names(base_earned), names(scen_earned))
    be <- base_earned[keys]
    se <- scen_earned[keys]
    delta <- diff_earned(be, se)
    keep  <- which(!is.na(delta) & abs(delta) > 1e-9)

    if (length(keep) > 0L) {
      meta <- dec_s$indicators
      idx  <- match(keys[keep], driver_key(meta))
      ord  <- order(-delta[keep])
      driver_rows[[length(driver_rows) + 1L]] <- tibble::tibble(
        scenario         = nm,
        indicator_id     = meta$indicator_id[idx][ord],
        indicator_name   = meta$indicator_name[idx][ord],
        indicator_domain = meta$indicator_domain[idx][ord],
        baseline_earned  = round(unname(be[keep][ord]), 4),
        scenario_earned  = round(unname(se[keep][ord]), 4),
        delta_earned     = round(delta[keep][ord], 4)
      )
    }
  }

  comparison <- dplyr::bind_rows(comp_rows)
  drivers <- if (length(driver_rows) == 0L) {
    tibble::tibble(
      scenario = character(0), indicator_id = character(0),
      indicator_name = character(0), indicator_domain = character(0),
      baseline_earned = numeric(0), scenario_earned = numeric(0),
      delta_earned = numeric(0)
    )
  } else {
    dplyr::bind_rows(driver_rows)
  }

  structure(
    list(
      baseline = list(
        sci         = res0$SCI,
        band        = res0$band,
        gated       = isTRUE(res0$gated),
        gate_reason = res0$gate_reason,
        n_critical  = res0$n_critical
      ),
      comparison = comparison,
      drivers    = drivers,
      config     = config
    ),
    class = "sci_scenario"
  )
}


# Coerce the `scenarios` argument into a named list of validated edit frames.
# Accepts a single data.frame (one scenario) or a list of data.frames.
normalize_scenarios <- function(scenarios) {
  if (is.data.frame(scenarios)) {
    specs <- list(scenario = scenarios)
  } else if (is.list(scenarios)) {
    if (length(scenarios) == 0L) {
      cli::cli_abort("{.arg scenarios} must contain at least one scenario.")
    }
    if (!all(vapply(scenarios, is.data.frame, logical(1)))) {
      cli::cli_abort("Each scenario in {.arg scenarios} must be a data frame of edits.")
    }
    specs <- scenarios
    nms <- names(specs)
    if (is.null(nms)) nms <- rep("", length(specs))
    blank <- !nzchar(nms)
    nms[blank] <- paste0("scenario_", seq_along(nms))[blank]
    if (anyDuplicated(nms)) {
      cli::cli_abort("Scenario names must be unique; got {.val {nms[duplicated(nms)]}}.")
    }
    names(specs) <- nms
  } else {
    cli::cli_abort(
      "{.arg scenarios} must be a data frame or a list of data frames."
    )
  }

  for (nm in names(specs)) validate_edit_frame(specs[[nm]], nm)
  specs
}


# Validate a single edit frame: non-empty, has indicator_id and at least one of
# result/severity, and no other (silently ignored) columns.
validate_edit_frame <- function(spec, nm) {
  if (!is.data.frame(spec) || nrow(spec) == 0L) {
    cli::cli_abort("Scenario {.val {nm}} must be a non-empty data frame of edits.")
  }
  if (!"indicator_id" %in% names(spec)) {
    cli::cli_abort("Scenario {.val {nm}} must have an {.field indicator_id} column.")
  }
  override_cols <- intersect(c("result", "severity"), names(spec))
  if (length(override_cols) == 0L) {
    cli::cli_abort(
      "Scenario {.val {nm}} must supply a {.field result} and/or {.field severity} column to change."
    )
  }
  allowed <- c("indicator_id", "result", "severity")
  extra <- setdiff(names(spec), allowed)
  if (length(extra) > 0L) {
    cli::cli_abort(
      "Scenario {.val {nm}} has unsupported column(s): {.val {extra}}. \\
       Only {.val {allowed}} are used."
    )
  }
  invisible(spec)
}


# Apply one edit frame to a copy of the evidence. Matches evidence rows by
# indicator_id and overwrites result/severity where supplied (non-NA). Returns
# the edited evidence plus counts of what changed.
apply_scenario_edits <- function(evidence, spec) {
  ev <- evidence
  n_rows_changed <- 0L
  touched_ids <- character(0)

  for (i in seq_len(nrow(spec))) {
    id   <- spec$indicator_id[i]
    rows <- which(ev$indicator_id == id)
    if (length(rows) == 0L) {
      cli::cli_abort("Indicator {.val {id}} not found in evidence.")
    }

    if ("result" %in% names(spec) && !is.na(spec$result[i])) {
      newv <- as.character(spec$result[i])
      n_rows_changed <- n_rows_changed +
        sum(ev$result[rows] != newv, na.rm = TRUE)
      ev$result[rows] <- newv
    }
    if ("severity" %in% names(spec) && !is.na(spec$severity[i])) {
      newv <- as.character(spec$severity[i])
      n_rows_changed <- n_rows_changed +
        sum(ev$severity[rows] != newv, na.rm = TRUE)
      ev$severity[rows] <- newv
    }
    touched_ids <- c(touched_ids, id)
  }

  list(
    evidence             = ev,
    n_rows_changed       = as.integer(n_rows_changed),
    n_indicators_changed = length(unique(touched_ids))
  )
}


# A stable per-indicator key (id + domain) for aligning decomposition rows.
driver_key <- function(ind) paste0(ind$indicator_id, "\x01", ind$indicator_domain)

# Named vector of earned SCI points per indicator from a decomposition.
earned_by_indicator <- function(dec) {
  ind <- dec$indicators
  stats::setNames(ind$earned, driver_key(ind))
}

# Elementwise earned delta treating a missing (NA/absent) side as 0 only when
# the other side is present; NA - NA stays NA so untouched unscored pillars drop.
diff_earned <- function(be, se) {
  b <- unname(be); s <- unname(se)
  both_na <- is.na(b) & is.na(s)
  b[is.na(b)] <- 0
  s[is.na(s)] <- 0
  out <- s - b
  out[both_na] <- NA_real_
  out
}


#' Print an SCI Scenario Analysis
#' @param x An `sci_scenario` object.
#' @param n_drivers Number of top drivers to show per scenario (default `3`).
#' @param ... Ignored.
#' @export
print.sci_scenario <- function(x, n_drivers = 3L, ...) {
  cli::cli_h2("SCI scenario analysis")
  cli::cli_alert_info(
    "Baseline SCI {.val {x$baseline$sci}} / 100   Band {.val {x$baseline$band}}"
  )
  if (isTRUE(x$baseline$gated)) {
    cli::cli_alert_warning("Baseline band capped: {x$baseline$gate_reason}")
  }

  cm <- x$comparison
  for (i in seq_len(nrow(cm))) {
    delta <- cm$delta_sci[i]
    sign  <- if (is.na(delta)) "" else if (delta >= 0) "+" else ""
    band_note <- if (isTRUE(cm$band_changed[i])) {
      sprintf(" [band %s -> %s]", cm$baseline_band[i], cm$scenario_band[i])
    } else ""
    gate_note <- if (isTRUE(cm$gated[i])) " (gated)" else ""
    cli::cli_h3("{cm$scenario[i]}")
    cli::cli_li(
      "SCI {.val {cm$scenario_sci[i]}} ({sign}{delta}){band_note}{gate_note}"
    )

    drv <- x$drivers[x$drivers$scenario == cm$scenario[i], , drop = FALSE]
    drv <- utils::head(drv, n_drivers)
    if (nrow(drv) > 0L) {
      for (k in seq_len(nrow(drv))) {
        d <- drv$delta_earned[k]
        ds <- if (d >= 0) paste0("+", round(d, 2)) else as.character(round(d, 2))
        cli::cli_li(
          "  {drv$indicator_id[k]} ({drv$indicator_domain[k]}): {ds} SCI points"
        )
      }
    }
  }
  invisible(x)
}
