#' End-to-End Submission Readiness Report
#'
#' Composes the pieces of an SCI analysis into a single report object: the
#' Submission Confidence Index and its decision band, the per-pillar breakdown,
#' the findings most worth acting on, and the pillars that offer the biggest
#' improvement. It is the one call that answers "where do we stand and what do
#' we fix next" from an evidence table.
#'
#' @details
#' The report reuses [compute_pillar_scores()] and [compute_sci()] for the score
#' and band (including the non-compensatory critical gate), [sci_explain()] for
#' pillar contributions, and, when `targets` is supplied,
#' [sci_gap_to_target()] for the distance to goal and the ranked pillar levers.
#' When `targets` is `NULL` the levers are ranked by headroom
#' (`weight * (100 - current)`), the SCI points a pillar could add.
#'
#' The report is authority-agnostic. To produce an authority-calibrated report,
#' pass a profile configuration as `config`, for example
#' `config = r4subprofile::profile_sci_config("FDA", "NDA")`.
#'
#' @param evidence A validated R4SUB evidence data.frame (from `r4subcore`).
#' @param config An `sci_config` from [sci_config_default()]. Pass a profile
#'   configuration here for authority-calibrated weights and bands.
#' @param targets Optional readiness goal: a band name (for example `"ready"`),
#'   a numeric SCI, or an [sci_targets()] object. When supplied, the report
#'   includes the gap to target and whether it is met.
#' @param top_n Maximum number of findings to keep in `gaps`. Default `10`.
#'
#' @return A list of class `"sci_report"` with elements `sci`, `band`, `gated`,
#'   `gate_reason`, `n_critical`, `pillars` (data.frame: pillar, score, weight,
#'   contribution), `gaps` (data.frame of the top failing and warning findings,
#'   worst first), `n_gaps` (total findings before truncation), `levers`
#'   (data.frame of pillars ranked by improvement opportunity), `target` (a list
#'   with target_sci, target_band, gap, met when `targets` is supplied, else
#'   `NULL`), and `generated_at`.
#'
#' @examples
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY1", "DEV"))
#' ev <- suppressMessages(r4subcore::as_evidence(
#'   data.frame(
#'     asset_type = "validation", asset_id = "ADSL",
#'     source_name = "pinnacle21",
#'     indicator_id = c("Q1", "Q2", "T1", "R1", "U1"),
#'     indicator_name = c("Missing var", "Label", "Trace", "Risk", "Reviewer guide"),
#'     indicator_domain = c("quality", "quality", "trace", "risk", "usability"),
#'     severity = c("high", "low", "medium", "high", "low"),
#'     result = c("fail", "pass", "warn", "fail", "pass"),
#'     stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' ))
#' rpt <- sci_report(ev, targets = "ready")
#' rpt
#'
#' @importFrom cli cli_abort
#' @export
sci_report <- function(evidence, config = sci_config_default(),
                       targets = NULL, top_n = 10L) {
  r4subcore::validate_evidence(evidence)
  if (!is.numeric(top_n) || length(top_n) != 1L || top_n < 1) {
    cli::cli_abort("{.arg top_n} must be a single positive integer.")
  }
  top_n <- as.integer(top_n)

  ps  <- compute_pillar_scores(evidence, config = config)
  res <- compute_sci(ps, config = config)
  expl <- sci_explain(evidence, config = config)
  pc <- expl$pillar_contributions

  pillars <- data.frame(
    pillar       = pc$pillar,
    score        = round(ifelse(is.na(pc$pillar_score), NA_real_,
                                pc$pillar_score * 100), 1),
    weight       = round(pc$weight, 3),
    contribution = round(pc$contribution, 1),
    stringsAsFactors = FALSE
  )

  # Findings worth acting on: failing and warning rows, worst first.
  sev_rank <- c(critical = 1L, high = 2L, medium = 3L, low = 4L, info = 5L)
  res_rank <- c(fail = 1L, warn = 2L)
  is_gap <- evidence$result %in% c("fail", "warn")
  g <- evidence[is_gap, , drop = FALSE]
  n_gaps <- nrow(g)
  if (n_gaps > 0L) {
    ord <- order(sev_rank[g$severity], res_rank[g$result], g$indicator_id)
    g <- g[ord, , drop = FALSE]
    gaps <- data.frame(
      indicator_id     = g$indicator_id,
      indicator_name   = g$indicator_name,
      indicator_domain = g$indicator_domain,
      severity         = g$severity,
      result           = g$result,
      message          = if ("message" %in% names(g)) g$message else NA_character_,
      stringsAsFactors = FALSE
    )
    gaps <- utils::head(gaps, top_n)
    rownames(gaps) <- NULL
  } else {
    gaps <- data.frame(
      indicator_id = character(0), indicator_name = character(0),
      indicator_domain = character(0), severity = character(0),
      result = character(0), message = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Levers: where improvement buys the most SCI.
  target <- NULL
  if (!is.null(targets)) {
    gap <- sci_gap_to_target(res, targets, config = config)
    levers <- gap$pillars
    target <- list(
      target_sci  = gap$target_sci,
      target_band = gap$target_band,
      gap         = gap$gap,
      met         = gap$met
    )
  } else {
    current <- pillars$score
    max_lift <- ifelse(is.na(current), NA_real_,
                       round(pillars$weight * (100 - current), 1))
    levers <- data.frame(
      pillar   = pillars$pillar,
      weight   = pillars$weight,
      current  = current,
      max_lift = max_lift,
      stringsAsFactors = FALSE
    )
    levers <- levers[order(-levers$max_lift, na.last = TRUE), , drop = FALSE]
    rownames(levers) <- NULL
  }

  structure(
    list(
      sci          = res$SCI,
      band         = res$band,
      gated        = res$gated,
      gate_reason  = res$gate_reason,
      n_critical   = res$n_critical,
      pillars      = pillars,
      gaps         = gaps,
      n_gaps       = n_gaps,
      levers       = levers,
      target       = target,
      generated_at = Sys.time()
    ),
    class = "sci_report"
  )
}

#' Print an SCI Report
#' @param x An `sci_report` object.
#' @param ... Ignored.
#' @export
print.sci_report <- function(x, ...) {
  cli::cli_h1("Submission Readiness Report")
  cli::cli_alert_info("SCI: {.val {x$sci}} / 100   Band: {.val {x$band}}")
  if (isTRUE(x$gated)) {
    cli::cli_alert_warning("Band capped: {x$gate_reason}")
  }
  if (!is.null(x$target)) {
    if (isTRUE(x$target$met)) {
      cli::cli_alert_success(
        "Meets target {.val {x$target$target_sci}} (band {.val {x$target$target_band}})."
      )
    } else {
      cli::cli_alert_warning(
        "{.val {x$target$gap}} SCI points below target {.val {x$target$target_sci}} (band {.val {x$target$target_band}})."
      )
    }
  }

  cli::cli_h2("Pillars")
  for (i in seq_len(nrow(x$pillars))) {
    sc <- x$pillars$score[i]
    sc_str <- if (is.na(sc)) "N/A" else sc
    cli::cli_li(
      "{x$pillars$pillar[i]}: {.val {sc_str}}  (weight {x$pillars$weight[i]}, contributes {x$pillars$contribution[i]})"
    )
  }

  cli::cli_h2("Top gaps ({x$n_gaps} finding{?s})")
  if (nrow(x$gaps) == 0L) {
    cli::cli_alert_success("No failing or warning findings.")
  } else {
    for (i in seq_len(nrow(x$gaps))) {
      msg <- x$gaps$message[i]
      tail <- if (is.na(msg) || !nzchar(msg)) "" else paste0(": ", msg)
      cli::cli_li(
        "[{x$gaps$severity[i]}/{x$gaps$result[i]}] {x$gaps$indicator_id[i]} ({x$gaps$indicator_domain[i]}){tail}"
      )
    }
  }

  cli::cli_h2("Biggest levers")
  lv <- x$levers
  top <- utils::head(lv[!is.na(lv$max_lift), , drop = FALSE], 3L)
  if (nrow(top) == 0L) {
    cli::cli_alert_info("No further headroom to report.")
  } else {
    for (i in seq_len(nrow(top))) {
      cli::cli_li(
        "{top$pillar[i]}: up to {.val {top$max_lift[i]}} SCI points"
      )
    }
  }
  invisible(x)
}
