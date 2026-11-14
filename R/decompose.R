#' Exact Additive Decomposition of the SCI
#'
#' Breaks the reported Submission Confidence Index into the exact points each
#' pillar, and each indicator, earns and leaves on the table, so a single score
#' can be read as a reconciled waterfall rather than a black box. Unlike
#' [sci_explain()], which ranks loss contributions using the nominal pillar
#' weights, this decomposition uses the *effective* weights that
#' [compute_sci()] actually applied, so the earned points sum back to the
#' reported SCI even when a pillar is missing and its weight has been
#' redistributed.
#'
#' @details
#' The SCI is a weighted average of pillar scores scaled to 0--100:
#'
#' `SCI = sum(effective_weight * pillar_score) * 100`
#'
#' where the effective weights are the nominal `config` weights renormalised
#' over the pillars that actually have a score (see [compute_sci()]). This
#' function reuses those exact effective weights rather than recomputing them,
#' so no scoring semantics are introduced or changed here.
#'
#' For each scored pillar it reports two quantities that partition the pillar's
#' weight budget (`effective_weight * 100`):
#'
#' - `earned`: the points the pillar contributes to the SCI,
#'   `effective_weight * pillar_score * 100`.
#' - `shortfall`: the points the pillar leaves on the table,
#'   `effective_weight * (1 - pillar_score) * 100`.
#'
#' `earned + shortfall` equals the pillar's weight budget, the earned column
#' sums to the (unrounded) SCI, and the shortfall column sums to `100 - SCI`.
#'
#' The same partition is pushed down to indicators. A pillar score is the mean
#' of its `k` scored indicators, so each scored indicator carries an equal share
#' `effective_weight / k` of the pillar's budget and earns
#' `effective_weight / k * indicator_score * 100`. Indicator earned points sum
#' to their pillar's earned points, and therefore to the SCI overall. Pillars
#' and indicators with no score (`NA`) are reported with `NA` contributions and
#' are excluded from the sums, exactly as they are excluded from the SCI.
#'
#' A tiny rounding residual (`sci_exact - sci`) can arise because the reported
#' SCI is rounded to one decimal place; it is returned as `residual` and is at
#' most 0.05 in magnitude.
#'
#' @param evidence A validated evidence data.frame (from `r4subcore`).
#' @param config An `sci_config` from [sci_config_default()]. Pass a profile
#'   configuration here for authority-calibrated weights and bands.
#'
#' @return A list of class `"sci_decomposition"` with:
#'   - `sci`: the reported SCI (rounded), after any band gate on the score's
#'     band (the numeric score itself is never gated)
#'   - `sci_exact`: the unrounded weighted sum the earned points reconcile to
#'   - `band`: the decision band, including any non-compensatory gate
#'   - `residual`: `sci - sci_exact`, the rounding residual
#'   - `pillars`: a tibble with `pillar`, `pillar_score` (0--1),
#'     `nominal_weight`, `effective_weight`, `weight_budget`, `earned`,
#'     `shortfall`, and `scored`
#'   - `indicators`: a tibble with `indicator_id`, `indicator_name`,
#'     `indicator_domain`, `indicator_score`, `effective_weight`,
#'     `n_in_pillar`, `earned`, and `shortfall`, ordered by `earned` descending
#'
#' @seealso [sci_explain()] for loss-ranked contributions under nominal
#'   weights, [indicator_contributions()] for remediation headroom, and
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
#' dec <- sci_decompose(ev)
#' dec
#' dec$pillars
#'
#' @export
sci_decompose <- function(evidence, config = sci_config_default()) {
  r4subcore::validate_evidence(evidence)
  if (!inherits(config, "sci_config")) {
    cli::cli_abort("{.arg config} must be an {.cls sci_config}.")
  }

  ind <- compute_indicator_scores(evidence)
  ps  <- compute_pillar_scores(evidence, config = config)
  res <- compute_sci(ps, config = config)

  # Effective (renormalised) weights are the single source of truth: reuse the
  # exact vector compute_sci() applied so the earned points reconcile to the SCI.
  eff <- res$weights_used
  eff_for <- function(p) {
    w <- unname(eff[p])
    if (length(w) == 0L || is.na(w)) 0 else w
  }

  # Scored-indicator count per pillar (matches how the pillar mean is formed).
  scored_ind <- ind[!is.na(ind$indicator_score), , drop = FALSE]
  k_by_pillar <- table(scored_ind$indicator_domain)

  # Pillar-level decomposition ---------------------------------------------
  prows <- lapply(seq_len(nrow(ps)), function(i) {
    p       <- ps$pillar[i]
    score   <- ps$pillar_score[i]
    nominal <- ps$weight[i]
    ew      <- eff_for(p)
    scored  <- !is.na(score)

    # earned, shortfall, and budget are stored unrounded so they reconcile
    # exactly to the SCI; tibble printing abbreviates them for display.
    budget    <- ew * 100
    earned    <- if (scored) ew * score * 100 else NA_real_
    shortfall <- if (scored) ew * (1 - score) * 100 else NA_real_

    tibble::tibble(
      pillar           = p,
      pillar_score     = score,
      nominal_weight   = round(nominal, 4),
      effective_weight = round(ew, 4),
      weight_budget    = budget,
      earned           = earned,
      shortfall        = shortfall,
      scored           = scored
    )
  })
  pillars <- dplyr::bind_rows(prows)

  # Indicator-level decomposition ------------------------------------------
  irows <- lapply(seq_len(nrow(ind)), function(i) {
    domain <- ind$indicator_domain[i]
    score  <- ind$indicator_score[i]
    ew     <- eff_for(domain)
    k      <- as.integer(k_by_pillar[domain])
    if (is.na(k) || k == 0L) k <- NA_integer_

    if (is.na(score) || is.na(k)) {
      share     <- NA_real_
      earned    <- NA_real_
      shortfall <- NA_real_
    } else {
      share     <- ew / k
      earned    <- share * score * 100
      shortfall <- share * (1 - score) * 100
    }

    tibble::tibble(
      indicator_id     = ind$indicator_id[i],
      indicator_name   = ind$indicator_name[i],
      indicator_domain = domain,
      indicator_score  = score,
      effective_weight = round(ew, 4),
      n_in_pillar      = k,
      earned           = earned,
      shortfall        = shortfall
    )
  })
  indicators <- dplyr::bind_rows(irows)
  indicators <- indicators[order(-indicators$earned, na.last = TRUE), , drop = FALSE]

  # Exact (unrounded) reconciliation target.
  valid <- !is.na(ps$pillar_score)
  sci_exact <- if (!any(valid)) {
    NA_real_
  } else {
    sum(vapply(which(valid), function(i) {
      eff_for(ps$pillar[i]) * ps$pillar_score[i] * 100
    }, numeric(1)))
  }

  residual <- if (is.na(res$SCI) || is.na(sci_exact)) {
    NA_real_
  } else {
    round(res$SCI - sci_exact, 6)
  }

  structure(
    list(
      sci        = res$SCI,
      sci_exact  = if (is.na(sci_exact)) NA_real_ else round(sci_exact, 6),
      band       = res$band,
      residual   = residual,
      pillars    = pillars,
      indicators = tibble::as_tibble(indicators)
    ),
    class = "sci_decomposition"
  )
}


#' Print an SCI Decomposition
#' @param x An `sci_decomposition` object.
#' @param n_top Number of top earning indicators to list (default `5`).
#' @param ... Ignored.
#' @export
print.sci_decomposition <- function(x, n_top = 5L, ...) {
  cli::cli_h2("SCI decomposition")
  cli::cli_alert_info("SCI {.val {x$sci}} / 100   Band {.val {x$band}}")

  cli::cli_h3("Pillars (earned / budget)")
  pl <- x$pillars
  for (i in seq_len(nrow(pl))) {
    if (isTRUE(pl$scored[i])) {
      cli::cli_li(
        "{pl$pillar[i]}: {.val {round(pl$earned[i], 2)}} earned, {.val {round(pl$shortfall[i], 2)}} short (of {round(pl$weight_budget[i], 2)})"
      )
    } else {
      cli::cli_li("{pl$pillar[i]}: not scored (weight redistributed)")
    }
  }

  cli::cli_h3("Top earning indicators")
  top <- utils::head(x$indicators[!is.na(x$indicators$earned), , drop = FALSE],
                     n_top)
  if (nrow(top) == 0L) {
    cli::cli_alert_info("No scored indicators to report.")
  } else {
    for (i in seq_len(nrow(top))) {
      cli::cli_li(
        "{top$indicator_id[i]} ({top$indicator_domain[i]}): {.val {round(top$earned[i], 2)}} SCI points"
      )
    }
  }
  invisible(x)
}
