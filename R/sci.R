#' Compute Submission Confidence Index (SCI)
#'
#' Computes the SCI from pillar scores as a weighted sum scaled to 0--100,
#' with decision band classification.
#'
#' @details
#' The SCI is computed as:
#'
#' `SCI = round(sum(pillar_score * weight) * 100, 1)`
#'
#' Pillars with `NA` scores are excluded from both the numerator and the
#' weight normalization denominator.
#'
#' @param pillar_scores A tibble from [compute_pillar_scores()] with columns
#'   `pillar`, `pillar_score`, `weight`.
#' @param config An `sci_config` from [sci_config_default()].
#' @param n_critical Number of open critical findings. When `NULL` (the
#'   default), it is read from the `n_critical` attribute that
#'   [compute_pillar_scores()] attaches, so the gate applies automatically
#'   through the standard pipeline. Pass a value to override.
#'
#' @details
#' The score is a weighted average, which on its own lets a strong pillar offset
#' a weak one. The gate in `config$gate` adds a non-compensatory rule on top: an
#' open critical finding caps the reported band (by default at `conditional`), so
#' a critical failure cannot be washed out by good scores elsewhere. The numeric
#' SCI is left untouched; only the band is capped, and the reason is recorded.
#'
#' @return A list of class `"sci_result"` with:
#'   - `SCI`: numeric 0--100
#'   - `band`: character band classification, after any gate cap
#'   - `pillar_scores`: the input pillar scores tibble
#'   - `weights_used`: named numeric vector of effective weights
#'   - `n_critical`: number of open critical findings considered
#'   - `gated`: `TRUE` when a critical finding capped the band
#'   - `gate_reason`: a short explanation when `gated`, otherwise `NA`
#'
#' @examples
#' \dontrun{
#' ps <- compute_pillar_scores(evidence)
#' result <- compute_sci(ps)
#' result$SCI
#' result$band
#' }
#'
#' @export
compute_sci <- function(pillar_scores, config = sci_config_default(),
                        n_critical = NULL) {
  if (!is.data.frame(pillar_scores)) {
    cli::cli_abort("{.arg pillar_scores} must be a data.frame.")
  }

  if (is.null(n_critical)) {
    n_critical <- attr(pillar_scores, "n_critical")
    if (is.null(n_critical)) n_critical <- 0L
  }
  n_critical <- as.integer(n_critical)

  required_cols <- c("pillar", "pillar_score", "weight")
  missing_cols <- setdiff(required_cols, names(pillar_scores))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "{.arg pillar_scores} is missing column(s): {.val {missing_cols}}."
    )
  }

  scores  <- pillar_scores$pillar_score
  weights <- pillar_scores$weight

  # Handle NA pillars: exclude from computation and renormalize weights
  valid <- !is.na(scores)
  if (!any(valid)) {
    sci_value <- NA_real_
    band <- "unclassified"
    effective_weights <- stats::setNames(rep(NA_real_, length(weights)),
                                         pillar_scores$pillar)
  } else {
    w <- weights[valid]
    s <- scores[valid]

    # Renormalize weights to sum to 1 among valid pillars
    w_sum <- sum(w)
    if (w_sum == 0) {
      cli::cli_abort("All valid pillar weights are zero.")
    }
    w_norm <- w / w_sum

    sci_raw <- sum(s * w_norm)
    sci_value <- round(sci_raw * 100, 1)

    # Clamp to 0-100
    sci_value <- max(0, min(100, sci_value))

    band <- classify_band(sci_value, bands = config$bands)

    effective_weights <- stats::setNames(
      ifelse(valid, weights / w_sum, NA_real_),
      pillar_scores$pillar
    )
  }

  # Non-compensatory gate: an open critical finding caps the band regardless of
  # the numeric score. The score itself is left unchanged.
  gated <- FALSE
  gate_reason <- NA_character_
  gate <- config$gate
  if (!is.null(gate) && isTRUE(gate$critical_caps_band) && n_critical > 0L &&
      band %in% names(config$bands)) {
    capped <- cap_band(band, gate$critical_cap, names(config$bands))
    if (!identical(capped, band)) {
      gated <- TRUE
      gate_reason <- sprintf(
        "%d open critical finding%s; band capped at %s.",
        n_critical, if (n_critical == 1L) "" else "s", gate$critical_cap
      )
      band <- capped
    }
  }

  structure(
    list(
      SCI           = sci_value,
      band          = band,
      pillar_scores = pillar_scores,
      weights_used  = effective_weights,
      n_critical    = n_critical,
      gated         = gated,
      gate_reason   = gate_reason
    ),
    class = "sci_result"
  )
}

# Cap a band at `cap`: return whichever of `band` and `cap` is worse, using the
# best-to-worst order in `band_order`. Internal.
cap_band <- function(band, cap, band_order) {
  bi <- match(band, band_order)
  ci <- match(cap, band_order)
  if (is.na(bi) || is.na(ci)) return(band)
  band_order[max(bi, ci)]
}


#' Print SCI Result
#' @param x An `sci_result` object.
#' @param ... Ignored.
#' @export
print.sci_result <- function(x, ...) {
  cli::cli_alert_info("Submission Confidence Index: {.val {x$SCI}}")
  cli::cli_alert_info("Decision Band: {.val {x$band}}")
  if (isTRUE(x$gated)) {
    cli::cli_alert_warning("Band capped: {x$gate_reason}")
  }

  ps <- x$pillar_scores
  for (i in seq_len(nrow(ps))) {
    score_str <- if (is.na(ps$pillar_score[i])) "N/A" else
      round(ps$pillar_score[i] * 100, 1)
    cli::cli_alert_info(
      "  {ps$pillar[i]}: {score_str} (weight: {round(ps$weight[i], 2)})"
    )
  }
  invisible(x)
}
