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
#'
#' @return A list of class `"sci_result"` with:
#'   - `SCI`: numeric 0--100
#'   - `band`: character band classification
#'   - `pillar_scores`: the input pillar scores tibble
#'   - `weights_used`: named numeric vector of effective weights
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
compute_sci <- function(pillar_scores, config = sci_config_default()) {
  if (!is.data.frame(pillar_scores)) {
    cli::cli_abort("{.arg pillar_scores} must be a data.frame.")
  }

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

  structure(
    list(
      SCI           = sci_value,
      band          = band,
      pillar_scores = pillar_scores,
      weights_used  = effective_weights
    ),
    class = "sci_result"
  )
}


#' Print SCI Result
#' @param x An `sci_result` object.
#' @param ... Ignored.
#' @export
print.sci_result <- function(x, ...) {
  cli::cli_alert_info("Submission Confidence Index: {.val {x$SCI}}")
  cli::cli_alert_info("Decision Band: {.val {x$band}}")

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
