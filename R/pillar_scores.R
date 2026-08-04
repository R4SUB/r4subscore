#' Compute Pillar Scores
#'
#' Aggregates indicator scores into pillar-level scores (one per domain).
#' Each pillar score is the mean of its indicator scores.
#'
#' @param evidence A validated evidence data.frame.
#' @param config An `sci_config` from [sci_config_default()].
#'
#' @return A tibble with columns: `pillar`, `pillar_score`, `n_indicators`,
#'   `weight`. The number of open critical findings (severity `critical`,
#'   result `fail`) is attached as an `n_critical` attribute, which
#'   [compute_sci()] reads to apply the non-compensatory gate.
#'
#' @examples
#' \dontrun{
#' ps <- compute_pillar_scores(evidence)
#' ps
#' }
#'
#' @export
compute_pillar_scores <- function(evidence, config = sci_config_default()) {
  ind_scores <- compute_indicator_scores(evidence)

  all_pillars <- names(config$pillar_weights)

  rows <- lapply(all_pillars, function(p) {
    sub <- ind_scores[ind_scores$indicator_domain == p, , drop = FALSE]
    if (nrow(sub) == 0L) {
      score <- NA_real_
      n_ind <- 0L
    } else {
      valid <- sub$indicator_score[!is.na(sub$indicator_score)]
      score <- if (length(valid) == 0L) NA_real_ else mean(valid)
      n_ind <- nrow(sub)
    }

    tibble::tibble(
      pillar       = p,
      pillar_score = score,
      n_indicators = as.integer(n_ind),
      weight       = unname(config$pillar_weights[p])
    )
  })

  ps <- dplyr::bind_rows(rows)

  # Carry the open critical-finding count so compute_sci() can gate the band.
  n_critical <- sum(
    evidence$severity == "critical" & evidence$result == "fail",
    na.rm = TRUE
  )
  attr(ps, "n_critical") <- as.integer(n_critical)
  ps
}
