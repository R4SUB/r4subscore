#' Compute Indicator-Level Scores
#'
#' Converts each indicator in an evidence table into a numeric score (0--1)
#' using severity-weighted result scoring.
#'
#' @details
#' For each evidence row:
#' - `result_score` = `r4subcore::result_to_score(result)` (pass=1, warn=0.5, fail=0)
#' - `severity_weight` = `r4subcore::severity_to_weight(severity)` (info=0, ..., critical=1)
#' - `weighted_score` = `result_score * (1 - severity_weight)`
#'
#' Rows are grouped by `indicator_id` and `indicator_domain`, and the
#' indicator score is the mean of `weighted_score` within each group.
#'
#' @param evidence A validated evidence data.frame (from `r4subcore`).
#'
#' @return A tibble with columns: `indicator_id`, `indicator_name`,
#'   `indicator_domain`, `n_evidence`, `indicator_score`.
#'
#' @examples
#' \dontrun{
#' scores <- compute_indicator_scores(evidence)
#' scores
#' }
#'
#' @export
compute_indicator_scores <- function(evidence) {
  r4subcore::validate_evidence(evidence)

  result_scores    <- r4subcore::result_to_score(evidence$result)
  severity_weights <- r4subcore::severity_to_weight(evidence$severity)

  # weighted_score: penalize more for higher severity

  # A "pass" at "critical" severity still gets 1 * (1 - 1) = 0
  # A "fail" at "info" severity gets 0 * (1 - 0) = 0
  # A "pass" at "info" severity gets 1 * (1 - 0) = 1
  weighted_scores <- result_scores * (1 - severity_weights)

  ev <- evidence
  ev$.ws <- weighted_scores

  # Group by indicator_id + indicator_domain
  grp_key <- paste0(ev$indicator_id, "\x01", ev$indicator_domain)
  groups <- split(seq_len(nrow(ev)), grp_key)

  rows <- lapply(names(groups), function(g) {
    idx <- groups[[g]]
    ws <- ev$.ws[idx]
    valid_ws <- ws[!is.na(ws)]
    score <- if (length(valid_ws) == 0L) NA_real_ else mean(valid_ws)

    tibble::tibble(
      indicator_id     = ev$indicator_id[idx[1]],
      indicator_name   = ev$indicator_name[idx[1]],
      indicator_domain = ev$indicator_domain[idx[1]],
      n_evidence       = length(idx),
      indicator_score  = score
    )
  })

  dplyr::bind_rows(rows)
}
