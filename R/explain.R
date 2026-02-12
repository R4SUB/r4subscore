#' Explain SCI Contributors
#'
#' Identifies which indicators contribute most to SCI loss and provides
#' a breakdown of pillar contributions.
#'
#' @details
#' For each indicator, the contribution to SCI loss is:
#'
#' `loss = pillar_weight * (1 - indicator_score) / n_indicators_in_pillar`
#'
#' This gives a sense of how much each indicator drags the SCI down.
#' Results are sorted by `loss` descending (worst contributors first).
#'
#' @param evidence A validated evidence data.frame.
#' @param config An `sci_config` from [sci_config_default()].
#'
#' @return A list with:
#'   - `indicator_contributions`: tibble of per-indicator loss contributions
#'   - `pillar_contributions`: tibble of per-pillar contributions to SCI
#'
#' @examples
#' \dontrun{
#' expl <- sci_explain(evidence)
#' expl$indicator_contributions
#' expl$pillar_contributions
#' }
#'
#' @export
sci_explain <- function(evidence, config = sci_config_default()) {
  ind_scores <- compute_indicator_scores(evidence)
  ps <- compute_pillar_scores(evidence, config = config)

  # Per-pillar: count indicators and get weight
  pillar_info <- ps[, c("pillar", "pillar_score", "n_indicators", "weight")]

  # Per-indicator: compute loss contribution
  ind_loss <- lapply(seq_len(nrow(ind_scores)), function(i) {
    domain <- ind_scores$indicator_domain[i]
    score  <- ind_scores$indicator_score[i]

    p_row <- pillar_info[pillar_info$pillar == domain, , drop = FALSE]
    if (nrow(p_row) == 0L || is.na(score)) {
      return(tibble::tibble(
        indicator_id     = ind_scores$indicator_id[i],
        indicator_name   = ind_scores$indicator_name[i],
        indicator_domain = domain,
        indicator_score  = score,
        loss             = NA_real_,
        pct_of_total_loss = NA_real_
      ))
    }

    w <- p_row$weight[1]
    n_ind <- p_row$n_indicators[1]

    # Loss: how much this indicator drags SCI down relative to a perfect score
    # Scaled to the 0-100 SCI space
    loss <- w * (1 - score) / max(n_ind, 1L) * 100

    tibble::tibble(
      indicator_id     = ind_scores$indicator_id[i],
      indicator_name   = ind_scores$indicator_name[i],
      indicator_domain = domain,
      indicator_score  = score,
      loss             = round(loss, 2),
      pct_of_total_loss = NA_real_  # filled below
    )
  })

  ind_contrib <- dplyr::bind_rows(ind_loss)

  # Compute % of total loss
  total_loss <- sum(ind_contrib$loss, na.rm = TRUE)
  if (total_loss > 0) {
    ind_contrib$pct_of_total_loss <- round(
      ind_contrib$loss / total_loss * 100, 1
    )
  }

  # Sort by loss descending
  ind_contrib <- ind_contrib[order(-ind_contrib$loss, na.last = TRUE), ]

  # Pillar contributions to SCI
  pillar_contrib <- ps
  pillar_contrib$contribution <- round(
    ifelse(is.na(ps$pillar_score), 0, ps$pillar_score * ps$weight) * 100, 1
  )
  pillar_contrib$loss <- round(
    ifelse(is.na(ps$pillar_score), ps$weight,
           ps$weight * (1 - ps$pillar_score)) * 100, 1
  )

  list(
    indicator_contributions = tibble::as_tibble(ind_contrib),
    pillar_contributions    = tibble::as_tibble(pillar_contrib)
  )
}
