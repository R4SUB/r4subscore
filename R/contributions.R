#' Per-Indicator Contribution Breakdown
#'
#' Breaks a pillar score down into the contribution of each indicator, ranks
#' indicators by how much score could be recovered by fixing them, and attaches
#' a short remediation hint for each one. Use this to answer "what do we fix
#' first?" once [compute_sci()] has answered the go/no-go question.
#'
#' @details
#' A pillar score is the mean of its indicator scores (missing indicators are
#' dropped), so within a pillar with `k` scored indicators each indicator
#' contributes `indicator_score / k` to the pillar score. Bringing one indicator
#' up to a perfect 1 would raise the pillar score by `(1 - indicator_score) / k`;
#' that quantity is the `pillar_headroom`. Multiplying it by the pillar weight
#' gives `sci_headroom`, an approximate cap on how many SCI points a single fix
#' can buy. Indicators are returned in `sci_headroom` order so the highest-value
#' fixes come first.
#'
#' The `sci_headroom` figure uses the nominal pillar weight from `config`. When
#' a whole pillar is missing its weight is redistributed across the remaining
#' pillars, so the exact SCI movement from a fix can differ slightly. Use
#' [sci_what_if()] for an exact recomputation.
#'
#' @param evidence A validated evidence data.frame (from `r4subcore`).
#' @param config An `sci_config` from [sci_config_default()].
#'
#' @return A tibble sorted by `sci_headroom` descending, with columns:
#'   `indicator_id`, `indicator_name`, `indicator_domain`, `indicator_score`,
#'   `n_in_pillar`, `pillar_contribution`, `pillar_headroom`, `sci_headroom`,
#'   `worst_result`, `suggestion`.
#'
#' @examples
#' \dontrun{
#' contrib <- indicator_contributions(evidence)
#' head(contrib)
#' }
#'
#' @export
indicator_contributions <- function(evidence, config = sci_config_default()) {
  r4subcore::validate_evidence(evidence)

  ind <- compute_indicator_scores(evidence)
  worst <- worst_result_by_indicator(evidence)

  # Count scored indicators per pillar (matches how the pillar mean is formed).
  scored <- ind[!is.na(ind$indicator_score), , drop = FALSE]
  n_in_pillar <- table(scored$indicator_domain)

  rows <- lapply(seq_len(nrow(ind)), function(i) {
    domain <- ind$indicator_domain[i]
    score  <- ind$indicator_score[i]
    weight <- unname(config$pillar_weights[domain])
    if (is.na(weight)) weight <- 0

    k <- as.integer(n_in_pillar[domain])
    if (is.na(k) || k == 0L) k <- NA_integer_

    if (is.na(score) || is.na(k)) {
      pillar_contribution <- NA_real_
      pillar_headroom     <- NA_real_
      sci_headroom        <- NA_real_
    } else {
      pillar_contribution <- score / k
      pillar_headroom     <- (1 - score) / k
      sci_headroom        <- round(pillar_headroom * weight * 100, 2)
    }

    wr <- worst[[ind$indicator_id[i]]]
    result_label <- if (is.null(wr)) NA_character_ else wr$result

    tibble::tibble(
      indicator_id        = ind$indicator_id[i],
      indicator_name      = ind$indicator_name[i],
      indicator_domain    = domain,
      indicator_score     = score,
      n_in_pillar         = k,
      pillar_contribution = round(pillar_contribution, 4),
      pillar_headroom     = round(pillar_headroom, 4),
      sci_headroom        = sci_headroom,
      worst_result        = result_label,
      suggestion          = remediation_hint(
        ind$indicator_id[i], ind$indicator_name[i], domain, wr
      )
    )
  })

  out <- dplyr::bind_rows(rows)
  out[order(-out$sci_headroom, na.last = TRUE), ]
}


#' What-If Analysis for a Single Indicator
#'
#' Recomputes the pillar score and the SCI as if one indicator had a different
#' score, leaving every other indicator untouched. This gives the exact SCI
#' movement a remediation would produce, including the effect of weight
#' redistribution when pillars are missing.
#'
#' @param evidence A validated evidence data.frame.
#' @param indicator_id The `indicator_id` to adjust.
#' @param new_score The hypothetical indicator score, a number in `[0, 1]`.
#' @param config An `sci_config` from [sci_config_default()].
#'
#' @return A list with `indicator_id`, `indicator_domain`, `old_indicator_score`,
#'   `new_indicator_score`, `old_pillar_score`, `new_pillar_score`, `old_SCI`,
#'   `new_SCI`, `delta_SCI`, `old_band`, and `new_band`.
#'
#' @examples
#' \dontrun{
#' sci_what_if(evidence, "Q1", new_score = 1)
#' }
#'
#' @export
sci_what_if <- function(evidence, indicator_id, new_score,
                        config = sci_config_default()) {
  r4subcore::validate_evidence(evidence)

  if (!is.numeric(new_score) || length(new_score) != 1L ||
      is.na(new_score) || new_score < 0 || new_score > 1) {
    cli::cli_abort("{.arg new_score} must be a single number in [0, 1].")
  }

  ind <- compute_indicator_scores(evidence)
  match_idx <- which(ind$indicator_id == indicator_id)
  if (length(match_idx) == 0L) {
    cli::cli_abort("Indicator {.val {indicator_id}} not found in evidence.")
  }
  if (length(match_idx) > 1L) {
    cli::cli_abort(
      "Indicator {.val {indicator_id}} maps to more than one domain; \\
       cannot resolve a single what-if."
    )
  }

  domain  <- ind$indicator_domain[match_idx]
  old_ind <- ind$indicator_score[match_idx]

  ps_old  <- pillars_from_indicators(ind, config)
  sci_old <- compute_sci(ps_old, config = config)

  ind_new <- ind
  ind_new$indicator_score[match_idx] <- new_score
  ps_new  <- pillars_from_indicators(ind_new, config)
  sci_new <- compute_sci(ps_new, config = config)

  old_pillar <- ps_old$pillar_score[ps_old$pillar == domain]
  new_pillar <- ps_new$pillar_score[ps_new$pillar == domain]

  list(
    indicator_id        = indicator_id,
    indicator_domain    = domain,
    old_indicator_score = old_ind,
    new_indicator_score = new_score,
    old_pillar_score    = old_pillar,
    new_pillar_score    = new_pillar,
    old_SCI             = sci_old$SCI,
    new_SCI             = sci_new$SCI,
    delta_SCI           = round(sci_new$SCI - sci_old$SCI, 1),
    old_band            = sci_old$band,
    new_band            = sci_new$band
  )
}


# Rebuild the pillar_scores tibble from a table of indicator scores, using the
# same aggregation rule as compute_pillar_scores (mean of non-NA indicator
# scores per pillar). Kept internal so what-if analysis stays consistent with
# the main pipeline.
pillars_from_indicators <- function(ind, config) {
  all_pillars <- names(config$pillar_weights)

  rows <- lapply(all_pillars, function(p) {
    sub <- ind[ind$indicator_domain == p, , drop = FALSE]
    valid <- sub$indicator_score[!is.na(sub$indicator_score)]
    score <- if (length(valid) == 0L) NA_real_ else mean(valid)

    tibble::tibble(
      pillar       = p,
      pillar_score = score,
      n_indicators = nrow(sub),
      weight       = unname(config$pillar_weights[p])
    )
  })

  dplyr::bind_rows(rows)
}


# For each indicator_id, find the evidence row with the lowest result score
# (fail before warn before pass). Returns a named list keyed by indicator_id,
# each element a one-row list with result, severity, message.
worst_result_by_indicator <- function(evidence) {
  scores <- r4subcore::result_to_score(evidence$result)
  # Treat NA result scores as neutral so they never win the "worst" slot.
  scores[is.na(scores)] <- 1

  ids <- unique(evidence$indicator_id)
  out <- lapply(ids, function(id) {
    idx <- which(evidence$indicator_id == id)
    pick <- idx[which.min(scores[idx])]
    list(
      result   = evidence$result[pick],
      severity = evidence$severity[pick],
      message  = evidence$message[pick]
    )
  })
  names(out) <- ids
  out
}


# Build a short, human-readable remediation hint for one indicator. Prefers the
# check's own message when the worst evidence row failed or warned; otherwise
# falls back to a domain-based template.
remediation_hint <- function(indicator_id, indicator_name, domain, worst) {
  if (!is.null(worst) && worst$result %in% c("fail", "warn") &&
      !is.na(worst$message) && nzchar(worst$message)) {
    return(paste0(indicator_id, ": ", worst$message))
  }

  templates <- c(
    quality   = "review data quality checks and close open findings",
    trace     = "complete the traceability chain for this indicator",
    risk      = "add or strengthen mitigations for the flagged risk",
    usability = "improve the reviewer-facing documentation"
  )
  action <- templates[[domain]]
  if (is.null(action)) action <- "review the underlying checks"

  paste0(indicator_id, " (", indicator_name, "): ", action)
}
