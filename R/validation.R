#' SCI Degradation Curve
#'
#' Progressively degrades an evidence table and records how the Submission
#' Confidence Index responds. The curve is the operational form of construct
#' validity: a well-behaved index must fall (never rise) as passing checks are
#' turned into failures.
#'
#' @details
#' Starting from the supplied evidence, the function repeatedly converts a
#' growing number of currently-passing checks to `"fail"` and recomputes the
#' SCI at each step. Passing checks are degraded in order of how much they
#' contribute to the baseline SCI, largest contribution first, so the curve
#' shows the steepest descent the evidence allows and is reproducible. Checks
#' that already `warn` or `fail` are left untouched.
#'
#' A passing check contributes `1 - severity_weight` to its indicator score, so
#' ordering by contribution degrades the lower-severity passing checks earliest.
#' This is a property of the scoring model, not of this function.
#'
#' Step 0 is the unmodified evidence. The final step degrades every passing
#' check. When the number of passing checks is smaller than `n_steps`, the
#' number of steps is reduced to the number of passing checks so that each step
#' degrades at least one additional check.
#'
#' @param evidence A validated evidence data.frame (from `r4subcore`).
#' @param n_steps Integer number of degradation steps after the baseline.
#'   Must be at least 1.
#' @param config An `sci_config` from [sci_config_default()].
#'
#' @return A tibble with one row per step and columns:
#'   - `step`: integer step index, starting at 0
#'   - `n_degraded`: number of passing checks turned into failures
#'   - `frac_degraded`: `n_degraded` divided by the baseline passing count
#'   - `SCI`: the Submission Confidence Index at that step
#'   - `band`: the decision band at that step
#'   - `delta`: change in SCI from the previous step (`NA` at step 0)
#'
#' @seealso [sci_monotone_check()] to confirm a curve never rises.
#'
#' @examples
#' \dontrun{
#' curve <- sci_degradation_curve(evidence, n_steps = 8)
#' curve
#' }
#'
#' @export
sci_degradation_curve <- function(evidence, n_steps = 10L,
                                  config = sci_config_default()) {
  r4subcore::validate_evidence(evidence)

  n_steps <- as.integer(n_steps)
  if (length(n_steps) != 1L || is.na(n_steps) || n_steps < 1L) {
    cli::cli_abort("{.arg n_steps} must be a single integer of at least 1.")
  }

  is_pass <- evidence$result == "pass"
  pass_idx <- which(is_pass)
  n_pass <- length(pass_idx)

  baseline <- compute_sci(compute_pillar_scores(evidence, config), config)

  if (n_pass == 0L) {
    return(tibble::tibble(
      step          = 0L,
      n_degraded    = 0L,
      frac_degraded = 0,
      SCI           = baseline$SCI,
      band          = baseline$band,
      delta         = NA_real_
    ))
  }

  # Degrade the highest-contributing passing checks first, deterministically.
  # A passing check contributes (1 - severity_weight) to its indicator score.
  sev_weight <- r4subcore::severity_to_weight(evidence$severity[pass_idx])
  contribution <- 1 - sev_weight
  ordered_idx <- pass_idx[order(-contribution, pass_idx)]

  # One step per degraded check when passes are fewer than the requested steps.
  effective_steps <- min(n_steps, n_pass)
  degrade_counts <- unique(round(seq(0, n_pass, length.out = effective_steps + 1L)))

  rows <- lapply(seq_along(degrade_counts), function(i) {
    k <- degrade_counts[i]
    ev_k <- evidence
    if (k > 0L) {
      ev_k$result[ordered_idx[seq_len(k)]] <- "fail"
    }
    res <- compute_sci(compute_pillar_scores(ev_k, config), config)
    tibble::tibble(
      step          = as.integer(i - 1L),
      n_degraded    = as.integer(k),
      frac_degraded = k / n_pass,
      SCI           = res$SCI,
      band          = res$band
    )
  })

  out <- dplyr::bind_rows(rows)
  out$delta <- c(NA_real_, diff(out$SCI))
  out
}


#' Check that an SCI Degradation Curve Never Rises
#'
#' Verifies that a degradation curve is monotone non-increasing: injecting more
#' failures must never improve the Submission Confidence Index. A rising step
#' would signal a defect in the scoring model.
#'
#' @param curve A tibble from [sci_degradation_curve()], or any data.frame with
#'   `step` and `SCI` columns.
#' @param tol Numeric tolerance for treating a small rise as noise. Defaults to
#'   `1e-8`.
#'
#' @return A list with:
#'   - `monotone`: `TRUE` if no step rises beyond `tol`
#'   - `max_increase`: the largest step-to-step increase (0 if none)
#'   - `violations`: a tibble of the steps that rose, empty if none
#'
#' @examples
#' \dontrun{
#' curve <- sci_degradation_curve(evidence)
#' sci_monotone_check(curve)$monotone
#' }
#'
#' @export
sci_monotone_check <- function(curve, tol = 1e-8) {
  if (!is.data.frame(curve) ||
      !all(c("step", "SCI") %in% names(curve))) {
    cli::cli_abort(
      "{.arg curve} must be a data.frame with {.field step} and {.field SCI}."
    )
  }

  sci <- curve$SCI
  step_delta <- diff(sci)
  rose <- which(step_delta > tol)

  max_increase <- if (length(step_delta) == 0L) 0 else max(0, max(step_delta))

  violations <- tibble::tibble(
    from_step = curve$step[rose],
    to_step   = curve$step[rose + 1L],
    increase  = step_delta[rose]
  )

  list(
    monotone     = length(rose) == 0L,
    max_increase = max_increase,
    violations   = violations
  )
}


#' Count Severity-Weighted Conformance Findings
#'
#' Summarises an evidence table into an independent count of unresolved
#' conformance findings, weighted by severity. This is the kind of figure a
#' reviewer reads off a validation report, and it is computed without the SCI
#' pillar weights, so it can serve as an external reference when assessing the
#' concurrent validity of the SCI.
#'
#' @details
#' Each non-passing check contributes its severity weight
#' (`r4subcore::severity_to_weight()`) to the total. A `warn` contributes half
#' as much as a `fail` of the same severity. Passing checks contribute nothing.
#'
#' @param evidence A validated evidence data.frame (from `r4subcore`).
#'
#' @return A single non-negative numeric value. Higher means more, or more
#'   severe, unresolved findings.
#'
#' @examples
#' \dontrun{
#' conformance_findings(evidence)
#' }
#'
#' @export
conformance_findings <- function(evidence) {
  r4subcore::validate_evidence(evidence)

  sev_weight <- r4subcore::severity_to_weight(evidence$severity)
  result_penalty <- ifelse(
    evidence$result == "fail", 1,
    ifelse(evidence$result == "warn", 0.5, 0)
  )

  sum(sev_weight * result_penalty, na.rm = TRUE)
}
