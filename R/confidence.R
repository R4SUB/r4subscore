#' Bootstrap Confidence Interval for the SCI
#'
#' Turns the SCI point estimate into an interval by resampling the underlying
#' evidence. This answers the difference between "solidly conditional" and
#' "borderline ready depending on measurement noise" that a single number
#' cannot.
#'
#' @details
#' Two resampling schemes are offered:
#'
#' - `by = "indicator"` (default) resamples the indicator scores within each
#'   pillar with replacement, then recomputes the pillar mean and the SCI. This
#'   treats the indicators actually measured in a pillar as a sample from the
#'   checks that could have been run, and is stable when the raw evidence is
#'   uneven across pillars.
#' - `by = "evidence"` resamples the raw evidence rows within each pillar with
#'   replacement and runs the full scoring pipeline on each replicate. This
#'   propagates row-level noise but can drop whole indicators from a replicate.
#'
#' A pillar with a single indicator carries no resampling variance, so a narrow
#' interval there is expected rather than a sign of high confidence.
#'
#' @param evidence A validated evidence data.frame (from `r4subcore`).
#' @param config An `sci_config` from [sci_config_default()].
#' @param level Confidence level, a number in `(0, 1)`. Default `0.95`.
#' @param n_boot Number of bootstrap replicates. Default `2000`.
#' @param by Resampling scheme, `"indicator"` or `"evidence"`.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A list of class `"sci_ci"` with `SCI` (point estimate), `lower`,
#'   `upper`, `level`, `n_boot`, `by`, `band`, `lower_band`, `upper_band`, and
#'   `replicates` (the bootstrap SCI vector).
#'
#' @examples
#' \dontrun{
#' ci <- sci_confidence_interval(evidence, seed = 1)
#' ci
#' }
#'
#' @export
sci_confidence_interval <- function(evidence, config = sci_config_default(),
                                    level = 0.95, n_boot = 2000,
                                    by = c("indicator", "evidence"),
                                    seed = NULL) {
  r4subcore::validate_evidence(evidence)
  by <- match.arg(by)

  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    cli::cli_abort("{.arg level} must be a single number in (0, 1).")
  }
  if (!is.numeric(n_boot) || length(n_boot) != 1L || n_boot < 1) {
    cli::cli_abort("{.arg n_boot} must be a positive integer.")
  }
  n_boot <- as.integer(n_boot)

  if (!is.null(seed)) set.seed(seed)

  point <- compute_sci(compute_pillar_scores(evidence, config = config),
                       config = config)

  reps <- if (by == "indicator") {
    boot_by_indicator(evidence, config, n_boot)
  } else {
    boot_by_evidence(evidence, config, n_boot)
  }

  reps <- reps[!is.na(reps)]
  if (length(reps) == 0L) {
    cli::cli_abort("All bootstrap replicates produced an undefined SCI.")
  }

  alpha <- 1 - level
  bounds <- stats::quantile(reps, probs = c(alpha / 2, 1 - alpha / 2),
                            names = FALSE, type = 7)
  lower <- round(bounds[1], 1)
  upper <- round(bounds[2], 1)

  structure(
    list(
      SCI        = point$SCI,
      lower      = lower,
      upper      = upper,
      level      = level,
      n_boot     = n_boot,
      by         = by,
      band       = point$band,
      lower_band = classify_band(lower, bands = config$bands),
      upper_band = classify_band(upper, bands = config$bands),
      replicates = reps
    ),
    class = "sci_ci"
  )
}


#' Format an SCI Confidence Interval
#'
#' Produces the compact `SCI = 82.0 [76.0, 88.0] (95% CI)` form for reports.
#'
#' @param x An `sci_ci` object from [sci_confidence_interval()].
#' @param ... Ignored.
#'
#' @return A single character string.
#'
#' @examples
#' \dontrun{
#' format_sci_ci(sci_confidence_interval(evidence, seed = 1))
#' }
#'
#' @export
format_sci_ci <- function(x, ...) {
  if (!inherits(x, "sci_ci")) {
    cli::cli_abort("{.arg x} must be an {.cls sci_ci} object.")
  }
  sprintf(
    "SCI = %s [%s, %s] (%g%% CI)",
    format(x$SCI, nsmall = 1), format(x$lower, nsmall = 1),
    format(x$upper, nsmall = 1), round(x$level * 100)
  )
}


#' Print an SCI Confidence Interval
#' @param x An `sci_ci` object.
#' @param ... Ignored.
#' @export
print.sci_ci <- function(x, ...) {
  cli::cli_alert_info(format_sci_ci(x))
  cli::cli_alert_info("Point band: {.val {x$band}}")
  if (x$lower_band != x$upper_band) {
    cli::cli_alert_warning(
      "Interval spans bands {.val {x$lower_band}} to {.val {x$upper_band}}."
    )
  }
  invisible(x)
}


# Resample indicator scores within each pillar and recompute the SCI.
boot_by_indicator <- function(evidence, config, n_boot) {
  ind <- compute_indicator_scores(evidence)
  pillars <- names(config$pillar_weights)

  # Pre-split the non-NA indicator scores by pillar.
  by_pillar <- lapply(pillars, function(p) {
    s <- ind$indicator_score[ind$indicator_domain == p]
    s[!is.na(s)]
  })
  names(by_pillar) <- pillars

  vapply(seq_len(n_boot), function(b) {
    ps <- lapply(pillars, function(p) {
      s <- by_pillar[[p]]
      if (length(s) == 0L) {
        score <- NA_real_
      } else {
        # Index sampling avoids sample()'s length-1 "sample from 1:x" trap.
        score <- mean(s[sample.int(length(s), replace = TRUE)])
      }
      tibble::tibble(
        pillar = p, pillar_score = score, n_indicators = length(s),
        weight = unname(config$pillar_weights[p])
      )
    })
    compute_sci(dplyr::bind_rows(ps), config = config)$SCI
  }, numeric(1))
}


# Resample raw evidence rows within each pillar and rerun the full pipeline.
boot_by_evidence <- function(evidence, config, n_boot) {
  domain <- evidence$indicator_domain
  idx_by_pillar <- split(seq_len(nrow(evidence)), domain)

  vapply(seq_len(n_boot), function(b) {
    picks <- unlist(lapply(idx_by_pillar, function(idx) {
      idx[sample.int(length(idx), replace = TRUE)]
    }), use.names = FALSE)
    resampled <- evidence[picks, , drop = FALSE]
    ps <- compute_pillar_scores(resampled, config = config)
    compute_sci(ps, config = config)$SCI
  }, numeric(1))
}
