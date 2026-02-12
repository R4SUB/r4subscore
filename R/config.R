#' Default SCI Configuration
#'
#' Returns a configuration list with default pillar weights, decision bands,
#' and scoring parameters for the Submission Confidence Index.
#'
#' @param pillar_weights Named numeric vector of weights for each pillar.
#'   Must sum to 1. Names must be a subset of
#'   `"quality"`, `"trace"`, `"risk"`, `"usability"`.
#' @param bands Named list of numeric length-2 vectors defining SCI band
#'   boundaries `c(lower, upper)`. Evaluated in order; first match wins.
#'
#' @return A list of class `"sci_config"` with elements:
#'   `pillar_weights`, `bands`.
#'
#' @examples
#' cfg <- sci_config_default()
#' cfg$pillar_weights
#' cfg$bands
#'
#' # Custom weights (must sum to 1)
#' sci_config_default(
#'   pillar_weights = c(quality = 0.40, trace = 0.20, risk = 0.30, usability = 0.10)
#' )
#'
#' @export
sci_config_default <- function(
    pillar_weights = c(quality = 0.35, trace = 0.25, risk = 0.25, usability = 0.15),
    bands = list(
      ready       = c(85, 100),
      minor_gaps  = c(70, 84),
      conditional = c(50, 69),
      high_risk   = c(0, 49)
    )
) {
  # Validate pillar_weights
  if (!is.numeric(pillar_weights) || is.null(names(pillar_weights))) {
    cli::cli_abort("{.arg pillar_weights} must be a named numeric vector.")
  }

  valid_pillars <- c("quality", "trace", "risk", "usability")
  bad_names <- setdiff(names(pillar_weights), valid_pillars)
  if (length(bad_names) > 0L) {
    cli::cli_abort("Invalid pillar name(s): {.val {bad_names}}. Must be one of {.val {valid_pillars}}.")
  }

  wt_sum <- sum(pillar_weights)
  if (abs(wt_sum - 1.0) > 1e-6) {
    cli::cli_abort("Pillar weights must sum to 1, got {.val {round(wt_sum, 4)}}.")
  }

  # Validate bands
  if (!is.list(bands)) {
    cli::cli_abort("{.arg bands} must be a named list of numeric pairs.")
  }
  for (nm in names(bands)) {
    b <- bands[[nm]]
    if (!is.numeric(b) || length(b) != 2L) {
      cli::cli_abort("Band {.val {nm}} must be a numeric vector of length 2.")
    }
  }

  structure(
    list(
      pillar_weights = pillar_weights,
      bands          = bands
    ),
    class = "sci_config"
  )
}


#' Classify SCI Value into Decision Band
#'
#' @param sci_value Numeric SCI score (0--100).
#' @param bands Named list of band boundaries from [sci_config_default()].
#'
#' @return Character band name.
#'
#' @examples
#' classify_band(92)
#' classify_band(55)
#'
#' @export
classify_band <- function(sci_value, bands = sci_config_default()$bands) {
  if (!is.numeric(sci_value) || length(sci_value) != 1L) {
    cli::cli_abort("{.arg sci_value} must be a single numeric value.")
  }

  for (nm in names(bands)) {
    rng <- bands[[nm]]
    if (sci_value >= rng[1] && sci_value <= rng[2]) {
      return(nm)
    }
  }

  "unclassified"
}
