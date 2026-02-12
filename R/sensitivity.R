#' SCI Sensitivity Analysis
#'
#' Evaluates the stability of the Submission Confidence Index under
#' alternative pillar weight scenarios.
#'
#' @param evidence A validated evidence data.frame.
#' @param weight_grid A data.frame where each row is a weight scenario.
#'   Column names must match pillar names (`quality`, `trace`, `risk`,
#'   `usability`). Each row must sum to 1.
#'
#' @return A tibble with one row per scenario, containing:
#'   `scenario` (row number), the weight columns, `SCI`, and `band`.
#'
#' @examples
#' \dontrun{
#' grid <- data.frame(
#'   quality   = c(0.4, 0.3, 0.25),
#'   trace     = c(0.2, 0.3, 0.25),
#'   risk      = c(0.3, 0.2, 0.25),
#'   usability = c(0.1, 0.2, 0.25)
#' )
#' sci_sensitivity_analysis(evidence, grid)
#' }
#'
#' @export
sci_sensitivity_analysis <- function(evidence, weight_grid) {
  r4subcore::validate_evidence(evidence)

  if (!is.data.frame(weight_grid) || nrow(weight_grid) == 0L) {
    cli::cli_abort("{.arg weight_grid} must be a non-empty data.frame.")
  }

  # Validate each row sums to ~1
  row_sums <- rowSums(weight_grid[, intersect(names(weight_grid),
    c("quality", "trace", "risk", "usability")), drop = FALSE])
  bad_rows <- which(abs(row_sums - 1.0) > 1e-6)
  if (length(bad_rows) > 0L) {
    cli::cli_abort(
      "Weight grid row(s) {.val {bad_rows}} do not sum to 1."
    )
  }

  results <- lapply(seq_len(nrow(weight_grid)), function(i) {
    w <- as.numeric(weight_grid[i, ])
    names(w) <- names(weight_grid)

    cfg <- sci_config_default(pillar_weights = w)
    ps <- compute_pillar_scores(evidence, config = cfg)
    sci_res <- compute_sci(ps, config = cfg)

    row <- weight_grid[i, , drop = FALSE]
    row$scenario <- i
    row$SCI <- sci_res$SCI
    row$band <- sci_res$band
    row
  })

  result <- dplyr::bind_rows(results)
  # Reorder: scenario first
  cols <- c("scenario", setdiff(names(result), c("scenario", "SCI", "band")),
            "SCI", "band")
  tibble::as_tibble(result[, cols])
}
