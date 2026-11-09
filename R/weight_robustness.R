#' Weight Robustness Diagnostic for the Decision Band
#'
#' Auto-perturbs the pillar weights around the current configuration and
#' reports the smallest weight change that flips the reported decision
#' `band`. It answers a single question: *is our band robust to the weights
#' we chose?*
#'
#' @details
#' This is a **robustness diagnostic**, not a calibration or validation of the
#' weights. It does not say which weights are correct, nor does it propose new
#' ones. It only measures how far the current weights can move before the
#' decision band would change, so you know whether the band is sitting safely
#' inside its weight neighbourhood or balanced on a knife edge.
#'
#' The scan reuses the existing [compute_sci()] and [classify_band()] exactly.
#' Pillar scores depend only on the evidence (not on the weights), so they are
#' computed once from `config` and held fixed; only the pillar weights are
#' perturbed. The number of open critical findings is held fixed too, so the
#' non-compensatory gate behaves exactly as in the unperturbed run.
#'
#' Two families of perturbation are explored:
#'
#' - **One-at-a-time** (`"oat"`): each pillar's weight is nudged up and down by
#'   a growing shift, with the remaining weights rescaled proportionally so the
#'   vector still sums to 1. The reported magnitude is the absolute change in
#'   the nudged pillar's weight.
#' - **Transfer** (`"transfer"`): a small mass is moved along the simplex from
#'   one pillar to another (a directed pairwise step). The reported magnitude is
#'   the mass moved.
#'
#' For every move the scan grows the shift from `step` up to `max_shift` and
#' records the first magnitude at which the band flips, if any.
#'
#' @param evidence A validated evidence data.frame.
#' @param config An `sci_config` from [sci_config_default()]. Its
#'   `pillar_weights` define the point the scan perturbs around.
#' @param step Positive numeric grid resolution for the shift search
#'   (default `0.005`).
#' @param max_shift Largest shift magnitude to explore, in weight units
#'   (default `0.5`).
#' @param methods Character vector choosing the perturbation families to run:
#'   any of `"oat"` and `"transfer"` (both by default).
#' @param n_critical Number of open critical findings. When `NULL` (the
#'   default) it is taken from the evidence through [compute_pillar_scores()],
#'   exactly as in a standard run. It is held fixed across all perturbations.
#'
#' @return A list of class `"sci_weight_robustness"` with:
#'   - `baseline_sci`: the unperturbed SCI (numeric 0--100)
#'   - `baseline_band`: the unperturbed decision band
#'   - `weights`: named numeric vector of the weights scanned around
#'   - `min_shift`: smallest weight change (any move) that flips the band, or
#'     `NA` when no flip was found within `max_shift`
#'   - `robust`: `TRUE` when no flip was found within `max_shift`
#'   - `moves`: a tidy tibble, one row per move tested, with columns
#'     `method`, `move`, `pillar`, `to`, `direction`, `min_shift`, `new_band`,
#'     and `flips`
#'   - `max_shift`, `step`: the search settings used
#'
#' @seealso [sci_sensitivity_analysis()] for scoring explicit weight scenarios.
#'
#' @examples
#' ctx <- r4subcore::r4sub_run_context(study_id = "DEMO", environment = "DEV")
#' ev <- r4subcore::as_evidence(
#'   data.frame(
#'     asset_type = "dataset", asset_id = "ADSL", source_name = "demo",
#'     source_version = NA_character_,
#'     indicator_id = c("Q1", "T1", "R1", "U1"),
#'     indicator_name = c("q", "t", "r", "u"),
#'     indicator_domain = c("quality", "trace", "risk", "usability"),
#'     severity = c("high", "low", "medium", "low"),
#'     result = c("warn", "pass", "pass", "pass"),
#'     metric_value = NA_real_, metric_unit = NA_character_,
#'     message = NA_character_, location = NA_character_,
#'     evidence_payload = "{}", stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' )
#'
#' rob <- sci_weight_robustness(ev)
#' rob
#' rob$min_shift
#'
#' @export
sci_weight_robustness <- function(evidence,
                                  config = sci_config_default(),
                                  step = 0.005,
                                  max_shift = 0.5,
                                  methods = c("oat", "transfer"),
                                  n_critical = NULL) {
  if (!inherits(config, "sci_config")) {
    cli::cli_abort("{.arg config} must be an {.cls sci_config}.")
  }
  if (!is.numeric(step) || length(step) != 1L || step <= 0) {
    cli::cli_abort("{.arg step} must be a single positive number.")
  }
  if (!is.numeric(max_shift) || length(max_shift) != 1L || max_shift <= 0) {
    cli::cli_abort("{.arg max_shift} must be a single positive number.")
  }
  if (step > max_shift) {
    cli::cli_abort("{.arg step} must not exceed {.arg max_shift}.")
  }
  methods <- match.arg(methods, c("oat", "transfer"), several.ok = TRUE)

  # Pillar scores depend only on evidence, not weights, so compute them once.
  ps <- compute_pillar_scores(evidence, config = config)
  if (is.null(n_critical)) {
    n_critical <- attr(ps, "n_critical")
    if (is.null(n_critical)) n_critical <- 0L
  }
  n_critical <- as.integer(n_critical)

  base <- compute_sci(ps, config = config, n_critical = n_critical)
  if (is.na(base$SCI)) {
    cli::cli_abort(
      "Baseline SCI is unclassified; nothing to probe for robustness."
    )
  }

  pillars <- ps$pillar
  w0 <- stats::setNames(ps$weight, pillars)
  base_band <- base$band

  # Evaluate the band for an arbitrary (named) weight vector, reusing the
  # unperturbed pillar scores and a fixed critical count.
  band_for <- function(w) {
    ps_i <- ps
    ps_i$weight <- unname(w[ps_i$pillar])
    compute_sci(ps_i, config = config, n_critical = n_critical)$band
  }

  shifts <- seq(step, max_shift, by = step)

  # Smallest shift (and resulting band) at which `make_weights(s)` flips the
  # band, scanning the shift grid; make_weights returns NULL for an infeasible
  # shift, which is skipped.
  first_flip <- function(make_weights) {
    for (s in shifts) {
      w <- make_weights(s)
      if (is.null(w)) next
      b <- band_for(w)
      if (!identical(b, base_band)) {
        return(list(shift = s, new_band = b))
      }
    }
    NULL
  }

  rows <- list()

  if ("oat" %in% methods) {
    for (p in pillars) {
      others <- setdiff(pillars, p)
      for (dir in c("increase", "decrease")) {
        sgn <- if (dir == "increase") 1 else -1
        make_weights <- function(s) {
          wp <- w0[[p]] + sgn * s
          if (wp < 0 || wp > 1) return(NULL)
          denom <- 1 - w0[[p]]
          w <- w0
          w[[p]] <- wp
          if (denom <= 0) {
            # The pillar held all the weight; spread the remainder evenly.
            if (length(others) > 0) w[others] <- (1 - wp) / length(others)
          } else {
            w[others] <- w0[others] * (1 - wp) / denom
          }
          w
        }
        hit <- first_flip(make_weights)
        rows[[length(rows) + 1L]] <- tibble::tibble(
          method    = "one_at_a_time",
          move      = sprintf("%s %s", dir, p),
          pillar    = p,
          to        = NA_character_,
          direction = dir,
          min_shift = if (is.null(hit)) NA_real_ else hit$shift,
          new_band  = if (is.null(hit)) NA_character_ else hit$new_band,
          flips     = !is.null(hit)
        )
      }
    }
  }

  if ("transfer" %in% methods) {
    for (from in pillars) {
      for (to in setdiff(pillars, from)) {
        make_weights <- function(s) {
          if (w0[[from]] - s < 0) return(NULL)
          w <- w0
          w[[from]] <- w0[[from]] - s
          w[[to]]   <- w0[[to]] + s
          w
        }
        hit <- first_flip(make_weights)
        rows[[length(rows) + 1L]] <- tibble::tibble(
          method    = "transfer",
          move      = sprintf("%s -> %s", from, to),
          pillar    = from,
          to        = to,
          direction = NA_character_,
          min_shift = if (is.null(hit)) NA_real_ else hit$shift,
          new_band  = if (is.null(hit)) NA_character_ else hit$new_band,
          flips     = !is.null(hit)
        )
      }
    }
  }

  moves <- dplyr::bind_rows(rows)
  flipped <- moves$min_shift[moves$flips]
  min_shift <- if (length(flipped) == 0L) NA_real_ else min(flipped)

  structure(
    list(
      baseline_sci  = base$SCI,
      baseline_band = base_band,
      weights       = w0,
      min_shift     = min_shift,
      robust        = is.na(min_shift),
      moves         = moves,
      max_shift     = max_shift,
      step          = step
    ),
    class = "sci_weight_robustness"
  )
}


#' Print a Weight Robustness Diagnostic
#'
#' @param x An `sci_weight_robustness` object.
#' @param n_top Number of closest band-flipping moves to list (default `3`).
#' @param ... Ignored.
#' @export
print.sci_weight_robustness <- function(x, n_top = 3L, ...) {
  cli::cli_h3("SCI weight robustness (diagnostic)")
  cli::cli_alert_info(
    "Baseline SCI {.val {x$baseline_sci}}, band {.val {x$baseline_band}}."
  )

  if (isTRUE(x$robust)) {
    cli::cli_alert_success(
      "Band holds: no weight change up to {.val {x$max_shift}} flips the band."
    )
  } else {
    cli::cli_alert_warning(
      "Smallest weight change that flips the band: {.val {x$min_shift}}."
    )
    flips <- x$moves[x$moves$flips, , drop = FALSE]
    flips <- flips[order(flips$min_shift), , drop = FALSE]
    top <- utils::head(flips, n_top)
    for (i in seq_len(nrow(top))) {
      cli::cli_alert_info(
        "  {top$move[i]} (shift {top$min_shift[i]}) -> {top$new_band[i]}"
      )
    }
  }

  cli::cli_text(
    "{.emph Diagnostic only: this probes band stability, not whether the ",
    "weights are correct.}"
  )
  invisible(x)
}
