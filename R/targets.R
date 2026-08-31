#' Define an SCI Target
#'
#' Records the readiness goal a submission is aiming for: an overall SCI target
#' and, optionally, a minimum score for individual pillars. Pair it with
#' [sci_gap_to_target()] to measure how far a scored run is from filing.
#'
#' @param target Either a band name (for example `"ready"`), in which case the
#'   target SCI is that band's lower bound, or a single numeric SCI on the
#'   0--100 scale.
#' @param pillar_targets Optional named numeric vector of per-pillar minimum
#'   scores on the 0--100 scale. Names must be among `"quality"`, `"trace"`,
#'   `"risk"`, `"usability"`.
#' @param config An `sci_config` from [sci_config_default()], used to resolve a
#'   band name to a numeric bound and to classify a numeric target.
#'
#' @return A list of class `"sci_targets"` with `sci` (numeric target),
#'   `band` (the band the target falls in), and `pillars` (named numeric vector
#'   of per-pillar targets, possibly empty).
#'
#' @examples
#' sci_targets("ready")
#' sci_targets(80, pillar_targets = c(risk = 75, trace = 85))
#'
#' @export
sci_targets <- function(target = "ready",
                        pillar_targets = NULL,
                        config = sci_config_default()) {
  bands <- config$bands
  if (is.character(target) && length(target) == 1L) {
    if (!target %in% names(bands)) {
      cli::cli_abort(
        "{.arg target} band {.val {target}} is not one of {.val {names(bands)}}."
      )
    }
    target_sci <- bands[[target]][1]
    band <- target
  } else if (is.numeric(target) && length(target) == 1L && !is.na(target)) {
    if (target < 0 || target > 100) {
      cli::cli_abort("A numeric {.arg target} must be between 0 and 100.")
    }
    target_sci <- as.double(target)
    band <- classify_band(target_sci, bands)
  } else {
    cli::cli_abort("{.arg target} must be a single band name or a numeric 0--100 value.")
  }

  pt <- numeric(0)
  if (!is.null(pillar_targets)) {
    if (!is.numeric(pillar_targets) || is.null(names(pillar_targets))) {
      cli::cli_abort("{.arg pillar_targets} must be a named numeric vector.")
    }
    valid_pillars <- c("quality", "trace", "risk", "usability")
    bad <- setdiff(names(pillar_targets), valid_pillars)
    if (length(bad) > 0L) {
      cli::cli_abort("Invalid pillar name(s): {.val {bad}}. Must be among {.val {valid_pillars}}.")
    }
    if (any(pillar_targets < 0 | pillar_targets > 100, na.rm = TRUE)) {
      cli::cli_abort("{.arg pillar_targets} values must be between 0 and 100.")
    }
    pt <- pillar_targets
  }

  structure(
    list(sci = target_sci, band = band, pillars = pt),
    class = "sci_targets"
  )
}

#' Measure the Gap Between a Scored Run and a Target
#'
#' Compares a computed SCI result against an [sci_targets()] goal and reports
#' the shortfall overall and per pillar. For each pillar it also reports the
#' SCI points that closing that pillar would add, so a team can see which pillar
#' is the biggest lever rather than guessing.
#'
#' @details
#' Because the SCI is a weighted average, each pillar contributes
#' `weight * pillar_score` points to the 0--100 total. The headroom a pillar
#' offers is therefore `weight * (100 - current)`, reported as `max_lift`, and
#' the pillars are ordered by it so the largest opportunity is first.
#'
#' @param x An `sci_result` from [compute_sci()].
#' @param targets An `sci_targets` object, or a band name / numeric passed
#'   straight to [sci_targets()].
#' @param config An `sci_config`, used only when `targets` needs building.
#'
#' @return A list of class `"sci_gap"` with `current_sci`, `target_sci`, `gap`
#'   (0 when met, otherwise the shortfall), `met` (logical), `band`,
#'   `target_band`, and `pillars`: a data.frame with `pillar`, `weight`,
#'   `current`, `target`, `gap`, `met`, `max_lift`, and `target_lift`.
#'
#' @examples
#' \dontrun{
#' ps <- compute_pillar_scores(evidence)
#' res <- compute_sci(ps)
#' sci_gap_to_target(res, "ready")
#' }
#'
#' @export
sci_gap_to_target <- function(x, targets = "ready", config = sci_config_default()) {
  if (!inherits(x, "sci_result")) {
    cli::cli_abort("{.arg x} must be an {.cls sci_result} from {.fn compute_sci}.")
  }
  if (!inherits(targets, "sci_targets")) {
    targets <- sci_targets(targets, config = config)
  }

  current_sci <- x$SCI
  target_sci  <- targets$sci
  gap <- if (is.na(current_sci)) NA_real_ else max(0, round(target_sci - current_sci, 1))
  met <- if (is.na(current_sci)) NA else current_sci >= target_sci

  ps <- x$pillar_scores
  current100 <- ps$pillar_score * 100
  weight <- ps$weight

  ptarget <- rep(NA_real_, nrow(ps))
  if (length(targets$pillars) > 0L) {
    idx <- match(ps$pillar, names(targets$pillars))
    ptarget <- unname(targets$pillars[idx])
  }

  max_lift <- weight * (100 - current100)               # points if pillar -> 100
  pillar_gap <- ifelse(is.na(ptarget), NA_real_,
                       pmax(0, ptarget - current100))
  target_lift <- ifelse(is.na(pillar_gap), NA_real_, weight * pillar_gap)
  pillar_met <- ifelse(is.na(ptarget), NA, current100 >= ptarget)

  pillars <- data.frame(
    pillar      = ps$pillar,
    weight      = round(weight, 3),
    current     = round(current100, 1),
    target      = round(ptarget, 1),
    gap         = round(pillar_gap, 1),
    met         = pillar_met,
    max_lift    = round(max_lift, 1),
    target_lift = round(target_lift, 1),
    stringsAsFactors = FALSE
  )
  # Biggest opportunity first; NA lift (NA pillar score) sinks to the bottom.
  pillars <- pillars[order(-pillars$max_lift, na.last = TRUE), , drop = FALSE]
  rownames(pillars) <- NULL

  structure(
    list(
      current_sci = current_sci,
      target_sci  = target_sci,
      gap         = gap,
      met         = met,
      band        = x$band,
      target_band = targets$band,
      pillars     = pillars
    ),
    class = "sci_gap"
  )
}

#' Print an SCI Target
#' @param x An `sci_targets` object.
#' @param ... Ignored.
#' @export
print.sci_targets <- function(x, ...) {
  cli::cli_alert_info("SCI target: {.val {x$sci}} (band {.val {x$band}})")
  if (length(x$pillars) > 0L) {
    for (nm in names(x$pillars)) {
      cli::cli_alert_info("  {nm}: >= {x$pillars[[nm]]}")
    }
  }
  invisible(x)
}

#' Print an SCI Gap
#' @param x An `sci_gap` object.
#' @param ... Ignored.
#' @export
print.sci_gap <- function(x, ...) {
  if (isTRUE(x$met)) {
    cli::cli_alert_success(
      "SCI {.val {x$current_sci}} meets target {.val {x$target_sci}} (band {.val {x$target_band}})."
    )
  } else {
    cli::cli_alert_warning(
      "SCI {.val {x$current_sci}} is {.val {x$gap}} below target {.val {x$target_sci}} (band {.val {x$target_band}})."
    )
  }
  top <- x$pillars[!is.na(x$pillars$max_lift), , drop = FALSE]
  if (nrow(top) > 0L) {
    cli::cli_alert_info("Biggest lever: {.val {top$pillar[1]}} (up to {.val {top$max_lift[1]}} SCI points).")
  }
  invisible(x)
}
