# Tests for the SCI validation helpers (R/validation.R)

# A larger evidence table with a spread of severities and all-passing results,
# so degradation has room to move the score.
make_passing_evidence <- function(n_per_domain = 4L) {
  ctx <- suppressMessages(
    r4subcore::r4sub_run_context(study_id = "VAL001", environment = "DEV")
  )

  domains   <- c("quality", "trace", "risk", "usability")
  severities <- c("info", "low", "medium", "high")

  rows <- do.call(rbind, lapply(domains, function(d) {
    data.frame(
      asset_type       = "dataset",
      asset_id         = "ADSL",
      source_name      = "test_source",
      source_version   = NA_character_,
      indicator_id     = paste0(toupper(substr(d, 1, 1)), seq_len(n_per_domain)),
      indicator_name   = paste(d, "check", seq_len(n_per_domain)),
      indicator_domain = d,
      severity         = rep(severities, length.out = n_per_domain),
      result           = "pass",
      metric_value     = NA_real_,
      metric_unit      = NA_character_,
      message          = NA_character_,
      location         = NA_character_,
      evidence_payload = "{}",
      stringsAsFactors = FALSE
    )
  }))

  suppressMessages(r4subcore::as_evidence(rows, ctx = ctx))
}

test_that("degradation curve starts at the baseline SCI", {
  ev <- make_passing_evidence()
  baseline <- compute_sci(compute_pillar_scores(ev))

  curve <- sci_degradation_curve(ev, n_steps = 8)

  expect_equal(curve$step[1], 0L)
  expect_equal(curve$n_degraded[1], 0L)
  expect_equal(curve$SCI[1], baseline$SCI)
  expect_true(is.na(curve$delta[1]))
})

test_that("degradation curve never rises", {
  ev <- make_passing_evidence()
  curve <- sci_degradation_curve(ev, n_steps = 8)

  chk <- sci_monotone_check(curve)
  expect_true(chk$monotone)
  expect_lte(chk$max_increase, 1e-8)
  expect_equal(nrow(chk$violations), 0L)
})

test_that("full degradation lowers the SCI below the baseline", {
  ev <- make_passing_evidence()
  curve <- sci_degradation_curve(ev, n_steps = 8)

  first <- curve$SCI[1]
  last  <- curve$SCI[nrow(curve)]
  expect_lt(last, first)
  expect_equal(curve$frac_degraded[nrow(curve)], 1)
})

test_that("frac_degraded is non-decreasing and bounded in [0, 1]", {
  ev <- make_passing_evidence()
  curve <- sci_degradation_curve(ev, n_steps = 6)

  expect_true(all(diff(curve$frac_degraded) >= 0))
  expect_true(all(curve$frac_degraded >= 0 & curve$frac_degraded <= 1))
})

test_that("degradation is deterministic", {
  ev <- make_passing_evidence()
  c1 <- sci_degradation_curve(ev, n_steps = 7)
  c2 <- sci_degradation_curve(ev, n_steps = 7)
  expect_identical(c1, c2)
})

test_that("n_steps is capped at the number of passing checks", {
  ev <- make_passing_evidence(n_per_domain = 1L) # 4 passing checks total
  curve <- sci_degradation_curve(ev, n_steps = 50)
  # Baseline plus at most one step per passing check.
  expect_lte(nrow(curve), 5L)
  expect_equal(max(curve$n_degraded), 4L)
})

test_that("evidence with no passing checks returns a single baseline row", {
  ev <- make_passing_evidence()
  ev$result <- "fail" # still a valid evidence table, no re-wrap needed
  curve <- sci_degradation_curve(ev, n_steps = 5)
  expect_equal(nrow(curve), 1L)
  expect_equal(curve$n_degraded, 0L)
})

test_that("sci_degradation_curve validates n_steps", {
  ev <- make_passing_evidence()
  expect_error(sci_degradation_curve(ev, n_steps = 0), "n_steps")
  expect_error(sci_degradation_curve(ev, n_steps = -3), "n_steps")
})

test_that("sci_monotone_check flags a rising curve", {
  rising <- data.frame(step = 0:2, SCI = c(50, 40, 55))
  chk <- sci_monotone_check(rising)
  expect_false(chk$monotone)
  expect_gt(chk$max_increase, 0)
  expect_equal(nrow(chk$violations), 1L)
  expect_equal(chk$violations$from_step, 1L)
  expect_equal(chk$violations$to_step, 2L)
})

test_that("sci_monotone_check accepts a flat curve", {
  flat <- data.frame(step = 0:2, SCI = c(70, 70, 70))
  expect_true(sci_monotone_check(flat)$monotone)
})

test_that("sci_monotone_check rejects malformed input", {
  expect_error(sci_monotone_check(data.frame(a = 1)), "curve")
})

test_that("conformance_findings rises as results worsen", {
  ev_clean <- make_passing_evidence()
  expect_equal(conformance_findings(ev_clean), 0)

  ev_bad <- ev_clean
  ev_bad$result <- "fail" # still a valid evidence table, no re-wrap needed

  expect_gt(conformance_findings(ev_bad), conformance_findings(ev_clean))
})

test_that("conformance_findings ignores passing checks and info fails", {
  ctx <- suppressMessages(
    r4subcore::r4sub_run_context(study_id = "VAL002", environment = "DEV")
  )
  ev <- suppressMessages(r4subcore::as_evidence(data.frame(
    asset_type = "dataset", asset_id = "ADSL", source_name = "s",
    source_version = NA_character_,
    indicator_id = c("A", "B"), indicator_name = c("a", "b"),
    indicator_domain = c("quality", "quality"),
    severity = c("info", "info"), result = c("pass", "fail"),
    metric_value = NA_real_, metric_unit = NA_character_,
    message = NA_character_, location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  ), ctx = ctx))
  # info severity has weight 0, so an info fail adds nothing.
  expect_equal(conformance_findings(ev), 0)
})
