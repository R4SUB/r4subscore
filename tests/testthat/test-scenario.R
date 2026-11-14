# Edit the baseline evidence by hand so a scenario result can be cross-checked
# against an independent engine run (proving sci_scenario only re-runs the
# existing scoring math, inventing nothing).
edit_evidence <- function(ev, id, result = NULL, severity = NULL) {
  rows <- which(ev$indicator_id == id)
  if (!is.null(result))   ev$result[rows]   <- result
  if (!is.null(severity)) ev$severity[rows] <- severity
  ev
}

test_that("sci_scenario returns the documented structure", {
  ev <- make_test_evidence()
  sc <- sci_scenario(ev, data.frame(indicator_id = "Q1", result = "pass"))

  expect_s3_class(sc, "sci_scenario")
  expect_true(all(c("baseline", "comparison", "drivers", "config") %in% names(sc)))
  expect_setequal(
    names(sc$comparison),
    c("scenario", "baseline_sci", "scenario_sci", "delta_sci",
      "baseline_band", "scenario_band", "band_changed", "gated",
      "n_indicators_changed", "n_rows_changed")
  )
  expect_setequal(
    names(sc$drivers),
    c("scenario", "indicator_id", "indicator_name", "indicator_domain",
      "baseline_earned", "scenario_earned", "delta_earned")
  )
  # Single data frame becomes one scenario named "scenario".
  expect_identical(sc$comparison$scenario, "scenario")
})

test_that("scenario SCI equals an independent engine recompute on edited evidence", {
  ev <- make_test_evidence()
  sc <- sci_scenario(ev, list(fix_q1 = data.frame(indicator_id = "Q1", result = "pass")))

  manual_ev  <- edit_evidence(ev, "Q1", result = "pass")
  manual_sci <- compute_sci(compute_pillar_scores(manual_ev))

  expect_equal(sc$comparison$scenario_sci, manual_sci$SCI)
  expect_equal(sc$comparison$scenario_band, manual_sci$band)

  base_sci <- compute_sci(compute_pillar_scores(ev))
  expect_equal(sc$baseline$sci, base_sci$SCI)
  expect_equal(sc$comparison$baseline_sci, base_sci$SCI)
  expect_equal(sc$comparison$delta_sci, round(manual_sci$SCI - base_sci$SCI, 1))
})

test_that("fixing a failing indicator raises the SCI", {
  ev <- make_test_evidence()
  sc <- sci_scenario(ev, data.frame(indicator_id = "Q1", result = "pass"))
  expect_gt(sc$comparison$delta_sci, 0)
})

test_that("drivers reconcile to the SCI delta and are sorted", {
  ev <- make_test_evidence()
  sc <- sci_scenario(
    ev,
    list(fix = data.frame(indicator_id = c("Q1", "R1"),
                          result = c("pass", "pass")))
  )
  drv <- sc$drivers[sc$drivers$scenario == "fix", ]

  # Earned deltas account for the SCI move (two one-decimal roundings).
  expect_lt(abs(sum(drv$delta_earned) - sc$comparison$delta_sci), 0.11)
  # Sorted descending by delta_earned.
  expect_equal(drv$delta_earned, sort(drv$delta_earned, decreasing = TRUE))
  # Only the touched pillars' indicators moved.
  expect_true(all(drv$indicator_domain %in% c("quality", "risk")))
})

test_that("severity edits flow through the scoring engine", {
  ev <- make_test_evidence()
  # Q1 fail at high severity already scores 0; drop its partner Q1 pass to info
  # severity should not move it, but raising R1 warn severity should lower SCI.
  sc <- sci_scenario(ev, data.frame(indicator_id = "R1", severity = "critical"))
  manual <- compute_sci(compute_pillar_scores(
    edit_evidence(ev, "R1", severity = "critical")))
  expect_equal(sc$comparison$scenario_sci, manual$SCI)
})

test_that("band_changed and gated flags are reported", {
  # A high-scoring baseline (ready) where a single critical failure barely dents
  # the score but trips the non-compensatory gate, capping the band.
  ctx <- suppressMessages(r4subcore::r4sub_run_context("GATE", "DEV"))
  ev <- suppressMessages(r4subcore::as_evidence(
    data.frame(
      asset_type = "dataset", asset_id = "ADSL", source_name = "s",
      indicator_id = c("Q1", "Q2", "Q3", "Q4", "Q5", "T1", "R1", "U1"),
      indicator_name = c("Q1", "Q2", "Q3", "Q4", "Q5", "T1", "R1", "U1"),
      indicator_domain = c(rep("quality", 5), "trace", "risk", "usability"),
      severity = rep("info", 8),
      result = rep("pass", 8),
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
  base <- compute_sci(compute_pillar_scores(ev))
  expect_equal(base$band, "ready")

  sc <- sci_scenario(ev, data.frame(indicator_id = "Q1",
                                    result = "fail", severity = "critical"))
  expect_true(sc$comparison$gated)
  expect_true(sc$comparison$band_changed)
  expect_equal(sc$comparison$scenario_band, "conditional")
})

test_that("multiple scenarios, named and auto-named, are handled", {
  ev <- make_test_evidence()
  sc <- sci_scenario(ev, list(
    data.frame(indicator_id = "Q1", result = "pass"),
    fix_r = data.frame(indicator_id = "R1", result = "pass")
  ))
  expect_equal(nrow(sc$comparison), 2L)
  expect_identical(sc$comparison$scenario, c("scenario_1", "fix_r"))
})

test_that("change counts are reported", {
  ev <- make_test_evidence()
  # Q1 has two evidence rows (fail + pass); flipping to fail changes one row.
  sc <- sci_scenario(ev, data.frame(indicator_id = "Q1", result = "fail"))
  expect_equal(sc$comparison$n_indicators_changed, 1L)
  expect_equal(sc$comparison$n_rows_changed, 1L)
})

test_that("sci_scenario leaves the baseline scoring math byte-identical", {
  ev <- make_test_evidence()
  before <- compute_sci(compute_pillar_scores(ev))
  invisible(sci_scenario(ev, data.frame(indicator_id = "Q1", result = "pass")))
  after <- compute_sci(compute_pillar_scores(ev))
  expect_identical(serialize(before, NULL), serialize(after, NULL))
})

test_that("input validation catches malformed scenarios", {
  ev <- make_test_evidence()

  expect_error(sci_scenario(ev, data.frame(result = "pass")), "indicator_id")
  expect_error(sci_scenario(ev, data.frame(indicator_id = "Q1")), "result")
  expect_error(
    sci_scenario(ev, data.frame(indicator_id = "ZZ", result = "pass")),
    "not found"
  )
  expect_error(
    sci_scenario(ev, data.frame(indicator_id = "Q1", result = "passed")),
    "invalid value"
  )
  expect_error(
    sci_scenario(ev, data.frame(indicator_id = "Q1", result = "pass", extra = 1)),
    "unsupported column"
  )
  expect_error(sci_scenario(ev, list()), "at least one")
  expect_error(sci_scenario(ev, data.frame(indicator_id = "Q1", result = "pass"),
                            config = list()), "sci_config")
  expect_error(sci_scenario(ev, 42), "data frame")
  expect_error(
    sci_scenario(ev, list(a = data.frame(indicator_id = "Q1", result = "pass"),
                          a = data.frame(indicator_id = "R1", result = "pass"))),
    "unique"
  )
})

test_that("print returns the object invisibly", {
  ev <- make_test_evidence()
  sc <- sci_scenario(ev, data.frame(indicator_id = "Q1", result = "pass"))
  out <- capture.output(print(sc), type = "message")
  expect_true(any(grepl("scenario analysis", out)))
  expect_invisible(print(sc))
})
