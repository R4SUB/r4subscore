# Tests for SCI snapshots and diffs (R/snapshots.R)

ctx <- suppressMessages(r4subcore::r4sub_run_context("S1", "DEV"))

mk_ev <- function(res) {
  suppressMessages(r4subcore::as_evidence(
    data.frame(
      asset_type = "dataset", asset_id = "ADSL", source_name = "p21",
      indicator_id = c("Q1", "T1", "R1"),
      indicator_name = c("Q1", "T1", "R1"),
      indicator_domain = c("quality", "trace", "risk"),
      severity = c("low", "high", "medium"),
      result = res,
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}

test_that("a snapshot captures the SCI, band, and pillar scores", {
  snap <- sci_snapshot(mk_ev(c("pass", "fail", "pass")), "run 1")
  expect_s3_class(snap, "sci_snapshot")
  expect_equal(snap$label, "run 1")
  expect_type(snap$sci, "double")
  expect_true(nrow(snap$pillars) >= 1L)
  expect_true(all(c("indicator_id", "indicator_score") %in%
                    names(snap$indicators)))
})

test_that("sci_snapshot rejects a bad label", {
  expect_error(sci_snapshot(mk_ev(c("pass", "pass", "pass")), c("a", "b")),
               "single non-empty string")
})

test_that("sci_diff reports the overall and per-indicator movement", {
  before <- sci_snapshot(mk_ev(c("pass", "fail", "fail")), "before")
  after  <- sci_snapshot(mk_ev(c("pass", "pass", "fail")), "after")
  d <- sci_diff(before, after)

  expect_s3_class(d, "sci_diff")
  expect_gt(d$sci_delta, 0)   # fixing T1 raises the SCI
  t1 <- d$indicators[d$indicators$indicator_id == "T1", ]
  expect_equal(t1$status, "improved")
  expect_gt(t1$delta, 0)
  # R1 stayed failing
  r1 <- d$indicators[d$indicators$indicator_id == "R1", ]
  expect_equal(r1$status, "unchanged")
})

test_that("sci_diff flags added and removed indicators", {
  before <- sci_snapshot(mk_ev(c("pass", "pass", "pass")), "before")

  after_ev <- suppressMessages(r4subcore::as_evidence(
    data.frame(
      asset_type = "dataset", asset_id = "ADSL", source_name = "p21",
      indicator_id = c("Q1", "T1"), indicator_name = c("Q1", "T1"),
      indicator_domain = c("quality", "trace"),
      severity = c("low", "high"), result = c("pass", "pass"),
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
  after <- sci_snapshot(after_ev, "after")

  d <- sci_diff(before, after)
  r1 <- d$indicators[d$indicators$indicator_id == "R1", ]
  expect_equal(r1$status, "removed")
})

test_that("sci_diff detects a band change", {
  before <- sci_snapshot(mk_ev(c("fail", "fail", "fail")), "before")
  after  <- sci_snapshot(mk_ev(c("pass", "pass", "pass")), "after")
  d <- sci_diff(before, after)
  expect_true(d$band_changed)
  expect_equal(d$band_from, "high_risk")
})

test_that("sci_diff requires two snapshots", {
  snap <- sci_snapshot(mk_ev(c("pass", "pass", "pass")), "x")
  expect_error(sci_diff(snap, list()), "sci_snapshot")
})

test_that("history binds snapshots into one row each with pillar columns", {
  s1 <- sci_snapshot(mk_ev(c("pass", "fail", "fail")), "week 1")
  s2 <- sci_snapshot(mk_ev(c("pass", "pass", "fail")), "week 2")
  s3 <- sci_snapshot(mk_ev(c("pass", "pass", "pass")), "week 3")
  h <- sci_snapshot_history(list(s1, s2, s3))

  expect_equal(nrow(h), 3L)
  expect_equal(h$label, c("week 1", "week 2", "week 3"))
  expect_true(all(c("quality", "trace", "risk") %in% names(h)))
  # SCI should climb as failures are fixed
  expect_true(all(diff(h$sci) >= 0))
})

test_that("history rejects a non-snapshot list", {
  expect_error(sci_snapshot_history(list(1, 2)), "sci_snapshot")
})
