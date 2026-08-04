# Tests for the non-compensatory critical-finding gate (R/sci.R, R/config.R)

strong_ps <- function() {
  tibble::tibble(
    pillar       = c("quality", "trace", "risk", "usability"),
    pillar_score = c(0.90, 0.90, 0.90, 0.90),
    weight       = c(0.35, 0.25, 0.25, 0.15)
  )
}

test_that("without critical findings the band is unchanged", {
  res <- compute_sci(strong_ps(), n_critical = 0L)
  expect_equal(res$SCI, 90)
  expect_equal(res$band, "ready")
  expect_false(res$gated)
  expect_true(is.na(res$gate_reason))
})

test_that("a critical finding caps a ready band at conditional", {
  res <- compute_sci(strong_ps(), n_critical = 2L)
  expect_equal(res$SCI, 90)          # score itself is untouched
  expect_equal(res$band, "conditional")
  expect_true(res$gated)
  expect_match(res$gate_reason, "2 open critical findings")
})

test_that("the gate never improves an already-worse band", {
  weak_ps <- tibble::tibble(
    pillar       = c("quality", "trace", "risk", "usability"),
    pillar_score = c(0.30, 0.30, 0.30, 0.30),
    weight       = c(0.35, 0.25, 0.25, 0.15)
  )
  res <- compute_sci(weak_ps, n_critical = 1L)
  expect_equal(res$band, "high_risk")
  expect_false(res$gated)
})

test_that("the gate can be disabled through the config", {
  cfg <- sci_config_default(gate = list(critical_caps_band = FALSE))
  res <- compute_sci(strong_ps(), config = cfg, n_critical = 3L)
  expect_equal(res$band, "ready")
  expect_false(res$gated)
})

test_that("n_critical is read from the pillar-scores attribute", {
  ps <- strong_ps()
  attr(ps, "n_critical") <- 1L
  res <- compute_sci(ps)
  expect_equal(res$n_critical, 1L)
  expect_true(res$gated)
  expect_equal(res$band, "conditional")
})

test_that("compute_pillar_scores attaches the critical count", {
  ctx <- suppressMessages(r4subcore::r4sub_run_context("S1", "DEV"))
  ev <- suppressMessages(r4subcore::as_evidence(
    data.frame(
      asset_type = "dataset", asset_id = "ADSL", source_name = "p21",
      indicator_id = c("Q1", "Q2"), indicator_name = c("Q1", "Q2"),
      indicator_domain = "quality",
      severity = c("critical", "low"), result = c("fail", "pass"),
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
  ps <- compute_pillar_scores(ev)
  expect_equal(attr(ps, "n_critical"), 1L)
  res <- compute_sci(ps)
  expect_equal(res$n_critical, 1L)
})

test_that("an invalid gate cap is rejected", {
  expect_error(
    sci_config_default(gate = list(critical_caps_band = TRUE,
                                   critical_cap = "not_a_band")),
    "must be one of the band names"
  )
})
