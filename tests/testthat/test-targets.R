# Tests for sci_targets() and sci_gap_to_target() (R/targets.R)

mk_result <- function(scores = c(quality = 0.8, trace = 0.7, risk = 0.6, usability = 0.9),
                      weights = c(quality = 0.35, trace = 0.25, risk = 0.25, usability = 0.15),
                      n_critical = 0L) {
  ps <- data.frame(
    pillar = names(scores),
    pillar_score = unname(scores),
    weight = unname(weights),
    stringsAsFactors = FALSE
  )
  compute_sci(ps, n_critical = n_critical)
}

test_that("sci_targets resolves a band name to its lower bound", {
  t <- sci_targets("ready")
  expect_s3_class(t, "sci_targets")
  expect_equal(t$sci, 85)
  expect_equal(t$band, "ready")
  expect_length(t$pillars, 0L)
})

test_that("sci_targets accepts a numeric target and pillar minimums", {
  t <- sci_targets(80, pillar_targets = c(risk = 75, trace = 85))
  expect_equal(t$sci, 80)
  expect_equal(unname(t$pillars[["risk"]]), 75)
  expect_equal(t$band, classify_band(80))
})

test_that("sci_targets rejects bad input", {
  expect_error(sci_targets("nonsense"), "band")
  expect_error(sci_targets(150), "between 0 and 100")
  expect_error(sci_targets(80, pillar_targets = c(bogus = 50)), "Invalid pillar")
  expect_error(sci_targets(80, pillar_targets = c(risk = 200)), "between 0 and 100")
})

test_that("gap is zero when the target is met", {
  res <- mk_result(scores = c(quality = 0.95, trace = 0.9, risk = 0.9, usability = 0.95))
  g <- sci_gap_to_target(res, "ready")
  expect_s3_class(g, "sci_gap")
  expect_true(g$met)
  expect_equal(g$gap, 0)
})

test_that("gap reports the shortfall to target", {
  res <- mk_result(scores = c(quality = 0.6, trace = 0.6, risk = 0.6, usability = 0.6))
  # SCI = 60, target ready = 85, gap = 25
  g <- sci_gap_to_target(res, "ready")
  expect_false(g$met)
  expect_equal(g$current_sci, 60)
  expect_equal(g$target_sci, 85)
  expect_equal(g$gap, 25)
})

test_that("max_lift ranks the biggest opportunity first and matches weight * headroom", {
  res <- mk_result(scores = c(quality = 0.5, trace = 0.5, risk = 0.5, usability = 0.5))
  g <- sci_gap_to_target(res, "ready")
  # all pillars at 50, so headroom is weight * 50; quality has the largest weight
  expect_equal(g$pillars$pillar[1], "quality")
  q <- g$pillars[g$pillars$pillar == "quality", ]
  expect_equal(q$max_lift, round(0.35 * (100 - 50), 1))
})

test_that("per-pillar targets produce gaps and met flags", {
  res <- mk_result(scores = c(quality = 0.9, trace = 0.6, risk = 0.6, usability = 0.9))
  g <- sci_gap_to_target(res, sci_targets(80, pillar_targets = c(trace = 85, quality = 80)))
  tr <- g$pillars[g$pillars$pillar == "trace", ]
  ql <- g$pillars[g$pillars$pillar == "quality", ]
  expect_equal(tr$target, 85); expect_equal(tr$gap, 25); expect_false(tr$met)
  expect_equal(ql$target, 80); expect_equal(ql$gap, 0);  expect_true(ql$met)
})

test_that("a bare band name or number is accepted as targets", {
  res <- mk_result()
  expect_s3_class(sci_gap_to_target(res, "minor_gaps"), "sci_gap")
  expect_s3_class(sci_gap_to_target(res, 90), "sci_gap")
})

test_that("NA pillar scores sink to the bottom and do not error", {
  res <- mk_result(scores = c(quality = 0.8, trace = NA, risk = 0.6, usability = 0.9))
  g <- sci_gap_to_target(res, "ready")
  expect_true(is.na(g$pillars$max_lift[nrow(g$pillars)]))
  expect_equal(g$pillars$pillar[nrow(g$pillars)], "trace")
})

test_that("sci_gap_to_target rejects a non sci_result", {
  expect_error(sci_gap_to_target(list(SCI = 80), "ready"), "sci_result")
})
