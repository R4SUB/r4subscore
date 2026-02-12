test_that("compute_sci returns expected structure", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  ps <- compute_pillar_scores(ev)
  result <- compute_sci(ps)

  expect_s3_class(result, "sci_result")
  expect_true(is.numeric(result$SCI))
  expect_true(is.character(result$band))
  expect_true(is.data.frame(result$pillar_scores))
  expect_true(is.numeric(result$weights_used))
})

test_that("SCI is between 0 and 100", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  ps <- compute_pillar_scores(ev)
  result <- compute_sci(ps)

  expect_true(result$SCI >= 0 && result$SCI <= 100)
})

test_that("perfect evidence produces SCI = 100", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "T", environment = "DEV")
  ev <- data.frame(
    asset_type = "dataset", asset_id = "DS",
    source_name = "test", source_version = NA_character_,
    indicator_id = c("Q1", "T1", "R1", "U1"),
    indicator_name = c("Q1", "T1", "R1", "U1"),
    indicator_domain = c("quality", "trace", "risk", "usability"),
    severity = rep("info", 4),
    result = rep("pass", 4),
    metric_value = NA_real_, metric_unit = NA_character_,
    message = NA_character_, location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  )
  ev <- r4subcore::as_evidence(ev, ctx = ctx)
  ps <- compute_pillar_scores(ev)
  result <- compute_sci(ps)

  expect_equal(result$SCI, 100)
  expect_equal(result$band, "ready")
})

test_that("SCI handles missing pillars via renormalization", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "T", environment = "DEV")
  # Only quality evidence
  ev <- data.frame(
    asset_type = "dataset", asset_id = "DS",
    source_name = "test", source_version = NA_character_,
    indicator_id = "Q1", indicator_name = "Q1",
    indicator_domain = "quality",
    severity = "info", result = "pass",
    metric_value = NA_real_, metric_unit = NA_character_,
    message = NA_character_, location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  )
  ev <- r4subcore::as_evidence(ev, ctx = ctx)
  ps <- compute_pillar_scores(ev)
  result <- compute_sci(ps)

  # Only quality with score 1.0, renormalized weight = 1.0
  # SCI = 1.0 * 1.0 * 100 = 100
  expect_equal(result$SCI, 100)
})

test_that("band classification works for known SCI values", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  ps <- compute_pillar_scores(ev)
  result <- compute_sci(ps)

  valid_bands <- c("ready", "minor_gaps", "conditional", "high_risk")
  expect_true(result$band %in% valid_bands)
})

test_that("compute_sci rejects invalid input", {
  expect_error(compute_sci("not_a_df"), "must be a data.frame")
  expect_error(
    compute_sci(data.frame(pillar = "x")),
    "missing column"
  )
})

test_that("print.sci_result does not error", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  ps <- compute_pillar_scores(ev)
  result <- compute_sci(ps)
  expect_no_error(print(result))
})
