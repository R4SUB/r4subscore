test_that("compute_pillar_scores returns all 4 pillars", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  ps <- compute_pillar_scores(ev)

  expect_s3_class(ps, "tbl_df")
  expect_equal(nrow(ps), 4)
  expect_true(all(c("quality", "trace", "risk", "usability") %in% ps$pillar))
})

test_that("compute_pillar_scores has correct columns", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  ps <- compute_pillar_scores(ev)

  expect_true(all(c("pillar", "pillar_score", "n_indicators", "weight")
                   %in% names(ps)))
})

test_that("pillar scores are bounded 0-1", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  ps <- compute_pillar_scores(ev)

  valid <- ps$pillar_score[!is.na(ps$pillar_score)]
  expect_true(all(valid >= 0 & valid <= 1))
})

test_that("pillar with all-pass info evidence scores 1", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "T", environment = "DEV")
  ev <- data.frame(
    asset_type = "dataset", asset_id = "DS",
    source_name = "test", source_version = NA_character_,
    indicator_id = c("Q1", "Q2"),
    indicator_name = c("Q1", "Q2"),
    indicator_domain = c("quality", "quality"),
    severity = c("info", "info"),
    result = c("pass", "pass"),
    metric_value = NA_real_, metric_unit = NA_character_,
    message = NA_character_, location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  )
  ev <- r4subcore::as_evidence(ev, ctx = ctx)
  ps <- compute_pillar_scores(ev)

  q_score <- ps$pillar_score[ps$pillar == "quality"]
  expect_equal(q_score, 1.0)
})

test_that("pillar weights come from config", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  cfg <- sci_config_default(
    pillar_weights = c(quality = 0.4, trace = 0.2, risk = 0.3, usability = 0.1)
  )
  ps <- compute_pillar_scores(ev, config = cfg)

  expect_equal(ps$weight[ps$pillar == "quality"], 0.4)
  expect_equal(ps$weight[ps$pillar == "trace"], 0.2)
})

test_that("missing pillars get NA score and 0 indicators", {
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

  trace_row <- ps[ps$pillar == "trace", ]
  expect_true(is.na(trace_row$pillar_score))
  expect_equal(trace_row$n_indicators, 0L)
})
