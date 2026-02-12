test_that("compute_indicator_scores returns correct structure", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  scores <- compute_indicator_scores(ev)

  expect_s3_class(scores, "tbl_df")
  expect_true(all(c("indicator_id", "indicator_name", "indicator_domain",
                     "n_evidence", "indicator_score") %in% names(scores)))
})

test_that("compute_indicator_scores groups by indicator_id", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  scores <- compute_indicator_scores(ev)

  # Q1 has 2 rows, Q2 has 1, T1 has 1, T2 has 1, R1 has 1, R2 has 1, U1 has 1
  q1 <- scores[scores$indicator_id == "Q1", ]
  expect_equal(q1$n_evidence, 2)
})

test_that("compute_indicator_scores produces bounded scores", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  scores <- compute_indicator_scores(ev)

  valid <- scores$indicator_score[!is.na(scores$indicator_score)]
  expect_true(all(valid >= 0 & valid <= 1))
})

test_that("all-pass evidence gets high scores", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "T", environment = "DEV")
  ev <- data.frame(
    asset_type = "dataset", asset_id = "DS",
    source_name = "test", source_version = NA_character_,
    indicator_id = c("A", "B"), indicator_name = c("A", "B"),
    indicator_domain = c("quality", "trace"),
    severity = c("info", "info"),
    result = c("pass", "pass"),
    metric_value = NA_real_, metric_unit = NA_character_,
    message = NA_character_, location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  )
  ev <- r4subcore::as_evidence(ev, ctx = ctx)
  scores <- compute_indicator_scores(ev)

  # info severity = weight 0, pass = score 1, so weighted = 1 * (1-0) = 1
  expect_true(all(scores$indicator_score == 1.0))
})

test_that("all-fail critical evidence gets zero scores", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "T", environment = "DEV")
  ev <- data.frame(
    asset_type = "dataset", asset_id = "DS",
    source_name = "test", source_version = NA_character_,
    indicator_id = "X", indicator_name = "X",
    indicator_domain = "quality",
    severity = "critical", result = "fail",
    metric_value = NA_real_, metric_unit = NA_character_,
    message = NA_character_, location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  )
  ev <- r4subcore::as_evidence(ev, ctx = ctx)
  scores <- compute_indicator_scores(ev)

  # fail=0, critical weight=1, so 0 * (1-1) = 0
  expect_equal(scores$indicator_score[1], 0)
})

test_that("severity modulates score correctly", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "T", environment = "DEV")
  ev <- data.frame(
    asset_type = "dataset", asset_id = "DS",
    source_name = "test", source_version = NA_character_,
    indicator_id = c("A", "B"), indicator_name = c("A", "B"),
    indicator_domain = c("quality", "quality"),
    severity = c("info", "high"),
    result = c("pass", "pass"),
    metric_value = NA_real_, metric_unit = NA_character_,
    message = NA_character_, location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  )
  ev <- r4subcore::as_evidence(ev, ctx = ctx)
  scores <- compute_indicator_scores(ev)

  # A: pass at info -> 1 * (1-0) = 1
  # B: pass at high -> 1 * (1-0.75) = 0.25
  a_score <- scores$indicator_score[scores$indicator_id == "A"]
  b_score <- scores$indicator_score[scores$indicator_id == "B"]
  expect_equal(a_score, 1.0)
  expect_equal(b_score, 0.25)
})
