test_that("sci_explain returns expected structure", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  expl <- sci_explain(ev)

  expect_true(is.list(expl))
  expect_named(expl, c("indicator_contributions", "pillar_contributions"))
  expect_s3_class(expl$indicator_contributions, "tbl_df")
  expect_s3_class(expl$pillar_contributions, "tbl_df")
})

test_that("sci_explain indicator contributions have required columns", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  expl <- sci_explain(ev)

  required <- c("indicator_id", "indicator_name", "indicator_domain",
                 "indicator_score", "loss", "pct_of_total_loss")
  expect_true(all(required %in% names(expl$indicator_contributions)))
})

test_that("sci_explain sorts by loss descending", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  expl <- sci_explain(ev)

  losses <- expl$indicator_contributions$loss
  valid_losses <- losses[!is.na(losses)]
  if (length(valid_losses) > 1) {
    expect_true(all(diff(valid_losses) <= 0))
  }
})

test_that("sci_explain pillar contributions have loss column", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  expl <- sci_explain(ev)

  expect_true("contribution" %in% names(expl$pillar_contributions))
  expect_true("loss" %in% names(expl$pillar_contributions))
})

test_that("sci_explain pct_of_total_loss sums to ~100", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  expl <- sci_explain(ev)

  pct <- expl$indicator_contributions$pct_of_total_loss
  valid_pct <- pct[!is.na(pct)]
  if (length(valid_pct) > 0) {
    expect_true(abs(sum(valid_pct) - 100) < 1)  # within rounding tolerance
  }
})

test_that("perfect evidence produces zero loss", {
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
  expl <- sci_explain(ev)

  expect_true(all(expl$indicator_contributions$loss == 0))
  expect_true(all(expl$pillar_contributions$loss == 0))
})
