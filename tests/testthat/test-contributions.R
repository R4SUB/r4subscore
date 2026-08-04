# Tests for the per-indicator contribution breakdown (R/contributions.R)

# Evidence with a failing check that carries a message, so remediation hints
# have something to surface.
make_message_evidence <- function() {
  ctx <- suppressMessages(
    r4subcore::r4sub_run_context(study_id = "CON001", environment = "DEV")
  )

  ev <- data.frame(
    asset_type       = "dataset",
    asset_id         = "ADSL",
    source_name      = "test_source",
    source_version   = NA_character_,
    indicator_id     = c("Q1", "Q2", "T1", "R1", "U1"),
    indicator_name   = c("Quality 1", "Quality 2", "Trace 1",
                         "Risk 1", "Usability 1"),
    indicator_domain = c("quality", "quality", "trace", "risk", "usability"),
    severity         = c("high", "low", "medium", "high", "low"),
    result           = c("fail", "pass", "pass", "warn", "pass"),
    metric_value     = NA_real_,
    metric_unit      = NA_character_,
    message          = c("Add dataset labels to Define-XML", NA, NA,
                         "Document the derivation risk", NA),
    location         = NA_character_,
    evidence_payload = "{}",
    stringsAsFactors = FALSE
  )

  suppressMessages(r4subcore::as_evidence(ev, ctx = ctx))
}

test_that("indicator_contributions returns one row per indicator", {
  ev <- make_test_evidence()
  contrib <- indicator_contributions(ev)

  expect_s3_class(contrib, "tbl_df")
  expect_setequal(contrib$indicator_id, unique(ev$indicator_id))
  expect_true(all(c("pillar_contribution", "pillar_headroom",
                    "sci_headroom", "suggestion") %in% names(contrib)))
})

test_that("rows are ranked by sci_headroom descending", {
  ev <- make_test_evidence()
  contrib <- indicator_contributions(ev)

  h <- contrib$sci_headroom[!is.na(contrib$sci_headroom)]
  expect_false(is.unsorted(rev(h)))
})

test_that("pillar contributions sum to the pillar score", {
  ev <- make_test_evidence()
  contrib <- indicator_contributions(ev)
  ps <- compute_pillar_scores(ev)

  for (p in unique(contrib$indicator_domain)) {
    got <- sum(contrib$pillar_contribution[contrib$indicator_domain == p],
               na.rm = TRUE)
    want <- ps$pillar_score[ps$pillar == p]
    if (!is.na(want)) expect_equal(got, want, tolerance = 1e-6)
  }
})

test_that("a perfect indicator has zero headroom", {
  ev <- make_test_evidence()
  contrib <- indicator_contributions(ev)

  perfect <- contrib[!is.na(contrib$indicator_score) &
                       contrib$indicator_score == 1, ]
  if (nrow(perfect) > 0) {
    expect_true(all(perfect$pillar_headroom == 0))
    expect_true(all(perfect$sci_headroom == 0))
  }
})

test_that("suggestions use the check message when a check failed", {
  ev <- make_message_evidence()
  contrib <- indicator_contributions(ev)

  q1 <- contrib$suggestion[contrib$indicator_id == "Q1"]
  expect_match(q1, "Add dataset labels to Define-XML", fixed = TRUE)

  # A passing indicator with no message falls back to the domain template.
  u1 <- contrib$suggestion[contrib$indicator_id == "U1"]
  expect_match(u1, "documentation")
})

test_that("what-if recomputes the SCI exactly", {
  ev <- make_test_evidence()
  base <- compute_sci(compute_pillar_scores(ev))

  wi <- sci_what_if(ev, "Q1", new_score = 1)

  expect_equal(wi$old_SCI, base$SCI)
  expect_gte(wi$new_SCI, wi$old_SCI)
  expect_equal(wi$delta_SCI, round(wi$new_SCI - wi$old_SCI, 1))
  expect_gte(wi$new_pillar_score, wi$old_pillar_score)
})

test_that("what-if with no change leaves the SCI untouched", {
  ev <- make_test_evidence()
  ind <- compute_indicator_scores(ev)
  q1_score <- ind$indicator_score[ind$indicator_id == "Q1"]

  wi <- sci_what_if(ev, "Q1", new_score = q1_score)
  expect_equal(wi$delta_SCI, 0)
  expect_equal(wi$new_SCI, wi$old_SCI)
})

test_that("what-if validates its inputs", {
  ev <- make_test_evidence()
  expect_error(sci_what_if(ev, "Q1", new_score = 2), "\\[0, 1\\]")
  expect_error(sci_what_if(ev, "Q1", new_score = NA), "\\[0, 1\\]")
  expect_error(sci_what_if(ev, "does_not_exist", new_score = 1), "not found")
})
