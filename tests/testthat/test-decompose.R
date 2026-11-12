test_that("sci_decompose returns an sci_decomposition with both tables", {
  ev <- make_test_evidence()
  dec <- sci_decompose(ev)

  expect_s3_class(dec, "sci_decomposition")
  expect_true(all(c("sci", "sci_exact", "band", "residual",
                    "pillars", "indicators") %in% names(dec)))
  expect_s3_class(dec$pillars, "tbl_df")
  expect_s3_class(dec$indicators, "tbl_df")
  expect_setequal(
    names(dec$pillars),
    c("pillar", "pillar_score", "nominal_weight", "effective_weight",
      "weight_budget", "earned", "shortfall", "scored")
  )
  expect_setequal(
    names(dec$indicators),
    c("indicator_id", "indicator_name", "indicator_domain", "indicator_score",
      "effective_weight", "n_in_pillar", "earned", "shortfall")
  )
})

test_that("decomposition matches compute_sci exactly", {
  ev <- make_test_evidence()
  res <- compute_sci(compute_pillar_scores(ev))
  dec <- sci_decompose(ev)

  expect_equal(dec$sci, res$SCI)
  expect_equal(dec$band, res$band)
})

test_that("earned points reconcile to the SCI and budgets partition cleanly", {
  ev <- make_test_evidence()
  dec <- sci_decompose(ev)

  # Earned points sum (unrounded) to the SCI.
  earned_total <- sum(dec$pillars$earned, na.rm = TRUE)
  expect_equal(earned_total, dec$sci_exact, tolerance = 1e-6)

  # Reported SCI matches the exact sum up to the one-decimal rounding.
  expect_lt(abs(dec$residual), 0.05 + 1e-9)

  # earned + shortfall == weight budget for each scored pillar.
  scored <- dec$pillars[dec$pillars$scored, ]
  expect_equal(scored$earned + scored$shortfall, scored$weight_budget,
               tolerance = 1e-6)

  # Effective weights of scored pillars sum to 1, so budgets sum to 100.
  expect_equal(sum(scored$weight_budget), 100, tolerance = 1e-6)

  # Earned + shortfall over all scored pillars sums to 100.
  expect_equal(sum(scored$earned) + sum(scored$shortfall), 100,
               tolerance = 1e-6)
})

test_that("indicator earned points sum to their pillar earned points", {
  ev <- make_test_evidence()
  dec <- sci_decompose(ev)

  ind <- dec$indicators[!is.na(dec$indicators$earned), ]
  by_pillar <- tapply(ind$earned, ind$indicator_domain, sum)

  for (p in names(by_pillar)) {
    pill_earned <- dec$pillars$earned[dec$pillars$pillar == p]
    expect_equal(unname(by_pillar[[p]]), pill_earned, tolerance = 1e-6,
                 info = p)
  }
})

test_that("missing pillars are reported but excluded from the SCI", {
  ctx <- suppressMessages(r4subcore::r4sub_run_context("S", "DEV"))
  ev <- suppressMessages(r4subcore::as_evidence(
    data.frame(
      asset_type = "dataset", asset_id = "ADSL", source_name = "s",
      indicator_id = c("Q1", "T1"), indicator_name = c("Q1", "T1"),
      indicator_domain = c("quality", "trace"),
      severity = c("low", "low"), result = c("pass", "pass"),
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
  dec <- sci_decompose(ev)

  missing <- dec$pillars[!dec$pillars$scored, ]
  # risk and usability have no indicators here.
  expect_true(all(c("risk", "usability") %in% missing$pillar))
  expect_true(all(is.na(missing$earned)))
  expect_true(all(missing$effective_weight == 0))

  # Effective weights still partition 100 across the two scored pillars, and
  # the earned points reconcile to the SCI.
  scored <- dec$pillars[dec$pillars$scored, ]
  expect_equal(sum(scored$weight_budget), 100, tolerance = 1e-6)
  expect_equal(sum(scored$earned), dec$sci_exact, tolerance = 1e-6)
})

test_that("decomposition reconciles under a custom weight configuration", {
  ev <- make_test_evidence()
  cfg <- sci_config_default(
    pillar_weights = c(quality = 0.5, trace = 0.2, risk = 0.2, usability = 0.1)
  )
  dec <- sci_decompose(ev, config = cfg)
  res <- compute_sci(compute_pillar_scores(ev, cfg), config = cfg)

  expect_equal(dec$sci, res$SCI)
  expect_equal(sum(dec$pillars$earned, na.rm = TRUE), dec$sci_exact,
               tolerance = 1e-6)
})

test_that("print returns the object invisibly", {
  ev <- make_test_evidence()
  dec <- sci_decompose(ev)
  out <- capture.output(print(dec), type = "message")
  expect_true(any(grepl("SCI decomposition", out)))
  expect_invisible(print(dec))
})

test_that("sci_decompose validates its inputs", {
  ev <- make_test_evidence()
  expect_error(sci_decompose(ev, config = list()), "sci_config")
})
