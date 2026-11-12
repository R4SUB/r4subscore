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

# ---------------------------------------------------------------------------
# Additional reconciliation tests
# ---------------------------------------------------------------------------

# Local builder: assemble a validated evidence table from vectors so each test
# can target a specific pillar/severity/result shape. One row per element.
dec_build_ev <- function(indicator_id, indicator_domain, severity, result,
                         indicator_name = indicator_id) {
  ctx <- suppressMessages(r4subcore::r4sub_run_context("DECTEST", "DEV"))
  suppressMessages(r4subcore::as_evidence(
    data.frame(
      asset_type       = "dataset",
      asset_id         = "ADSL",
      source_name      = "dec_src",
      indicator_id     = indicator_id,
      indicator_name   = indicator_name,
      indicator_domain = indicator_domain,
      severity         = severity,
      result           = result,
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}

# Reusable invariant check: everything the decomposition promises to reconcile.
dec_expect_reconciles <- function(dec, label = "") {
  scored <- dec$pillars[dec$pillars$scored, , drop = FALSE]

  if (nrow(scored) == 0L) {
    expect_true(is.na(dec$sci_exact), info = label)
    return(invisible(NULL))
  }

  # earned + shortfall partitions each pillar's budget.
  expect_equal(scored$earned + scored$shortfall, scored$weight_budget,
               tolerance = 1e-6, info = label)
  # Budgets of scored pillars sum to 100.
  expect_equal(sum(scored$weight_budget), 100, tolerance = 1e-6, info = label)
  # Earned points reconcile to the exact (unrounded) SCI.
  expect_equal(sum(scored$earned), dec$sci_exact, tolerance = 1e-6, info = label)
  # Earned + shortfall over all scored pillars sums to 100.
  expect_equal(sum(scored$earned) + sum(scored$shortfall), 100,
               tolerance = 1e-6, info = label)
  # Reported SCI is the exact sum up to one-decimal rounding.
  expect_lt(abs(dec$residual), 0.05 + 1e-9)
  # Indicator earned points sum to their pillar's earned points.
  ind <- dec$indicators[!is.na(dec$indicators$earned), , drop = FALSE]
  by_pillar <- tapply(ind$earned, ind$indicator_domain, sum)
  for (p in names(by_pillar)) {
    pill_earned <- dec$pillars$earned[dec$pillars$pillar == p]
    expect_equal(unname(by_pillar[[p]]), pill_earned, tolerance = 1e-6,
                 info = paste(label, p))
  }
  invisible(NULL)
}

test_that("each pillar dropped in turn is unscored and the rest reconcile", {
  pillars <- c("quality", "trace", "risk", "usability")
  nominal <- sci_config_default()$pillar_weights

  for (drop in pillars) {
    present <- setdiff(pillars, drop)
    ev <- dec_build_ev(
      indicator_id     = paste0("I", seq_along(present)),
      indicator_domain = present,
      severity         = rep("low", length(present)),
      result           = c("pass", "warn", "fail")[seq_along(present)]
    )
    dec <- sci_decompose(ev)

    dropped_row <- dec$pillars[dec$pillars$pillar == drop, ]
    expect_false(dropped_row$scored, info = drop)
    expect_equal(dropped_row$effective_weight, 0, info = drop)
    expect_true(is.na(dropped_row$earned), info = drop)
    expect_true(is.na(dropped_row$shortfall), info = drop)

    # Effective weights of the present pillars are the nominal weights
    # renormalised over the present set.
    scored <- dec$pillars[dec$pillars$scored, ]
    denom <- sum(nominal[present])
    for (p in present) {
      expect_equal(
        dec$pillars$effective_weight[dec$pillars$pillar == p],
        round(unname(nominal[p]) / denom, 4),
        tolerance = 1e-4, info = paste(drop, p)
      )
    }
    dec_expect_reconciles(dec, label = paste("drop", drop))
  }
})

test_that("several pillars dropped still reconciles across combinations", {
  combos <- list(
    c("risk", "usability"),      # drop quality + trace
    c("quality", "usability"),   # drop trace + risk
    c("quality", "trace", "risk")
  )
  for (present in combos) {
    ev <- dec_build_ev(
      indicator_id     = paste0("I", seq_along(present)),
      indicator_domain = present,
      severity         = rep("info", length(present)),
      result           = rep("pass", length(present))
    )
    dec <- sci_decompose(ev)

    absent <- setdiff(c("quality", "trace", "risk", "usability"), present)
    miss <- dec$pillars[dec$pillars$pillar %in% absent, ]
    expect_true(all(!miss$scored))
    expect_true(all(miss$effective_weight == 0))
    expect_true(all(is.na(miss$earned)))

    dec_expect_reconciles(dec, label = paste(present, collapse = "+"))
  }
})

test_that("a single present pillar earns the entire 100-point budget", {
  # Only quality present, one warn indicator -> effective weight 1.
  ev <- dec_build_ev(
    indicator_id     = "Q1",
    indicator_domain = "quality",
    severity         = "info",
    result           = "warn"      # weighted score 0.5
  )
  dec <- sci_decompose(ev)

  q <- dec$pillars[dec$pillars$pillar == "quality", ]
  expect_true(q$scored)
  expect_equal(q$effective_weight, 1, tolerance = 1e-6)
  expect_equal(q$weight_budget, 100, tolerance = 1e-6)
  expect_equal(q$pillar_score, 0.5, tolerance = 1e-6)
  expect_equal(q$earned, 50, tolerance = 1e-6)
  expect_equal(q$shortfall, 50, tolerance = 1e-6)
  expect_equal(dec$sci_exact, 50, tolerance = 1e-6)
  dec_expect_reconciles(dec, label = "single-pillar")
})

test_that("a single-indicator pillar's indicator earns the whole pillar budget", {
  # quality has exactly one indicator; its earned must equal the pillar earned.
  ev <- dec_build_ev(
    indicator_id     = c("Q1", "T1", "T2"),
    indicator_domain = c("quality", "trace", "trace"),
    severity         = c("info", "info", "info"),
    result           = c("pass", "warn", "fail")
  )
  dec <- sci_decompose(ev)

  q_ind <- dec$indicators[dec$indicators$indicator_domain == "quality", ]
  expect_equal(nrow(q_ind), 1L)
  expect_equal(q_ind$n_in_pillar, 1L)
  q_pill <- dec$pillars$earned[dec$pillars$pillar == "quality"]
  expect_equal(q_ind$earned, q_pill, tolerance = 1e-6)
  # The two-indicator trace pillar splits its budget evenly.
  t_ind <- dec$indicators[dec$indicators$indicator_domain == "trace", ]
  expect_true(all(t_ind$n_in_pillar == 2L))
  dec_expect_reconciles(dec, label = "single-indicator")
})

test_that("all-pass at info severity yields a full 100 SCI with zero shortfall", {
  ev <- dec_build_ev(
    indicator_id     = c("Q1", "Q2", "T1", "R1", "U1"),
    indicator_domain = c("quality", "quality", "trace", "risk", "usability"),
    severity         = rep("info", 5),
    result           = rep("pass", 5)
  )
  dec <- sci_decompose(ev)

  scored <- dec$pillars[dec$pillars$scored, ]
  expect_equal(nrow(scored), 4L)
  expect_true(all(scored$pillar_score == 1))
  expect_equal(dec$sci, 100)
  expect_equal(dec$sci_exact, 100, tolerance = 1e-6)
  # Every pillar earns its full budget and leaves nothing on the table.
  expect_equal(scored$earned, scored$weight_budget, tolerance = 1e-6)
  expect_true(all(abs(scored$shortfall) < 1e-9))
  expect_true(all(abs(dec$indicators$shortfall) < 1e-9))
  dec_expect_reconciles(dec, label = "all-pass")
})

test_that("all-fail yields a zero SCI with earned zero and shortfall the budget", {
  ev <- dec_build_ev(
    indicator_id     = c("Q1", "T1", "T2", "R1", "U1"),
    indicator_domain = c("quality", "trace", "trace", "risk", "usability"),
    severity         = c("low", "high", "medium", "critical", "info"),
    result           = rep("fail", 5)
  )
  dec <- sci_decompose(ev)

  scored <- dec$pillars[dec$pillars$scored, ]
  expect_equal(nrow(scored), 4L)
  expect_true(all(scored$pillar_score == 0))
  expect_equal(dec$sci, 0)
  expect_equal(dec$sci_exact, 0, tolerance = 1e-6)
  # Nothing earned; the full budget is shortfall.
  expect_true(all(abs(scored$earned) < 1e-9))
  expect_equal(scored$shortfall, scored$weight_budget, tolerance = 1e-6)
  dec_expect_reconciles(dec, label = "all-fail")
})

test_that("reconciliation holds under several custom weight configurations", {
  ev <- make_test_evidence()
  configs <- list(
    c(quality = 0.70, trace = 0.10, risk = 0.10, usability = 0.10),
    c(quality = 0.25, trace = 0.25, risk = 0.25, usability = 0.25),
    c(quality = 0.10, trace = 0.10, risk = 0.70, usability = 0.10),
    c(quality = 0.40, trace = 0.30, risk = 0.20, usability = 0.10)
  )
  for (w in configs) {
    cfg <- sci_config_default(pillar_weights = w)
    dec <- sci_decompose(ev, config = cfg)
    res <- compute_sci(compute_pillar_scores(ev, cfg), config = cfg)

    expect_equal(dec$sci, res$SCI, info = paste(w, collapse = ","))
    expect_equal(dec$band, res$band, info = paste(w, collapse = ","))
    # All four pillars are present here, so effective == nominal weights.
    for (p in names(w)) {
      row <- dec$pillars[dec$pillars$pillar == p, ]
      expect_equal(row$effective_weight, round(unname(w[p]), 4),
                   tolerance = 1e-4, info = p)
      expect_equal(row$weight_budget, unname(w[p]) * 100, tolerance = 1e-6,
                   info = p)
    }
    dec_expect_reconciles(dec, label = paste(w, collapse = ","))
  }
})

test_that("decomposition reconciles across many random-but-fixed inputs", {
  domains  <- c("quality", "trace", "risk", "usability")
  sevs     <- c("info", "low", "medium", "high", "critical")
  results  <- c("pass", "warn", "fail")

  for (seed in 1:40) {
    set.seed(seed)
    n <- sample(4:12, 1)
    ev <- dec_build_ev(
      indicator_id     = paste0("I", seq_len(n)),
      indicator_domain = sample(domains, n, replace = TRUE),
      severity         = sample(sevs, n, replace = TRUE),
      result           = sample(results, n, replace = TRUE)
    )
    dec <- sci_decompose(ev)
    res <- compute_sci(compute_pillar_scores(ev))

    expect_equal(dec$sci, res$SCI, info = paste("seed", seed))
    dec_expect_reconciles(dec, label = paste("seed", seed))
  }
})

test_that("sci_decompose has no effect on compute_sci or sci_explain output", {
  ev <- make_test_evidence()

  res_before  <- compute_sci(compute_pillar_scores(ev))
  expl_before <- sci_explain(ev)

  invisible(sci_decompose(ev))

  res_after  <- compute_sci(compute_pillar_scores(ev))
  expl_after <- sci_explain(ev)

  # Byte-identical: serialize and compare the raw vectors.
  expect_identical(serialize(res_before, NULL), serialize(res_after, NULL))
  expect_identical(serialize(expl_before, NULL), serialize(expl_after, NULL))
})
