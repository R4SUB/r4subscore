test_that("sci_weight_robustness returns a tidy structured result", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  rob <- sci_weight_robustness(ev)

  expect_s3_class(rob, "sci_weight_robustness")
  expect_s3_class(rob$moves, "tbl_df")
  expect_true(all(c("method", "move", "pillar", "to", "direction",
                    "min_shift", "new_band", "flips") %in% names(rob$moves)))
  expect_type(rob$baseline_sci, "double")
  expect_type(rob$baseline_band, "character")
  expect_named(rob$weights, c("quality", "trace", "risk", "usability"))
})

test_that("baseline matches the standard pipeline", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  ps <- compute_pillar_scores(ev)
  base <- compute_sci(ps)

  rob <- sci_weight_robustness(ev)
  expect_equal(rob$baseline_sci, base$SCI)
  expect_equal(rob$baseline_band, base$band)
})

test_that("both perturbation families appear and can be selected", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  rob <- sci_weight_robustness(ev)
  expect_setequal(unique(rob$moves$method),
                  c("one_at_a_time", "transfer"))

  oat <- sci_weight_robustness(ev, methods = "oat")
  expect_setequal(unique(oat$moves$method), "one_at_a_time")
  # 4 pillars x 2 directions
  expect_equal(nrow(oat$moves), 8)

  tr <- sci_weight_robustness(ev, methods = "transfer")
  expect_setequal(unique(tr$moves$method), "transfer")
  # 4 pillars x 3 targets (ordered pairs)
  expect_equal(nrow(tr$moves), 12)
})

test_that("flips are consistent with min_shift and robust flag", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  rob <- sci_weight_robustness(ev, step = 0.01, max_shift = 0.6)

  # min_shift recorded exactly when a move flips
  expect_true(all(!is.na(rob$moves$min_shift) == rob$moves$flips))
  # every flipping shift is within the search range
  flipped <- rob$moves$min_shift[rob$moves$flips]
  if (length(flipped) > 0) {
    expect_true(all(flipped > 0 & flipped <= 0.6))
    expect_equal(rob$min_shift, min(flipped))
    expect_false(rob$robust)
  } else {
    expect_true(is.na(rob$min_shift))
    expect_true(rob$robust)
  }
})

test_that("a reported flip genuinely changes the band when applied", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  rob <- sci_weight_robustness(ev, step = 0.01, max_shift = 0.8)
  flips <- rob$moves[rob$moves$flips, , drop = FALSE]
  skip_if(nrow(flips) == 0, "no flip found to verify")

  ps <- compute_pillar_scores(ev)
  w0 <- stats::setNames(ps$weight, ps$pillar)

  row <- flips[which.min(flips$min_shift), ]
  w <- w0
  if (row$method == "transfer") {
    w[[row$pillar]] <- w0[[row$pillar]] - row$min_shift
    w[[row$to]]     <- w0[[row$to]] + row$min_shift
  } else {
    sgn <- if (row$direction == "increase") 1 else -1
    wp <- w0[[row$pillar]] + sgn * row$min_shift
    others <- setdiff(names(w0), row$pillar)
    w[[row$pillar]] <- wp
    w[others] <- w0[others] * (1 - wp) / (1 - w0[[row$pillar]])
  }
  ps$weight <- unname(w[ps$pillar])
  expect_equal(compute_sci(ps)$band, row$new_band)
  expect_false(identical(compute_sci(ps)$band, rob$baseline_band))
})

test_that("existing weights and baseline are unchanged by the diagnostic", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  before <- compute_sci(compute_pillar_scores(ev))
  rob <- sci_weight_robustness(ev)
  after <- compute_sci(compute_pillar_scores(ev))

  expect_equal(before$SCI, after$SCI)
  expect_equal(before$band, after$band)
  expect_equal(unname(rob$weights),
               sci_config_default()$pillar_weights[names(rob$weights)] |>
                 unname())
})

test_that("input validation is enforced", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  expect_error(sci_weight_robustness(ev, step = 0), "positive")
  expect_error(sci_weight_robustness(ev, max_shift = -1), "positive")
  expect_error(sci_weight_robustness(ev, step = 0.9, max_shift = 0.5),
               "exceed")
  expect_error(sci_weight_robustness(ev, config = list()), "sci_config")
})

test_that("print method is labelled a diagnostic and returns invisibly", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  rob <- sci_weight_robustness(ev)

  res <- withVisible(print(rob))
  expect_false(res$visible)
  expect_identical(res$value, rob)

  out <- c(
    capture.output(print(rob)),
    capture.output(print(rob), type = "message")
  )
  expect_true(any(grepl("diagnostic", out, ignore.case = TRUE)))
})
