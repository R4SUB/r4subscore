test_that("sci_sensitivity_analysis produces multiple scenarios", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  grid <- data.frame(
    quality   = c(0.40, 0.25, 0.25),
    trace     = c(0.20, 0.25, 0.25),
    risk      = c(0.30, 0.25, 0.25),
    usability = c(0.10, 0.25, 0.25)
  )

  result <- sci_sensitivity_analysis(ev, grid)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true(all(c("scenario", "SCI", "band") %in% names(result)))
})

test_that("sci_sensitivity_analysis SCI values are bounded", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  grid <- data.frame(
    quality   = c(0.40, 0.10),
    trace     = c(0.20, 0.30),
    risk      = c(0.30, 0.30),
    usability = c(0.10, 0.30)
  )

  result <- sci_sensitivity_analysis(ev, grid)

  expect_true(all(result$SCI >= 0 & result$SCI <= 100))
})

test_that("sci_sensitivity_analysis rejects weights not summing to 1", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  grid <- data.frame(
    quality = 0.5, trace = 0.5, risk = 0.5, usability = 0.5
  )

  expect_error(sci_sensitivity_analysis(ev, grid), "sum to 1")
})

test_that("sci_sensitivity_analysis rejects empty grid", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  expect_error(
    sci_sensitivity_analysis(ev, data.frame()),
    "non-empty"
  )
})

test_that("equal weights produce equal-weight SCI", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()

  grid <- data.frame(
    quality = 0.25, trace = 0.25, risk = 0.25, usability = 0.25
  )

  result <- sci_sensitivity_analysis(ev, grid)
  expect_equal(nrow(result), 1)
  expect_true(result$SCI[1] > 0)
})
