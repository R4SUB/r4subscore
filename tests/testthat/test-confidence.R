# Tests for the SCI bootstrap confidence interval (R/confidence.R)

test_that("confidence interval brackets the point estimate", {
  ev <- make_test_evidence()
  ci <- sci_confidence_interval(ev, n_boot = 300, seed = 1)

  expect_s3_class(ci, "sci_ci")
  expect_lte(ci$lower, ci$SCI)
  expect_gte(ci$upper, ci$SCI)
  expect_true(ci$lower >= 0 && ci$upper <= 100)
  expect_length(ci$replicates, 300)
})

test_that("a fixed seed is reproducible", {
  ev <- make_test_evidence()
  a <- sci_confidence_interval(ev, n_boot = 200, seed = 42)
  b <- sci_confidence_interval(ev, n_boot = 200, seed = 42)

  expect_equal(a$lower, b$lower)
  expect_equal(a$upper, b$upper)
  expect_identical(a$replicates, b$replicates)
})

test_that("the evidence resampling scheme also yields a valid interval", {
  ev <- make_test_evidence()
  ci <- sci_confidence_interval(ev, n_boot = 200, by = "evidence", seed = 7)

  expect_equal(ci$by, "evidence")
  expect_lte(ci$lower, ci$upper)
  expect_true(ci$lower >= 0 && ci$upper <= 100)
})

test_that("a wider level gives an interval at least as wide", {
  ev <- make_test_evidence()
  narrow <- sci_confidence_interval(ev, level = 0.80, n_boot = 500, seed = 3)
  wide   <- sci_confidence_interval(ev, level = 0.99, n_boot = 500, seed = 3)

  expect_gte(wide$upper - wide$lower, narrow$upper - narrow$lower)
})

test_that("format_sci_ci matches the compact report form", {
  ev <- make_test_evidence()
  ci <- sci_confidence_interval(ev, n_boot = 200, seed = 1)
  txt <- format_sci_ci(ci)

  expect_match(txt, "^SCI = [0-9.]+ \\[[0-9.]+, [0-9.]+\\] \\(95% CI\\)$")
  expect_error(format_sci_ci(list()), "sci_ci")
})

test_that("print returns its input invisibly", {
  ev <- make_test_evidence()
  ci <- sci_confidence_interval(ev, n_boot = 100, seed = 1)
  expect_invisible(print(ci))
})

test_that("inputs are validated", {
  ev <- make_test_evidence()
  expect_error(sci_confidence_interval(ev, level = 1.5), "level")
  expect_error(sci_confidence_interval(ev, level = 0), "level")
  expect_error(sci_confidence_interval(ev, n_boot = 0), "n_boot")
})
