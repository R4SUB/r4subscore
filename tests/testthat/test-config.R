test_that("sci_config_default returns expected structure", {
  cfg <- sci_config_default()
  expect_s3_class(cfg, "sci_config")
  expect_named(cfg, c("pillar_weights", "bands"))
  expect_equal(sum(cfg$pillar_weights), 1.0)
  expect_length(cfg$bands, 4)
})

test_that("sci_config_default validates weights sum to 1", {
  expect_error(
    sci_config_default(pillar_weights = c(quality = 0.5, trace = 0.5,
                                           risk = 0.5, usability = 0.5)),
    "sum to 1"
  )
})

test_that("sci_config_default rejects invalid pillar names", {
  expect_error(
    sci_config_default(pillar_weights = c(quality = 0.5, bogus = 0.5)),
    "Invalid pillar"
  )
})

test_that("sci_config_default accepts custom weights", {
  cfg <- sci_config_default(
    pillar_weights = c(quality = 0.4, trace = 0.2, risk = 0.3, usability = 0.1)
  )
  expect_equal(cfg$pillar_weights[["quality"]], 0.4)
  expect_equal(sum(cfg$pillar_weights), 1.0)
})

test_that("classify_band returns correct bands", {
  expect_equal(classify_band(92), "ready")
  expect_equal(classify_band(85), "ready")
  expect_equal(classify_band(75), "minor_gaps")
  expect_equal(classify_band(60), "conditional")
  expect_equal(classify_band(30), "high_risk")
  expect_equal(classify_band(0), "high_risk")
  expect_equal(classify_band(100), "ready")
})

test_that("classify_band rejects non-numeric input", {
  expect_error(classify_band("high"), "single numeric")
})

test_that("classify_band works with custom bands", {
  bands <- list(green = c(80, 100), yellow = c(50, 79), red = c(0, 49))
  expect_equal(classify_band(90, bands), "green")
  expect_equal(classify_band(60, bands), "yellow")
  expect_equal(classify_band(40, bands), "red")
})
