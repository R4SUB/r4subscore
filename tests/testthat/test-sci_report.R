# Tests for sci_report() (R/sci_report.R)

mk_ev <- function(severity, result,
                  domain = c("quality", "quality", "trace", "risk", "usability"),
                  ids = c("Q1", "Q2", "T1", "R1", "U1")) {
  ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY1", "DEV"))
  suppressMessages(r4subcore::as_evidence(
    data.frame(
      asset_type = "validation", asset_id = "ADSL",
      source_name = "pinnacle21",
      indicator_id = ids,
      indicator_name = ids,
      indicator_domain = domain,
      severity = severity,
      result = result,
      message = paste0("msg-", ids),
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}

test_that("sci_report returns a well-formed report", {
  ev <- mk_ev(
    severity = c("high", "low", "medium", "high", "low"),
    result   = c("fail", "pass", "warn", "fail", "pass")
  )
  rpt <- sci_report(ev)
  expect_s3_class(rpt, "sci_report")
  expect_true(is.numeric(rpt$sci))
  expect_true(is.character(rpt$band))
  expect_equal(nrow(rpt$pillars), 4L)
  expect_setequal(rpt$pillars$pillar, c("quality", "trace", "risk", "usability"))
  expect_null(rpt$target)
  expect_s3_class(rpt$generated_at, "POSIXct")
})

test_that("gaps hold only failing and warning findings, worst first", {
  ev <- mk_ev(
    severity = c("high", "low", "medium", "critical", "low"),
    result   = c("fail", "pass", "warn", "fail", "pass")
  )
  rpt <- sci_report(ev)
  # only fail/warn rows -> Q1(fail), T1(warn), R1(fail) => 3
  expect_equal(rpt$n_gaps, 3L)
  expect_true(all(rpt$gaps$result %in% c("fail", "warn")))
  # critical fail must rank first
  expect_equal(rpt$gaps$indicator_id[1], "R1")
  expect_equal(rpt$gaps$severity[1], "critical")
})

test_that("top_n truncates the gap list but n_gaps keeps the true total", {
  ev <- mk_ev(
    severity = rep("high", 5),
    result   = rep("fail", 5)
  )
  rpt <- sci_report(ev, top_n = 2)
  expect_equal(nrow(rpt$gaps), 2L)
  expect_equal(rpt$n_gaps, 5L)
})

test_that("no findings yields an empty gaps table", {
  ev <- mk_ev(
    severity = rep("low", 5),
    result   = rep("pass", 5)
  )
  rpt <- sci_report(ev)
  expect_equal(rpt$n_gaps, 0L)
  expect_equal(nrow(rpt$gaps), 0L)
})

test_that("levers without a target are ranked by headroom", {
  ev <- mk_ev(
    severity = c("high", "low", "low", "low", "low"),
    result   = c("fail", "pass", "pass", "pass", "pass")
  )
  rpt <- sci_report(ev)
  expect_true(all(c("pillar", "weight", "current", "max_lift") %in% names(rpt$levers)))
  lifts <- rpt$levers$max_lift[!is.na(rpt$levers$max_lift)]
  expect_false(is.unsorted(rev(lifts)))
  # quality has a failing indicator and the largest weight -> top lever
  expect_equal(rpt$levers$pillar[1], "quality")
})

test_that("a target produces a gap, met flag, and lift-ranked levers", {
  ev <- mk_ev(
    severity = c("medium", "low", "medium", "medium", "low"),
    result   = c("warn", "pass", "warn", "warn", "pass")
  )
  rpt <- sci_report(ev, targets = "ready")
  expect_false(is.null(rpt$target))
  expect_equal(rpt$target$target_band, "ready")
  expect_equal(rpt$target$target_sci, 85)
  expect_true(is.logical(rpt$target$met))
  expect_true("max_lift" %in% names(rpt$levers))
  lifts <- rpt$levers$max_lift[!is.na(rpt$levers$max_lift)]
  expect_false(is.unsorted(rev(lifts)))
})

test_that("an open critical finding caps a high band down", {
  # quality/trace/risk all pass at info severity (score 1.0 -> 0.85*100 = SCI 85,
  # raw band ready); one critical fail in usability triggers the cap. info
  # severity is used so a pass scores a full 1.0 (a low-severity pass scores 0.75).
  ev <- mk_ev(
    severity = c("info", "info", "info", "info", "critical"),
    result   = c("pass", "pass", "pass", "pass", "fail")
  )
  rpt <- sci_report(ev)
  expect_equal(rpt$n_critical, 1L)
  expect_true(rpt$gated)
  expect_equal(rpt$band, "conditional")
})

test_that("print runs and reports the key sections", {
  # cli writes to stderr, so capture with type = "message" (package convention
  # tests print with expect_invisible / expect_no_error).
  ev1 <- mk_ev(severity = c("info","info","info","info","critical"),
               result = c("pass","pass","pass","pass","fail"))
  expect_invisible(print(sci_report(ev1)))
  out_gated <- capture.output(print(sci_report(ev1)), type = "message")
  expect_true(any(grepl("Submission Readiness Report", out_gated)))
  expect_true(any(grepl("capped", out_gated)))

  ev2 <- mk_ev(severity = rep("low",5), result = rep("pass",5))
  out_t <- capture.output(print(sci_report(ev2, targets = "ready")), type = "message")
  expect_true(any(grepl("target", out_t)))

  out_clean <- capture.output(print(sci_report(ev2)), type = "message")
  expect_true(any(grepl("No failing or warning findings", out_clean)))
})

test_that("works on the bundled pharma evidence when available", {
  skip_if_not_installed("r4subdata")
  ev <- r4subdata::evidence_pharma
  rpt <- sci_report(ev, targets = "ready")
  expect_s3_class(rpt, "sci_report")
  expect_true(rpt$sci >= 0 && rpt$sci <= 100)
  expect_equal(nrow(rpt$pillars), 4L)
})
