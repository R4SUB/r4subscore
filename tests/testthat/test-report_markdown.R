# Tests for report_markdown() (R/report_markdown.R)

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

test_that("report_markdown renders the SCI, band, and every pillar", {
  ev <- mk_ev(
    severity = c("high", "low", "medium", "high", "low"),
    result   = c("fail", "pass", "warn", "fail", "pass")
  )
  rpt <- sci_report(ev)
  md <- report_markdown(rpt)

  expect_type(md, "character")
  expect_length(md, 1L)

  # SCI and band appear
  expect_true(grepl("Submission Confidence Index", md, fixed = TRUE))
  expect_true(grepl(as.character(rpt$sci), md, fixed = TRUE))
  expect_true(grepl(rpt$band, md, fixed = TRUE))

  # every pillar name is present
  for (p in rpt$pillars$pillar) {
    expect_true(grepl(p, md, fixed = TRUE))
  }
})

test_that("the memo marks itself as decision support, not a filing", {
  ev <- mk_ev(
    severity = rep("low", 5),
    result   = rep("pass", 5)
  )
  md <- report_markdown(sci_report(ev))
  expect_true(grepl("decision-support", md, fixed = TRUE))
  expect_true(grepl("not a regulatory filing document", md, fixed = TRUE))
})

test_that("a clean report reports no findings", {
  ev <- mk_ev(
    severity = rep("low", 5),
    result   = rep("pass", 5)
  )
  md <- report_markdown(sci_report(ev))
  expect_true(grepl("No failing or warning findings", md, fixed = TRUE))
})

test_that("findings and the gate reason are rendered", {
  ev <- mk_ev(
    severity = c("info", "info", "info", "info", "critical"),
    result   = c("pass", "pass", "pass", "pass", "fail")
  )
  rpt <- sci_report(ev)
  md <- report_markdown(rpt)
  expect_true(grepl("Band capped", md, fixed = TRUE))
  expect_true(grepl("Findings to act on", md, fixed = TRUE))
  # the critical finding's id shows up in the findings table
  expect_true(grepl("U1", md, fixed = TRUE))
})

test_that("a target renders a target line", {
  ev <- mk_ev(
    severity = rep("low", 5),
    result   = rep("pass", 5)
  )
  md <- report_markdown(sci_report(ev, targets = "ready"))
  expect_true(grepl("Target", md, fixed = TRUE))
})

test_that("top_n truncation is noted in the memo", {
  ev <- mk_ev(
    severity = rep("high", 5),
    result   = rep("fail", 5)
  )
  md <- report_markdown(sci_report(ev, top_n = 2))
  expect_true(grepl("Showing the top 2 of 5", md, fixed = TRUE))
})
