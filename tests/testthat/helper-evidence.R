# Shared test helper: create a small evidence table
make_test_evidence <- function() {
  ctx <- r4subcore::r4sub_run_context(study_id = "TEST001", environment = "DEV")

  ev <- data.frame(
    asset_type       = "dataset",
    asset_id         = rep("ADSL", 8),
    source_name      = "test_source",
    source_version   = NA_character_,
    indicator_id     = c("Q1", "Q1", "Q2", "T1", "T2", "R1", "R2", "U1"),
    indicator_name   = c("Quality Check 1", "Quality Check 1",
                         "Quality Check 2", "Trace Check 1",
                         "Trace Check 2", "Risk Check 1",
                         "Risk Check 2", "Usability Check 1"),
    indicator_domain = c("quality", "quality", "quality",
                         "trace", "trace",
                         "risk", "risk",
                         "usability"),
    severity         = c("high", "low", "medium",
                         "low", "info",
                         "high", "medium",
                         "low"),
    result           = c("fail", "pass", "pass",
                         "pass", "pass",
                         "warn", "pass",
                         "pass"),
    metric_value     = NA_real_,
    metric_unit      = NA_character_,
    message          = NA_character_,
    location         = NA_character_,
    evidence_payload = "{}",
    stringsAsFactors = FALSE
  )

  r4subcore::as_evidence(ev, ctx = ctx)
}
