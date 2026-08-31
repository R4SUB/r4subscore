# r4subscore 0.3.0

- Add `sci_targets()` and `sci_gap_to_target()`. The first records the readiness
  goal a submission is aiming for, either a band name such as `"ready"` or a
  numeric SCI, with optional per-pillar minimums. The second measures how far a
  scored run is from that goal and, for each pillar, reports the SCI points that
  closing it would add, ordered so the biggest lever comes first. Where
  `sci_diff()` answers "are we moving in the right direction", this answers "how
  far are we from filing, and what should we fix next".

# r4subscore 0.2.1

- Add `sci_snapshot()`, `sci_diff()`, and `sci_snapshot_history()` for tracking
  readiness over time. A snapshot records the SCI, band, and pillar and indicator
  scores for one run under a label; `sci_diff()` reports what moved between two
  runs (overall SCI change, band change, per-pillar deltas, and which indicators
  improved, regressed, were added, or removed); and `sci_snapshot_history()`
  binds a series into a tidy table for a trend plot. This turns the SCI from a
  single-run report card into a way to answer whether a submission is on
  trajectory to file.

# r4subscore 0.2.0

- Add a non-compensatory gate to the SCI. An open critical finding (severity
  `critical`, result `fail`) now caps the reported band at `conditional` by
  default, so a strong score in one pillar cannot wash out a critical failure in
  another. The numeric SCI is left unchanged; only the band is capped, and
  `compute_sci()` records `n_critical`, `gated`, and a `gate_reason`. The gate is
  configurable through `sci_config_default(gate = ...)` and applies automatically
  through the standard pipeline. The default is a starting point sponsors should
  calibrate, not validated truth.
- Add `indicator_contributions()`, which breaks a pillar score down into
  per-indicator contributions, ranks indicators by how many SCI points a fix
  could recover, and attaches a short remediation hint to each one.
- Add `sci_what_if()`, which recomputes the pillar score and the SCI exactly as
  if one indicator had a different score, for planning remediation.
- Add `sci_confidence_interval()`, a bootstrap confidence interval for the SCI
  so a readiness decision can account for measurement noise, with a
  `format_sci_ci()` helper for the compact `SCI = 82.0 [76.0, 88.0]` form and a
  print method that flags when the interval straddles two decision bands.
- Document confidence-interval interpretation in the scoring vignette.
- Add vignette: "Case study: scoring a CDISC pilot for submission readiness", a
  worked walkthrough of the SCI on the example pharma evidence from `r4subdata`.
- Add validation helpers: `sci_degradation_curve()` records how the SCI responds
  as passing checks are turned into failures, `sci_monotone_check()` confirms a
  curve never rises, and `conformance_findings()` produces an independent
  severity-weighted finding count for concurrent-validity checks.
- Add vignette: "Validating the Submission Confidence Index", covering construct,
  discriminant, concurrent, and robustness validity, and documenting a
  calibration finding about how severity discounts passing credit.
- Clarified the package DESCRIPTION: "R4SUB" expands to "Ready for Submission"
  (previously "R for Regulatory Submission", inconsistent with the rest of the
  ecosystem).

# r4subscore 0.1.1

- Add vignette: "Submission Confidence Index (SCI) Scoring" covering
  `compute_pillar_scores()`, `compute_sci()`, `sci_explain()`,
  `sci_sensitivity_analysis()`, and custom weight configuration.

# r4subscore 0.1.0

- Initial CRAN release.
