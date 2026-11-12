# r4subscore (development version)

- Add `sci_decompose()`, an exact additive decomposition of the reported SCI
  into the points each pillar, and each indicator, earns and leaves on the
  table. It reuses the effective (renormalised) weights that `compute_sci()`
  actually applied, so the earned points reconcile to the reported SCI even
  when a pillar is missing and its weight has been redistributed, which is
  where the nominal-weight contributions in `sci_explain()` do not add up. It
  reports a dual `earned`/`shortfall` split that partitions each pillar's
  weight budget and sums to 100 overall, and pushes the same split down to
  indicators. It introduces no new scoring semantics, weights, or thresholds;
  it only re-expresses the existing score as a reconciled waterfall.

- Add `sci_weight_robustness()`, a read-only diagnostic that auto-perturbs the
  pillar weights around the current configuration (one-at-a-time nudges and
  small simplex transfers) and reports the smallest weight change that flips
  the reported decision band. It reuses `compute_sci()` and `classify_band()`
  unchanged and holds the pillar scores and the critical-finding count fixed, so
  it only measures band stability. It is a robustness diagnostic, not a
  calibration or validation of the weights, and returns a tidy result with a
  print method.

- Add `report_markdown()`, which renders the object from `sci_report()` as a
  shareable Markdown readiness memo: the SCI and decision band, the distance to
  target, the per-pillar breakdown, the findings worth acting on, and the
  biggest levers, ready to paste into a pull request, issue, or status note. The
  memo is labelled decision support, not a regulatory filing document. The
  `sci_report()` object and all scoring, band, and gate logic are unchanged.

# r4subscore 0.4.1

- Complete the pkgdown reference index so the documentation site builds, and polish metadata for CRAN.

# r4subscore 0.4.0

- Add `sci_report()`, an end-to-end readiness report that composes the SCI, the
  decision band and its critical gate, the per-pillar breakdown, the findings
  most worth acting on, and the pillars that offer the biggest improvement into
  one object with a readable print method. Pass `targets` for a distance-to-goal
  section, or a profile configuration as `config` for an authority-calibrated
  report. This is the single call that answers "where do we stand and what do we
  fix next" from an evidence table.

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
