# r4subscore (development version)

- Add `sci_confidence_interval()`, a bootstrap confidence interval for the SCI
  so a readiness decision can account for measurement noise, with a
  `format_sci_ci()` helper for the compact `SCI = 82.0 [76.0, 88.0]` form and a
  print method that flags when the interval straddles two decision bands.
- Document confidence-interval interpretation in the scoring vignette.
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
