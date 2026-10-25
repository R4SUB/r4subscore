# End-to-End Submission Readiness Report

Composes the pieces of an SCI analysis into a single report object: the
Submission Confidence Index and its decision band, the per-pillar
breakdown, the findings most worth acting on, and the pillars that offer
the biggest improvement. It is the one call that answers "where do we
stand and what do we fix next" from an evidence table.

## Usage

``` r
sci_report(
  evidence,
  config = sci_config_default(),
  targets = NULL,
  top_n = 10L
)
```

## Arguments

- evidence:

  A validated R4SUB evidence data.frame (from `r4subcore`).

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).
  Pass a profile configuration here for authority-calibrated weights and
  bands.

- targets:

  Optional readiness goal: a band name (for example `"ready"`), a
  numeric SCI, or an
  [`sci_targets()`](https://r4sub.github.io/r4subscore/reference/sci_targets.md)
  object. When supplied, the report includes the gap to target and
  whether it is met.

- top_n:

  Maximum number of findings to keep in `gaps`. Default `10`.

## Value

A list of class `"sci_report"` with elements `sci`, `band`, `gated`,
`gate_reason`, `n_critical`, `pillars` (data.frame: pillar, score,
weight, contribution), `gaps` (data.frame of the top failing and warning
findings, worst first), `n_gaps` (total findings before truncation),
`levers` (data.frame of pillars ranked by improvement opportunity),
`target` (a list with target_sci, target_band, gap, met when `targets`
is supplied, else `NULL`), and `generated_at`.

## Details

The report reuses
[`compute_pillar_scores()`](https://r4sub.github.io/r4subscore/reference/compute_pillar_scores.md)
and
[`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.md)
for the score and band (including the non-compensatory critical gate),
[`sci_explain()`](https://r4sub.github.io/r4subscore/reference/sci_explain.md)
for pillar contributions, and, when `targets` is supplied,
[`sci_gap_to_target()`](https://r4sub.github.io/r4subscore/reference/sci_gap_to_target.md)
for the distance to goal and the ranked pillar levers. When `targets` is
`NULL` the levers are ranked by headroom (`weight * (100 - current)`),
the SCI points a pillar could add.

The report is authority-agnostic. To produce an authority-calibrated
report, pass a profile configuration as `config`, for example
`config = r4subprofile::profile_sci_config("FDA", "NDA")`.

## Examples

``` r
ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY1", "DEV"))
ev <- suppressMessages(r4subcore::as_evidence(
  data.frame(
    asset_type = "validation", asset_id = "ADSL",
    source_name = "pinnacle21",
    indicator_id = c("Q1", "Q2", "T1", "R1", "U1"),
    indicator_name = c("Missing var", "Label", "Trace", "Risk", "Reviewer guide"),
    indicator_domain = c("quality", "quality", "trace", "risk", "usability"),
    severity = c("high", "low", "medium", "high", "low"),
    result = c("fail", "pass", "warn", "fail", "pass"),
    stringsAsFactors = FALSE
  ),
  ctx = ctx
))
rpt <- sci_report(ev, targets = "ready")
rpt
#> 
#> ── Submission Readiness Report ─────────────────────────────────────────────────
#> ℹ SCI: 30.6 / 100   Band: "high_risk"
#> ! 54.4 SCI points below target 85 (band "ready").
#> 
#> ── Pillars ──
#> 
#> • quality: 37.5 (weight 0.35, contributes 13.1)
#> • trace: 25 (weight 0.25, contributes 6.2)
#> • risk: 0 (weight 0.25, contributes 0)
#> • usability: 75 (weight 0.15, contributes 11.2)
#> 
#> ── Top gaps (3 findings) ──
#> 
#> • [high/fail] Q1 (quality)
#> • [high/fail] R1 (risk)
#> • [medium/warn] T1 (trace)
#> 
#> ── Biggest levers ──
#> 
#> • risk: up to 25 SCI points
#> • quality: up to 21.9 SCI points
#> • trace: up to 18.8 SCI points
```
