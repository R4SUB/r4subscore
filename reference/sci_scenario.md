# SCI Scenario / What-If Analyzer

Recomputes the Submission Confidence Index for one or more
caller-specified hypothetical changes to the evidence and returns a tidy
comparison against the baseline: the baseline SCI, each scenario's SCI,
the delta, the band movement, and which indicators drove the change. Use
it to answer "what is the highest-leverage fix to raise our readiness
score?" purely mechanically.

## Usage

``` r
sci_scenario(evidence, scenarios, config = sci_config_default())
```

## Arguments

- evidence:

  A validated evidence data.frame (from `r4subcore`).

- scenarios:

  A single edit data frame, or a (optionally named) list of edit data
  frames. See Details for the edit-frame contract.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).
  Pass a profile configuration here for authority-calibrated weights and
  bands.

## Value

A list of class `"sci_scenario"` with:

- `baseline`: a list with `sci`, `band`, `gated`, `gate_reason`,
  `n_critical`

- `comparison`: a tibble, one row per scenario, with `scenario`,
  `baseline_sci`, `scenario_sci`, `delta_sci`, `baseline_band`,
  `scenario_band`, `band_changed`, `gated`, `n_indicators_changed`, and
  `n_rows_changed`

- `drivers`: a tidy tibble of the indicators whose earned points moved,
  with `scenario`, `indicator_id`, `indicator_name`, `indicator_domain`,
  `baseline_earned`, `scenario_earned`, and `delta_earned`, ordered
  within each scenario by `delta_earned` descending

- `config`: the configuration used

## Details

Each scenario describes hypothetical edits to the evidence - for example
"if indicator `Q1` flips from `fail` to `pass`" or "if these findings
were resolved". Every edit is **caller-supplied**: this function invents
no weights, thresholds, or scores. It only applies the requested edits
to a copy of the evidence and re-runs the existing scoring engine
([`compute_pillar_scores()`](https://r4sub.github.io/r4subscore/reference/compute_pillar_scores.md)
then
[`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.md))
on the modified table, so the scenario SCI is produced by exactly the
same math as the baseline.

A scenario is a data frame of edits with a required `indicator_id`
column and at least one of the scoring-input columns `result` and
`severity`. For each edit row, every evidence row carrying that
`indicator_id` has the supplied `result` and/or `severity` overwritten
(an `NA` leaves that field unchanged). The modified evidence is
re-validated with
[`r4subcore::validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.html),
so an invalid `result` or `severity` value raises an error rather than
being scored silently.

`scenarios` may be a single edit data frame (treated as one scenario
named `"scenario"`) or a named list of edit data frames (one named
comparison per element; unnamed elements are labelled `scenario_1`,
`scenario_2`, ...).

The drivers reuse
[`sci_decompose()`](https://r4sub.github.io/r4subscore/reference/sci_decompose.md):
each indicator's change in *earned* SCI points
(`scenario_earned - baseline_earned`) is reported, so the drivers of a
scenario sum to its `delta_sci` up to the usual one-decimal rounding.
This makes "which items drove it" an exact accounting of the move, not a
heuristic ranking.

## See also

[`sci_what_if()`](https://r4sub.github.io/r4subscore/reference/sci_what_if.md)
for a single-indicator score override,
[`sci_decompose()`](https://r4sub.github.io/r4subscore/reference/sci_decompose.md)
for the earned-points accounting the drivers reuse, and
[`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.md)
for the underlying score.

## Examples

``` r
ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY1", "DEV"))
ev <- suppressMessages(r4subcore::as_evidence(
  data.frame(
    asset_type = "validation", asset_id = "ADSL",
    source_name = "pinnacle21",
    indicator_id = c("Q1", "Q2", "T1", "R1", "U1"),
    indicator_name = c("Missing var", "Label", "Trace", "Risk", "Guide"),
    indicator_domain = c("quality", "quality", "trace", "risk", "usability"),
    severity = c("high", "low", "medium", "high", "low"),
    result = c("fail", "pass", "warn", "fail", "pass"),
    stringsAsFactors = FALSE
  ),
  ctx = ctx
))

sc <- sci_scenario(
  ev,
  scenarios = list(
    fix_quality = data.frame(indicator_id = "Q1", result = "pass"),
    fix_both    = data.frame(
      indicator_id = c("Q1", "R1"), result = c("pass", "pass")
    )
  )
)
sc
#> 
#> ── SCI scenario analysis ──
#> 
#> ℹ Baseline SCI 30.6 / 100   Band "high_risk"
#> 
#> ── fix_quality 
#> • SCI 35 (+4.4)
#> • Q1 (quality): +4.38 SCI points
#> 
#> ── fix_both 
#> • SCI 41.2 (+10.6)
#> • R1 (risk): +6.25 SCI points
#> • Q1 (quality): +4.38 SCI points
sc$comparison
#> # A tibble: 2 × 10
#>   scenario    baseline_sci scenario_sci delta_sci baseline_band scenario_band
#>   <chr>              <dbl>        <dbl>     <dbl> <chr>         <chr>        
#> 1 fix_quality         30.6         35         4.4 high_risk     high_risk    
#> 2 fix_both            30.6         41.2      10.6 high_risk     high_risk    
#> # ℹ 4 more variables: band_changed <lgl>, gated <lgl>,
#> #   n_indicators_changed <int>, n_rows_changed <int>
```
