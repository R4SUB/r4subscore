# Exact Additive Decomposition of the SCI

Breaks the reported Submission Confidence Index into the exact points
each pillar, and each indicator, earns and leaves on the table, so a
single score can be read as a reconciled waterfall rather than a black
box. Unlike
[`sci_explain()`](https://r4sub.github.io/r4subscore/reference/sci_explain.md),
which ranks loss contributions using the nominal pillar weights, this
decomposition uses the *effective* weights that
[`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.md)
actually applied, so the earned points sum back to the reported SCI even
when a pillar is missing and its weight has been redistributed.

## Usage

``` r
sci_decompose(evidence, config = sci_config_default())
```

## Arguments

- evidence:

  A validated evidence data.frame (from `r4subcore`).

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).
  Pass a profile configuration here for authority-calibrated weights and
  bands.

## Value

A list of class `"sci_decomposition"` with:

- `sci`: the reported SCI (rounded), after any band gate on the score's
  band (the numeric score itself is never gated)

- `sci_exact`: the unrounded weighted sum the earned points reconcile to

- `band`: the decision band, including any non-compensatory gate

- `residual`: `sci - sci_exact`, the rounding residual

- `pillars`: a tibble with `pillar`, `pillar_score` (0–1),
  `nominal_weight`, `effective_weight`, `weight_budget`, `earned`,
  `shortfall`, and `scored`

- `indicators`: a tibble with `indicator_id`, `indicator_name`,
  `indicator_domain`, `indicator_score`, `effective_weight`,
  `n_in_pillar`, `earned`, and `shortfall`, ordered by `earned`
  descending

## Details

The SCI is a weighted average of pillar scores scaled to 0–100:

`SCI = sum(effective_weight * pillar_score) * 100`

where the effective weights are the nominal `config` weights
renormalised over the pillars that actually have a score (see
[`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.md)).
This function reuses those exact effective weights rather than
recomputing them, so no scoring semantics are introduced or changed
here.

For each scored pillar it reports two quantities that partition the
pillar's weight budget (`effective_weight * 100`):

- `earned`: the points the pillar contributes to the SCI,
  `effective_weight * pillar_score * 100`.

- `shortfall`: the points the pillar leaves on the table,
  `effective_weight * (1 - pillar_score) * 100`.

`earned + shortfall` equals the pillar's weight budget, the earned
column sums to the (unrounded) SCI, and the shortfall column sums to
`100 - SCI`.

The same partition is pushed down to indicators. A pillar score is the
mean of its `k` scored indicators, so each scored indicator carries an
equal share `effective_weight / k` of the pillar's budget and earns
`effective_weight / k * indicator_score * 100`. Indicator earned points
sum to their pillar's earned points, and therefore to the SCI overall.
Pillars and indicators with no score (`NA`) are reported with `NA`
contributions and are excluded from the sums, exactly as they are
excluded from the SCI.

A tiny rounding residual (`sci_exact - sci`) can arise because the
reported SCI is rounded to one decimal place; it is returned as
`residual` and is at most 0.05 in magnitude.

## See also

[`sci_explain()`](https://r4sub.github.io/r4subscore/reference/sci_explain.md)
for loss-ranked contributions under nominal weights,
[`indicator_contributions()`](https://r4sub.github.io/r4subscore/reference/indicator_contributions.md)
for remediation headroom, and
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
dec <- sci_decompose(ev)
dec
#> 
#> ── SCI decomposition ──
#> 
#> ℹ SCI 30.6 / 100   Band "high_risk"
#> 
#> ── Pillars (earned / budget) 
#> • quality: 13.12 earned, 21.88 short (of 35)
#> • trace: 6.25 earned, 18.75 short (of 25)
#> • risk: 0 earned, 25 short (of 25)
#> • usability: 11.25 earned, 3.75 short (of 15)
#> 
#> ── Top earning indicators 
#> • Q2 (quality): 13.12 SCI points
#> • U1 (usability): 11.25 SCI points
#> • T1 (trace): 6.25 SCI points
#> • Q1 (quality): 0 SCI points
#> • R1 (risk): 0 SCI points
dec$pillars
#> # A tibble: 4 × 8
#>   pillar    pillar_score nominal_weight effective_weight weight_budget earned
#>   <chr>            <dbl>          <dbl>            <dbl>         <dbl>  <dbl>
#> 1 quality          0.375           0.35             0.35            35  13.1 
#> 2 trace            0.25            0.25             0.25            25   6.25
#> 3 risk             0               0.25             0.25            25   0   
#> 4 usability        0.75            0.15             0.15            15  11.2 
#> # ℹ 2 more variables: shortfall <dbl>, scored <lgl>
```
