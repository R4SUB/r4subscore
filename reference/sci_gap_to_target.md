# Measure the Gap Between a Scored Run and a Target

Compares a computed SCI result against an
[`sci_targets()`](https://r4sub.github.io/r4subscore/reference/sci_targets.md)
goal and reports the shortfall overall and per pillar. For each pillar
it also reports the SCI points that closing that pillar would add, so a
team can see which pillar is the biggest lever rather than guessing.

## Usage

``` r
sci_gap_to_target(x, targets = "ready", config = sci_config_default())
```

## Arguments

- x:

  An `sci_result` from
  [`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.md).

- targets:

  An `sci_targets` object, or a band name / numeric passed straight to
  [`sci_targets()`](https://r4sub.github.io/r4subscore/reference/sci_targets.md).

- config:

  An `sci_config`, used only when `targets` needs building.

## Value

A list of class `"sci_gap"` with `current_sci`, `target_sci`, `gap` (0
when met, otherwise the shortfall), `met` (logical), `band`,
`target_band`, and `pillars`: a data.frame with `pillar`, `weight`,
`current`, `target`, `gap`, `met`, `max_lift`, and `target_lift`.

## Details

Because the SCI is a weighted average, each pillar contributes
`weight * pillar_score` points to the 0–100 total. The headroom a pillar
offers is therefore `weight * (100 - current)`, reported as `max_lift`,
and the pillars are ordered by it so the largest opportunity is first.

## Examples

``` r
if (FALSE) { # \dontrun{
ps <- compute_pillar_scores(evidence)
res <- compute_sci(ps)
sci_gap_to_target(res, "ready")
} # }
```
