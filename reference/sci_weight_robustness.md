# Weight Robustness Diagnostic for the Decision Band

Auto-perturbs the pillar weights around the current configuration and
reports the smallest weight change that flips the reported decision
`band`. It answers a single question: *is our band robust to the weights
we chose?*

## Usage

``` r
sci_weight_robustness(
  evidence,
  config = sci_config_default(),
  step = 0.005,
  max_shift = 0.5,
  methods = c("oat", "transfer"),
  n_critical = NULL
)
```

## Arguments

- evidence:

  A validated evidence data.frame.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).
  Its `pillar_weights` define the point the scan perturbs around.

- step:

  Positive numeric grid resolution for the shift search (default
  `0.005`).

- max_shift:

  Largest shift magnitude to explore, in weight units (default `0.5`).

- methods:

  Character vector choosing the perturbation families to run: any of
  `"oat"` and `"transfer"` (both by default).

- n_critical:

  Number of open critical findings. When `NULL` (the default) it is
  taken from the evidence through
  [`compute_pillar_scores()`](https://r4sub.github.io/r4subscore/reference/compute_pillar_scores.md),
  exactly as in a standard run. It is held fixed across all
  perturbations.

## Value

A list of class `"sci_weight_robustness"` with:

- `baseline_sci`: the unperturbed SCI (numeric 0–100)

- `baseline_band`: the unperturbed decision band

- `weights`: named numeric vector of the weights scanned around

- `min_shift`: smallest weight change (any move) that flips the band, or
  `NA` when no flip was found within `max_shift`

- `robust`: `TRUE` when no flip was found within `max_shift`

- `moves`: a tidy tibble, one row per move tested, with columns
  `method`, `move`, `pillar`, `to`, `direction`, `min_shift`,
  `new_band`, and `flips`

- `max_shift`, `step`: the search settings used

## Details

This is a **robustness diagnostic**, not a calibration or validation of
the weights. It does not say which weights are correct, nor does it
propose new ones. It only measures how far the current weights can move
before the decision band would change, so you know whether the band is
sitting safely inside its weight neighbourhood or balanced on a knife
edge.

The scan reuses the existing
[`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.md)
and
[`classify_band()`](https://r4sub.github.io/r4subscore/reference/classify_band.md)
exactly. Pillar scores depend only on the evidence (not on the weights),
so they are computed once from `config` and held fixed; only the pillar
weights are perturbed. The number of open critical findings is held
fixed too, so the non-compensatory gate behaves exactly as in the
unperturbed run.

Two families of perturbation are explored:

- **One-at-a-time** (`"oat"`): each pillar's weight is nudged up and
  down by a growing shift, with the remaining weights rescaled
  proportionally so the vector still sums to 1. The reported magnitude
  is the absolute change in the nudged pillar's weight.

- **Transfer** (`"transfer"`): a small mass is moved along the simplex
  from one pillar to another (a directed pairwise step). The reported
  magnitude is the mass moved.

For every move the scan grows the shift from `step` up to `max_shift`
and records the first magnitude at which the band flips, if any.

## See also

[`sci_sensitivity_analysis()`](https://r4sub.github.io/r4subscore/reference/sci_sensitivity_analysis.md)
for scoring explicit weight scenarios.

## Examples

``` r
ctx <- r4subcore::r4sub_run_context(study_id = "DEMO", environment = "DEV")
#> ℹ Run context created: "R4S-20261115072749-6glbznel"
ev <- r4subcore::as_evidence(
  data.frame(
    asset_type = "dataset", asset_id = "ADSL", source_name = "demo",
    source_version = NA_character_,
    indicator_id = c("Q1", "T1", "R1", "U1"),
    indicator_name = c("q", "t", "r", "u"),
    indicator_domain = c("quality", "trace", "risk", "usability"),
    severity = c("high", "low", "medium", "low"),
    result = c("warn", "pass", "pass", "pass"),
    metric_value = NA_real_, metric_unit = NA_character_,
    message = NA_character_, location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  ),
  ctx = ctx
)
#> ✔ Evidence table created: 4 rows

rob <- sci_weight_robustness(ev)
rob
#> 
#> ── SCI weight robustness (diagnostic) 
#> ℹ Baseline SCI 46.9, band "high_risk".
#> ! Smallest weight change that flips the band: 0.035.
#> ℹ   quality -> trace (shift 0.035) -> unclassified
#> ℹ   quality -> usability (shift 0.035) -> unclassified
#> ℹ   decrease quality (shift 0.045) -> unclassified
#> Diagnostic only: this probes band stability, not whether the weights are
#> correct.
rob$min_shift
#> [1] 0.035
```
