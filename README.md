# r4subscore

**r4subscore** is the scoring and calibration engine of the R4SUB ecosystem.

It converts standardized evidence (from `r4subcore` and companion packages like `r4subtrace`) into:

- Indicator Scores (per-indicator 0--1)
- Pillar Scores (per-domain 0--1)
- Submission Confidence Index (SCI, 0--100)
- Decision Bands (Ready / Minor Gaps / Conditional / High Risk)
- Sensitivity Analysis (SCI stability under weight variations)
- Explainability Tables (which indicators drive SCI up/down)

It answers the executive question:

> Are we ready for regulatory submission -- and how confident are we?

## Installation

```r
pak::pak(c("R4SUB/r4subcore", "R4SUB/r4subscore"))
```

## Quick Start

```r
library(r4subcore)
library(r4subscore)

# assume ev is a validated evidence table
pillar_scores <- compute_pillar_scores(ev)
sci <- compute_sci(pillar_scores)

sci$SCI
sci$band
```

## Core Functions

| Function | Purpose |
|---|---|
| `sci_config_default()` | Pillar weights + decision bands config |
| `classify_band()` | Classify an SCI value into a decision band |
| `compute_indicator_scores()` | Severity-weighted indicator-level scores |
| `compute_pillar_scores()` | Aggregate indicators into pillar scores |
| `compute_sci()` | Compute SCI (0--100) + band classification |
| `sci_sensitivity_analysis()` | SCI under alternative weight scenarios |
| `sci_explain()` | Top loss contributors + pillar breakdown |

## SCI Bands

| SCI | Band | Interpretation |
|-----|------|----------------|
| 85--100 | `ready` | Ready for Submission |
| 70--84  | `minor_gaps` | Minor Gaps to Address |
| 50--69  | `conditional` | Conditional -- Address Key Issues |
| 0--49   | `high_risk` | High Risk |

## Scoring Logic

1. Each evidence row gets a **weighted score**: `result_score * (1 - severity_weight)`
2. Indicator scores = mean weighted score per indicator
3. Pillar scores = mean indicator score per domain
4. SCI = weighted sum of pillar scores x 100

## License

MIT
