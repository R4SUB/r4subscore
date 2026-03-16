# r4subscore

<!-- badges: start -->
[![R-CMD-check](https://github.com/R4SUB/r4subscore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R4SUB/r4subscore/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/r4subscore)](https://CRAN.R-project.org/package=r4subscore)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/r4subscore)](https://CRAN.R-project.org/package=r4subscore)
[![r-universe](https://r4sub.r-universe.dev/badges/r4subscore)](https://r4sub.r-universe.dev/r4subscore)
<!-- badges: end -->

**r4subscore** is the scoring and calibration engine of the R4SUB ecosystem. It converts standardized evidence (from `r4subcore` and companion packages) into a **Submission Confidence Index (SCI)** — a single 0–100 score with decision bands, explainability tables, and sensitivity analysis.

> Are we ready for regulatory submission — and how confident are we?

## Installation

```r
install.packages("r4subscore")
```

Development version:

```r
pak::pak(c("R4SUB/r4subcore", "R4SUB/r4subscore"))
```

## Quick Start

```r
library(r4subcore)
library(r4subscore)

pillar_scores <- compute_pillar_scores(ev)
sci           <- compute_sci(pillar_scores)

sci$SCI   # 0–100
sci$band  # "ready", "minor_gaps", "conditional", or "high_risk"
```

## SCI Decision Bands

| SCI | Band | Interpretation |
|---|---|---|
| 85–100 | `ready` | Ready for Submission |
| 70–84 | `minor_gaps` | Minor Gaps to Address |
| 50–69 | `conditional` | Conditional — Address Key Issues |
| 0–49 | `high_risk` | High Risk |

## Scoring Logic

1. Each evidence row gets a **weighted score**: `result_score × (1 − severity_weight)`
2. **Indicator scores** = mean weighted score per indicator
3. **Pillar scores** = mean indicator score per domain (quality, trace, risk, usability)
4. **SCI** = weighted sum of pillar scores × 100

## Key Functions

| Function | Purpose |
|---|---|
| `sci_config_default()` | Pillar weights and decision bands configuration |
| `classify_band()` | Classify an SCI value into a decision band |
| `compute_indicator_scores()` | Severity-weighted indicator-level scores |
| `compute_pillar_scores()` | Aggregate indicators into pillar scores |
| `compute_sci()` | Compute SCI (0–100) and band classification |
| `sci_sensitivity_analysis()` | SCI under alternative weight scenarios |
| `sci_explain()` | Top loss contributors and pillar breakdown |

## Integration with r4subprofile

```r
library(r4subprofile)
library(r4subscore)

prof <- submission_profile("FDA", "NDA")
cfg  <- profile_sci_config(prof)

pillar_scores <- compute_pillar_scores(ev, config = cfg)
sci           <- compute_sci(pillar_scores, config = cfg)
```

## License

MIT
