# Default SCI Configuration

Returns a configuration list with default pillar weights, decision
bands, and scoring parameters for the Submission Confidence Index.

## Usage

``` r
sci_config_default(
  pillar_weights = c(quality = 0.35, trace = 0.25, risk = 0.25, usability = 0.15),
  bands = list(ready = c(85, 100), minor_gaps = c(70, 84), conditional = c(50, 69),
    high_risk = c(0, 49))
)
```

## Arguments

- pillar_weights:

  Named numeric vector of weights for each pillar. Must sum to 1. Names
  must be a subset of `"quality"`, `"trace"`, `"risk"`, `"usability"`.

- bands:

  Named list of numeric length-2 vectors defining SCI band boundaries
  `c(lower, upper)`. Evaluated in order; first match wins.

## Value

A list of class `"sci_config"` with elements: `pillar_weights`, `bands`.

## Examples

``` r
cfg <- sci_config_default()
cfg$pillar_weights
#>   quality     trace      risk usability 
#>      0.35      0.25      0.25      0.15 
cfg$bands
#> $ready
#> [1]  85 100
#> 
#> $minor_gaps
#> [1] 70 84
#> 
#> $conditional
#> [1] 50 69
#> 
#> $high_risk
#> [1]  0 49
#> 

# Custom weights (must sum to 1)
sci_config_default(
  pillar_weights = c(quality = 0.40, trace = 0.20, risk = 0.30, usability = 0.10)
)
#> $pillar_weights
#>   quality     trace      risk usability 
#>       0.4       0.2       0.3       0.1 
#> 
#> $bands
#> $bands$ready
#> [1]  85 100
#> 
#> $bands$minor_gaps
#> [1] 70 84
#> 
#> $bands$conditional
#> [1] 50 69
#> 
#> $bands$high_risk
#> [1]  0 49
#> 
#> 
#> attr(,"class")
#> [1] "sci_config"
```
