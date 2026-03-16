# Classify SCI Value into Decision Band

Classify SCI Value into Decision Band

## Usage

``` r
classify_band(sci_value, bands = sci_config_default()$bands)
```

## Arguments

- sci_value:

  Numeric SCI score (0–100).

- bands:

  Named list of band boundaries from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

## Value

Character band name.

## Examples

``` r
classify_band(92)
#> [1] "ready"
classify_band(55)
#> [1] "conditional"
```
