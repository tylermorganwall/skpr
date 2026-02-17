# Generates Poisson Anticipated Coefficients

Generates Poisson Anticipated Coefficients Solves the Poisson link
function mean = exp(beta0 + beta1 \* x) such that mean = mean_low when x
= -1, and mean = mean_high when x = +1. Equivalently, solves this set of
equations for beta0 and beta1: mean_low = exp(beta0 - beta1) mean_high =
exp(beta0 + beta1)

## Usage

``` r
gen_poisson_anticoef(anticoef, mean_low, mean_high)
```

## Arguments

- anticoef:

  input anticipated coefficients of the proper length for your model
  matrix

- mean_low:

  The low value of the mean value

- mean_high:

  The high value of the mean value

## Value

Anticipated coefficients.
