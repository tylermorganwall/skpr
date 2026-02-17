# Generates Anticipated Coefficients from delta

Generates Anticipated Coefficients from delta parameter The logic for
generating anticipated coefficients from delta varies with glm family.

## Usage

``` r
anticoef_from_delta(default_coef, delta, glmfamily)
```

## Arguments

- default_coef:

  a vector of default coefficients, from gen_anticoef

- delta:

  the user-input delta parameter, must be a numeric vector of length 1
  or 2

- glmfamily:

  the user-supplied glmfamily, either a string or a glmfamily object

## Value

Anticipated coefficients.
