# Generates Anticipated Coefficients from delta for eval_design_suvival_mc

Generates Anticipated Coefficients from delta parameter The logic for
generating anticipated coefficients from delta varies with glm family.

## Usage

``` r
anticoef_from_delta_surv(default_coef, delta, distribution)
```

## Arguments

- default_coef:

  a vector of default coefficients, from gen_anticoef

- delta:

  the user-input delta parameter, must be a numeric vector of length 1
  or 2

- distribution:

  the user-supplied distribution, either a string or a survreg
  distribution object

## Value

Anticipated coefficients.
