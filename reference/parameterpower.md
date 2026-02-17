# Calculates parameter power

Calculates parameter power

## Usage

``` r
parameterpower(
  RunMatrix,
  levelvector = NULL,
  anticoef,
  alpha,
  vinv = NULL,
  degrees = NULL,
  parameter_names
)
```

## Arguments

- RunMatrix:

  The run matrix

- levelvector:

  The number of levels in each parameter (1st is always the intercept)

- anticoef:

  The anticipated coefficients

- alpha:

  the specified type-I error

- vinv:

  The V inverse matrix

## Value

The parameter power for the parameters
