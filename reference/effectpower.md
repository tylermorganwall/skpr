# Calculate Effect Power

Calculates the effect power given the anticipated coefficients and the
type-I error

## Usage

``` r
effectpower(
  RunMatrix,
  levelvector,
  anticoef,
  alpha,
  vinv = NULL,
  degrees = NULL
)
```

## Arguments

- RunMatrix:

  The model matrix

- levelvector:

  The number of levels in each parameter (1st is always the intercept)

- anticoef:

  The anticipated coefficients

- alpha:

  the specified type-I error

- vinv:

  The V inverse matrix

- degrees:

  Degrees of freedom

## Value

The effect power for the parameters
