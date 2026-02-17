# Calculate Degrees of Freedom

Calculates the degrees of freedom adjustment for models with random
effects from Penheiro and Bates, pg. 91

## Usage

``` r
calculate_degrees_of_freedom(
  run_matrix_processed,
  nointercept,
  model,
  contrasts
)
```

## Arguments

- run_matrix_processed:

  The design

- nointercept:

  Whether the intercept term is present

## Value

Vector of numbers indicating split plot layers
