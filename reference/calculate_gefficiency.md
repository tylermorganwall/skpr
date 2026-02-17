# Calculate G Efficiency

Either calculates G-Efficiency by Monte Carlo sampling from the design
space (ignoring constraints), searching for the maximum point (slower
but higher quality), or by using a user-specified candidate set for the
design space (fastest).

## Usage

``` r
calculate_gefficiency(
  design,
  calculation_type = "random",
  randsearches = 10000,
  design_space_mm = NULL
)
```

## Value

Normalized run matrix
