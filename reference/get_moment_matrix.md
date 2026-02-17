# Gets or calculates the moment matrix, given a candidate set, a design, and a model

Returns number of levels prior to each parameter

## Usage

``` r
get_moment_matrix(
  design = NA,
  factors = NA,
  classvector = NA,
  candidate_set_normalized = NA,
  model = ~.,
  moment_sample_density = 10,
  high_resolution_candidate_set = NA
)
```

## Value

Returns a vector consisting of the number of levels preceeding each
parameter (including the intercept)
