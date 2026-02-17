# Approximate continuous moment matrix over a numeric bounding box

Treats the min–max range of each numeric column as a bounding box and
samples points uniformly to approximate the integral of the outer
product of the model terms, weighted by whether the point is on the
edge.

## Usage

``` r
gen_momentsmatrix_constrained(
  candidate_set,
  formula = ~.,
  n_samples_per_dimension = 10,
  user_provided_high_res_candidateset = FALSE
)
```

## Arguments

- candidate_set:

  Candidate set

- formula:

  Default `~ .`. Model formula specifying the terms.

- n_samples_per_dimension:

  Default `10`. Number of samples to take per dimension when
  interpolating inside the convex hull.

## Value

A matrix of size `p x p` (where `p` is the number of columns in the
model matrix), approximating the continuous moment matrix.
