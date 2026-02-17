# Interpolate points within a convex hull

Interpolate points within a convex hull

## Usage

``` r
interpolate_convex_hull(points, ch_halfspace, n_samples_per_dimension = 11)
```

## Arguments

- points:

  A numeric matrix (n x d).

- ch_halfspace:

  Convex hull. Output of `convhull_halfspace`

- n_samples_per_dimension:

  Number of samples per dimension (pre filtering) to take

## Value

A list with:

- data: The interpolated points

- on_edge: A logical vector indicating whether the points
