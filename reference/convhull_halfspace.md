# Compute convex hull in half-space form

Compute convex hull in half-space form

## Usage

``` r
convhull_halfspace(points)
```

## Arguments

- points:

  A matrix or data frame of numeric coordinates, size n x d

## Value

A list with elements:

- A: a matrix of size m x d (m = number of facets)

- b: a length-m vector so that x is inside the hull if and only if A
  %\*% x + b \>= 0 (for all rows).

- volume: Total volume of the convex hull
