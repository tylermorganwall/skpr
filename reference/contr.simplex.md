# Orthonormal Contrast Generator

Generates orthonormal (orthogonal and normalized) contrasts. Each row is
the vertex of an N-dimensional simplex. The only exception are contrasts
for the 2-level case, which return 1 and -1.

## Usage

``` r
contr.simplex(n, size = NULL)
```

## Arguments

- n:

  The number of levels in the catagorical variable. If this is a factor
  or character vector, `n` will be `length(n)`

- size:

  Default `1`. The length of the simplex vector.

## Value

A matrix of Orthonormal contrasts.

## Examples

``` r
contr.simplex(4)
#>            [,1]       [,2]      [,3]
#> [1,]  1.7320508  0.0000000  0.000000
#> [2,] -0.5773503  1.6329932  0.000000
#> [3,] -0.5773503 -0.8164966  1.414214
#> [4,] -0.5773503 -0.8164966 -1.414214
```
