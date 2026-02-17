# Calculate V from Blocks

Calculates the V matrix from the block structure

## Usage

``` r
calculate_v_from_blocks(
  nrow_design,
  blockgroups,
  blockstructure,
  varianceratios
)
```

## Arguments

- nrow_design:

  The number of runs in the design

- blockgroups:

  List indicating the size of each block for each layer

- blockstructure:

  Matrix indicating the block structure from the rownames

## Value

Variance-Covariance Matrix
