# Convert Block Column to Rownames

Detect externally generated blocking columns and convert to rownames

## Usage

``` r
convert_blockcolumn_rownames(design, blocking, varianceratios, verbose = FALSE)
```

## Arguments

- design:

  The run matrix

- blocking:

  Whether random effects should be included.

- varianceratios:

  A vector of variance ratios for each level of restricted randomization

## Value

Row-name encoded blocked run matrix
