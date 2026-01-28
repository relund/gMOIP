# Add 2D discrete points to a non-dominated set and classify them into extreme supported, non-extreme supported, non-supported.

Add 2D discrete points to a non-dominated set and classify them into
extreme supported, non-extreme supported, non-supported.

## Usage

``` r
addNDSet2D(pts, nDSet = NULL, crit = "max", keepDom = FALSE)
```

## Arguments

- pts:

  A data frame. It is assumed that z1 and z2 are in the two first
  columns.

- nDSet:

  A data frame with current non-dominated set (NULL is none yet).

- crit:

  Either max or min.

- keepDom:

  Keep dominated points.

## Value

A data frame with columns z1 and z2, `nD` (non-dominated), `ext`
(extreme), `nonExt` (non-extreme supported).

## Author

Lars Relund <lars@relund.dk>

## Examples

``` r
nDSet <- data.frame(z1=c(12,14,16,18), z2=c(18,16,12,4))
pts <- data.frame(z1 = c(18,18,14,15,15), z2=c(2,6,14,14,16))
addNDSet2D(pts, nDSet, crit = "max")
#>   z1 z2   nD   ext nonExt
#> 6 12 18 TRUE  TRUE  FALSE
#> 5 15 16 TRUE  TRUE  FALSE
#> 8 16 12 TRUE FALSE  FALSE
#> 2 18  6 TRUE  TRUE  FALSE
addNDSet2D(pts, nDSet, crit = "max", keepDom = TRUE)
#>   z1 z2    nD   ext nonExt
#> 6 12 18  TRUE  TRUE  FALSE
#> 5 15 16  TRUE  TRUE  FALSE
#> 7 14 16 FALSE FALSE  FALSE
#> 4 15 14 FALSE FALSE  FALSE
#> 3 14 14 FALSE FALSE  FALSE
#> 8 16 12  TRUE FALSE  FALSE
#> 2 18  6  TRUE  TRUE  FALSE
#> 9 18  4 FALSE FALSE  FALSE
#> 1 18  2 FALSE FALSE  FALSE
addNDSet2D(pts, nDSet, crit = "min")
#>   z1 z2   nD   ext nonExt
#> 1 18  2 TRUE  TRUE  FALSE
#> 8 16 12 TRUE FALSE  FALSE
#> 3 14 14 TRUE FALSE  FALSE
#> 6 12 18 TRUE  TRUE  FALSE
```
