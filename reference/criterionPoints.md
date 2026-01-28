# Calculate the criterion points of a set of points and ranges to find the set of non-dominated points (Pareto points) and classify them into extreme supported, non-extreme supported, non-supported.

Calculate the criterion points of a set of points and ranges to find the
set of non-dominated points (Pareto points) and classify them into
extreme supported, non-extreme supported, non-supported.

## Usage

``` r
criterionPoints(pts, obj, crit, labels = "coord")
```

## Arguments

- pts:

  A data frame with a column for each variable in the solution space
  (can also be a `rangePoints`).

- obj:

  A p x n matrix(one row for each criterion).

- crit:

  Either `max` or `min`.

- labels:

  If `NULL` or "n" don't add any labels (empty string). If equals
  `coord`, labels are the solution space coordinates. Otherwise number
  all points from one based on the solution space points.

## Value

A data frame with columns
`x1, ..., xn, z1, ..., zp, lbl (label), nD (non-dominated), ext (extreme), nonExt (non-extreme supported)`.

## Author

Lars Relund <lars@relund.dk>

## Examples

``` r
A <- matrix( c(3, -2, 1, 2, 4, -2, -3, 2, 1), nc = 3, byrow = TRUE)
b <- c(10,12,3)
pts <- integerPoints(A, b)
obj <- matrix( c(1,-3,1,-1,1,-1), byrow = TRUE, ncol = 3 )
criterionPoints(pts, obj, crit = "max", labels = "numb")
#>     x1 x2 x3 z1 z2 lbl    nd    se   sne    us cls
#> 1    0  0  0  0  0   1  TRUE  TRUE FALSE FALSE  se
#> 2    1  0  0  1 -1   2  TRUE FALSE  TRUE FALSE sne
#> 3    1  0  0  1 -1   2  TRUE FALSE  TRUE FALSE sne
#> 4    2  0  0  2 -2   3  TRUE FALSE  TRUE FALSE sne
#> 5    2  0  0  2 -2   3  TRUE FALSE  TRUE FALSE sne
#> 6    2  0  0  2 -2   3  TRUE FALSE  TRUE FALSE sne
#> 7    3  0  0  3 -3   4  TRUE FALSE  TRUE FALSE sne
#> 8    3  0  0  3 -3   4  TRUE FALSE  TRUE FALSE sne
#> 9    3  0  0  3 -3   4  TRUE FALSE  TRUE FALSE sne
#> 10   3  0  0  3 -3   4  TRUE FALSE  TRUE FALSE sne
#> 11   0  1  0 -3  1   5  TRUE  TRUE FALSE FALSE  se
#> 12   1  1  0 -2  0   6 FALSE FALSE FALSE  TRUE   d
#> 13   1  1  0 -2  0   6 FALSE FALSE FALSE  TRUE   d
#> 14   2  1  0 -1 -1   7 FALSE FALSE FALSE  TRUE   d
#> 15   2  1  0 -1 -1   7 FALSE FALSE FALSE  TRUE   d
#> 16   3  1  0  0 -2   8 FALSE FALSE FALSE  TRUE   d
#> 17   3  1  0  0 -2   8 FALSE FALSE FALSE  TRUE   d
#> 18   3  1  0  0 -2   8 FALSE FALSE FALSE  TRUE   d
#> 19   4  1  0  1 -3   9 FALSE FALSE FALSE  TRUE   d
#> 20   4  1  0  1 -3   9 FALSE FALSE FALSE  TRUE   d
#> 21   4  1  0  1 -3   9 FALSE FALSE FALSE  TRUE   d
#> 22   4  1  0  1 -3   9 FALSE FALSE FALSE  TRUE   d
#> 23   1  2  0 -5  1  10 FALSE FALSE FALSE  TRUE   d
#> 24   2  2  0 -4  0  11 FALSE FALSE FALSE  TRUE   d
#> 25   2  2  0 -4  0  11 FALSE FALSE FALSE  TRUE   d
#> 26   0  0  1  1 -1  12  TRUE FALSE  TRUE FALSE sne
#> 27   0  0  1  1 -1  12  TRUE FALSE  TRUE FALSE sne
#> 28   1  0  1  2 -2  13  TRUE FALSE  TRUE FALSE sne
#> 29   1  0  1  2 -2  13  TRUE FALSE  TRUE FALSE sne
#> 30   1  0  1  2 -2  13  TRUE FALSE  TRUE FALSE sne
#> 31   2  0  1  3 -3  14  TRUE FALSE  TRUE FALSE sne
#> 32   2  0  1  3 -3  14  TRUE FALSE  TRUE FALSE sne
#> 33   2  0  1  3 -3  14  TRUE FALSE  TRUE FALSE sne
#> 34   2  0  1  3 -3  14  TRUE FALSE  TRUE FALSE sne
#> 35   3  0  1  4 -4  15  TRUE FALSE  TRUE FALSE sne
#> 36   3  0  1  4 -4  15  TRUE FALSE  TRUE FALSE sne
#> 37   3  0  1  4 -4  15  TRUE FALSE  TRUE FALSE sne
#> 38   0  1  1 -2  0  16 FALSE FALSE FALSE  TRUE   d
#> 39   0  1  1 -2  0  16 FALSE FALSE FALSE  TRUE   d
#> 40   1  1  1 -1 -1  17 FALSE FALSE FALSE  TRUE   d
#> 41   1  1  1 -1 -1  17 FALSE FALSE FALSE  TRUE   d
#> 42   2  1  1  0 -2  18 FALSE FALSE FALSE  TRUE   d
#> 43   2  1  1  0 -2  18 FALSE FALSE FALSE  TRUE   d
#> 44   2  1  1  0 -2  18 FALSE FALSE FALSE  TRUE   d
#> 45   3  1  1  1 -3  19 FALSE FALSE FALSE  TRUE   d
#> 46   3  1  1  1 -3  19 FALSE FALSE FALSE  TRUE   d
#> 47   3  1  1  1 -3  19 FALSE FALSE FALSE  TRUE   d
#> 48   3  1  1  1 -3  19 FALSE FALSE FALSE  TRUE   d
#> 49   1  2  1 -4  0  20 FALSE FALSE FALSE  TRUE   d
#> 50   1  2  1 -4  0  20 FALSE FALSE FALSE  TRUE   d
#> 51   2  2  1 -3 -1  21 FALSE FALSE FALSE  TRUE   d
#> 52   2  2  1 -3 -1  21 FALSE FALSE FALSE  TRUE   d
#> 53   3  2  1 -2 -2  22 FALSE FALSE FALSE  TRUE   d
#> 54   3  2  1 -2 -2  22 FALSE FALSE FALSE  TRUE   d
#> 55   0  0  2  2 -2  23  TRUE FALSE  TRUE FALSE sne
#> 56   0  0  2  2 -2  23  TRUE FALSE  TRUE FALSE sne
#> 57   0  0  2  2 -2  23  TRUE FALSE  TRUE FALSE sne
#> 58   1  0  2  3 -3  24  TRUE FALSE  TRUE FALSE sne
#> 59   1  0  2  3 -3  24  TRUE FALSE  TRUE FALSE sne
#> 60   1  0  2  3 -3  24  TRUE FALSE  TRUE FALSE sne
#> 61   1  0  2  3 -3  24  TRUE FALSE  TRUE FALSE sne
#> 62   2  0  2  4 -4  25  TRUE FALSE  TRUE FALSE sne
#> 63   2  0  2  4 -4  25  TRUE FALSE  TRUE FALSE sne
#> 64   2  0  2  4 -4  25  TRUE FALSE  TRUE FALSE sne
#> 65   1  1  2  0 -2  26 FALSE FALSE FALSE  TRUE   d
#> 66   1  1  2  0 -2  26 FALSE FALSE FALSE  TRUE   d
#> 67   1  1  2  0 -2  26 FALSE FALSE FALSE  TRUE   d
#> 68   2  1  2  1 -3  27 FALSE FALSE FALSE  TRUE   d
#> 69   2  1  2  1 -3  27 FALSE FALSE FALSE  TRUE   d
#> 70   2  1  2  1 -3  27 FALSE FALSE FALSE  TRUE   d
#> 71   2  1  2  1 -3  27 FALSE FALSE FALSE  TRUE   d
#> 72   3  1  2  2 -4  28 FALSE FALSE FALSE  TRUE   d
#> 73   3  1  2  2 -4  28 FALSE FALSE FALSE  TRUE   d
#> 74   3  1  2  2 -4  28 FALSE FALSE FALSE  TRUE   d
#> 75   1  2  2 -3 -1  29 FALSE FALSE FALSE  TRUE   d
#> 76   1  2  2 -3 -1  29 FALSE FALSE FALSE  TRUE   d
#> 77   2  2  2 -2 -2  30 FALSE FALSE FALSE  TRUE   d
#> 78   2  2  2 -2 -2  30 FALSE FALSE FALSE  TRUE   d
#> 79   3  2  2 -1 -3  31 FALSE FALSE FALSE  TRUE   d
#> 80   3  2  2 -1 -3  31 FALSE FALSE FALSE  TRUE   d
#> 81   4  2  2  0 -4  32 FALSE FALSE FALSE  TRUE   d
#> 82   4  2  2  0 -4  32 FALSE FALSE FALSE  TRUE   d
#> 83   4  2  2  0 -4  32 FALSE FALSE FALSE  TRUE   d
#> 84   2  3  2 -5 -1  33 FALSE FALSE FALSE  TRUE   d
#> 85   0  0  3  3 -3  34  TRUE FALSE  TRUE FALSE sne
#> 86   0  0  3  3 -3  34  TRUE FALSE  TRUE FALSE sne
#> 87   0  0  3  3 -3  34  TRUE FALSE  TRUE FALSE sne
#> 88   0  0  3  3 -3  34  TRUE FALSE  TRUE FALSE sne
#> 89   1  0  3  4 -4  35  TRUE FALSE  TRUE FALSE sne
#> 90   1  0  3  4 -4  35  TRUE FALSE  TRUE FALSE sne
#> 91   1  0  3  4 -4  35  TRUE FALSE  TRUE FALSE sne
#> 92   2  0  3  5 -5  36  TRUE FALSE  TRUE FALSE sne
#> 93   2  0  3  5 -5  36  TRUE FALSE  TRUE FALSE sne
#> 94   1  1  3  1 -3  37 FALSE FALSE FALSE  TRUE   d
#> 95   1  1  3  1 -3  37 FALSE FALSE FALSE  TRUE   d
#> 96   1  1  3  1 -3  37 FALSE FALSE FALSE  TRUE   d
#> 97   1  1  3  1 -3  37 FALSE FALSE FALSE  TRUE   d
#> 98   2  1  3  2 -4  38 FALSE FALSE FALSE  TRUE   d
#> 99   2  1  3  2 -4  38 FALSE FALSE FALSE  TRUE   d
#> 100  2  1  3  2 -4  38 FALSE FALSE FALSE  TRUE   d
#> 101  3  1  3  3 -5  39 FALSE FALSE FALSE  TRUE   d
#> 102  3  1  3  3 -5  39 FALSE FALSE FALSE  TRUE   d
#> 103  2  2  3 -1 -3  40 FALSE FALSE FALSE  TRUE   d
#> 104  2  2  3 -1 -3  40 FALSE FALSE FALSE  TRUE   d
#> 105  3  2  3  0 -4  41 FALSE FALSE FALSE  TRUE   d
#> 106  3  2  3  0 -4  41 FALSE FALSE FALSE  TRUE   d
#> 107  3  2  3  0 -4  41 FALSE FALSE FALSE  TRUE   d
#> 108  2  3  3 -4 -2  42 FALSE FALSE FALSE  TRUE   d
#> 109  3  3  3 -3 -3  43 FALSE FALSE FALSE  TRUE   d
#> 110  1  0  4  5 -5  44  TRUE FALSE  TRUE FALSE sne
#> 111  1  0  4  5 -5  44  TRUE FALSE  TRUE FALSE sne
#> 112  2  0  4  6 -6  45  TRUE FALSE  TRUE FALSE sne
#> 113  2  0  4  6 -6  45  TRUE FALSE  TRUE FALSE sne
#> 114  1  1  4  2 -4  46 FALSE FALSE FALSE  TRUE   d
#> 115  1  1  4  2 -4  46 FALSE FALSE FALSE  TRUE   d
#> 116  1  1  4  2 -4  46 FALSE FALSE FALSE  TRUE   d
#> 117  2  1  4  3 -5  47 FALSE FALSE FALSE  TRUE   d
#> 118  2  1  4  3 -5  47 FALSE FALSE FALSE  TRUE   d
#> 119  2  2  4  0 -4  48 FALSE FALSE FALSE  TRUE   d
#> 120  2  2  4  0 -4  48 FALSE FALSE FALSE  TRUE   d
#> 121  2  2  4  0 -4  48 FALSE FALSE FALSE  TRUE   d
#> 122  3  2  4  1 -5  49 FALSE FALSE FALSE  TRUE   d
#> 123  3  2  4  1 -5  49 FALSE FALSE FALSE  TRUE   d
#> 124  3  3  4 -2 -4  50 FALSE FALSE FALSE  TRUE   d
#> 125  4  3  4 -1 -5  51 FALSE FALSE FALSE  TRUE   d
#> 126  4  3  4 -1 -5  51 FALSE FALSE FALSE  TRUE   d
#> 127  1  0  5  6 -6  52  TRUE FALSE  TRUE FALSE sne
#> 128  1  0  5  6 -6  52  TRUE FALSE  TRUE FALSE sne
#> 129  2  1  5  4 -6  53 FALSE FALSE FALSE  TRUE   d
#> 130  2  2  5  1 -5  54 FALSE FALSE FALSE  TRUE   d
#> 131  2  2  5  1 -5  54 FALSE FALSE FALSE  TRUE   d
#> 132  3  2  5  2 -6  55 FALSE FALSE FALSE  TRUE   d
#> 133  3  3  5 -1 -5  56 FALSE FALSE FALSE  TRUE   d
#> 134  3  3  5 -1 -5  56 FALSE FALSE FALSE  TRUE   d
#> 135  1  0  6  7 -7  57  TRUE  TRUE FALSE FALSE  se
#> 136  2  1  6  5 -7  58 FALSE FALSE FALSE  TRUE   d
#> 137  3  3  6  0 -6  59 FALSE FALSE FALSE  TRUE   d
#> 138  4  4  6 -2 -6  60 FALSE FALSE FALSE  TRUE   d
```
