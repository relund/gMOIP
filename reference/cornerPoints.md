# Calculate the corner points for the polytope Ax\<=b.

Calculate the corner points for the polytope Ax\<=b.

## Usage

``` r
cornerPoints(A, b, type = rep("c", ncol(A)), nonneg = rep(TRUE, ncol(A)))
```

## Arguments

- A:

  Constraint matrix.

- b:

  Right hand side.

- type:

  A character vector of same length as number of variables. If entry k
  is 'i' variable \\k\\ must be integer and if 'c' continuous.

- nonneg:

  A boolean vector of same length as number of variables. If entry k is
  TRUE then variable k must be non-negative.

## Value

A data frame with a corner point in each row.

## Author

Lars Relund <lars@relund.dk>

## Examples

``` r
A <- matrix( c(3,-2, 1, 2, 4,-2,-3, 2, 1), nc = 3, byrow = TRUE)
b <- c(10, 12, 3)
cornerPoints(A, b, type = c("c", "c", "c"))
#>            x1    x2  x3
#> [1,] 4.000000 4.250 6.5
#> [2,] 1.166667 0.000 6.5
#> [3,] 0.000000 0.000 3.0
#> [4,] 4.000000 1.000 0.0
#> [5,] 0.750000 2.625 0.0
#> [6,] 0.000000 1.500 0.0
#> [7,] 3.333333 0.000 0.0
#> [8,] 0.000000 0.000 0.0
cornerPoints(A, b, type = c("i", "i", "i"))
#>       x1 x2 x3
#>  [1,]  2  0  4
#>  [2,]  2  1  6
#>  [3,]  1  0  6
#>  [4,]  2  3  2
#>  [5,]  2  2  0
#>  [6,]  1  2  0
#>  [7,]  3  0  1
#>  [8,]  3  0  0
#>  [9,]  4  1  0
#> [10,]  2  3  3
#> [11,]  4  4  6
#> [12,]  3  3  6
#> [13,]  0  1  0
#> [14,]  0  0  0
#> [15,]  0  0  3
#> [16,]  0  1  1
cornerPoints(A, b, type = c("i", "c", "c"))
#>       x1   x2  x3
#>  [1,]  0 0.00 0.0
#>  [2,]  0 1.50 0.0
#>  [3,]  0 0.00 3.0
#>  [4,]  1 2.50 0.0
#>  [5,]  1 2.75 0.5
#>  [6,]  2 0.00 4.0
#>  [7,]  1 0.00 6.0
#>  [8,]  2 1.25 6.5
#>  [9,]  3 0.00 1.0
#> [10,]  3 0.00 0.0
#> [11,]  4 1.00 0.0
#> [12,]  4 4.25 6.5
```
