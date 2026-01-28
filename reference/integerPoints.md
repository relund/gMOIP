# Integer points in the feasible region (Ax\<=b).

Integer points in the feasible region (Ax\<=b).

## Usage

``` r
integerPoints(A, b, nonneg = rep(TRUE, ncol(A)))
```

## Arguments

- A:

  Constraint matrix.

- b:

  Right hand side.

- nonneg:

  A boolean vector of same length as number of variables. If entry k is
  TRUE then variable k must be non-negative.

## Value

A data frame with all integer points inside the feasible region.

## Note

Do a simple enumeration of all integer points between min and max values
found using the continuous polytope.

## Author

Lars Relund <lars@relund.dk>.

## Examples

``` r
A <- matrix( c(3,-2, 1, 2, 4,-2,-3, 2, 1), nc = 3, byrow = TRUE)
b <- c(10, 12, 3)
integerPoints(A, b)
#>       x1 x2 x3
#>  [1,]  0  0  0
#>  [2,]  1  0  0
#>  [3,]  2  0  0
#>  [4,]  3  0  0
#>  [5,]  0  1  0
#>  [6,]  1  1  0
#>  [7,]  2  1  0
#>  [8,]  3  1  0
#>  [9,]  4  1  0
#> [10,]  1  2  0
#> [11,]  2  2  0
#> [12,]  0  0  1
#> [13,]  1  0  1
#> [14,]  2  0  1
#> [15,]  3  0  1
#> [16,]  0  1  1
#> [17,]  1  1  1
#> [18,]  2  1  1
#> [19,]  3  1  1
#> [20,]  1  2  1
#> [21,]  2  2  1
#> [22,]  3  2  1
#> [23,]  0  0  2
#> [24,]  1  0  2
#> [25,]  2  0  2
#> [26,]  1  1  2
#> [27,]  2  1  2
#> [28,]  3  1  2
#> [29,]  1  2  2
#> [30,]  2  2  2
#> [31,]  3  2  2
#> [32,]  4  2  2
#> [33,]  2  3  2
#> [34,]  0  0  3
#> [35,]  1  0  3
#> [36,]  2  0  3
#> [37,]  1  1  3
#> [38,]  2  1  3
#> [39,]  3  1  3
#> [40,]  2  2  3
#> [41,]  3  2  3
#> [42,]  2  3  3
#> [43,]  3  3  3
#> [44,]  1  0  4
#> [45,]  2  0  4
#> [46,]  1  1  4
#> [47,]  2  1  4
#> [48,]  2  2  4
#> [49,]  3  2  4
#> [50,]  3  3  4
#> [51,]  4  3  4
#> [52,]  1  0  5
#> [53,]  2  1  5
#> [54,]  2  2  5
#> [55,]  3  2  5
#> [56,]  3  3  5
#> [57,]  1  0  6
#> [58,]  2  1  6
#> [59,]  3  3  6
#> [60,]  4  4  6

A <- matrix(c(9, 10, 2, 4, -3, 2), ncol = 2, byrow = TRUE)
b <- c(90, 27, 3)
integerPoints(A, b)
#>       x1 x2
#>  [1,]  0  0
#>  [2,]  1  0
#>  [3,]  2  0
#>  [4,]  3  0
#>  [5,]  4  0
#>  [6,]  5  0
#>  [7,]  6  0
#>  [8,]  7  0
#>  [9,]  8  0
#> [10,]  9  0
#> [11,] 10  0
#> [12,]  0  1
#> [13,]  1  1
#> [14,]  2  1
#> [15,]  3  1
#> [16,]  4  1
#> [17,]  5  1
#> [18,]  6  1
#> [19,]  7  1
#> [20,]  8  1
#> [21,]  1  2
#> [22,]  2  2
#> [23,]  3  2
#> [24,]  4  2
#> [25,]  5  2
#> [26,]  6  2
#> [27,]  7  2
#> [28,]  1  3
#> [29,]  2  3
#> [30,]  3  3
#> [31,]  4  3
#> [32,]  5  3
#> [33,]  6  3
#> [34,]  2  4
#> [35,]  3  4
#> [36,]  4  4
#> [37,]  5  4
#> [38,]  3  5
```
