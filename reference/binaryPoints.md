# Binary (0-1) points in the feasible region (Ax\<=b).

Binary (0-1) points in the feasible region (Ax\<=b).

## Usage

``` r
binaryPoints(A, b)
```

## Arguments

- A:

  Constraint matrix.

- b:

  Right hand side.

## Value

A data frame with all binary points inside the feasible region.

## Note

Do a simple enumeration of all binary points. Will not work if `ncol(A)`
large.

## Author

Lars Relund <lars@relund.dk>.

## Examples

``` r
A <- matrix( c(3,-2, 1, 2, 4,-2,-3, 2, 1), nc = 3, byrow = TRUE)
b <- c(10, 12, 3)
binaryPoints(A, b)
#>      x1 x2 x3
#> [1,]  0  0  0
#> [2,]  1  0  0
#> [3,]  0  1  0
#> [4,]  1  1  0
#> [5,]  0  0  1
#> [6,]  1  0  1
#> [7,]  0  1  1
#> [8,]  1  1  1

A <- matrix(c(9, 10, 2, 4, -3, 2), ncol = 2, byrow = TRUE)
b <- c(90, 27, 3)
binaryPoints(A, b)
#>      x1 x2
#> [1,]  0  0
#> [2,]  1  0
#> [3,]  0  1
#> [4,]  1  1
```
