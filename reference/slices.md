# Find all corner points in the slices define for each fixed integer combination.

Find all corner points in the slices define for each fixed integer
combination.

## Usage

``` r
slices(
  A,
  b,
  type = rep("c", ncol(A)),
  nonneg = rep(TRUE, ncol(A)),
  collapse = FALSE
)
```

## Arguments

- A:

  The constraint matrix.

- b:

  Right hand side.

- type:

  A character vector of same length as number of variables. If entry k
  is 'i' variable \\k\\ must be integer and if 'c' continuous.

- nonneg:

  A boolean vector of same length as number of variables. If entry k is
  TRUE then variable k must be non-negative.

- collapse:

  Collapse list to a data frame with unique points.

## Value

A list with the corner points (one entry for each slice).

## Examples

``` r
A <- matrix( c(3, -2, 1,2, 4, -2,-3, 2, 1), nc = 3, byrow = TRUE)
b <- c(10,12,3)
slices(A, b, type=c("i","c","i"))
#> [[1]]
#>      x1  x2 x3
#> [1,]  0 1.5  0
#> [2,]  0 0.0  0
#> 
#> [[2]]
#>      x1  x2 x3
#> [1,]  1 2.5  0
#> [2,]  1 0.0  0
#> 
#> [[3]]
#>      x1 x2 x3
#> [1,]  2  2  0
#> [2,]  2  0  0
#> 
#> [[4]]
#>      x1  x2 x3
#> [1,]  3 1.5  0
#> [2,]  3 0.0  0
#> 
#> [[5]]
#>      x1 x2 x3
#> [1,]  4  1  0
#> [2,]  4  1  0
#> 
#> [[6]]
#>                x1 x2 x3
#> [1,] 1.480297e-16  1  1
#> [2,] 0.000000e+00  0  1
#> 
#> [[7]]
#>      x1  x2 x3
#> [1,]  1 2.5  1
#> [2,]  1 0.0  1
#> 
#> [[8]]
#>      x1  x2 x3
#> [1,]  2 2.5  1
#> [2,]  2 0.0  1
#> 
#> [[9]]
#>      x1           x2 x3
#> [1,]  3 4.163336e-16  1
#> [2,]  3 2.000000e+00  1
#> [3,]  3 0.000000e+00  1
#> 
#> [[10]]
#>      x1  x2 x3
#> [1,]  4 1.5  1
#> [2,]  4 1.5  1
#> 
#> [[11]]
#>                x1  x2 x3
#> [1,] 7.401487e-17 0.5  2
#> [2,] 0.000000e+00 0.0  2
#> 
#> [[12]]
#>      x1 x2 x3
#> [1,]  1  2  2
#> [2,]  1  0  2
#> 
#> [[13]]
#>      x1 x2 x3
#> [1,]  2  3  2
#> [2,]  2  0  2
#> 
#> [[14]]
#>      x1  x2 x3
#> [1,]  3 0.5  2
#> [2,]  3 2.5  2
#> 
#> [[15]]
#>      x1 x2 x3
#> [1,]  4  2  2
#> [2,]  4  2  2
#> 
#> [[16]]
#>      x1 x2 x3
#> [1,]  0  0  3
#> 
#> [[17]]
#>      x1  x2 x3
#> [1,]  1 1.5  3
#> [2,]  1 0.0  3
#> 
#> [[18]]
#>      x1 x2 x3
#> [1,]  2  3  3
#> [2,]  2  0  3
#> 
#> [[19]]
#>      x1 x2 x3
#> [1,]  3  1  3
#> [2,]  3  3  3
#> 
#> [[20]]
#>      x1  x2 x3
#> [1,]  4 2.5  3
#> [2,]  4 2.5  3
#> 
#> [[21]]
#>      x1 x2 x3
#> [1,]  1  1  4
#> [2,]  1  0  4
#> 
#> [[22]]
#>      x1           x2 x3
#> [1,]  2 3.330669e-16  4
#> [2,]  2 2.500000e+00  4
#> [3,]  2 0.000000e+00  4
#> 
#> [[23]]
#>      x1  x2 x3
#> [1,]  3 1.5  4
#> [2,]  3 3.5  4
#> 
#> [[24]]
#>      x1 x2 x3
#> [1,]  4  3  4
#> 
#> [[25]]
#>      x1  x2 x3
#> [1,]  1 0.5  5
#> [2,]  1 0.0  5
#> 
#> [[26]]
#>      x1  x2 x3
#> [1,]  2 0.5  5
#> [2,]  2 2.0  5
#> 
#> [[27]]
#>      x1  x2 x3
#> [1,]  3 2.0  5
#> [2,]  3 3.5  5
#> 
#> [[28]]
#>      x1  x2 x3
#> [1,]  4 3.5  5
#> [2,]  4 3.5  5
#> 
#> [[29]]
#>      x1 x2 x3
#> [1,]  1  0  6
#> 
#> [[30]]
#>      x1  x2 x3
#> [1,]  2 1.0  6
#> [2,]  2 1.5  6
#> 
#> [[31]]
#>      x1  x2 x3
#> [1,]  3 2.5  6
#> [2,]  3 3.0  6
#> 
#> [[32]]
#>      x1 x2 x3
#> [1,]  4  4  6
#> [2,]  4  4  6
#> 

A <- matrix(c(9,10,2,4,-3,2), ncol = 2, byrow = TRUE)
b <- c(90,27,3)
slices(A, b, type=c("c","i"), collapse = TRUE)
#>               x1 x2
#>  [1,] 10.0000000  0
#>  [2,]  0.0000000  0
#>  [3,]  8.8888889  1
#>  [4,]  0.0000000  1
#>  [5,]  7.7777778  2
#>  [6,]  0.3333333  2
#>  [7,]  6.6666667  3
#>  [8,]  1.0000000  3
#>  [9,]  5.5000000  4
#> [10,]  1.6666667  4
#> [11,]  3.5000000  5
#> [12,]  2.3333333  5
```
