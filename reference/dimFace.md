# Return the dimension of the convex hull of a set of points.

Return the dimension of the convex hull of a set of points.

## Usage

``` r
dimFace(pts, dim = NULL)
```

## Arguments

- pts:

  A matrix/data frame/vector that can be converted to a matrix with a
  row for each point.

- dim:

  The dimension of the points, i.e. assume that column 1-dim specify the
  points. If NULL assume that the dimension are the number of columns.

## Value

The dimension of the object.

## Examples

``` r
## In 1D
pts <- matrix(c(3), ncol = 1, byrow = TRUE)
dimFace(pts)
#> [1] 0
pts <- matrix(c(1,3,4), ncol = 1, byrow = TRUE)
dimFace(pts)
#> [1] 1

## In 2D
pts <- matrix(c(3,3,6,3,3,6), ncol = 2, byrow = TRUE)
dimFace(pts)
#> [1] 2
pts <- matrix(c(1,1,2,2,3,3), ncol = 2, byrow = TRUE)
dimFace(pts)
#> [1] 1
pts <- matrix(c(0,0), ncol = 2, byrow = TRUE)
dimFace(pts)
#> [1] 0

## In 3D
pts <- c(3,3,3,6,3,3,3,6,3,6,6,3)
dimFace(pts, dim = 3)
#> [1] 2
pts <- matrix( c(1,1,1), ncol = 3, byrow = TRUE)
dimFace(pts)
#> [1] 0
pts <- matrix( c(1,1,1,2,2,2), ncol = 3, byrow = TRUE)
dimFace(pts)
#> [1] 1
pts <- matrix(c(2,2,2,3,2,2), ncol=3, byrow= TRUE)
dimFace(pts)
#> [1] 1
pts <- matrix(c(0,0,0,0,1,1,0,2,2,0,5,2,0,6,1), ncol = 3, byrow = TRUE)
dimFace(pts)
#> [1] 2
pts <- matrix(c(0,0,0,0,1,1,0,2,2,0,0,2,1,1,1), ncol = 3, byrow = TRUE)
dimFace(pts)
#> [1] 3

## In 4D
pts <- matrix(c(2,2,2,3,2,2,3,4,1,2,3,4), ncol=4, byrow= TRUE)
dimFace(pts,)
#> [1] 2
```
