# Plot a cone defined by a point in 2D.

The cones are defined as the point plus/minus rays of R2.

## Usage

``` r
plotCones2D(
  pts,
  drawPoint = TRUE,
  drawLines = TRUE,
  drawPolygons = TRUE,
  direction = 1,
  rectangle = FALSE,
  drawPlot = TRUE,
  m = apply(pts, 2, min) - 5,
  M = apply(pts, 2, max) + 5,
  ...
)
```

## Arguments

- pts:

  A matrix with a point in each row.

- drawPoint:

  Draw the points defining the cone.

- drawLines:

  Draw lines of the cone.

- drawPolygons:

  Draw polygons of the cone.

- direction:

  Ray direction. If i'th entry is positive, consider the i'th column of
  `pts` plus a value greater than on equal zero (minimize objective
  \$i\$). If negative, consider the i'th column of `pts` minus a value
  greater than on equal zero (maximize objective \$i\$).

- rectangle:

  Draw the cone as a rectangle.

- drawPlot:

  Draw the `ggplot`. Set to FALSE if you want to combine hulls in a
  single plot.

- m:

  Minimum values of the bounding box.

- M:

  Maximum values of the bounding box.

- ...:

  Further arguments passed to
  [plotHull2D](http://relund.github.io/gMOIP/reference/plotHull2D.md)

## Value

A `ggplot` object

## Examples

``` r
library(ggplot2)
plotCones2D(c(4,4), drawLines = FALSE, drawPoint = TRUE,
           argsGeom_point = list(col = "red", size = 10),
           argsGeom_polygon = list(alpha = 0.5), rectangle = TRUE)

plotCones2D(c(1,1), rectangle = FALSE)

plotCones2D(matrix(c(3,3,2,2), ncol = 2, byrow = TRUE))


## The Danish flag
lst <- list(argsGeom_polygon = list(alpha = 0.85, fill = "red"),
            drawPlot = FALSE, drawPoint = FALSE, drawLines = FALSE)
p1 <- do.call(plotCones2D, args = c(list(c(2,4), direction = 1), lst))
p2 <- do.call(plotCones2D, args = c(list(c(1,2), direction = -1), lst))
p3 <- do.call(plotCones2D, args = c(list(c(2,2), direction = c(1,-1)), lst))
p4 <- do.call(plotCones2D, args = c(list(c(1,4), direction = c(-1,1)), lst))
ggplot() + p1 + p2 + p3 + p4 + theme_void()
```
