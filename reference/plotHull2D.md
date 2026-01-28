# Plot the convex hull of a set of points in 2D.

Plot the convex hull of a set of points in 2D.

## Usage

``` r
plotHull2D(
  pts,
  drawPoints = FALSE,
  drawLines = TRUE,
  drawPolygons = TRUE,
  addText = FALSE,
  addRays = FALSE,
  direction = 1,
  drawPlot = TRUE,
  drawBBoxHull = FALSE,
  m = apply(pts, 2, min) - 5,
  M = apply(pts, 2, max) + 5,
  ...
)
```

## Arguments

- pts:

  A matrix with a point in each row.

- drawPoints:

  Draw the points.

- drawLines:

  Draw lines of the facets.

- drawPolygons:

  Fill the hull.

- addText:

  Add text to the points. Currently `coord` (coordinates), `rownames`
  (rownames) and `both` supported or a vector with text.

- addRays:

  Add the ray defined by `direction`.

- direction:

  Ray direction. If i'th entry is positive, consider the i'th column of
  `pts` plus a value greater than on equal zero (minimize objective
  \$i\$). If negative, consider the i'th column of `pts` minus a value
  greater than on equal zero (maximize objective \$i\$).

- drawPlot:

  Draw the `ggplot`. Set to FALSE if you want to combine hulls in a
  single plot.

- drawBBoxHull:

  If `addRays` then draw the hull areas hitting the bounding box also.

- m:

  Minimum values of the bounding box.

- M:

  Maximum values of the bounding box.

- ...:

  Further arguments passed on the the `ggplot` plotting functions. This
  must be done as lists. Currently the following arguments are
  supported:

  - `argsGeom_point`: A list of arguments for
    [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html).

  - `argsGeom_path`: A list of arguments for
    [`ggplot2::geom_path`](https://ggplot2.tidyverse.org/reference/geom_path.html).

  - `argsGeom_polygon`: A list of arguments for
    [`ggplot2::geom_polygon`](https://ggplot2.tidyverse.org/reference/geom_polygon.html).

  - `argsGeom_label`: A list of arguments for
    [`ggplot2::geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html).

## Value

The `ggplot` object if `drawPlot = TRUE`; otherwise, a list of `ggplot`
components.

## Examples

``` r
library(ggplot2)
pts<-matrix(c(1,1), ncol = 2, byrow = TRUE)
plotHull2D(pts)

pts1<-matrix(c(2,2, 3,3), ncol = 2, byrow = TRUE)
plotHull2D(pts1, drawPoints = TRUE)

plotHull2D(pts1, drawPoints = TRUE, addRays = TRUE, addText = "coord")

plotHull2D(pts1, drawPoints = TRUE, addRays = TRUE, addText = "coord", drawBBoxHull = TRUE)

plotHull2D(pts1, drawPoints = TRUE, addRays = TRUE, direction = -1, addText = "coord")

pts2<-matrix(c(1,1, 2,2, 0,1), ncol = 2, byrow = TRUE)
plotHull2D(pts2, drawPoints = TRUE, addText = "coord")

plotHull2D(pts2, drawPoints = TRUE, addRays = TRUE, addText = "coord")

plotHull2D(pts2, drawPoints = TRUE, addRays = TRUE, direction = -1, addText = "coord")

## Combine hulls
ggplot() +
  plotHull2D(pts2, drawPoints = TRUE, addText = "coord", drawPlot = FALSE) +
  plotHull2D(pts1, drawPoints = TRUE, drawPlot = FALSE) +
  gMOIPTheme() +
  xlab(expression(x[1])) +
  ylab(expression(x[2]))


# Plotting an LP
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
obj <- c(7.75, 10)
pts3 <- cornerPoints(A, b)
plotHull2D(pts3, drawPoints = TRUE, addText = "coord", argsGeom_polygon = list(fill = "red"))
```
