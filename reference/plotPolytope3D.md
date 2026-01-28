# Plot the polytope (bounded convex set) of a linear mathematical program

Plot the polytope (bounded convex set) of a linear mathematical program

## Usage

``` r
plotPolytope3D(
  A,
  b,
  obj = NULL,
  type = rep("c", ncol(A)),
  nonneg = rep(TRUE, ncol(A)),
  crit = "max",
  faces = rep("c", ncol(A)),
  plotFaces = TRUE,
  plotFeasible = TRUE,
  plotOptimum = FALSE,
  latex = FALSE,
  labels = NULL,
  ...
)
```

## Arguments

- A:

  The constraint matrix.

- b:

  Right hand side.

- obj:

  A vector with objective coefficients.

- type:

  A character vector of same length as number of variables. If entry k
  is 'i' variable \\k\\ must be integer and if 'c' continuous.

- nonneg:

  A boolean vector of same length as number of variables. If entry k is
  TRUE then variable k must be non-negative.

- crit:

  Either max or min (only used if add the iso-profit line)

- faces:

  A character vector of same length as number of variables. If entry k
  is 'i' variable \\k\\ must be integer and if 'c' continuous. Useful if
  e.g. want to show the linear relaxation of an IP.

- plotFaces:

  If `True` then plot the faces.

- plotFeasible:

  If `True` then plot the feasible points/segments (relevant for
  ILP/MILP).

- plotOptimum:

  Show the optimum corner solution point (if alternative solutions only
  one is shown) and add the iso-profit line.

- latex:

  If `True` make latex math labels for TikZ.

- labels:

  If `NULL` don't add any labels. If 'n' no labels but show the points.
  If equal `coord` add coordinates to the points. Otherwise number all
  points from one.

- ...:

  Further arguments passed on the the RGL plotting functions. This must
  be done as lists. Currently the following arguments are supported:

  - `argsAxes3d`: A list of arguments for
    [`rgl::axes3d`](https://dmurdoch.github.io/rgl/dev/reference/axes3d.html).

  - `argsPlot3d`: A list of arguments for
    [`rgl::plot3d`](https://dmurdoch.github.io/rgl/dev/reference/plot3d.html)
    to open the RGL window.

  - `argsTitle3d`: A list of arguments for
    [`rgl::title3d`](https://dmurdoch.github.io/rgl/dev/reference/axes3d.html).

  - `argsFaces`: A list of arguments for
    [`plotHull3D`](http://relund.github.io/gMOIP/reference/plotHull3D.md).

  - `argsFeasible`: A list of arguments for RGL functions:

    - `points3d`: A list of arguments for
      [`rgl::points3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html).

    - `segments3d`: A list of arguments for
      [`rgl::segments3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html).

    - `triangles3d`: A list of arguments for
      [`rgl::triangles3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html).

  - `argsLabels`: A list of arguments for RGL functions:

    - `points3d`: A list of arguments for
      [`rgl::points3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html).

    - `text3d`: A list of arguments for
      [`rgl::text3d`](https://dmurdoch.github.io/rgl/dev/reference/texts.html).

  - `argsOptimum`: A list of arguments for RGL functions:

    - `points3d`: A list of arguments for
      [`rgl::points3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html).

## Value

A RGL window with 3D plot.

## Note

In general use
[`plotPolytope()`](http://relund.github.io/gMOIP/reference/plotPolytope.md)
instead of this function. The feasible region defined by the constraints
must be bounded otherwise you may see strange results.

## See also

[`plotPolytope()`](http://relund.github.io/gMOIP/reference/plotPolytope.md)
for examples.

## Author

Lars Relund <lars@relund.dk>
