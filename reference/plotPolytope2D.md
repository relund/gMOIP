# Plot the polytope (bounded convex set) of a linear mathematical program

Plot the polytope (bounded convex set) of a linear mathematical program

## Usage

``` r
plotPolytope2D(
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

  Further arguments passed on the the `ggplot` plotting functions. This
  must be done as lists. Currently the following arguments are
  supported:

  - `argsFaces`: A list of arguments for
    [`plotHull2D`](http://relund.github.io/gMOIP/reference/plotHull2D.md).

  - `argsFeasible`: A list of arguments for `ggplotl2` functions:

    - `geom_point`: A list of arguments for
      [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html).

    - `geom_line`: A list of arguments for
      [`ggplot2::geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html).

  - `argsLabels`: A list of arguments for `ggplotl2` functions:

    - `geom_text`: A list of arguments for
      [`ggplot2::geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html).

  - `argsOptimum`:

    - `geom_point`: A list of arguments for
      [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html).

    - `geom_abline`: A list of arguments for
      [`ggplot2::geom_abline`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

    - `geom_label`: A list of arguments for
      [`ggplot2::geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html).

  - `argsTheme`: A list of arguments for
    [`ggplot2::theme`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A `ggplot` object.

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
