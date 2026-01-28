# Plot the lines of a linear mathematical program (Ax = b)

Plot the lines of a linear mathematical program (Ax = b)

## Usage

``` r
plotLines2D(A, b, nonneg = rep(TRUE, ncol(A)), latex = FALSE, ...)
```

## Arguments

- A:

  The constraint matrix.

- b:

  Right hand side.

- nonneg:

  A boolean vector of same length as number of variables. If entry k is
  TRUE then variable k must be non-negative and the line is plotted too.

- latex:

  If `True` make latex math labels for TikZ.

- ...:

  Further arguments passed on the the `ggplot` plotting functions. This
  must be done as lists. Currently the following arguments are
  supported:

  - `argsTheme`: A list of arguments for
    [`ggplot2::theme`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A `ggplot` object.

## Note

In general you will properly use
[`plotPolytope()`](http://relund.github.io/gMOIP/reference/plotPolytope.md)
instead of this function.

## See also

[`plotPolytope()`](http://relund.github.io/gMOIP/reference/plotPolytope.md).

## Author

Lars Relund <lars@relund.dk>
