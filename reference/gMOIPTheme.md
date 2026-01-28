# The `ggplot` theme for the package

The `ggplot` theme for the package

## Usage

``` r
gMOIPTheme(...)
```

## Arguments

- ...:

  Further arguments parsed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

The theme object.

## Examples

``` r
pts <- matrix(c(1,1), ncol = 2, byrow = TRUE)
plotHull2D(pts)

pts1 <- matrix(c(2,2, 3,3), ncol = 2, byrow = TRUE)
pts2 <- matrix(c(1,1, 2,2, 0,1), ncol = 2, byrow = TRUE)
ggplot2::ggplot() +
  plotHull2D(pts2, drawPoints = TRUE, addText = "coord", drawPlot = FALSE) +
  plotHull2D(pts1, drawPoints = TRUE, drawPlot = FALSE) +
  gMOIPTheme() +
  ggplot2::xlab(expression(x[1])) +
  ggplot2::ylab(expression(x[2]))
#> Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
#> ℹ Please use the `linewidth` argument instead.
#> ℹ The deprecated feature was likely used in the gMOIP package.
#>   Please report the issue at <https://github.com/relund/gMOIP/issues>.
```
