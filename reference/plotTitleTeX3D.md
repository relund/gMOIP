# Draw boxes, axes and other text outside the data using TeX strings.

Draw boxes, axes and other text outside the data using TeX strings.

## Usage

``` r
plotTitleTeX3D(
  main = NULL,
  sub = NULL,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  line = NA,
  ...
)
```

## Arguments

- main:

  The main title for the plot.

- sub:

  The subtitle for the plot.

- xlab:

  The axis labels for the plot .

- ylab:

  The axis labels for the plot .

- zlab:

  The axis labels for the plot .

- line:

  The “line” of the plot margin to draw the label on.

- ...:

  Additional parameters which are passed to
  [`plotMTeX3D()`](http://relund.github.io/gMOIP/reference/plotMTeX3D.md).

## Value

The object IDs of objects added to the scene.

## Details

The rectangular prism holding the 3D plot has 12 edges. They are
identified using 3 character strings. The first character (`x', `y', or
`z') selects the direction of the axis. The next two characters are each `-'
or
`+', selecting the lower or upper end of one of the other coordinates. If only one or two characters are given, the remaining characters default to `-'.
For example `edge = 'x+'` draws an x-axis at the high level of y and the
low level of z.

By default,
[`rgl::axes3d()`](https://dmurdoch.github.io/rgl/dev/reference/axes3d.html)
uses the
[`rgl::bbox3d()`](https://dmurdoch.github.io/rgl/dev/reference/bbox.html)
function to draw the axes. The labels will move so that they do not
obscure the data. Alternatively, a vector of arguments as described
above may be used, in which case fixed axes are drawn using
[`rgl::axis3d()`](https://dmurdoch.github.io/rgl/dev/reference/axes3d.html).

If `pos` is a numeric vector of length 3, `edge` determines the
direction of the axis and the tick marks, and the values of the other
two coordinates in `pos` determine the position. See the examples.

## Examples

``` r
if (FALSE) { # \dontrun{
ini3D(argsPlot3d = list(xlim = c(0, 2), ylim = c(0, 2), zlim = c(0, 2)))
plotTitleTeX3D(main = "\\LaTeX", sub = "subtitle $\\alpha$",
               xlab = "$x^1_2$", ylab = "$\\beta$", zlab = "$x\\cdot y$")
finalize3D()
} # }
```
