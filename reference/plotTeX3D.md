# Plot TeX at a position.

Plot TeX at a position.

## Usage

``` r
plotTeX3D(
  x,
  y,
  z,
  tex,
  cex = graphics::par("cex"),
  fixedSize = FALSE,
  size = 480,
  ...
)
```

## Arguments

- x:

  Coordinate.

- y:

  Coordinate.

- z:

  Coordinate.

- tex:

  TeX string.

- cex:

  Expansion factor (you properly have to fine tune it).

- fixedSize:

  Fix the size of the object (no scaling when zoom).

- size:

  Size of the generated png.

- ...:

  Arguments passed on to
  [`rgl::sprites3d()`](https://dmurdoch.github.io/rgl/dev/reference/sprites.html)
  and
  [`texToPng()`](http://relund.github.io/gMOIP/reference/texToPng.md).

## Value

The shape ID of the displayed object is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
tex0 <- "$\\mathbb{R}_{\\geqq}$"
tex1 <- "\\LaTeX"
tex2 <- "This is a title"
ini3D(argsPlot3d = list(xlim = c(0, 2), ylim = c(0, 2), zlim = c(0, 2)))
plotTeX3D(0.75,0.75,0.75, tex0)
plotTeX3D(0.5,0.5,0.5, tex0, cex = 2)
plotTeX3D(1,1,1, tex2)
finalize3D()
ini3D(new = TRUE, argsPlot3d = list(xlim = c(0, 200), ylim = c(0, 200), zlim = c(0, 200)))
plotTeX3D(75,75,75, tex0)
plotTeX3D(50,50,50, tex1)
plotTeX3D(100,100,100, tex2)
finalize3D()
} # }
```
