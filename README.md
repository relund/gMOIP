
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/relund/gMOIP.svg?branch=master)](https://travis-ci.org/relund/gMOIP)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/gMOIP)](https://CRAN.R-project.org/package=gMOIP)
[![CRAN\_Downloads\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/gMOIP?color=brightgreen)](http://cranlogs.r-pkg.org/downloads/total/last-month/gMOIP)

# gMOIP - 2D and 3D plots of LP/ILP/MILP programming models

Make 2D and 3D plots of the polyeder of a linear programming (LP),
integer linear programming (ILP) model, or mixed integer linear
programming (MILP) model with 2 or 3 variables, including integer
points, ranges and iso profit curve.

Can also make a plot of the bi-objective criterion space and the
non-dominated (Pareto) set for bi-objective LP/ILP/MILP programming
models.

## Usage

We define the model \(\max\{cx | Ax \leq b\}\) (could also be minimized)
with 2 variables:

``` r
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
obj <- c(7.75, 10)  # coefficients c
```

The polytope of the LP model with non-negative continuous variables
(\(x \geq 0\)):

``` r
plotPolytope(
   A,
   b,
   obj,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "coord"
)
```

![](README-lp-1.png)<!-- -->

The polytope of the ILP model with LP faces (\(x\in \mathbb{Z}_0\)):

``` r
plotPolytope(
   A,
   b,
   obj,
   type = rep("i", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "coord"
)
```

![](README-ilp-1.png)<!-- -->

The polytope of the MILP model (first variable integer) with LP faces:

``` r
plotPolytope(
   A,
   b,
   obj,
   type = c("i", "c"),
   crit = "max",
   faces = c("c", "c"),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "coord"
)
```

![](README-milp-1.png)<!-- -->

You can do the same with three variables:

``` r
A <- matrix( c(
   3, 2, 5,
   2, 1, 1,
   1, 1, 3,
   5, 2, 4
), nc = 3, byrow = TRUE)
b <- c(55, 26, 30, 57)
obj <- c(20, 10, 15)
```

``` r
# LP model
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087,
                  -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405,
                  0.231208890676498, 0, 0, 0, 0, 1), nc = 4)   
loadView(v = view)  # set view angle
plotPolytope(A, b, plotOptimum = TRUE, obj = obj, labels = "coord")
```

![](README-3d-1.png)<!-- -->

The 3D plot is here a static image (so it can be seen in markdown).
However, it may be an interactive plot as seen in [this
example](http://htmlpreview.github.io/?https://github.com/relund/gMOIP/blob/master/inst/examples/3d_interactive.html).

For more examples see `example("gMOIP-package")` or
`browseVignettes('gMOIP')`.

## LaTeX support

You may create a TikZ file of the plot for LaTeX using

``` r
library(tikzDevice)
tikz(file = "plot_polytope.tex", standAlone=F, width = 7, height = 6)
plotPolytope(
   A,
   b,
   obj,
   type = rep("i", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "coord"
)
dev.off()
```

## Installation

Install the latest stable release from CRAN:

``` r
install.packages("gMOIP")
```

Alternatively, install the latest development version from GitHub:

``` r
install.packages("devtools")
devtools::install_github("relund/gMOIP")

library(gMOIP)
browseVignettes('gMOIP')
example("gMOIP-package")
```
