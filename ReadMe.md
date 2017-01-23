
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/relund/gMOIP.svg?branch=master)](https://travis-ci.org/relund/gMOIP) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/gMOIP)](https://CRAN.R-project.org/package=gMOIP) [![CRAN\_Downloads\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/gMOIP?color=brightgreen)](http://cranlogs.r-pkg.org/downloads/total/last-month/gMOIP)

gMOIP - 2D plots of LP/IP programming models
============================================

Make 2D plots of the polyeder of a linear programming (LP) or integer programming (IP) model, including integer points and iso profit curve. Can also make a plot of a bi-objective criterion space and the non-dominated (Pareto) set.

Usage
-----

``` r
LP <- make.lp(0, 2)
set.objfn(LP, c(7.75, 10))
add.constraint(LP, c(9, 10), "<=", 90)
add.constraint(LP, c(2, 4), "<=", 27)
add.constraint(LP, c(-3, 2), "<=", 3)
colNames <- c("x1", "x2")
colnames(LP) <- colNames
set.type(LP, c(1,2), type = "integer")
control <- lp.control(LP, sense='max')

# Corner points of the polytope
cPoints<-cornerPoints(getA(LP), get.rhs(LP))
# Integer points in the polytope
iPoints<-integerPoints(LP)
# plot polytope (ggplot2)
plotPolytope(cPoints, iPoints, iso = getC(LP), crit = substr(lp.control(LP)$sense,1,3))
```

![](README-ex-1.png)

``` r
# Plot of criterion points given a bi-objective vector
zPoints<-criterionPoints(iPoints, c1 = c(getC(LP)[1], 0), c2 = c(0, getC(LP)[2]),
                         crit = substr(lp.control(LP)$sense,1,3))
plotCriterion(zPoints, addHull = FALSE, addTriangles = TRUE)
```

![](README-ex-2.png)

For more examples see `example("gMOIP-package")`. You may also create a tikz file of the plot for LaTeX using

``` r
library(tikzDevice)
tikz(file = "plot_polytope.tex", standAlone=F, width = 7, height = 6)
plotPolytope(cPoints, zPoints, showLbl = TRUE)
dev.off()
```

Installation
------------

Install the latest stable release from CRAN:

``` r
install.packages("gMOIP")
```

Alternatively, install the latest development version from GitHub:

``` r
install.packages("devtools")
devtools::install_github("relund/gMOIP")

library(gMOIP)
example("gMOIP-package")
```
