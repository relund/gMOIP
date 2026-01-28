# Plotting tri-objective models

With `gMOIP` you can make plots of the criterion space for tri-objective
models (linear programming (LP), integer linear programming (ILP), or
mixed integer linear programming (MILP)). This vignette gives examples
on how to make plots of the criterion space.

First we load the package:

``` r
library(gMOIP)
```

The criterion space can be plotted for tri-objective models. An example
with many unsupported:

``` r
view <- matrix( c(0.333316594362259, 0.938472270965576, -0.0903875231742859, 0, 0.83994072675705, -0.339126199483871, -0.423665106296539, 0, -0.428250730037689, 0.0652943551540375, -0.901297807693481, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)
set.seed(1234)
pts <- genNDSet(3, 20, argsSphere = list(below = FALSE), dubND = FALSE)
pts <- classifyNDSet(pts[,1:3])
head(pts)
#>   z1 z2 z3    se   sne    us cls
#> 1 48 87 17  TRUE FALSE FALSE  se
#> 2 14 59 83  TRUE FALSE FALSE  se
#> 3 70 75 12  TRUE FALSE FALSE  se
#> 4 98 36 55 FALSE FALSE  TRUE  us
#> 5 91 40 24  TRUE FALSE FALSE  se
#> 6 34 93 31  TRUE FALSE FALSE  se
ini3D(argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+2),
   ylim = c(min(pts[,2])-2,max(pts[,2])+2),
   zlim = c(min(pts[,3])-2,max(pts[,3])+2)))
plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
plotCones3D(pts[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1, color = "cornflowerblue"))
plotHull3D(pts[,1:3], addRays = TRUE, argsPolygon3d = list(alpha = 0.25, color = "red"), useRGLBBox = TRUE)
finalize3D(argsAxes3d = list(edges = "bbox"))
```

Example with many supported:

``` r
loadView(v = view)
pts <- genNDSet(3, 10, argsSphere = list(below = TRUE), dubND = FALSE)
pts <- classifyNDSet(pts[,1:3])
ini3D(argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+2),
   ylim = c(min(pts[,2])-2,max(pts[,2])+2),
   zlim = c(min(pts[,3])-2,max(pts[,3])+2)))
plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
plotCones3D(pts[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1, color = "cornflowerblue"))
plotHull3D(pts[,1:3], addRays = TRUE, argsPolygon3d = list(alpha = 0.25, color = "red"), useRGLBBox = TRUE)
finalize3D(argsAxes3d = list(edges = "bbox"))
```

## Classifying

Non-dominated points can be classified using `classifyNDSet`:

``` r
pts <- matrix(c(0,0,1, 0,1,0, 1,0,0, 0.5,0.2,0.5, 0.25,0.5,0.25), ncol = 3, byrow = TRUE)
open3d()
#> null 
#>    6
ini3D(argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+2),
  ylim = c(min(pts[,2])-2,max(pts[,2])+2),
  zlim = c(min(pts[,3])-2,max(pts[,3])+2)))
plotHull3D(pts, addRays = TRUE, argsPolygon3d = list(alpha = 0.5), useRGLBBox = TRUE)
pts <- classifyNDSet(pts[,1:3])
pts
#>     z1  z2   z3    se   sne    us cls
#> 1 0.00 0.0 1.00  TRUE FALSE FALSE  se
#> 2 0.00 1.0 0.00  TRUE FALSE FALSE  se
#> 3 1.00 0.0 0.00  TRUE FALSE FALSE  se
#> 4 0.50 0.2 0.50 FALSE FALSE  TRUE  us
#> 5 0.25 0.5 0.25 FALSE  TRUE FALSE sne
plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
plotCones3D(pts[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1))
finalize3D()
rglwidget(reuse = F)
```

``` r
pts <- genNDSet(3,20, dubND = FALSE)[,1:3]
open3d()
#> null 
#>    9
ini3D(argsPlot3d = list(xlim = c(0,max(pts$z1)+2),
  ylim = c(0,max(pts$z2)+2),
  zlim = c(0,max(pts$z3)+2)))
plotHull3D(pts, addRays = TRUE, argsPolygon3d = list(alpha = 0.5))
pts <- classifyNDSet(pts[,1:3])
pts
#>    z1 z2 z3    se   sne    us cls
#> 1   4 33 53  TRUE FALSE FALSE  se
#> 2  21 38 13  TRUE FALSE FALSE  se
#> 3  19 13 51  TRUE FALSE FALSE  se
#> 4  16 47 15  TRUE FALSE FALSE  se
#> 5  58  5 33  TRUE FALSE FALSE  se
#> 6   1 49 56  TRUE FALSE FALSE  se
#> 7  43 12 20  TRUE FALSE FALSE  se
#> 8  29 48  6  TRUE FALSE FALSE  se
#> 9  23 10 42  TRUE FALSE FALSE  se
#> 10  6 63 32  TRUE FALSE FALSE  se
#> 11 31 49  5  TRUE FALSE FALSE  se
#> 12  9 45 23  TRUE FALSE FALSE  se
#> 13  2 41 46  TRUE FALSE FALSE  se
#> 14 35 11 25  TRUE FALSE FALSE  se
#> 15 49  2 42  TRUE FALSE FALSE  se
#> 16 39  2 47  TRUE FALSE FALSE  se
#> 17 18 14 48  TRUE FALSE FALSE  se
#> 18 61  4 37 FALSE FALSE  TRUE  us
#> 19 28 32 11  TRUE FALSE FALSE  se
#> 20 34  8 32  TRUE FALSE FALSE  se
plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
finalize3D()
rglwidget(reuse = F)
```

The classification is done using the `qhull` algorithm that find the
convex hull of the points including the rays. If a vertex then if must
be supported extreme. Next we use the `inHull` algorithm to find out if
the remaining are on the border or not (supported non-extreme and
unsupported).
