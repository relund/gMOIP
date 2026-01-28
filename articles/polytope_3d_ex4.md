# Plotting polytopes in 3D - Example 4

With `gMOIP` you can make 3D plots of the polytope/feasible
region/solution space of a linear programming (LP), integer linear
programming (ILP) model, or mixed integer linear programming (MILP)
model. This vignette gives examples on how to make plots given a model
with three variables.

First we load the package:

``` r
library(gMOIP)
```

We define the model:

``` r
Ab <- matrix( c(
   1, 1, 2, 5,
   2, -1, 0, 3,
   -1, 2, 1, 3,
   0, -3, 5, 2
   #   0, 1, 0, 4,
   #   1, 0, 0, 4
), nc = 4, byrow = TRUE)
A <- Ab[,1:3]
b <- Ab[,4]
obj <- c(1,1,3)
```

We load the preferred view angle for the RGL window:

``` r
view <- matrix( c(-0.452365815639496, -0.446501553058624, 0.77201122045517, 0, 0.886364221572876,
                  -0.320795893669128, 0.333835482597351, 0, 0.0986008867621422, 0.835299551486969,
                  0.540881276130676, 0, 0, 0, 0, 1), nc = 4)
```

The LP polytope:

``` r
loadView(v = view, close = F, zoom = 0.75)
plotPolytope(A, b, plotOptimum = TRUE, obj = obj, labels = "coord")
rglwidget()
```

Note in the html file you can zoom/turn/twist the figure with your mouse
(`rglwidget`).

The ILP model with LP and ILP faces:

``` r
loadView(v = view)
rgl::mfrow3d(nr = 1, nc = 2, sharedMouse = TRUE)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","i"), plotOptimum = TRUE, obj = obj, 
             argsTitle3d = list(main = "With LP faces"), argsPlot3d = list(box = F, axes = T) )
plotPolytope(A, b, faces = c("i","i","i"), type = c("i","i","i"), plotFeasible = FALSE, obj = obj,
             argsTitle3d = list(main = "ILP faces") )
rglwidget()
```

Let us have a look at some MILP models. MILP model with variable 1 and 3
integer:

``` r
loadView(v = view, close = T, zoom = 0.75)
plotPolytope(A, b, type = c("i","c","i"), plotOptimum = TRUE, obj = obj, faces = c("c","c","c"))
rglwidget()
```

MILP model with variable 2 and 3 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("c","i","i"), plotOptimum = TRUE, obj = obj, faces = c("c","c","c"))
rglwidget()
```

MILP model with variable 1 and 2 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("i","i","c"), plotOptimum = TRUE, obj = obj, faces = c("c","c","c"))
rglwidget()
```

MILP model with variable 1 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("i","c","c"), plotOptimum = TRUE, obj = obj, faces = c("c","c","c"))
rglwidget()
```

MILP model with variable 2 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("c","i","c"), plotOptimum = TRUE, obj = obj, faces = c("c","c","c"))
rglwidget()
```

MILP model with variable 3 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("c","c","i"), plotOptimum = TRUE, obj = obj, faces = c("c","c","c"))
rglwidget()
```
