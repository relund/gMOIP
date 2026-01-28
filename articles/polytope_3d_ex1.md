# Plotting polytopes in 3D - Example 1

With `gMOIP` you can make 3D plots of the polytope/feasible
region/solution space of a linear programming (LP), integer linear
programming (ILP) model, or mixed integer linear programming (MILP)
model. This vignette gives examples on how to make plots given a model
with three variables.

First we load the package:

``` r
library(gMOIP)
```

We define the model $\max\{ cx|Ax \leq b\}$ (could also be minimized)
with three variables:

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

We load the preferred view angle for the RGL window:

``` r
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087,
                  -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405,
                  0.231208890676498, 0, 0, 0, 0, 1), nc = 4)
```

The LP polytope:

``` r
loadView(v = view, close = F, zoom = 0.75)
plotPolytope(A, b, plotOptimum = TRUE, obj = obj)
```

Note you can zoom/turn/twist the figure with your mouse (`rglwidget`).

The ILP model with LP and ILP faces:

``` r
loadView(v = view)
mfrow3d(nr = 1, nc = 2, sharedMouse = TRUE)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","i"), plotOptimum = TRUE, obj = obj, 
             argsTitle3d = list(main = "With LP faces"), argsPlot3d = list(box = F, axes = T) )
plotPolytope(A, b, faces = c("i","i","i"), type = c("i","i","i"), plotFeasible = FALSE, obj = obj,
             argsTitle3d = list(main = "ILP faces") )
```

Let us have a look at some MILP models. MILP model with variable 1 and 3
integer:

``` r
loadView(v = view, close = T, zoom = 0.75)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","i"), plotOptimum = TRUE, obj = obj)
```

MILP model with variable 2 and 3 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","i"), plotOptimum = TRUE, obj = obj)
```

MILP model with variable 1 and 2 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotOptimum = TRUE, obj = obj)
```

MILP model with variable 1 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("i","c","c"), plotOptimum = TRUE, obj = obj, plotFaces = FALSE)
```

MILP model with variable 2 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("c","i","c"), plotOptimum = TRUE, obj = obj, plotFaces = FALSE)
```

MILP model with variable 3 integer:

``` r
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("c","c","i"), plotOptimum = TRUE, obj = obj, plotFaces = FALSE)
```
