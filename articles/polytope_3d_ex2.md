# Plotting polytopes in 3D - Example 2

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
A <- matrix( c(
   1, 1, 1,
   3, 0, 1
), nc = 3, byrow = TRUE)
b <- c(10, 24)
obj <- c(20, 10, 15)
```

We load the preferred view angle for the RGL window:

``` r
view <- matrix( c(-0.812462985515594, -0.029454167932272, 0.582268416881561, 0, 0.579295456409454,
                  -0.153386667370796, 0.800555109977722, 0, 0.0657325685024261, 0.987727105617523,
                  0.14168381690979, 0, 0, 0, 0, 1), nc = 4)
```

The LP polytope:

``` r
loadView(v = view, close = F, zoom = 0.75)
plotPolytope(A, b, plotOptimum = TRUE, obj = obj, labels = "coord")
```

Note you can zoom/turn/twist the figure with your mouse (`rglwidget`).

The ILP model (note since the vertices are integer the LP and ILP faces
are equal):

``` r
loadView(v = view, close = T, zoom = 0.75)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","i"), plotOptimum = TRUE, obj = obj, 
             argsTitle3d = list(main = "With LP faces"), argsPlot3d = list(box = F, axes = T) )
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
