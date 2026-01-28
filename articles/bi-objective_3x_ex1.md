# Plotting bi-objective models with three variables - Example 1

With `gMOIP` you can make plots of the criterion space for bi-objective
models (linear programming (LP), integer linear programming (ILP), or
mixed integer linear programming (MILP)). This vignette gives examples
on how to make plots of both the solution and criterion space.

First we load the package:

``` r
library(gMOIP)
```

We define functions for plotting the solution and criterion space:

``` r
plotSol <- function(A, b, type = rep("c", ncol(A)),
                        faces = rep("c", ncol(A)),
                        plotFaces = TRUE, labels = "numb")
{
   #loadView(v = view, close = F, zoom = 0.75)
   plotPolytope(A, b, type = type, faces = faces, labels = labels, plotFaces = plotFaces, 
                argsTitle3d = list(main = "Solution space"))
}

plotCrit <- function(A, b, obj, crit = "min", type = rep("c", ncol(A)), addTriangles = TRUE, 
                     labels = "numb") 
{
    plotCriterion2D(A, b, obj, type = type, crit = crit, addTriangles = addTriangles, 
                   labels = labels) + 
      ggplot2::ggtitle("Criterion space")
}
```

We define the model $\max\{ cx|Ax \leq b\}$ (could also be minimized)
with three variables:

``` r
Ab <- matrix( c(
   1, 1, 2, 5,
   2, -1, 0, 3,
   -1, 2, 1, 3,
   0, -3, 5, 2
), nc = 4, byrow = TRUE)
A <- Ab[,1:3]
b <- Ab[,4]
obj <- matrix(c(1, -6, 3, -4, 1, 6), nrow = 2)
```

We load the preferred view angle for the RGL window:

``` r
view <- matrix( c(-0.452365815639496, -0.446501553058624, 0.77201122045517, 0, 0.886364221572876,
                  -0.320795893669128, 0.333835482597351, 0, 0.0986008867621422, 0.835299551486969,
                  0.540881276130676, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)
```

LP model (solution space):

``` r
plotSol(A, b)
```

LP model (criterion space):

``` r
plotCrit(A, b, obj, addTriangles = FALSE) 
```

![](bi-objective_3x_ex1_files/figure-html/Ex1LPCrit-1.png)

ILP model (solution space):

``` r
plotSol(A, b, type = c("i","i","i"))
```

ILP model (criterion space):

``` r
plotCrit(A, b, obj, type = c("i","i","i"))
```

![](bi-objective_3x_ex1_files/figure-html/Ex1ILPCrit-1.png)

MILP model with variable 2 and 3 integer (solution space):

``` r
plotSol(A, b, type = c("c","i","i"))
```

MILP model with variable 2 and 3 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("c","i","i"))
```

![](bi-objective_3x_ex1_files/figure-html/Ex1MILPCrit1-1.png)

MILP model with variable 1 and 3 integer (solution space):

``` r
plotSol(A, b, type = c("i","c","i"))
```

MILP model with variable 1 and 3 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("i","c","i"))
```

![](bi-objective_3x_ex1_files/figure-html/Ex1MILPCrit2-1.png)

MILP model with variable 1 and 2 integer (solution space):

``` r
plotSol(A, b, type = c("i","i","c"))
```

MILP model with variable 1 and 2 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("i","i","c"))
```

![](bi-objective_3x_ex1_files/figure-html/Ex1MILPCrit3-1.png)

MILP model with variable 1 integer (solution space):

``` r
plotSol(A, b, type = c("i","c","c"), plotFaces = F)
```

MILP model with variable 1 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("i","c","c"))
```

![](bi-objective_3x_ex1_files/figure-html/Ex1MILP4Crit-1.png)

MILP model with variable 2 integer (solution space):

``` r
plotSol(A, b, type = c("c","i","c"), plotFaces = F)
```

MILP model with variable 2 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("c","i","c"))
```

![](bi-objective_3x_ex1_files/figure-html/Ex1MILPCrit5-1.png)

MILP model with variable 3 integer (solution space):

``` r
plotSol(A, b, type = c("c","c","i"), plotFaces = F)
```

MILP model with variable 3 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("c","c","i"))
```

![](bi-objective_3x_ex1_files/figure-html/Ex1MILPCrit6-1.png)
