# Plotting bi-objective models with three variables - Example 4

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
A <- matrix( c(
3, 2, 5,
2, 1, 1,
1, 1, 3,
5, 2, 4
), nc = 3, byrow = TRUE)
b <- c(55, 26, 30, 57)
obj <- matrix(c(1, -6, 3, -4, 1, -1), nrow = 2)
```

We load the preferred view angle for the RGL window:

``` r
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087,
                  -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405,
                  0.231208890676498, 0, 0, 0, 0, 1), nc = 4)
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

![](bi-objective_3x_ex4_files/figure-html/Ex4LPCrit-1.png)

ILP model (solution space):

``` r
plotSol(A, b, type = c("i","i","i"), labels="n")
```

ILP model (criterion space):

``` r
plotCrit(A, b, obj, type = c("i","i","i"), labels="n")
```

![](bi-objective_3x_ex4_files/figure-html/Ex4ILPCrit-1.png)

MILP model with variable 2 and 3 integer (solution space):

``` r
plotSol(A, b, type = c("c","i","i"), labels="n")
```

MILP model with variable 2 and 3 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("c","i","i"), labels="n")
```

![](bi-objective_3x_ex4_files/figure-html/Ex4MILPCrit1-1.png)

MILP model with variable 1 and 3 integer (solution space):

``` r
plotSol(A, b, type = c("i","c","i"), plotFaces = FALSE, labels="n")
```

MILP model with variable 1 and 3 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("i","c","i"), labels="n")
```

![](bi-objective_3x_ex4_files/figure-html/Ex4MILPCrit2-1.png)

MILP model with variable 1 and 2 integer (solution space):

``` r
plotSol(A, b, type = c("i","i","c"), labels="n")
```

MILP model with variable 1 and 2 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("i","i","c"), labels="n")
```

![](bi-objective_3x_ex4_files/figure-html/Ex4MILPCrit3-1.png)

MILP model with variable 1 integer (solution space):

``` r
plotSol(A, b, type = c("i","c","c"), labels="n")
```

MILP model with variable 1 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("i","c","c"), labels="n")
```

![](bi-objective_3x_ex4_files/figure-html/Ex4MILP4Crit-1.png)

MILP model with variable 2 integer (solution space):

``` r
plotSol(A, b, type = c("c","i","c"), plotFaces = F, labels="n")
```

MILP model with variable 2 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("c","i","c"), labels="n")
```

![](bi-objective_3x_ex4_files/figure-html/Ex4MILPCrit5-1.png)

MILP model with variable 3 integer (solution space):

``` r
plotSol(A, b, type = c("c","c","i"), labels="n")
```

MILP model with variable 3 integer (criterion space):

``` r
plotCrit(A, b, obj, type = c("c","c","i"), labels="n")
```

![](bi-objective_3x_ex4_files/figure-html/Ex4MILPCrit6-1.png)
