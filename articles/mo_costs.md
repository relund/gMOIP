# Generating costs in multi-objective programming

When generating instances for testing in multi-objective programming the
cost structure of the objective coefficients may have a huge impact on
the difficulty of the instance. Points in ${\mathbb{R}}_{n}$ can be
generated using function `genSample`. Different methods can be used for
generation:

**Random**

The coefficient are generated randomly with a uniform distribution in
the range $\lbrack a,b\rbrack$. For three objectives, random generated
coefficients looks as follows.

``` r
library(gMOIP)
range <- matrix(c(1,100, 50,100, 10,50), ncol = 2, byrow = TRUE )
pts <- genSample(3, 1000, range = range, random = TRUE)
ini3D()
plotPoints3D(pts)
finalize3D()
rglwidget()
```

**Between planes**

The coefficient are generated between two planes in the range
$\lbrack a,b\rbrack$.

``` r
range <- matrix(c(1,100, 1, 100, 1, 100), ncol = 2, byrow = TRUE )
center <- rowMeans(range)
planeU <- c(rep(1, 3), -1.2*sum(rowMeans(range)))
planeL <- c(rep(1, 3), -0.6*sum(rowMeans(range)))
pts <- genSample(3, 1000, range = range, planes = TRUE,
   argsPlanes = list(center = center, planeU = planeU, planeL = planeL))
ini3D(argsPlot3d = list(box = TRUE, axes = TRUE))
plotPoints3D(pts)
rgl::planes3d(planeL[1], planeL[2], planeL[3], planeL[4], alpha = 0.5)
rgl::planes3d(planeU[1], planeU[2], planeU[3], planeU[4], alpha = 0.5)
finalize3D()
rglwidget(reuse = F)
```

**Sphere**

The coefficients are generated on the lower part of a sphere (see next
picture). Note that the sphere is adjusted such that the coefficients
are in the range $\lbrack a,b\rbrack$, i.e. the sphere is not
necessarily included in $\lbrack a,b\rbrack^{p}$.

``` r
cent <- c(1000,1000,1000)
r <- 750
planeC <- c(cent-r/3)
planeC <- c(planeC, -sum(planeC^2))
pts <- genSample(3, 500,
  argsSphere = list(center = cent, radius = r, below = NULL, plane = planeC, factor = 6))
ini3D()
plotPoints3D(pts)
spheres3d(cent, radius=r, color = "grey100", alpha=0.1)
finalize3D()
rglwidget(reuse = F)
```

**Sphere down**

The coefficients are generated on the lower part of a sphere (see next
picture). Note that the sphere is adjusted such that the coefficients
are in the range $\lbrack a,b\rbrack$, i.e. the sphere is not
necessarily included in $\lbrack a,b\rbrack^{p}$.

``` r
cent <- c(1000,1000,1000)
r <- 750
planeC <- c(cent-r/3)
planeC <- c(planeC, -sum(planeC^2))
pts <- genSample(3, 500,
  argsSphere = list(center = cent, radius = r, below = TRUE, plane = planeC, factor = 6))
ini3D()
plotPoints3D(pts)
planes3d(planeC[1],planeC[2],planeC[3],planeC[4], alpha = 0.5, col = "red")
spheres3d(cent, radius=r, color = "grey100", alpha=0.1)
finalize3D()
rglwidget(reuse = F)
```

**Sphere up**

The coefficients are generated on the upper part of a sphere (see next
picture). Note that the sphere is adjusted such that the coefficients
are in the range $\lbrack a,b\rbrack$, i.e. the sphere is not
necessarily included in $\lbrack a,b\rbrack^{p}$.

``` r
cent <- c(1000,1000,1000)
r <- 750
planeC <- c(cent+r/3)
planeC <- c(planeC, -sum(planeC^2))
pts <- genSample(3, 500,
  argsSphere = list(center = cent, radius = r, below = FALSE, plane = planeC, factor = 6))
ini3D()
spheres3d(cent, radius=r, color = "grey100", alpha=0.1)
plotPoints3D(pts)
planes3d(planeC[1],planeC[2],planeC[3],planeC[4], alpha = 0.5, col = "red")
finalize3D()
rglwidget(reuse = F)
```

**2box**

The coefficients are generated randomly but in two specific parts of
$\lbrack a,b\rbrack^{p}$ (see next picture).

``` r
range <- matrix(c(1,1000, 1,1000, 1,1000), ncol = 2, byrow = TRUE )
ini3D(argsPlot3d = list(box = TRUE, axes = TRUE))
pts <- genSample(3, 300, range = range, box = TRUE)
plotPoints3D(pts)
finalize3D()
```
