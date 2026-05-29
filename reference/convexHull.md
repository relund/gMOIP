# Find the convex hull of a set of points.

Find the convex hull of a set of points.

## Usage

``` r
convexHull(
  pts,
  addRays = FALSE,
  useRGLBBox = FALSE,
  direction = 1,
  tol = mean(mean(abs(pts))) * sqrt(.Machine$double.eps) * 2,
  m = apply(pts, 2, min) - 5,
  M = apply(pts, 2, max) + 5
)
```

## Arguments

- pts:

  A matrix with a point in each row.

- addRays:

  Add the ray defined by `direction`.

- useRGLBBox:

  Use the RGL bounding box when add rays.

- direction:

  Ray direction. If i'th entry is positive, consider the i'th column of
  `pts` plus a value greater than on equal zero (minimize objective
  \$i\$). If negative, consider the i'th column of `pts` minus a value
  greater than on equal zero (maximize objective \$i\$).

- tol:

  Tolerance on standard deviation if using PCA.

- m:

  Minimum values of the bounding box.

- M:

  Maximum values of the bounding box.

## Value

A list with `hull` equal a matrix with row indices of the vertices
defining each facet in the hull and `pts` equal the input points (and
dummy points) and columns: `pt`, true if a point in the original input;
false if a dummy point (a point on a ray). `vtx`, TRUE if a vertex in
the hull.

## Examples

``` r
## 1D
pts<-matrix(c(1,2,3), ncol = 1, byrow = TRUE)
dimFace(pts) # a line
#> [1] 1
convexHull(pts)
#> $hull
#>      [,1] [,2]
#> [1,]    1    3
#> 
#> $pts
#>   p1 pt   vtx
#> 1  1  1  TRUE
#> 2  2  1 FALSE
#> 3  3  1  TRUE
#> 
convexHull(pts, addRays = TRUE)
#> $hull
#>      [,1] [,2]
#> [1,]    1    4
#> 
#> $pts
#>    p1 pt   vtx
#> 1   1  1  TRUE
#> 2   2  1 FALSE
#> 3   3  1 FALSE
#> 21  8  0  TRUE
#> 

## 2D
pts<-matrix(c(1,1, 2,2), ncol = 2, byrow = TRUE)
dimFace(pts) # a line
#> [1] 1
convexHull(pts)
#> $hull
#>      [,1] [,2]
#> [1,]    1    2
#> 
#> $pts
#>   p1 p2 pt  vtx
#> 1  1  1  1 TRUE
#> 2  2  2  1 TRUE
#> 
plotHull2D(pts, drawPoints = TRUE)
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the gMOIP package.
#>   Please report the issue at <https://github.com/relund/gMOIP/issues>.
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the gMOIP package.
#>   Please report the issue at <https://github.com/relund/gMOIP/issues>.

convexHull(pts, addRays = TRUE)
#> $hull
#>      [,1] [,2] [,3] [,4]
#> [1,]    3    1    4    5
#> 
#> $pts
#>    p1 p2 pt   vtx
#> 1   1  1  1  TRUE
#> 2   2  2  1 FALSE
#> 21  7  1  0  TRUE
#> 3   1  7  0  TRUE
#> 4   7  7  0  TRUE
#> 
plotHull2D(pts, addRays = TRUE, drawPoints = TRUE)

pts<-matrix(c(1,1, 2,2, 0,1), ncol = 2, byrow = TRUE)
dimFace(pts) # a polygon
#> [1] 2
convexHull(pts)
#> $hull
#>      [,1] [,2] [,3]
#> [1,]    1    3    2
#> 
#> $pts
#>   p1 p2 pt  vtx
#> 1  1  1  1 TRUE
#> 2  2  2  1 TRUE
#> 3  0  1  1 TRUE
#> 
plotHull2D(pts, drawPoints = TRUE)

convexHull(pts, addRays = TRUE, direction = c(-1,1))
#> $hull
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    2    1    4    5    6
#> 
#> $pts
#>    p1 p2 pt   vtx
#> 1   1  1  1  TRUE
#> 2   2  2  1  TRUE
#> 3   0  1  1 FALSE
#> 21 -5  1  0  TRUE
#> 4  -5  7  0  TRUE
#> 32  2  7  0  TRUE
#> 
plotHull2D(pts, addRays = TRUE, direction = c(-1,1), addText = "coord")


## 3D
pts<-matrix(c(1,1,1), ncol = 3, byrow = TRUE)
dimFace(pts) # a point
#> [1] 0
convexHull(pts)
#> $hull
#>      [,1]
#> [1,]    1
#> 
#> $pts
#>   p1 p2 p3 pt  vtx
#> 1  1  1  1  1 TRUE
#> 
pts<-matrix(c(0,0,0,1,1,1,2,2,2,3,3,3), ncol = 3, byrow = TRUE)
dimFace(pts) # a line
#> [1] 1
convexHull(pts)
#> $hull
#>      [,1] [,2]
#> [1,]    1    4
#> 
#> $pts
#>   p1 p2 p3 pt   vtx
#> 1  0  0  0  1  TRUE
#> 2  1  1  1  1 FALSE
#> 3  2  2  2  1 FALSE
#> 4  3  3  3  1  TRUE
#> 
pts<-matrix(c(0,0,0,0,1,1,0,2,2,0,0,2), ncol = 3, byrow = TRUE)
dimFace(pts) # a polygon
#> [1] 2
convexHull(pts)
#> $hull
#>      [,1] [,2] [,3]
#> [1,]    1    4    3
#> 
#> $pts
#>   p1 p2 p3 pt   vtx
#> 1  0  0  0  1  TRUE
#> 2  0  1  1  1 FALSE
#> 3  0  2  2  1  TRUE
#> 4  0  0  2  1  TRUE
#> 
convexHull(pts, addRays = TRUE)
#> $hull
#>      [,1] [,2] [,3] [,4]
#> [1,]    7    6    5    1
#> [2,]    9    8    5    1
#> [3,]    9    7   11    5
#> [4,]   10    8    6    1
#> [5,]   10    7   11    6
#> [6,]   10    9   11    8
#> 
#> $pts
#>    p1 p2 p3 pt   vtx
#> 1   0  0  0  1  TRUE
#> 2   0  1  1  1 FALSE
#> 3   0  2  2  1 FALSE
#> 4   0  0  2  1 FALSE
#> 21  5  0  0  0  TRUE
#> 31  0  7  0  0  TRUE
#> 41  5  7  0  0  TRUE
#> 5   0  0  7  0  TRUE
#> 6   5  0  7  0  TRUE
#> 7   0  7  7  0  TRUE
#> 8   5  7  7  0  TRUE
#> 
pts<-matrix(c(1,0,0,1,1,1,1,2,2,3,1,1), ncol = 3, byrow = TRUE)
dimFace(pts) # a polygon
#> [1] 2
convexHull(pts) # a polyhedron
#> $hull
#>      [,1] [,2] [,3]
#> [1,]    1    3    4
#> 
#> $pts
#>   p1 p2 p3 pt   vtx
#> 1  1  0  0  1  TRUE
#> 2  1  1  1  1 FALSE
#> 3  1  2  2  1  TRUE
#> 4  3  1  1  1  TRUE
#> 
pts<-matrix(c(1,1,1,2,2,1,2,1,1,1,1,2), ncol = 3, byrow = TRUE)
dimFace(pts) # a polytope (polyhedron)
#> [1] 3
convexHull(pts)
#> $hull
#>      [,1] [,2] [,3]
#> [1,]    3    2    1
#> [2,]    4    2    1
#> [3,]    4    3    1
#> [4,]    4    3    2
#> attr(,"convhulln")
#> <pointer: 0x5558d926a990>
#> 
#> $pts
#>   p1 p2 p3 pt  vtx
#> 1  1  1  1  1 TRUE
#> 2  2  2  1  1 TRUE
#> 3  2  1  1  1 TRUE
#> 4  1  1  2  1 TRUE
#> 

ini3D(argsPlot3d = list(xlim = c(0,3), ylim = c(0,3), zlim = c(0,3)))
pts<-matrix(c(1,1,1,2,2,1,2,1,1,1,1,2), ncol = 3, byrow = TRUE)
plotPoints3D(pts)
plotHull3D(pts, argsPolygon3d = list(color = "red"))
convexHull(pts)
#> $hull
#>      [,1] [,2] [,3]
#> [1,]    3    2    1
#> [2,]    4    2    1
#> [3,]    4    3    1
#> [4,]    4    3    2
#> attr(,"convhulln")
#> <pointer: 0x5558dd0533d0>
#> 
#> $pts
#>   p1 p2 p3 pt  vtx
#> 1  1  1  1  1 TRUE
#> 2  2  2  1  1 TRUE
#> 3  2  1  1  1 TRUE
#> 4  1  1  2  1 TRUE
#> 
plotHull3D(pts, addRays = TRUE)
convexHull(pts, addRays = TRUE)
#> $hull
#>      [,1] [,2] [,3] [,4]
#> [1,]    7    6    5    1
#> [2,]    9    8    5    1
#> [3,]    9    7   11    5
#> [4,]   10    8    6    1
#> [5,]   10    7   11    6
#> [6,]   10    9   11    8
#> 
#> $pts
#>    p1 p2 p3 pt   vtx
#> 1   1  1  1  1  TRUE
#> 2   2  2  1  1 FALSE
#> 3   2  1  1  1 FALSE
#> 4   1  1  2  1 FALSE
#> 21  7  1  1  0  TRUE
#> 31  1  7  1  0  TRUE
#> 41  7  7  1  0  TRUE
#> 5   1  1  7  0  TRUE
#> 6   7  1  7  0  TRUE
#> 7   1  7  7  0  TRUE
#> 8   7  7  7  0  TRUE
#> 
finalize3D()
3D plot

{"x":{"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":true,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmode":"modulate","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false,"polygon_offset":[0,0],"margin":"","floating":false,"tag":"","blend":["src_alpha","one_minus_src_alpha"]},"rootSubscene":745,"objects":{"753":{"id":753,"type":"clipplanes","offsets":[[-0],[3],[-0],[3],[-0],[3]],"normals":"0","flags":512},"754":{"id":754,"type":"lines","material":{"lit":false},"vertices":"1","colors":"2","centers":"3","ignoreExtent":false,"flags":32832},"755":{"id":755,"type":"lines","material":{"lit":false},"vertices":"4","colors":"5","centers":"6","ignoreExtent":false,"flags":32832},"757":{"id":757,"type":"text","material":{"lit":false,"margin":0,"floating":true,"edge":[0,1,1]},"vertices":"7","colors":"8","texts":[[""]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"9","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"758":{"id":758,"type":"text","material":{"lit":false,"margin":1,"floating":true,"edge":[1,1,1]},"vertices":"10","colors":"11","texts":[[""]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"12","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"759":{"id":759,"type":"text","material":{"lit":false,"margin":2,"floating":true,"edge":[1,1,1]},"vertices":"13","colors":"14","texts":[[""]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"15","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"760":{"id":760,"type":"points","material":{"lit":false,"size":5},"vertices":"16","colors":"17","centers":"18","ignoreExtent":false,"flags":34816},"761":{"id":761,"type":"linestrip","material":{"lit":false},"vertices":"19","colors":"20","centers":"21","ignoreExtent":false,"flags":32832},"762":{"id":762,"type":"triangles","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"22","colors":"24","centers":"25","normals":"23","ignoreExtent":false,"flags":32811},"763":{"id":763,"type":"linestrip","material":{"lit":false},"vertices":"26","colors":"27","centers":"28","ignoreExtent":false,"flags":32832},"764":{"id":764,"type":"triangles","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"29","colors":"31","centers":"32","normals":"30","ignoreExtent":false,"flags":32811},"765":{"id":765,"type":"linestrip","material":{"lit":false},"vertices":"33","colors":"34","centers":"35","ignoreExtent":false,"flags":32832},"766":{"id":766,"type":"triangles","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"36","colors":"38","centers":"39","normals":"37","ignoreExtent":false,"flags":32811},"767":{"id":767,"type":"linestrip","material":{"lit":false},"vertices":"40","colors":"41","centers":"42","ignoreExtent":false,"flags":32832},"768":{"id":768,"type":"triangles","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"43","colors":"45","centers":"46","normals":"44","ignoreExtent":false,"flags":32811},"769":{"id":769,"type":"linestrip","material":{"lit":false},"vertices":"47","colors":"48","centers":"49","ignoreExtent":false,"flags":32832},"770":{"id":770,"type":"quads","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"50","colors":"52","centers":"53","normals":"51","ignoreExtent":false,"flags":32811},"771":{"id":771,"type":"linestrip","material":{"lit":false},"vertices":"54","colors":"55","centers":"56","ignoreExtent":false,"flags":32832},"772":{"id":772,"type":"quads","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"57","colors":"59","centers":"60","normals":"58","ignoreExtent":false,"flags":32811},"773":{"id":773,"type":"linestrip","material":{"lit":false},"vertices":"61","colors":"62","centers":"63","ignoreExtent":false,"flags":32832},"774":{"id":774,"type":"quads","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"64","colors":"66","centers":"67","normals":"65","ignoreExtent":false,"flags":32811},"775":{"id":775,"type":"linestrip","material":{"lit":false},"vertices":"68","colors":"69","centers":"70","ignoreExtent":false,"flags":32832},"776":{"id":776,"type":"quads","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"71","colors":"73","centers":"74","normals":"72","ignoreExtent":false,"flags":32811},"777":{"id":777,"type":"linestrip","material":{"lit":false},"vertices":"75","colors":"76","centers":"77","ignoreExtent":false,"flags":32832},"778":{"id":778,"type":"quads","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"78","colors":"80","centers":"81","normals":"79","ignoreExtent":false,"flags":32811},"779":{"id":779,"type":"linestrip","material":{"lit":false},"vertices":"82","colors":"83","centers":"84","ignoreExtent":false,"flags":32832},"780":{"id":780,"type":"quads","material":{"alpha":0.09803921729326248,"lwd":2,"isTransparent":true},"vertices":"85","colors":"87","centers":"88","normals":"86","ignoreExtent":false,"flags":32811},"781":{"id":781,"type":"lines","material":{"lit":false},"vertices":"89","colors":"90","centers":"91","ignoreExtent":true,"flags":32832},"782":{"id":782,"type":"text","material":{"lit":false},"vertices":"92","colors":"93","texts":[["0.0"],["0.5"],["1.0"],["1.5"],["2.0"],["2.5"],["3.0"]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"94","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"783":{"id":783,"type":"lines","material":{"lit":false},"vertices":"95","colors":"96","centers":"97","ignoreExtent":true,"flags":32832},"784":{"id":784,"type":"text","material":{"lit":false},"vertices":"98","colors":"99","texts":[["0.0"],["0.5"],["1.0"],["1.5"],["2.0"],["2.5"],["3.0"]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"100","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"785":{"id":785,"type":"lines","material":{"lit":false},"vertices":"101","colors":"102","centers":"103","ignoreExtent":true,"flags":32832},"786":{"id":786,"type":"text","material":{"lit":false},"vertices":"104","colors":"105","texts":[["0.0"],["0.5"],["1.0"],["1.5"],["2.0"],["2.5"],["3.0"]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"106","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"787":{"id":787,"type":"sprites","material":{"lit":false,"textype":"rgba","isTransparent":true,"margin":0,"floating":true,"uri":"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFYAAABWCAMAAABiiJHFAAABTVBMVEUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACxjiUhAAAAb3RSTlMAAQIDBAUGBwgJCw0ODxASExYXGBodHh8mJygrLjEzNDY3Ozw+P0FFRkhMT1BRU1RYX2BmbG1ucnh6fIeLj5WWl5idn6GkpqqtsLGztri+v8HCycvM09TW3N7i4+Xo6ezt7/Dx8vP19vf5+/z9/v8+ky+AAAABgElEQVRYhe3WV1PCQBDAcYJRBFSKCPYK9oq9gQj2gti7RkUNcPn+j2ad8UG5m3i5G4dx9v+6N7/JQLKJw4FhGIZh/zOlO0atRYytzxvUFsTYviJVLUbF2Dn6xT55xdgNOpt1irF1/u8FMsRU38fE1J9Vr4OaH5arurdA1frlqt59UO975Kq+I/i3btrlqo2noF42y1UjF6Ceh+WqrVegngTlqh23oOZ8ctXeBxMlB4xHVg2Fau2og8+gbrsZ49ESGbGhjr+CmqlhzdOGHTaug5qoYs0Dmh12tmCqpWXmzlJShJ9VlkqgLiqsA7418wAv61wBtTBDn4YH5nfgd+dl1RQsFz1On7q0r7XOx7o2QX2bYI3tsZ49UF+GWHM1kYZyfGzDIVzIo+VLdoqLDRyDetdleZCLbToD9brN+iQPq34ubbK7WlbZW5eHjej0LwSDTIuwk4TBFjpF2ARDNTSPAOtMZhkly3YO3w3265BFFllk/4ANxmJRv3wWwzAMwyqhD0NX0GupxdOoAAAAAElFTkSuQmCC","edge":[0,1,1]},"vertices":"107","colors":"108","adj":[[0.5,0.5,0.5]],"radii":[[0.8600000143051147]],"centers":"109","ignoreExtent":true,"fixedSize":true,"rotating":false,"flags":33852},"788":{"id":788,"type":"sprites","material":{"lit":false,"textype":"rgba","isTransparent":true,"margin":1,"floating":true,"uri":"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFYAAABWCAMAAABiiJHFAAAB8lBMVEUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC516YIAAAApnRSTlMAAQIDBAUGBwgJCgsMDQ4PEBITFBUWFxgaHB0eHyAhIiMkJicoKSssLjAxMjM0Njc7PD0+P0FERUZHSExNT1FTVFhaW19gZmpsbW5weHp8g4WHiIuNjo+RlZaXmJmam5ydnp+ho6Smp6mqq6ytrq+wsbO2uLq+v8DBwsPGycvMzs/T1NXW2Nze4OHi4+Xm5+jp6+zt7u/w8fLz9PX29/j5+vv8/f7/5G2jRwAAAoZJREFUWIXt1/lTkkEYB/CXF4zI2yLLsLTLUsPssEszS7GL7kzKTkutrOyyy1KzsFK7EQwJkHf/z57HcZxXduldfNdpptnvT+w8y4edvd4XRZGRkZGRkfk/Y3HvYGadOTZ3gjBz3BxbPcVUp7abY4+yBzuebY69xWZ7VXNsTsHcONs1UCP7zanJybiG6sQ+sarjNqqBnWLV7Eeofq0Sq+Y/x9Ua2SRWLXyDqn+tWNX1DtW3q8Sq64dRfb1crFo+iuqLfLHq1u+Aao8ZR7bgYMu9J/cve1bPQ90dRLXbQRWW+SZnjnP8Tmm6an0Y1fZFVGHzB909EahLT22KotpqpQquMeQ+3jzTNoTn5Fd1OuqROHwlcZq+s9QeKAQbbfh5D477/RJu1HIigarXQpeq4PeC7plGEbhaM6+qnkU1fphVOweQd7ZVBx1fcV7Dtks4adEmVs3aBxfvitlmhp+QcB6Xau9AdfIAs7j4JyEDunXsIiRWxqNmPkQ1tJdddf4m5K6u3QaTVcGh5j3F/fMj1UO2JBSJXNC1H8BsFRurzj5Uv2zhGAAm6zMho/SJSU7RIKqfNnKqSjNM2A3DXrbpS1vraaHCfuqWwQLG3MySPq4o+w2BaB5W98oRqFxlnJmkNGgp2Hgl3dnuxcuoP8dQVVpTqCSQSfWtGcLCQKGxqvp6U8SXfEBXduL51jo5xsof27Hx6R1TLxJV1rzENQifzBKq7vqGt2bXfB5kf0ktbgB/jVhUKQ/BUnXkClYdcLwTp0y+QNPxaES7bnyw0oy1H15Kl4pWlQ0xQi4KV5VG2LFXDs1Jg4CJPk/fFxG7ebZ7QVj12cKMtoL+g71N+HaTkZGRkZH5x/kDuZCCFt8JnpsAAAAASUVORK5CYII=","edge":[1,1,1]},"vertices":"110","colors":"111","adj":[[0.5,0.5,0.5]],"radii":[[0.8600000143051147]],"centers":"112","ignoreExtent":true,"fixedSize":true,"rotating":false,"flags":33852},"789":{"id":789,"type":"sprites","material":{"lit":false,"textype":"rgba","isTransparent":true,"margin":2,"floating":true,"uri":"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFYAAABWCAMAAABiiJHFAAACBFBMVEUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADeXrS7AAAArHRSTlMAAQIDBAUGBwgJCgsMDQ4PEBESFRcaHB0eICEiIyQmJyssLi8wMTIzNDU2Nzk6Ozw9P0JERUhKTE1OUFFUVltdX2FkZ2tsbnBzdnd6e3x9fn+BhYeJioyNjo+TlJWWl5ucnZ6foKOnqKqrrK6vsLO0tba3uLm6vL2/wMHExcfJysvNzs/Q0dLT1NXW19rb3N3f4OLj5OXm5+nr7O3u7/Dx8vP09fb4+fv8/f7/se1uygAAAs1JREFUWIXtl+dbE0EQxu9SJJAoRGkqiCgGQVSwi0GMWLEggiLYFcUWQUDFCsGCDVQUVAxICQm5+SedAUPaRi7cfvDx2ffTzc3e79lnd+bdPUkSEhISEhL6P2W43cHUfm3YLC+wpOzWhrUrTKx3uTbsKSYVPhi1YSsaItQ+Pf1L2qhRsg3SyrYv4kstHiLqnSS+1K2jRL22gC/V7iHqeQNfqoNq2F+r4wqVD04R9ajMl1rlR6rvAFeopKujevU6+FKNF4nqKWOllq4tzEqYF9XURNTRbVEJnb1tjLrO07EnfrKlmajukqhExrOgE73Li5NqfUSffS+MSqR9DrUfd3zc1E76qD83KqFrwffj9eusiSuPu2m+iXFQM98QtTc7OrMRC7l/zcxzzgD231711BUfidqTwUg1AkxtDgRlWNYtqlsl7ytRO1MZKWMvgEsfiEzfAIbUelDRD6I+trJy6WgR52Yj+QHA5GJ11C0jZFn3LMykzQdQGQyd2ITpqqi7Joh6M8YGr25pbd0xG+meYFUsVEMtnyTqVXWnYSo6fJd+7nHSPh9RG9QMxaW9oIBySMXASjJC5aw1OVKMNZFtTuziF+w9CJPZzb4hAFSHDzTeeNhNOwvdmSomu2EqBtW/PXxgwvDM6xqTCqp0ONZkI6voDxYm6lPmpsp3Y2Ej70n6yhM1V3rIIN8vmxtbUBJDq5jjC1zI7ZrfIfE3pbzGuinnjpV2YkE6+Z72JPNPgD6N91OUnJufnxMSG14CDGtfXMMrgLch1zE9nk1j2rFyO8BISLsaPwF80b4I0hk8c4qDYTa6egeHa18pbv3lYHgSO6JKO1VKxtPLuykQFY3j0mZxwErHcH6/HNO7ZnCgMyj1PKhS0nNy5IGmutPXB8kTnpq5YKU0V4gFKW1L+FBxvtUBs1f6Knj+UlhKa2/db248sp7zf4qQkJCQkNA/ot/IqYn7fjqoHAAAAABJRU5ErkJggg==","edge":[1,1,1]},"vertices":"113","colors":"114","adj":[[0.5,0.5,0.5]],"radii":[[0.8600000143051147]],"centers":"115","ignoreExtent":true,"fixedSize":true,"rotating":false,"flags":33852},"749":{"id":749,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"751":{"id":751,"type":"background","material":{"lit":false,"back":"lines"},"colors":"116","centers":"117","sphere":false,"fogtype":"none","fogscale":1,"flags":32768},"756":{"id":756,"type":"bboxdeco","material":{"front":"culled","back":"culled"},"colors":"118","axes":{"mode":["none","none","none"],"step":[-1,-1,-1],"nticks":[0,0,0],"marklen":[15,15,15],"expand":[1.029999971389771,1.029999971389771,1.029999971389771]},"draw_front":false,"flags":32769},"752":{"id":752,"type":"subscene","par3d":{"antialias":8,"FOV":30,"ignoreExtent":false,"listeners":745,"mouseMode":{"none":"none","left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,12.71504878997803],"modelMatrix":[[1,0,0,-1.5],[0,0.3420201539993286,0.9396926164627075,-1.922569274902344],[0,-0.9396926164627075,0.3420201539993286,-11.8185396194458],[0,0,0,1]],"projMatrix":[[3.732050895690918,0,0,0],[0,3.732050895690918,0,0],[0,0,-3.863703727722168,-45.83628463745117],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[1,0,0,0],[0,0.3420201433256682,0.9396926207859085,0],[0,-0.9396926207859085,0.3420201433256682,0],[0,0,0,1]],"userProjection":[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]],"scale":[1,1,1],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[1,1,1,1,1,1],"windowRect":[0,0,256,256],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"NULL","maxClipPlanes":2147483647,"glVersion":"NA","activeSubscene":0},"embeddings":{"viewport":"inherit","projection":"inherit","model":"inherit","mouse":"inherit"},"objects":[753,754,749],"parent":745,"subscenes":[],"flags":33600},"745":{"id":745,"type":"subscene","par3d":{"antialias":8,"FOV":30,"ignoreExtent":false,"listeners":745,"mouseMode":{"none":"none","left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,12.71504878997803],"modelMatrix":[[1,0,0,-1.5],[0,0.3420201539993286,0.9396926164627075,-1.922569274902344],[0,-0.9396926164627075,0.3420201539993286,-11.8185396194458],[0,0,0,1]],"projMatrix":[[3.732050895690918,0,0,0],[0,3.732050895690918,0,0],[0,0,-3.863703727722168,-45.83628463745117],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[1,0,0,0],[0,0.3420201433256682,0.9396926207859085,0],[0,-0.9396926207859085,0.3420201433256682,0],[0,0,0,1]],"userProjection":[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]],"scale":[1,1,1],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[0,3,0,3,0,3],"windowRect":[0,0,256,256],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"NULL","maxClipPlanes":2147483647,"glVersion":"NA","activeSubscene":0},"embeddings":{"viewport":"replace","projection":"replace","model":"replace","mouse":"replace"},"objects":[751,756,755,757,758,759,760,761,762,763,764,765,766,767,768,769,770,771,772,773,774,775,776,777,778,779,780,781,782,783,784,785,786,787,788,789,749,752],"subscenes":752,"flags":36735}},"crosstalk":{"key":[],"group":[],"id":[],"options":[]},"width":700,"height":432.6328800988875,"buffer":{"accessors":[{"bufferView":0,"componentType":5120,"count":6,"type":"VEC3"},{"bufferView":1,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":2,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":3,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":4,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":5,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":6,"componentType":5121,"count":2,"type":"VEC3"},{"bufferView":7,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":8,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":9,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":10,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":11,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":12,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":13,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":14,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":15,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":16,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":17,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":18,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":19,"componentType":5126,"count":14,"type":"VEC3"},{"bufferView":20,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":21,"componentType":5126,"count":14,"type":"VEC3"},{"bufferView":22,"componentType":5121,"count":3,"type":"VEC3"},{"bufferView":23,"componentType":5121,"count":3,"type":"VEC3"},{"bufferView":24,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":25,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":26,"componentType":5126,"count":14,"type":"VEC3"},{"bufferView":27,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":28,"componentType":5126,"count":14,"type":"VEC3"},{"bufferView":29,"componentType":5121,"count":3,"type":"VEC3"},{"bufferView":30,"componentType":5126,"count":3,"type":"VEC3"},{"bufferView":31,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":32,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":33,"componentType":5126,"count":14,"type":"VEC3"},{"bufferView":34,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":35,"componentType":5126,"count":14,"type":"VEC3"},{"bufferView":36,"componentType":5121,"count":3,"type":"VEC3"},{"bufferView":37,"componentType":5121,"count":3,"type":"VEC3"},{"bufferView":38,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":39,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":40,"componentType":5126,"count":14,"type":"VEC3"},{"bufferView":41,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":42,"componentType":5126,"count":14,"type":"VEC3"},{"bufferView":43,"componentType":5121,"count":3,"type":"VEC3"},{"bufferView":44,"componentType":5126,"count":3,"type":"VEC3"},{"bufferView":45,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":46,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":47,"componentType":5126,"count":10,"type":"VEC3"},{"bufferView":48,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":49,"componentType":5126,"count":10,"type":"VEC3"},{"bufferView":50,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":51,"componentType":5120,"count":4,"type":"VEC3"},{"bufferView":52,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":53,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":54,"componentType":5126,"count":10,"type":"VEC3"},{"bufferView":55,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":56,"componentType":5126,"count":10,"type":"VEC3"},{"bufferView":57,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":58,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":59,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":60,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":61,"componentType":5126,"count":2,"type":"VEC3"},{"bufferView":62,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":63,"componentType":5126,"count":2,"type":"VEC3"},{"bufferView":64,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":65,"componentType":5120,"count":4,"type":"VEC3"},{"bufferView":66,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":67,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":68,"componentType":5126,"count":10,"type":"VEC3"},{"bufferView":69,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":70,"componentType":5126,"count":10,"type":"VEC3"},{"bufferView":71,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":72,"componentType":5120,"count":4,"type":"VEC3"},{"bufferView":73,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":74,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":75,"componentType":5126,"count":2,"type":"VEC3"},{"bufferView":76,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":77,"componentType":5126,"count":2,"type":"VEC3"},{"bufferView":78,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":79,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":80,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":81,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":82,"componentType":5126,"count":2,"type":"VEC3"},{"bufferView":83,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":84,"componentType":5126,"count":2,"type":"VEC3"},{"bufferView":85,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":86,"componentType":5120,"count":4,"type":"VEC3"},{"bufferView":87,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":88,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":89,"componentType":5126,"count":16,"type":"VEC3"},{"bufferView":90,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":91,"componentType":5126,"count":8,"type":"VEC3"},{"bufferView":92,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":93,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":94,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":95,"componentType":5126,"count":16,"type":"VEC3"},{"bufferView":96,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":97,"componentType":5126,"count":8,"type":"VEC3"},{"bufferView":98,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":99,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":100,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":101,"componentType":5126,"count":16,"type":"VEC3"},{"bufferView":102,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":103,"componentType":5126,"count":8,"type":"VEC3"},{"bufferView":104,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":105,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":106,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":107,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":108,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":109,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":110,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":111,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":112,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":113,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":114,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":115,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":116,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":117,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":118,"componentType":5121,"count":1,"type":"VEC4"}],"bufferViews":[{"buffer":0,"byteLength":18,"byteOffset":0},{"buffer":0,"byteLength":12,"byteOffset":18},{"buffer":0,"byteLength":4,"byteOffset":30},{"buffer":0,"byteLength":6,"byteOffset":34},{"buffer":0,"byteLength":12,"byteOffset":40},{"buffer":0,"byteLength":4,"byteOffset":52},{"buffer":0,"byteLength":6,"byteOffset":56},{"buffer":0,"byteLength":12,"byteOffset":64},{"buffer":0,"byteLength":4,"byteOffset":76},{"buffer":0,"byteLength":12,"byteOffset":80},{"buffer":0,"byteLength":12,"byteOffset":92},{"buffer":0,"byteLength":4,"byteOffset":104},{"buffer":0,"byteLength":12,"byteOffset":108},{"buffer":0,"byteLength":12,"byteOffset":120},{"buffer":0,"byteLength":4,"byteOffset":132},{"buffer":0,"byteLength":12,"byteOffset":136},{"buffer":0,"byteLength":12,"byteOffset":148},{"buffer":0,"byteLength":4,"byteOffset":160},{"buffer":0,"byteLength":12,"byteOffset":164},{"buffer":0,"byteLength":168,"byteOffset":176},{"buffer":0,"byteLength":16,"byteOffset":344},{"buffer":0,"byteLength":168,"byteOffset":360},{"buffer":0,"byteLength":9,"byteOffset":528},{"buffer":0,"byteLength":9,"byteOffset":537},{"buffer":0,"byteLength":16,"byteOffset":548},{"buffer":0,"byteLength":12,"byteOffset":564},{"buffer":0,"byteLength":168,"byteOffset":576},{"buffer":0,"byteLength":16,"byteOffset":744},{"buffer":0,"byteLength":168,"byteOffset":760},{"buffer":0,"byteLength":9,"byteOffset":928},{"buffer":0,"byteLength":36,"byteOffset":940},{"buffer":0,"byteLength":16,"byteOffset":976},{"buffer":0,"byteLength":12,"byteOffset":992},{"buffer":0,"byteLength":168,"byteOffset":1004},{"buffer":0,"byteLength":16,"byteOffset":1172},{"buffer":0,"byteLength":168,"byteOffset":1188},{"buffer":0,"byteLength":9,"byteOffset":1356},{"buffer":0,"byteLength":9,"byteOffset":1365},{"buffer":0,"byteLength":16,"byteOffset":1376},{"buffer":0,"byteLength":12,"byteOffset":1392},{"buffer":0,"byteLength":168,"byteOffset":1404},{"buffer":0,"byteLength":16,"byteOffset":1572},{"buffer":0,"byteLength":168,"byteOffset":1588},{"buffer":0,"byteLength":9,"byteOffset":1756},{"buffer":0,"byteLength":36,"byteOffset":1768},{"buffer":0,"byteLength":16,"byteOffset":1804},{"buffer":0,"byteLength":12,"byteOffset":1820},{"buffer":0,"byteLength":120,"byteOffset":1832},{"buffer":0,"byteLength":16,"byteOffset":1952},{"buffer":0,"byteLength":120,"byteOffset":1968},{"buffer":0,"byteLength":12,"byteOffset":2088},{"buffer":0,"byteLength":12,"byteOffset":2100},{"buffer":0,"byteLength":16,"byteOffset":2112},{"buffer":0,"byteLength":3,"byteOffset":2128},{"buffer":0,"byteLength":120,"byteOffset":2132},{"buffer":0,"byteLength":16,"byteOffset":2252},{"buffer":0,"byteLength":120,"byteOffset":2268},{"buffer":0,"byteLength":12,"byteOffset":2388},{"buffer":0,"byteLength":12,"byteOffset":2400},{"buffer":0,"byteLength":16,"byteOffset":2412},{"buffer":0,"byteLength":3,"byteOffset":2428},{"buffer":0,"byteLength":24,"byteOffset":2432},{"buffer":0,"byteLength":16,"byteOffset":2456},{"buffer":0,"byteLength":24,"byteOffset":2472},{"buffer":0,"byteLength":12,"byteOffset":2496},{"buffer":0,"byteLength":12,"byteOffset":2508},{"buffer":0,"byteLength":16,"byteOffset":2520},{"buffer":0,"byteLength":3,"byteOffset":2536},{"buffer":0,"byteLength":120,"byteOffset":2540},{"buffer":0,"byteLength":16,"byteOffset":2660},{"buffer":0,"byteLength":120,"byteOffset":2676},{"buffer":0,"byteLength":12,"byteOffset":2796},{"buffer":0,"byteLength":12,"byteOffset":2808},{"buffer":0,"byteLength":16,"byteOffset":2820},{"buffer":0,"byteLength":3,"byteOffset":2836},{"buffer":0,"byteLength":24,"byteOffset":2840},{"buffer":0,"byteLength":16,"byteOffset":2864},{"buffer":0,"byteLength":24,"byteOffset":2880},{"buffer":0,"byteLength":12,"byteOffset":2904},{"buffer":0,"byteLength":12,"byteOffset":2916},{"buffer":0,"byteLength":16,"byteOffset":2928},{"buffer":0,"byteLength":3,"byteOffset":2944},{"buffer":0,"byteLength":24,"byteOffset":2948},{"buffer":0,"byteLength":16,"byteOffset":2972},{"buffer":0,"byteLength":24,"byteOffset":2988},{"buffer":0,"byteLength":12,"byteOffset":3012},{"buffer":0,"byteLength":12,"byteOffset":3024},{"buffer":0,"byteLength":16,"byteOffset":3036},{"buffer":0,"byteLength":3,"byteOffset":3052},{"buffer":0,"byteLength":192,"byteOffset":3056},{"buffer":0,"byteLength":4,"byteOffset":3248},{"buffer":0,"byteLength":96,"byteOffset":3252},{"buffer":0,"byteLength":84,"byteOffset":3348},{"buffer":0,"byteLength":4,"byteOffset":3432},{"buffer":0,"byteLength":84,"byteOffset":3436},{"buffer":0,"byteLength":192,"byteOffset":3520},{"buffer":0,"byteLength":4,"byteOffset":3712},{"buffer":0,"byteLength":96,"byteOffset":3716},{"buffer":0,"byteLength":84,"byteOffset":3812},{"buffer":0,"byteLength":4,"byteOffset":3896},{"buffer":0,"byteLength":84,"byteOffset":3900},{"buffer":0,"byteLength":192,"byteOffset":3984},{"buffer":0,"byteLength":4,"byteOffset":4176},{"buffer":0,"byteLength":96,"byteOffset":4180},{"buffer":0,"byteLength":84,"byteOffset":4276},{"buffer":0,"byteLength":4,"byteOffset":4360},{"buffer":0,"byteLength":84,"byteOffset":4364},{"buffer":0,"byteLength":12,"byteOffset":4448},{"buffer":0,"byteLength":4,"byteOffset":4460},{"buffer":0,"byteLength":12,"byteOffset":4464},{"buffer":0,"byteLength":12,"byteOffset":4476},{"buffer":0,"byteLength":4,"byteOffset":4488},{"buffer":0,"byteLength":12,"byteOffset":4492},{"buffer":0,"byteLength":12,"byteOffset":4504},{"buffer":0,"byteLength":4,"byteOffset":4516},{"buffer":0,"byteLength":12,"byteOffset":4520},{"buffer":0,"byteLength":4,"byteOffset":4532},{"buffer":0,"byteLength":3,"byteOffset":4536},{"buffer":0,"byteLength":4,"byteOffset":4539}],"buffers":[{"byteLength":4543,"bytes":"AQAA/wAAAAEAAP8AAAABAAD/AQEBAQEBAQEBAQEBAAAAAQEBAQEBAQAAAAAAAAMDAwMDAwAA\nAAEAAAADAwMAAAAAwH8AAIBAAACAPwAAAAEAAMB/AACAQAAAgD8AAMB/AACAQAAAgD8AAAAB\nAADAfwAAgEAAAIA/AADAfwAAgEAAAIA/AAAAAQAAwH8AAIBAAACAPwEBAQICAQIBAQEBAgAA\nAAEBAQECAgECAQEBAQIAAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAABA\nAACAPwAAgD8AAABAAAAAQAAAgD8AAABAAACAPwAAgD8AAMB/AADAfwAAwH8AAABAAAAAQAAA\ngD8AAIA/AACAPwAAgD8AAABAAAAAQAAAgD8AAMB/AADAfwAAwH8AAABAAACAPwAAgD8AAIA/\nAACAPwAAgD8AAABAAACAPwAAgD/NzMw+zczMPs3MzD4AAIA/AADAfwAAwH8AAMB/AADAfwAA\nwH8AAMB/AADAfwAAwH8AAMB/AAAAQAAAgD8AAIA/AAAAQAAAAEAAAIA/AAAAQAAAgD8AAIA/\nAADAfwAAwH8AAMB/AAAAQAAAAEAAAIA/AACAPwAAgD8AAIA/AAAAQAAAAEAAAIA/AADAfwAA\nwH8AAMB/AAAAQAAAgD8AAIA/AACAPwAAgD8AAIA/AAAAQAAAgD8AAIA/AgEBAgIBAQEBAAAB\nAAABAAABAAAAAIA/AAAAAAAAAADJyMg9VlXVP6uqqj8AAIA/AADAfwAAwH8AAMB/AADAfwAA\nwH8AAMB/AADAfwAAwH8AAMB/AACAPwAAgD8AAABAAAAAQAAAAEAAAIA/AACAPwAAgD8AAABA\nAADAfwAAwH8AAMB/AAAAQAAAAEAAAIA/AACAPwAAgD8AAIA/AAAAQAAAAEAAAIA/AADAfwAA\nwH8AAMB/AACAPwAAgD8AAABAAACAPwAAgD8AAIA/AACAPwAAgD8AAABAzczMPs3MzD7NzMw+\nAACAPwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAAgD8AAIA/AAAAQAAA\nAEAAAABAAACAPwAAgD8AAIA/AAAAQAAAwH8AAMB/AADAfwAAAEAAAABAAACAPwAAgD8AAIA/\nAACAPwAAAEAAAABAAACAPwAAwH8AAMB/AADAfwAAgD8AAIA/AAAAQAAAgD8AAIA/AACAPwAA\ngD8AAIA/AAAAQAEBAgICAQEBAQAAAPMENb/zBDU/AAAAAPMENb/zBDU/AAAAAPMENb/zBDU/\nAAAAAAAAgD8AAAAAAAAAAMnIyD2rqqo/q6qqP6uqqj8AAMB/AADAfwAAwH8AAMB/AADAfwAA\nwH8AAMB/AADAfwAAwH8AAIA/AACAPwAAAEAAAABAAACAPwAAgD8AAIA/AACAPwAAAEAAAMB/\nAADAfwAAwH8AAABAAACAPwAAgD8AAIA/AACAPwAAgD8AAABAAACAPwAAgD8AAMB/AADAfwAA\nwH8AAIA/AACAPwAAAEAAAIA/AACAPwAAgD8AAIA/AACAPwAAAEDNzMw+zczMPs3MzD4AAIA/\nAADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AACAPwAAgD8AAABAAAAAQAAA\ngD8AAIA/AACAPwAAgD8AAABAAADAfwAAwH8AAMB/AAAAQAAAgD8AAIA/AACAPwAAgD8AAIA/\nAAAAQAAAgD8AAIA/AADAfwAAwH8AAMB/AACAPwAAgD8AAABAAACAPwAAgD8AAIA/AACAPwAA\ngD8AAABAAQECAgEBAQEBAAEAAAEAAAEAAAAAAIA/AAAAAAAAAADJyMg9q6qqPwAAgD+rqqo/\nAADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AACAPwAAgD8AAABAAAAAQAAA\ngD8AAIA/AACAPwAAgD8AAABAAADAfwAAwH8AAMB/AAAAQAAAgD8AAIA/AAAAQAAAAEAAAIA/\nAAAAQAAAgD8AAIA/AADAfwAAwH8AAMB/AACAPwAAgD8AAABAAAAAQAAAAEAAAIA/AACAPwAA\ngD8AAABAzczMPs3MzD7NzMw+AACAPwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/\nAADAfwAAgD8AAIA/AAAAQAAAAEAAAIA/AACAPwAAgD8AAIA/AAAAQAAAwH8AAMB/AADAfwAA\nAEAAAIA/AACAPwAAAEAAAABAAACAPwAAAEAAAIA/AACAPwAAwH8AAMB/AADAfwAAgD8AAIA/\nAAAAQAAAAEAAAABAAACAPwAAgD8AAIA/AAAAQAEBAgIBAQICAQAAAPMENT8AAACA8wQ1P/ME\nNT8AAACA8wQ1P/MENT8AAACA8wQ1PwAAgD8AAAAAAAAAAMnIyD1WVdU/q6qqP6uqqj8AAMB/\nAADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAEBAAACAPwAAgD8AAIA/AACAPwAA\ngD8AAEBAAACAPwAAgD8AAMB/AADAfwAAwH8AAIA/AACAPwAAgD8AAIA/AABAQAAAgD8AAIA/\nAACAPwAAgD/NzMw+zczMPs3MzD4AAIA/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAA\nwH8AAMB/AABAQAAAgD8AAIA/AACAPwAAgD8AAIA/AABAQAAAgD8AAIA/AADAfwAAwH8AAMB/\nAACAPwAAgD8AAIA/AACAPwAAQEAAAIA/AACAPwAAgD8AAIA/AwEBAQEBAQMBAwMBAAD/AAD/\nAAD/AAD/AACAPwAAgD8AAIA/ycjIPQICAQAAAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/\nAADAfwAAwH8AAEBAAACAPwAAgD8AAIA/AACAPwAAgD8AAEBAAACAPwAAgD8AAMB/AADAfwAA\nwH8AAIA/AACAPwAAgD8AAIA/AACAPwAAQEAAAIA/AACAPwAAgD/NzMw+zczMPs3MzD4AAIA/\nAADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AABAQAAAgD8AAIA/AACAPwAA\ngD8AAIA/AABAQAAAgD8AAIA/AADAfwAAwH8AAMB/AACAPwAAgD8AAIA/AACAPwAAgD8AAEBA\nAACAPwAAgD8AAIA/AwEBAQEBAQEDAwEDAAEAAAEAAAEAAAEAAACAPwAAgD8AAIA/ycjIPQIB\nAgAAAMB/AADAfwAAwH8AAMB/AADAfwAAwH/NzMw+zczMPs3MzD4AAIA/AADAfwAAwH8AAMB/\nAADAfwAAwH8AAMB/AwMBAwEBAwEDAwMD/wAA/wAA/wAA/wAAAACAPwAAgD8AAIA/ycjIPQMC\nAgAAAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/AADAfwAAwH8AAIA/AABAQAAAgD8AAIA/\nAACAPwAAgD8AAIA/AABAQAAAgD8AAMB/AADAfwAAwH8AAIA/AACAPwAAgD8AAIA/AACAPwAA\nQEAAAIA/AACAPwAAgD/NzMw+zczMPs3MzD4AAIA/AADAfwAAwH8AAMB/AADAfwAAwH8AAMB/\nAADAfwAAwH8AAMB/AACAPwAAQEAAAIA/AACAPwAAgD8AAIA/AACAPwAAQEAAAIA/AADAfwAA\n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\nAAAAAAAAAQ=="}]},"context":{"shiny":false,"rmarkdown":null},"vertexShader":"#line 2 1\n// File 1 is the vertex shader\n#ifdef GL_ES\n#ifdef GL_FRAGMENT_PRECISION_HIGH\nprecision highp float;\n#else\nprecision mediump float;\n#endif\n#endif\n\nattribute vec3 aPos;\nattribute vec4 aCol;\nuniform mat4 mvMatrix;\nuniform mat4 prMatrix;\nvarying vec4 vCol;\nvarying vec4 vPosition;\n\n#ifdef NEEDS_VNORMAL\nattribute vec3 aNorm;\nuniform mat4 normMatrix;\nvarying vec4 vNormal;\n#endif\n\n#if defined(HAS_TEXTURE) || defined (IS_TEXT)\nattribute vec2 aTexcoord;\nvarying vec2 vTexcoord;\n#endif\n\n#ifdef FIXED_SIZE\nuniform vec3 textScale;\n#endif\n\n#ifdef FIXED_QUADS\nattribute vec3 aOfs;\n#endif\n\n#ifdef IS_TWOSIDED\n#ifdef HAS_NORMALS\nvarying float normz;\nuniform mat4 invPrMatrix;\n#else\nattribute vec3 aPos1;\nattribute vec3 aPos2;\nvarying float normz;\n#endif\n#endif // IS_TWOSIDED\n\n#ifdef FAT_LINES\nattribute vec3 aNext;\nattribute vec2 aPoint;\nvarying vec2 vPoint;\nvarying float vLength;\nuniform float uAspect;\nuniform float uLwd;\n#endif\n\n#ifdef USE_ENVMAP\nvarying vec3 vReflection;\n#endif\n\nvoid main(void) {\n  \n#ifndef IS_BRUSH\n#if defined(NCLIPPLANES) || !defined(FIXED_QUADS) || defined(HAS_FOG) || defined(USE_ENVMAP)\n  vPosition = mvMatrix * vec4(aPos, 1.);\n#endif\n  \n#ifndef FIXED_QUADS\n  gl_Position = prMatrix * vPosition;\n#endif\n#endif // !IS_BRUSH\n  \n#ifdef IS_POINTS\n  gl_PointSize = POINTSIZE;\n#endif\n  \n  vCol = aCol;\n  \n// USE_ENVMAP implies NEEDS_VNORMAL\n\n#ifdef NEEDS_VNORMAL\n  vNormal = normMatrix * vec4(-aNorm, dot(aNorm, aPos));\n#endif\n\n#ifdef USE_ENVMAP\n  vReflection = normalize(reflect(vPosition.xyz/vPosition.w, \n                        normalize(vNormal.xyz/vNormal.w)));\n#endif\n  \n#ifdef IS_TWOSIDED\n#ifdef HAS_NORMALS\n  /* normz should be calculated *after* projection */\n  normz = (invPrMatrix*vNormal).z;\n#else\n  vec4 pos1 = prMatrix*(mvMatrix*vec4(aPos1, 1.));\n  pos1 = pos1/pos1.w - gl_Position/gl_Position.w;\n  vec4 pos2 = prMatrix*(mvMatrix*vec4(aPos2, 1.));\n  pos2 = pos2/pos2.w - gl_Position/gl_Position.w;\n  normz = pos1.x*pos2.y - pos1.y*pos2.x;\n#endif\n#endif // IS_TWOSIDED\n  \n#ifdef NEEDS_VNORMAL\n  vNormal = vec4(normalize(vNormal.xyz), 1);\n#endif\n  \n#if defined(HAS_TEXTURE) || defined(IS_TEXT)\n  vTexcoord = aTexcoord;\n#endif\n  \n#if defined(FIXED_SIZE) && !defined(ROTATING)\n  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);\n  pos = pos/pos.w;\n  gl_Position = pos + vec4(aOfs*textScale, 0.);\n#endif\n  \n#if defined(IS_SPRITES) && !defined(FIXED_SIZE)\n  vec4 pos = mvMatrix * vec4(aPos, 1.);\n  pos = pos/pos.w + vec4(aOfs,  0.);\n  gl_Position = prMatrix*pos;\n#endif\n  \n#ifdef FAT_LINES\n  /* This code was inspired by Matt Deslauriers' code in \n   https://mattdesl.svbtle.com/drawing-lines-is-hard */\n  vec2 aspectVec = vec2(uAspect, 1.0);\n  mat4 projViewModel = prMatrix * mvMatrix;\n  vec4 currentProjected = projViewModel * vec4(aPos, 1.0);\n  currentProjected = currentProjected/currentProjected.w;\n  vec4 nextProjected = projViewModel * vec4(aNext, 1.0);\n  vec2 currentScreen = currentProjected.xy * aspectVec;\n  vec2 nextScreen = (nextProjected.xy / nextProjected.w) * aspectVec;\n  float len = uLwd;\n  vec2 dir = vec2(1.0, 0.0);\n  vPoint = aPoint;\n  vLength = length(nextScreen - currentScreen)/2.0;\n  vLength = vLength/(vLength + len);\n  if (vLength > 0.0) {\n    dir = normalize(nextScreen - currentScreen);\n  }\n  vec2 normal = vec2(-dir.y, dir.x);\n  dir.x /= uAspect;\n  normal.x /= uAspect;\n  vec4 offset = vec4(len*(normal*aPoint.x*aPoint.y - dir), 0.0, 0.0);\n  gl_Position = currentProjected + offset;\n#endif\n  \n#ifdef IS_BRUSH\n  gl_Position = vec4(aPos, 1.);\n#endif\n}","fragmentShader":"#line 2 2\n// File 2 is the fragment shader\n#ifdef GL_ES\n#ifdef GL_FRAGMENT_PRECISION_HIGH\nprecision highp float;\n#else\nprecision mediump float;\n#endif\n#endif\nvarying vec4 vCol; // carries alpha\nvarying vec4 vPosition;\n#if defined(HAS_TEXTURE) || defined (IS_TEXT)\nvarying vec2 vTexcoord;\nuniform sampler2D uSampler;\n#endif\n\n#ifdef HAS_FOG\nuniform int uFogMode;\nuniform vec3 uFogColor;\nuniform vec4 uFogParms;\n#endif\n\n#if defined(IS_LIT) && !defined(FIXED_QUADS)\nvarying vec4 vNormal;\n#endif\n\n#if NCLIPPLANES > 0\nuniform vec4 vClipplane[NCLIPPLANES];\n#endif\n\n#if NLIGHTS > 0\nuniform mat4 mvMatrix;\n#endif\n\n#ifdef IS_LIT\nuniform vec3 emission;\nuniform float shininess;\n#if NLIGHTS > 0\nuniform vec3 ambient[NLIGHTS];\nuniform vec3 specular[NLIGHTS]; // light*material\nuniform vec3 diffuse[NLIGHTS];\nuniform vec3 lightDir[NLIGHTS];\nuniform bool viewpoint[NLIGHTS];\nuniform bool finite[NLIGHTS];\n#endif\n#endif // IS_LIT\n\n#ifdef IS_TWOSIDED\nuniform bool front;\nvarying float normz;\n#endif\n\n#ifdef FAT_LINES\nvarying vec2 vPoint;\nvarying float vLength;\n#endif\n\n#ifdef USE_ENVMAP\nvarying vec3 vReflection;\n#endif\n\nvoid main(void) {\n  vec4 fragColor;\n#ifdef FAT_LINES\n  vec2 point = vPoint;\n  bool neg = point.y < 0.0;\n  point.y = neg ? (point.y + vLength)/(1.0 - vLength) :\n                 -(point.y - vLength)/(1.0 - vLength);\n#if defined(IS_TRANSPARENT) && defined(IS_LINESTRIP)\n  if (neg && length(point) <= 1.0) discard;\n#endif\n  point.y = min(point.y, 0.0);\n  if (length(point) > 1.0) discard;\n#endif // FAT_LINES\n  \n#ifdef ROUND_POINTS\n  vec2 coord = gl_PointCoord - vec2(0.5);\n  if (length(coord) > 0.5) discard;\n#endif\n  \n#if NCLIPPLANES > 0\n  for (int i = 0; i < NCLIPPLANES; i++)\n    if (dot(vPosition, vClipplane[i]) < 0.0) discard;\n#endif\n    \n#ifdef FIXED_QUADS\n    vec3 n = vec3(0., 0., 1.);\n#elif defined(IS_LIT)\n    vec3 n = normalize(vNormal.xyz);\n#endif\n    \n#ifdef IS_TWOSIDED\n    if ((normz <= 0.) != front) discard;\n#endif\n\n#ifdef IS_LIT\n    vec3 eye = normalize(-vPosition.xyz/vPosition.w);\n    vec3 lightdir;\n    vec4 colDiff;\n    vec3 halfVec;\n    vec4 lighteffect = vec4(emission, 0.);\n    vec3 col;\n    float nDotL;\n#ifdef FIXED_QUADS\n    n = -faceforward(n, n, eye);\n#endif\n    \n#if NLIGHTS > 0\n    // Simulate two-sided lighting\n    if (n.z < 0.0)\n      n = -n;\n    for (int i=0;i<NLIGHTS;i++) {\n      colDiff = vec4(vCol.rgb * diffuse[i], vCol.a);\n      lightdir = lightDir[i];\n      if (!viewpoint[i]) {\n        if (finite[i]) {\n          lightdir = (mvMatrix * vec4(lightdir, 1.)).xyz;\n        } else {\n          lightdir = (mvMatrix * vec4(lightdir, 0.)).xyz;\n        }\n      }\n      if (!finite[i]) {\n        halfVec = normalize(lightdir + eye);\n      } else {\n        lightdir = normalize(lightdir - vPosition.xyz/vPosition.w);\n        halfVec = normalize(lightdir + eye);\n      }\n      col = ambient[i];\n      nDotL = dot(n, lightdir);\n      col = col + max(nDotL, 0.) * colDiff.rgb;\n      col = col + pow(max(dot(halfVec, n), 0.), shininess) * specular[i];\n      lighteffect = lighteffect + vec4(col, colDiff.a);\n    }\n#else\n    lighteffect.a = 1.;\n#endif\n    \n#else // not IS_LIT\n    vec4 colDiff = vCol;\n    vec4 lighteffect = colDiff;\n#endif\n    \n#ifdef IS_TEXT\n    vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n#endif\n    \n#ifdef HAS_TEXTURE\n\n// These calculations use the definitions from \n// https://docs.gl/gl3/glTexEnv\n\n#ifdef USE_ENVMAP\n    float m = 2.0 * sqrt(dot(vReflection, vReflection) + 2.0*vReflection.z + 1.0);\n    vec4 textureColor = texture2D(uSampler, vReflection.xy / m + vec2(0.5, 0.5));\n#else\n    vec4 textureColor = texture2D(uSampler, vTexcoord);\n#endif\n\n#ifdef TEXTURE_rgb\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(textureColor.rgb, lighteffect.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = lighteffect*vec4(textureColor.rgb, 1.);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb, lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(lighteffect.rgb + textureColor.rgb, lighteffect.a);\n#endif\n\n#endif //TEXTURE_rgb\n        \n#ifdef TEXTURE_rgba\n\n#ifdef TEXMODE_replace\n// already done\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = lighteffect*textureColor;\n#endif\n\n#ifdef TEXMODE_decal\n    textureColor = vec4((1. - textureColor.a)*lighteffect.rgb) +\n                     textureColor.a*textureColor.rgb, \n                     lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb,\n                    lighteffect.a*textureColor.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(lighteffect.rgb + textureColor.rgb,\n                    lighteffect.a*textureColor.a);\n#endif\n    \n#endif //TEXTURE_rgba\n    \n#ifdef TEXTURE_alpha\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(lighteffect.rgb, luminance);\n#endif \n\n#if defined(TEXMODE_modulate) || defined(TEXMODE_blend) || defined(TEXMODE_add)\n    textureColor = vec4(lighteffect.rgb, lighteffect.a*luminance);\n#endif\n \n#endif // TEXTURE_alpha\n    \n// The TEXTURE_luminance values are not from that reference    \n#ifdef TEXTURE_luminance\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(luminance, luminance, luminance, lighteffect.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = vec4(luminance*lighteffect.rgb, lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n                        lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(luminance + lighteffect.rgb, lighteffect.a);\n#endif\n\n#endif // TEXTURE_luminance\n \n    \n#ifdef TEXTURE_luminance_alpha\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(luminance, luminance, luminance, textureColor.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = vec4(luminance*lighteffect.rgb, \n                        textureColor.a*lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n                        textureColor.a*lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(luminance + lighteffect.rgb, \n                        textureColor.a*lighteffect.a);\n\n#endif\n\n#endif // TEXTURE_luminance_alpha\n    \n    fragColor = textureColor;\n\n#elif defined(IS_TEXT)\n    if (textureColor.a < 0.1)\n      discard;\n    else\n      fragColor = textureColor;\n#else\n    fragColor = lighteffect;\n#endif // HAS_TEXTURE\n    \n#ifdef HAS_FOG\n    // uFogParms elements: x = near, y = far, z = fogscale, w = (1-sin(FOV/2))/(1+sin(FOV/2))\n    // In Exp and Exp2: use density = density/far\n    // fogF will be the proportion of fog\n    // Initialize it to the linear value\n    float fogF;\n    if (uFogMode > 0) {\n      fogF = (uFogParms.y - vPosition.z/vPosition.w)/(uFogParms.y - uFogParms.x);\n      if (uFogMode > 1)\n        fogF = mix(uFogParms.w, 1.0, fogF);\n      fogF = fogF*uFogParms.z;\n      if (uFogMode == 2)\n        fogF = 1.0 - exp(-fogF);\n      // Docs are wrong: use (density*c)^2, not density*c^2\n      // https://gitlab.freedesktop.org/mesa/mesa/-/blob/master/src/mesa/swrast/s_fog.c#L58\n      else if (uFogMode == 3)\n        fogF = 1.0 - exp(-fogF*fogF);\n      fogF = clamp(fogF, 0.0, 1.0);\n      gl_FragColor = vec4(mix(fragColor.rgb, uFogColor, fogF), fragColor.a);\n    } else gl_FragColor = fragColor;\n#else\n    gl_FragColor = fragColor;\n#endif // HAS_FOG\n    \n}","players":[],"webGLoptions":{"preserveDrawingBuffer":true},"fastTransparency":true},"evals":[],"jsHooks":[]}
```
