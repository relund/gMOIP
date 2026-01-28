# Package index

## 2D graphics

- [`gMOIPTheme()`](http://relund.github.io/gMOIP/reference/gMOIPTheme.md)
  :

  The `ggplot` theme for the package

- [`plotCones2D()`](http://relund.github.io/gMOIP/reference/plotCones2D.md)
  : Plot a cone defined by a point in 2D.

- [`plotCriterion2D()`](http://relund.github.io/gMOIP/reference/plotCriterion2D.md)
  : Create a plot of the criterion space of a bi-objective problem

- [`plotHull2D()`](http://relund.github.io/gMOIP/reference/plotHull2D.md)
  : Plot the convex hull of a set of points in 2D.

- [`plotLines2D()`](http://relund.github.io/gMOIP/reference/plotLines2D.md)
  : Plot the lines of a linear mathematical program (Ax = b)

- [`plotNDSet2D()`](http://relund.github.io/gMOIP/reference/plotNDSet2D.md)
  : Create a plot of a discrete non-dominated set.

- [`plotPolytope2D()`](http://relund.github.io/gMOIP/reference/plotPolytope2D.md)
  : Plot the polytope (bounded convex set) of a linear mathematical
  program

- [`plotPolytope()`](http://relund.github.io/gMOIP/reference/plotPolytope.md)
  : Plot the polytope (bounded convex set) of a linear mathematical
  program (Ax \<= b)

## 3D graphics

- [`finalize3D()`](http://relund.github.io/gMOIP/reference/finalize3D.md)
  : Finalize the RGL window.
- [`ini3D()`](http://relund.github.io/gMOIP/reference/ini3D.md) :
  Initialize the RGL window.
- [`loadView()`](http://relund.github.io/gMOIP/reference/loadView.md) :
  Help function to load the view angle for the RGL 3D plot from a file
  or matrix
- [`plotCones3D()`](http://relund.github.io/gMOIP/reference/plotCones3D.md)
  : Plot a cone defined by a point in 3D.
- [`plotHull3D()`](http://relund.github.io/gMOIP/reference/plotHull3D.md)
  : Plot the convex hull of a set of points in 3D.
- [`plotMTeX3D()`](http://relund.github.io/gMOIP/reference/plotMTeX3D.md)
  : Plot TeX in the margin
- [`plotPlane3D()`](http://relund.github.io/gMOIP/reference/plotPlane3D.md)
  : Plot a plane in 3D.
- [`plotPoints3D()`](http://relund.github.io/gMOIP/reference/plotPoints3D.md)
  : Plot points in 3D.
- [`plotPolygon3D()`](http://relund.github.io/gMOIP/reference/plotPolygon3D.md)
  : Plot a polygon.
- [`plotPolytope3D()`](http://relund.github.io/gMOIP/reference/plotPolytope3D.md)
  : Plot the polytope (bounded convex set) of a linear mathematical
  program
- [`plotRectangle3D()`](http://relund.github.io/gMOIP/reference/plotRectangle3D.md)
  : Plot a rectangle defined by two corner points.
- [`plotTeX3D()`](http://relund.github.io/gMOIP/reference/plotTeX3D.md)
  : Plot TeX at a position.
- [`plotTitleTeX3D()`](http://relund.github.io/gMOIP/reference/plotTitleTeX3D.md)
  : Draw boxes, axes and other text outside the data using TeX strings.
- [`saveView()`](http://relund.github.io/gMOIP/reference/saveView.md) :
  Help function to save the view angle for the RGL 3D plot
- [`plotPolytope()`](http://relund.github.io/gMOIP/reference/plotPolytope.md)
  : Plot the polytope (bounded convex set) of a linear mathematical
  program (Ax \<= b)
- [`getTexture()`](http://relund.github.io/gMOIP/reference/getTexture.md)
  : Save a point symbol as a temporary file.

## Convex hull

- [`convexHull()`](http://relund.github.io/gMOIP/reference/convexHull.md)
  : Find the convex hull of a set of points.
- [`hullSegment()`](http://relund.github.io/gMOIP/reference/hullSegment.md)
  : Find segments (lines) of a face.
- [`inHull()`](http://relund.github.io/gMOIP/reference/inHull.md) :
  Efficient test for points inside a convex hull in p dimensions.
- [`plotHull2D()`](http://relund.github.io/gMOIP/reference/plotHull2D.md)
  : Plot the convex hull of a set of points in 2D.
- [`plotHull3D()`](http://relund.github.io/gMOIP/reference/plotHull3D.md)
  : Plot the convex hull of a set of points in 3D.
- [`dimFace()`](http://relund.github.io/gMOIP/reference/dimFace.md) :
  Return the dimension of the convex hull of a set of points.

## Matematical programming

- [`plotPolytope()`](http://relund.github.io/gMOIP/reference/plotPolytope.md)
  : Plot the polytope (bounded convex set) of a linear mathematical
  program (Ax \<= b)
- [`cornerPoints()`](http://relund.github.io/gMOIP/reference/cornerPoints.md)
  : Calculate the corner points for the polytope Ax\<=b.
- [`cornerPointsCont()`](http://relund.github.io/gMOIP/reference/cornerPointsCont.md)
  : Calculate the corner points for the polytope Ax\<=b assuming all
  variables are continuous.
- [`binaryPoints()`](http://relund.github.io/gMOIP/reference/binaryPoints.md)
  : Binary (0-1) points in the feasible region (Ax\<=b).
- [`criterionPoints()`](http://relund.github.io/gMOIP/reference/criterionPoints.md)
  : Calculate the criterion points of a set of points and ranges to find
  the set of non-dominated points (Pareto points) and classify them into
  extreme supported, non-extreme supported, non-supported.
- [`integerPoints()`](http://relund.github.io/gMOIP/reference/integerPoints.md)
  : Integer points in the feasible region (Ax\<=b).
- [`slices()`](http://relund.github.io/gMOIP/reference/slices.md) : Find
  all corner points in the slices define for each fixed integer
  combination.

## Multi-objective programming

- [`addNDSet()`](http://relund.github.io/gMOIP/reference/addNDSet.md) :
  Add discrete points to a non-dominated set and classify them into
  extreme supported, non-extreme supported, non-supported.
- [`classifyNDSet()`](http://relund.github.io/gMOIP/reference/classifyNDSet.md)
  : Classify a set of nondominated points
- [`classifyNDSetExtreme()`](http://relund.github.io/gMOIP/reference/classifyNDSetExtreme.md)
  : Find extreme points of a nondominated set of points
- [`genNDSet()`](http://relund.github.io/gMOIP/reference/genNDSet.md) :
  Generate a sample of nondominated points.
- [`genSample()`](http://relund.github.io/gMOIP/reference/genSample.md)
  : Generate a sample of points in dimension \$p\$.
- [`addRays()`](http://relund.github.io/gMOIP/reference/addRays.md) :
  Add all points on the bounding box hit by the rays.

## Other

- [`texToPng()`](http://relund.github.io/gMOIP/reference/texToPng.md) :
  Convert LaTeX to a png file
- [`pngSize()`](http://relund.github.io/gMOIP/reference/pngSize.md) : To
  size of the png file.
- [`df2String()`](http://relund.github.io/gMOIP/reference/df2String.md)
  : Convert each row to a string.
- [`mergeLists()`](http://relund.github.io/gMOIP/reference/mergeLists.md)
  : Merge two lists to one
