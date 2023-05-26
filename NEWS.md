# gMOIP 1.5.0

* Fix bug concerning degeneration.
* Added function `plotLines2D` for plotting the lines to Ax = b. 

# gMOIP 1.4.9

* Remove tidyverse dependence and instead depend on the packages in the tidyverse that are actually used.

# gMOIP 1.4.8

* Change `rgl.*` to `*3d` functions.
* Fixed bugs due to dependent package updates.

# gMOIP 1.4.7

* Fixed errors about pandoc missing and added `webshot2`.

# gMOIP 1.4.6

* Added support for plotting rectangles, points, planes, hulls in 3D. Further vignettes added.

# gMOIP 1.4.5

* Added support for plotting rectangles, points, planes, hulls in 2D. 

# gMOIP 1.4.4

* Added support for adding non-dominated sets (add/update). 

# gMOIP 1.4.3

* Added support for rays in different directions.

# gMOIP 1.4.2

* Classification of nondominated points.

# gMOIP 1.4.0

* Added support for plotting rectangles, points, planes, hulls in 3D. 
* Nondominated points in 3D can now be generated.

# gMOIP 1.3.0

* Added support for 3D plots using RGL.
* Added high level functions for plotting.
* Can plot the criterion space for bi-objective models.

# gMOIP 1.2.0

* Added support for MILP problems too. 

# gMOIP 1.1.1

* Fixed bug in `cornerPoints`.

# gMOIP 1.1.0

* Removed dependency on `lpSolveAPI` (seems a bit overkill). Now you only need to specify A, b and 
  the coefficient.

# gMOIP 1.0.0

* First version of the package