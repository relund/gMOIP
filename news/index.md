# Changelog

## gMOIP 1.5.5

CRAN release: 2025-06-25

- Fixed non-standard license specification.

## gMOIP 1.5.4

CRAN release: 2024-10-25

- Removed obsolete package `eaf` and replaced it with `moocore`.

## gMOIP 1.5.3

CRAN release: 2024-10-09

- Add classification algorithm for extreme nondominated points.

## gMOIP 1.5.2

CRAN release: 2024-02-21

- Updated classification algorithm for nondominated sets.

## gMOIP 1.5.1

- Updated filtering algorithm for nondominated sets.

## gMOIP 1.5.0

CRAN release: 2023-05-26

- Fix bug concerning degeneration.
- Added function `plotLines2D` for plotting the lines to Ax = b.

## gMOIP 1.4.9

CRAN release: 2023-02-15

- Remove tidyverse dependence and instead depend on the packages in the
  tidyverse that are actually used.

## gMOIP 1.4.8

CRAN release: 2023-01-26

- Change `rgl.*` to `*3d` functions.
- Fixed bugs due to dependent package updates.

## gMOIP 1.4.7

CRAN release: 2021-08-23

- Fixed errors about pandoc missing and added `webshot2`.

## gMOIP 1.4.6

CRAN release: 2021-01-19

- Added support for plotting rectangles, points, planes, hulls in 3D.
  Further vignettes added.

## gMOIP 1.4.5

- Added support for plotting rectangles, points, planes, hulls in 2D.

## gMOIP 1.4.4

- Added support for adding non-dominated sets (add/update).

## gMOIP 1.4.3

CRAN release: 2020-02-20

- Added support for rays in different directions.

## gMOIP 1.4.2

- Classification of nondominated points.

## gMOIP 1.4.0

- Added support for plotting rectangles, points, planes, hulls in 3D.
- Nondominated points in 3D can now be generated.

## gMOIP 1.3.0

CRAN release: 2019-08-05

- Added support for 3D plots using RGL.
- Added high level functions for plotting.
- Can plot the criterion space for bi-objective models.

## gMOIP 1.2.0

- Added support for MILP problems too.

## gMOIP 1.1.1

- Fixed bug in `cornerPoints`.

## gMOIP 1.1.0

CRAN release: 2017-02-20

- Removed dependency on `lpSolveAPI` (seems a bit overkill). Now you
  only need to specify A, b and the coefficient.

## gMOIP 1.0.0

CRAN release: 2017-01-23

- First version of the package
