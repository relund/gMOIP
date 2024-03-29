% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hull.R
\name{inHull}
\alias{inHull}
\title{Efficient test for points inside a convex hull in p dimensions.}
\usage{
inHull(
  pts,
  vertices,
  hull = NULL,
  tol = mean(mean(abs(as.matrix(vertices)))) * sqrt(.Machine$double.eps)
)
}
\arguments{
\item{pts}{A \eqn{nxp} array to test, \eqn{n} data points, in dimension \eqn{p}. If you have
many points to test, it is most efficient to call this function once with the entire set.}

\item{vertices}{A \eqn{mxp} array of vertices of the convex hull. May contain redundant
(non-vertex) points.}

\item{hull}{Tessellation (or triangulation) generated by \code{convhulln} (only works if the dimension
of the hull is \eqn{p}). If hull is \code{NULL}, then it will be generated.}

\item{tol}{Tolerance on the tests for inclusion in the convex hull. You can think of \code{tol} as the
difference a point value may be different from the values of the hull, and still be perceived
as on the surface of the hull. Because of numerical slop nothing can ever be done exactly here.
In higher dimensions, the numerical issues of floating point arithmetic will probably suggest a
larger value of \code{tol}. \code{tol} is not used if the dimension of the hull is larger than one and
not equal \eqn{p}.}
}
\value{
An integer vector of length \eqn{n} with values 1 (inside hull), -1 (outside hull) or 0
(on hull to precision indicated by \code{tol}).
}
\description{
Efficient test for points inside a convex hull in p dimensions.
}
\note{
Some of the code are inspired by the \href{https://www.mathworks.com/matlabcentral/fileexchange/10226-inhull}{Matlab code} by
John D'Errico and
\href{https://stat.ethz.ch/pipermail/r-help/2009-December/415377.html}{how to find a point inside a hull}.
If the dimension of the hull is below \eqn{p} then PCA may be used to check (a
warning will be given).
}
\examples{
## In 1D
vertices <- matrix(4, ncol = 1)
pt <- matrix(c(2,4), ncol = 1, byrow = TRUE)
inHull(pt, vertices)
vertices <- matrix(c(1,4), ncol = 1)
pt <- matrix(c(1,3,4,5), ncol = 1, byrow = TRUE)
inHull(pt, vertices)

## In 2D
vertices <- matrix(c(2,4), ncol = 2)
pt <- matrix(c(2,4, 1,1), ncol = 2, byrow = TRUE)
inHull(pt, vertices)
vertices <- matrix(c(0,0, 3,3), ncol = 2, byrow = TRUE)
pt <- matrix(c(0,0, 1,1, 2,2, 3,3, 4,4), ncol = 2, byrow = TRUE)
inHull(pt, vertices)
vertices <- matrix(c(0,0, 0,3, 3,0), ncol = 2, byrow = TRUE)
pt <- matrix(c(0,0, 1,1, 4,4), ncol = 2, byrow = TRUE)
inHull(pt, vertices)

\donttest{
## in 3D
vertices <- matrix(c(2,2,2), ncol = 3, byrow = TRUE)
pt <- matrix(c(1,1,1, 3,3,3, 2,2,2, 3,3,2), ncol = 3, byrow = TRUE)
inHull(pt, vertices)

vertices <- matrix(c(2,2,2, 4,4,4), ncol = 3, byrow = TRUE)
ini3D()
plotHull3D(vertices)
pt <- matrix(c(1,1,1, 2,2,2, 3,3,3, 4,4,4, 3,3,2), ncol = 3, byrow = TRUE)
plotPoints3D(pt, addText = TRUE)
finalize3D()
inHull(pt, vertices)

vertices <- matrix(c(1,0,0, 1,1,0, 1,0,1), ncol = 3, byrow = TRUE)
ini3D()
plotHull3D(vertices)
pt <- matrix(c(1,0.1,0.2, 3,3,2), ncol = 3, byrow = TRUE)
plotPoints3D(pt, addText = TRUE)
finalize3D()
inHull(pt, vertices)

vertices <- matrix(c(2,2,2, 2,4,4, 2,2,4, 4,4,2, 4,2,2, 2,4,2, 4,2,4, 4,4,4), ncol = 3,
            byrow = TRUE)
ini3D()
plotHull3D(vertices)
pt <- matrix(c(1,1,1, 3,3,3, 2,2,2, 3,3,2), ncol = 3, byrow = TRUE)
plotPoints3D(pt, addText = TRUE)
finalize3D()
inHull(pt, vertices)
}

## In 5D
vertices <- matrix(c(4,0,0,0,0, 0,4,0,0,0, 0,0,4,0,0, 0,0,0,4,0, 0,0,0,0,4, 0,0,0,0,0),
            ncol = 5, byrow = TRUE)
pt <- matrix(c(0.1,0.1,0.1,0.1,0.1, 3,3,3,3,3, 2,0,0,0,0), ncol = 5, byrow = TRUE)
inHull(pt, vertices)
}
\author{
Lars Relund \email{lars@relund.dk}
}
