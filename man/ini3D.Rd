% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{ini3D}
\alias{ini3D}
\title{Initialize the RGL window.}
\usage{
ini3D(new = TRUE, clear = FALSE, ...)
}
\arguments{
\item{new}{A new window is opened (otherwise the current is cleared).}

\item{clear}{Clear the current RGL window.}

\item{...}{Further arguments passed on the the RGL plotting functions. This must be done as
lists. Currently the following arguments are supported:
\itemize{
\item \code{argsPlot3d}: A list of arguments for \code{\link[rgl:plot3d]{rgl::plot3d}}.
\item \code{argsAspect3d}: A list of arguments for \code{\link[rgl:aspect3d]{rgl::aspect3d}}.
}}
}
\value{
NULL (invisible).
}
\description{
Initialize the RGL window.
}
\examples{
\donttest{
ini3D()
pts<-matrix(c(1,1,1,5,5,5), ncol = 3, byrow = TRUE)
plotPoints3D(pts)
finalize3D()

lim <- c(-1, 7)
ini3D(argsPlot3d = list(xlim = lim, ylim = lim, zlim = lim))
plotPoints3D(pts)
finalize3D()
}
}
