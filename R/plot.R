## Functions for plotting

#' Plot the convex hull of a set of points in 3D.
#'
#' @param pts A matrix with a point in each row.
#' @param drawPoints Draw the points.
#' @param drawLines Draw lines of the facets.
#' @param drawPolygons Fill the hull.
#' @param addText Add text to the points. Currently `coord` (coordinates), `rownames` (rownames)
#'   and `both` supported or a vector with text.
#' @param addRays Add the ray defined by `direction`.
#' @param direction Ray direction. If i'th entry is positive, consider the i'th column of `pts`
#'   plus a value greater than on equal zero (minimize objective $i$). If negative, consider the
#'   i'th column of `pts` minus a value greater than on equal zero (maximize objective $i$).
#' @param drawPlot Draw the ggplot. Set to FALSE if you want to combine hulls in a single plot.
#' @param drawBBoxHull If addRays then draw the hull areas hitting the bounding box also.
#' @param m Minimum values of the bounding box.
#' @param M Maximum values of the bounding box.
#' @param ... Further arguments passed on the the ggplot plotting functions. This must be done as
#'   lists. Currently the following arguments are supported:
#'
#'   * `argsGeom_point`: A list of arguments for [`ggplot2::geom_point`].
#'   * `argsGeom_path`: A list of arguments for [`ggplot2::geom_path`].
#'   * `argsGeom_polygon`: A list of arguments for [`ggplot2::geom_polygon`].
#'   * `argsGeom_label`: A list of arguments for [`ggplot2::geom_label`].
#'
#' @return The ggplot object if `drawPlot = TRUE`; otherwise, a list of ggplot components.
#' @export
#' @importFrom rlang .data
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' pts<-matrix(c(1,1), ncol = 2, byrow = TRUE)
#' plotHull2D(pts)
#' pts1<-matrix(c(2,2, 3,3), ncol = 2, byrow = TRUE)
#' plotHull2D(pts1, drawPoints = TRUE)
#' plotHull2D(pts1, drawPoints = TRUE, addRays = TRUE, addText = "coord")
#' plotHull2D(pts1, drawPoints = TRUE, addRays = TRUE, addText = "coord", drawBBoxHull = TRUE)
#' plotHull2D(pts1, drawPoints = TRUE, addRays = TRUE, direction = -1, addText = "coord")
#' pts2<-matrix(c(1,1, 2,2, 0,1), ncol = 2, byrow = TRUE)
#' plotHull2D(pts2, drawPoints = TRUE, addText = "coord")
#' plotHull2D(pts2, drawPoints = TRUE, addRays = TRUE, addText = "coord")
#' plotHull2D(pts2, drawPoints = TRUE, addRays = TRUE, direction = -1, addText = "coord")
#' ## Combine hulls
#' ggplot() +
#'   plotHull2D(pts2, drawPoints = TRUE, addText = "coord", drawPlot = FALSE) +
#'   plotHull2D(pts1, drawPoints = TRUE, drawPlot = FALSE) +
#'   gMOIPTheme() +
#'   xlab(expression(x[1])) +
#'   ylab(expression(x[2]))
#'
#' # Plotting an LP
#' A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
#' b <- c(3,27,90)
#' obj <- c(7.75, 10)
#' pts3 <- cornerPoints(A, b)
#' plotHull2D(pts3, drawPoints = TRUE, addText = "coord", argsGeom_polygon = list(fill = "red"))
plotHull2D <- function(pts,
                       drawPoints = FALSE,
                       drawLines = TRUE,
                       drawPolygons = TRUE,
                       addText = FALSE,
                       addRays = FALSE,
                       direction = 1,
                       drawPlot = TRUE,
                       drawBBoxHull = FALSE,
                       m = apply(pts,2,min)-5,
                       M = apply(pts,2,max)+5,
                       ...)
{
   args <- list(...)
   argsGeom_point <- mergeLists(list(size = 1.5, col = "black"), args$argsGeom_point)
   argsGeom_path <- mergeLists(list(size = 0.75, col = "grey40"), args$argsGeom_path)
   argsGeom_polygon <- mergeLists(list(fill = "black", alpha = 0.2), args$argsGeom_polygon)
   argsGeom_label <- mergeLists(list(nudge_x = 0.2), args$argsGeom_label)

   pts <- .checkPts(pts, p = 2)
   hull <- convexHull(pts, addRays = addRays, direction = direction, m = m, M = M)
   set <- hull$pts
   hull <- hull$hull
   d <- dimFace(set[,1:2, drop = FALSE])
   aES <- aes_string(x = colnames(set)[1], y = colnames(set)[2])
   aES1 <- aes_string(x = colnames(set)[1], y = colnames(set)[2], label = 'text')
   aES2 <- aes_string(x = colnames(set)[1], y = colnames(set)[2], label = 'addText')
   lst <- NULL

   if (d > 1) { # a polygon
      if (drawPolygons) {
         ptsT <- set[hull,]
         lst <- c(lst,
            do.call(geom_polygon, args = c(list(aES, data = ptsT), argsGeom_polygon)))
      }
   }
   if (d > 0) { # a line or polygon
      if (drawLines) {
         ptsT <- set[hull,]
         for (i in 2:nrow(ptsT)){
            if (!(ptsT$pt[i-1] == 0 & ptsT$pt[i] == 0) | drawBBoxHull) {
               ptsTT <- ptsT[(i-1):i,]
               lst <- c(lst,
                  do.call(geom_path, args = c(list(aES, data = ptsTT), argsGeom_path)))
            }
         }
         if (!(ptsT$pt[1] == 0 & ptsT$pt[nrow(ptsT)] == 0) | drawBBoxHull) {
            ptsTT <- ptsT[c(1,nrow(ptsT)),]
            lst <- c(lst,
               do.call(geom_path, args = c(list(aES, data = ptsTT), argsGeom_path)))
         }
      }
   }
   if (drawPoints | d == 0) {
      ptsT <- dplyr::filter(set, .data$pt == 1)
      lst <- c(lst,
         do.call(geom_point, args = c(list(aES, data = ptsT), argsGeom_point)))
   }

   ptsT <- dplyr::filter(set, .data$pt == 1)
   if (length(addText) > 1) {
      if (length(addText) == nrow(ptsT)) {
         lst <- c(lst,
            do.call(geom_label, args = c(list(aes_string(label = 'addText'), data = ptsT), argsGeom_label)))
      }
   } else if (length(addText) == 1) {
      if (addText == TRUE | addText == "coord") {
         ptsT$text <- paste0("(",ptsT[,1],",",ptsT[,2],")")
         lst <- c(lst,
            do.call(geom_label, args = c(list(aES1, data = ptsT), argsGeom_label)))
      } else if (addText == "rownames") {
         ptsT$text <- rownames(ptsT)
         lst <- c(lst,
            do.call(geom_label, args = c(list(aES1, data = ptsT), argsGeom_label)))
      } else if (addText == "both") {
         text <- paste0("(",ptsT[,1],",",ptsT[,2],")")
         ptsT$text <- paste(text,rownames(ptsT))
         lst <- c(lst,
            do.call(geom_label, args = c(list(aES1, data = ptsT), argsGeom_label)))
      } else if (addText != FALSE & length(addText) == nrow(ptsT)) {
         lst <- c(lst,
            do.call(geom_label, args = c(list(aES2, data = ptsT), argsGeom_label)))
      }
   }
   # if (latex) lst <- c(lst, xlab("$x_1$"), ylab("$x_2$"))
   # if (!latex) lst <- c(lst, xlab(expression(x[1])), ylab(expression(x[2])))
   if (drawPlot) lst <- ggplot() + lst
   return(lst)
}



#' Plot the polytope (bounded convex set) of a linear mathematical program
#'
#' @param A The constraint matrix.
#' @param b Right hand side.
#' @param obj A vector with objective coefficients.
#' @param type A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#' @param crit Either max or min (only used if add the iso profit line)
#' @param faces A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#'   Useful if e.g. want to show the linear relaxation of an IP.
#' @param plotFaces If \code{True} then plot the faces.
#' @param plotFeasible If \code{True} then plot the feasible points/segments
#'   (relevant for IPLP/MILP).
#' @param plotOptimum Show the optimum corner solution point (if alternative solutions
#'   only one is shown) and add the iso profit line.
#' @param latex If \code{True} make latex math labels for TikZ.
#' @param labels If \code{NULL} don't add any labels. If 'n' no labels but show the points. If
#'   'coord' add coordinates to the points. Otherwise number all points from one.
#' @param ... If 2D arguments passed to the \link{aes_string} function in
#'   \link{geom_point} or \link{geom_line}.
#'
#' @note The feasible region defined by the constraints must be bounded (i.e. no extreme rays)
#'   otherwise you may see strange results.
#'
#' @return If 2D a ggplot2 object. If 3D a rgl window with 3D plot.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @import rgl ggplot2
#' @example inst/examples/ex_polytope.R
plotPolytope <- function(A,
            b,
            obj = NULL,
            type = rep("c", ncol(A)),
            nonneg = rep(TRUE, ncol(A)),
            crit = "max",
            faces = type,
            plotFaces = TRUE,
            plotFeasible = TRUE,
            plotOptimum = FALSE,
            latex = FALSE,
            labels = NULL, ...)
{
   if (length(type)!=ncol(A)) stop("Arg. 'type' must be same length as columns in A!")
   if (length(faces)!=ncol(A)) stop("Arg. 'faces' must be same length as columns in A!")
   if (!is.null(obj))
      if (length(obj)!=ncol(A))
         stop("Arg. 'obj' must have the same columns as in A and be a single criterion!")
   if (ncol(A)==2) return(plotPolytope2D(A, b, obj, type, nonneg, crit, faces, plotFaces,
                                         plotFeasible, plotOptimum, latex, labels, ...))
   if (ncol(A)==3) return(plotPolytope3D(A, b, obj, type, nonneg, crit, faces, plotFaces,
                                         plotFeasible, plotOptimum, latex, labels, ...))
   stop("Only 2D or 3D variables supported!")
}



#' Plot the polytope (bounded convex set) of a linear mathematical program
#'
#' @param A The constraint matrix.
#' @param b Right hand side.
#' @param obj A vector with objective coefficients.
#' @param type A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#' @param crit Either max or min (only used if add the iso profit line)
#' @param faces A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#'   Useful if e.g. want to show the linear relaxation of an IP.
#' @param plotFaces If \code{True} then plot the faces.
#' @param plotFeasible If \code{True} then plot the feasible points/segments
#'   (relevant for ILP/MILP).
#' @param plotOptimum Show the optimum corner solution point (if alternative solutions
#'   only one is shown) and add the iso profit line.
#' @param latex If \code{True} make latex math labels for TikZ.
#' @param labels If \code{NULL} don't add any labels. If 'n' no labels but show the points. If
#'   'coord' add coordinates to the points. Otherwise number all points from one.
#' @param ... Further arguments passed on the the ggplot plotting functions. This must be done as
#'   lists. Currently the following arguments are supported:
#'
#'   * `argsFaces`: A list of arguments for [`plotHull2D`].
#'   * `argsFeasible`: A list of arguments for [`ggplot2::geom_point`] (if ILP)
#'                     and for [`ggplot2::geom_line`] (if MILP).
#'   * `argsLabels`: A list of arguments for [`ggplot2::geom_text`].
#'   * `argsOptimum`: A list of arguments for [`ggplot2::geom_abline`].
#'   * `argsTheme`: A list of arguments for [`ggplot2::theme`].
#'
#' @return A ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @import ggplot2
plotPolytope2D <-
   function(A,
            b,
            obj = NULL,
            type = rep("c", ncol(A)),
            nonneg = rep(TRUE, ncol(A)),
            crit = "max",
            faces = rep("c", ncol(A)),
            plotFaces = TRUE,
            plotFeasible = TRUE,
            plotOptimum = FALSE,
            latex = FALSE,
            labels = NULL,
            ...)
{
   args <- list(...)
   argsFaces <- mergeLists(list(argsGeom_polygon = list(fill = "gray90"), argsGeom_path = list(size = 0.5)), args$argsFaces)
   argsFeasible <- mergeLists(list(), args$argsFeasible)
   argsLabels <- mergeLists(list(size=3, color = "gray50", hjust = 1), args$argsLabels)
   argsOptimum <- mergeLists(list(lty="dashed"), args$argsOptimum)
   argsTheme <- mergeLists(list(), args$argsTheme)

   if (!is.null(obj) & (!is.vector(obj) | !length(obj) == ncol(A)))
     stop("Arg. obj must be a vector of same length as the number of columns in A.")
   # Set Custom theme
   # myTheme <- theme_bw()
   # myTheme <- myTheme + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   #                            panel.border = element_blank(),
   #                            #axis.line = element_blank(),
   #                            axis.line = element_line(colour = "black", size = 0.5,
   #                                                     arrow = arrow(length = unit(0.3,"cm")) ),
   #                            #axis.ticks = element_blank()
   #                            #axis.text.x = element_text(margin = margin(r = 30))
   #                            # axis.ticks.length = unit(0.5,"mm"),
   #                            #aspect.ratio=4/3,
   #                            legend.position="none"
   # )

   # Create solution plot
   p <- ggplot() #+ coord_fixed(ratio = 1)
   if (latex) p <- p + xlab("$x_1$") + ylab("$x_2$")
   if (!latex) p <- p + xlab(expression(x[1])) + ylab(expression(x[2]))

   if (plotFaces) {
      cPoints = cornerPoints(A, b, faces, nonneg)
      p <- p +
         do.call(plotHull2D, args = c(list(cPoints, drawPlot = FALSE), argsFaces))
   }

   # find feasible points
   if (all(type == "c")) {
      points <- cornerPoints(A, b, type, nonneg)
   } else if (all(type == "i")) {
      points <- integerPoints(A, b, nonneg)
   } else {
      pl <- slices(A, b, type, nonneg)
      pl <- lapply(pl, unique)
      for (i in 1:length(pl)) pl[[i]] <- cbind(pl[[i]],i)
      pl <- lapply(pl, function(x) {
         colnames(x) <- c("x1", "x2", "g")
         rownames(x) <- NULL
         x<-data.frame(x)
      })
      points <- do.call(rbind, pl)
      #points <- points[,1:2]
   }
   points <- as.data.frame(points)

   if (plotFeasible) {
      # if (all(type == "c")) {
      #    #p <- p + geom_point(aes_string(x = 'x1', y = 'x2'), data=points) #+ scale_colour_grey(start = 0.6, end = 0)
      # }
      if (all(type == "i")) {
         p <- p +
            do.call(geom_point, args = c(list(aes_string(x = 'x1', y = 'x2'), data=points), argsFeasible))
      }
      if (length(which(type == "c"))==1) {
         p <- p +
            do.call(geom_line, args = c(list(aes_string(x = 'x1', y = 'x2', group='g'), data=points), argsFeasible)) +
            do.call(geom_point, args = c(list(aes_string(x = 'x1', y = 'x2'), data=points), argsFeasible))
         # idx <- sapply(pl, function(x) nrow(x)==1)
         # pl <- pl[idx]
         # if (length(pl)>0) {
         #    tmp <- do.call(rbind, pl)
         #    p <- p +
         #       do.call(geom_point, args = c(list(aes_string(x = 'x1', y = 'x2'), data=tmp), argsFeasible))
         #       #geom_point(aes_string(x = 'x1', y = 'x2', ...), data=tmp)
         # }
      }
   }

   if (!is.null(labels)) {
      tmp <- points[,1:ncol(A)]
      tmp <- as.data.frame(tmp)
      if (labels == "coord")
         tmp$lbl <- df2String(tmp)
      else if (labels == "n")
         tmp$lbl <- ""
      else
         tmp$lbl <- 1:nrow(tmp)
      if (length(tmp$lbl)>0) {
         #p <- p + geom_point(aes_string(x = 'x1', y = 'x2'), data=tmp)
         nudgeS=-(max(tmp$x1)-min(tmp$x1))/100
         #if (anyDuplicated(cbind(tmp$x1,tmp$x2), MARGIN = 1) > 0)
            p <- p +
               do.call(ggrepel::geom_text_repel,
                       args = c(list(aes_string(x = 'x1', y = 'x2', label = 'lbl'), data=tmp),
                                argsLabels))
               #ggrepel::geom_text_repel(aes_string(x = 'x1', y = 'x2', label = 'lbl'),data=tmp, size=3, colour = "gray50")
         # if (anyDuplicated(cbind(tmp$x1,tmp$x2), MARGIN = 1) == 0)
         #    p <- p +
         #       do.call(geom_text,
         #               args = c(list(aes_string(x = 'x1', y = 'x2', label = 'lbl'), data=tmp, nudge_x = nudgeS, nudge_y = nudgeS, hjust=1),
         #                        argsLabels))
               #geom_text(aes_string(x = 'x1', y = 'x2', label = 'lbl'), data=tmp, nudge_x = nudgeS, nudge_y = nudgeS, hjust=1, size=3, colour = "gray50")
      }
   }

   if (plotOptimum) {
      if (!is.null(obj)) {    # add iso profit line
         tmp <- points
         tmp$lbl <- df2String(tmp)
         tmp$z <- as.matrix(points[,1:2]) %*% obj
         if (crit=="max") i <- which.max(tmp$z)
         if (crit=="min") i <- which.min(tmp$z)
         # if (latex) str <- paste0("$x^* = (", tmp$x1[i], ",", tmp$x2[i], ")$")
         # if (!latex) str <- paste0("x* = ", tmp$lbl[1])
         if (latex) str <- paste0("$z^* = ", round(tmp$z[i], 2) , "$")
         if (!latex) str <- paste0("z* = ", round(tmp$z[i],2) )
         if (obj[2]!=0) {
            p <- p +
               do.call(geom_abline, args = c(list(intercept = tmp$z[i]/obj[2], slope = -obj[1]/obj[2]), argsOptimum))
               #geom_abline(intercept = tmp$z[i]/obj[2], slope = -obj[1]/obj[2], lty="dashed")
         } else {
            p <- p +
               do.call(geom_abline, args = c(list(xintercept = tmp$x1[i]), argsOptimum))
               #geom_vline(xintercept = tmp$x1[i], lty="dashed")
         }
         p <- p +
            do.call(geom_label, args = c(list(aes_string(x = 'x1', y = 'x2', label = 'str'), data = tmp[i,]), argsLabels))
            #geom_label(aes_string(x = 'x1', y = 'x2', label = 'str'), data = tmp[i,], nudge_x = 1.0)
      }
   }

   p <- p +
      do.call(gMOIPTheme, args = c(list(), argsTheme))
   return(p)
}


#' Plot the polytope (bounded convex set) of a linear mathematical program
#'
#' @param A The constraint matrix.
#' @param b Right hand side.
#' @param obj A vector with objective coefficients.
#' @param type A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#' @param crit Either max or min (only used if add the iso profit line)
#' @param faces A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#'   Useful if e.g. want to show the linear relaxation of an IP.
#' @param plotFaces If \code{True} then plot the faces.
#' @param plotFeasible If \code{True} then plot the feasible points/segments
#'   (relevant for IPLP/MILP).
#' @param plotOptimum Show the optimum corner solution point (if alternative solutions
#'   only one is shown) and add the iso profit line.
#' @param latex If \code{True} make latex math labels for TikZ.
#' @param labels If \code{NULL} don't add any labels. If 'n' no labels but show the points. If
#'   'coord' add coordinates to the points. Otherwise number all points from one.
#' @param ... Arguments passed to axes3d, plot3d, title3d, text3d. Parsed using lists argsAxes3d,
#'   argsPlot3d, argsText3d and argsTitle3d.
#'
#' @note The feasible region defined by the constraints must be bounded otherwise you may see
#'   strange results.
#'
#' @return A rgl window with 3D plot.
#' @author Lars Relund \email{lars@@relund.dk}
#' @import rgl
plotPolytope3D <-
   function(A,
            b,
            obj = NULL,
            type = rep("c", ncol(A)),
            nonneg = rep(TRUE, ncol(A)),
            crit = "max",
            faces = rep("c", ncol(A)),
            plotFaces = TRUE,
            plotFeasible = TRUE,
            plotOptimum = FALSE,
            latex = FALSE,
            labels = NULL, ...)
   {
      # set plot parameters
      args <- list(...)
      argsAxes3d <- mergeLists(list(edges=c('x', 'y', 'z')), args$argsAxes3d)
      argsPlot3d <- mergeLists(list(xlab = '', box = FALSE, axes = FALSE), args$argsPlot3d)
      argsTitle3d <- mergeLists(list(xlab = 'x1', ylab = 'x2', zlab = 'x3'), args$argsTitle3d)
      argsText3d <- mergeLists(list(), args$argsText3d)  #cex = c(1.1,1.1), adj=2, font = 2

      #open3d()
      do.call(plot3d, args = c(list(x = replicate(2, 1:3), type = 'n'), argsPlot3d))
      aspect3d("iso")

      plotMat <- function(mat, col="black") {
         if (nrow(mat)==1) pch3d(mat, color=col, cex = 0.1, pch = 16) #points3d(mat, col=col, size = 10)
         if (nrow(mat)==2) { #segments3d(mat, col = "black", lwd=10, smooth= TRUE)
            cyl <- cylinder3d(mat, radius = 0.01)
            shade3d(cyl, col = col)
         }
         if (nrow(mat)==3) triangles3d(mat, col=col, alpha=0.6)
         #if (nrow(mat)==4) quads3d(mat, col="black", fill= TRUE)
         if (nrow(mat)>=4) {
            # idx <- apply(mat, 2, function(x) diff(range(x)) < 1e-10 )
            # hull <- geometry::convhulln(mat[,!idx])
            hull <- geometry::convhulln(mat, options = "QJ")
            tri <- t(hull)
            triangles3d(mat[tri,1], mat[tri,2], mat[tri,3], col=col, alpha=0.2)
         }
      }

      if (plotFaces) { # plot faces
         vertices <- cornerPoints(A, b, faces, nonneg)
         plotMat(vertices, col="grey100")
         # points3d(vertices, col="blue", size = 10)
         # text3d(vertices, text = paste0(1:nrow(vertices)), cex = 2, adj=2)
         hull <- geometry::convhulln(vertices)
         # tri <- t(hull)
         #
         # # colfunc <- colorRampPalette(c("red","yellow","springgreen","royalblue"))
         # #colfunc <- colorRampPalette(c("cornflowerblue", "peachpuff", "cadetblue2", "khaki"))
         # # colours <- paste0("Gray", seq(80,10, by = -10))
         # #colours <- colfunc(ncol(tri))
         # triangles3d(vertices[tri,1],vertices[tri,2],vertices[tri,3], col="grey70", alpha=0.6)
         # # find and plot segments
         seg <- t(hullSegment(vertices, hull))
         segments3d(vertices[seg,1],vertices[seg,2],vertices[seg,3], col = "grey70", lwd=2)
      }

      if (plotFeasible) {
         if (all(type == "c")) {
            # don't plot anything
         } else if (all(type == "i")) {
            iPoints <- integerPoints(A, b, nonneg)
            #points3d(iPoints[,1:3], col="black", size = 7)
            pch3d(iPoints[,1:3], color = "black", cex = 0.1, pch = 16)
         } else {
            pl <- slices(A, b, type, nonneg)
            for (i in 1:length(pl)) {
               mat <- pl[[i]]
               if (is.null(mat)) next
               if (nrow(mat)==1) {
                  #pch3d(mat, color="black", cex = 0.1, pch = 16)
                  points3d(mat, col="black", size = 7)
               }
               if (nrow(mat)==2) { #segments3d(mat, col = "black", lwd=10, smooth= TRUE)
                  cyl <- cylinder3d(mat, radius = 0.015)
                  shade3d(cyl, col = "black")
               }
               if (nrow(mat)==3) triangles3d(mat, col="grey100", alpha=0.6)
               #if (nrow(mat)==4) quads3d(mat, col="black", fill= TRUE)
               if (nrow(mat)>=4) {
                  # idx <- apply(mat, 2, function(x) diff(range(x)) < 1e-10 )
                  # hull <- geometry::convhulln(mat[,!idx])
                  hull <- geometry::convhulln(mat, options = "QJ")
                  tri <- t(hull)
                  triangles3d(mat[tri,1], mat[tri,2], mat[tri,3], col="grey100", alpha=0.6)
               }
            }
         }
      }

      if (plotOptimum) {
         if (is.null(obj)) stop("You need to specify the objective coefficients when using argument plotOption = TRUE.")
         vertices <- cornerPoints(A, b, type, nonneg) #points3d(vertices[,1:3], col="blue", size = 10)
         val <- vertices[,1:3] %*% as.matrix(obj)
         idx <- which.max(val)
         val <- vertices[idx,1:3]
         points3d(val[1], val[2], val[3], col="red", size = 14)
         #pch3d(val[1], val[2], val[3], col="red", cex = 0.2, pch = 16)
         #spheres3d(val[1], val[2], val[3], col="black", radius = 1)
         #planes3d(obj, d = -val, alpha = 0.6)
         #arrow3d(c(0,0,0), 0.25*obj, type="lines", barblen = 0.01, col="red", lwd=5)
      }

      if (!is.null(labels)) {
         if (all(type == "c")) {
            points <- cornerPoints(A, b, type, nonneg)
         } else if (all(type == "i")) {
            points <- integerPoints(A, b, nonneg)
         } else {
            points <- slices(A, b, type, nonneg, collapse = TRUE)
         }
         points <- as.data.frame(points)
         rownames(points) <- NULL
         if ((length(which(type == "c"))<length(type) & length(which(type == "c"))>0) |
             (length(which(type == "c"))==length(type))) {
            #pch3d(iPoints[,1:3], col="black", cex = 0.1, pch = 16)
            points3d(points[,1:3], col="grey50", size = 7)
         }

         if (labels=="coord")
            points$lbl <- df2String(points)
         else if (labels == "n")
            points$lbl <- ""
         else points$lbl <- 1:nrow(points)
         do.call(text3d, args = c(list(points[,1:3], texts = points$lbl), argsText3d))
      }

      do.call(axes3d, args = argsAxes3d)
      do.call(title3d, args = argsTitle3d)
   }


#' Merge two lists to one
#'
#' @param a First list.
#' @param b Second list.
mergeLists <- function (a,b) {
   c(a[setdiff(names(a), names(b))], b)
}


#' Create a plot of the criterion space of a bi-objective problem
#'
#' @param A The constraint matrix.
#' @param b Right hand side.
#' @param obj A p x n matrix(one row for each criterion).
#' @param type A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#' @param crit Either max or min (only used if add the iso profit line).
#' @param addTriangles Add search triangles defined by the non-dominated extreme
#'   points.
#' @param addHull Add the convex hull and the rays.
#' @param plotFeasible If \code{True} then plot the criterion points/slices.
#' @param latex If true make latex math labels for TikZ.
#' @param labels If \code{NULL} don't add any labels. If 'n' no labels but show the points. If
#'   'coord' add coordinates to the points. Otherwise number all points from one.
#'
#' @note Currently only points are checked for dominance. That is, for MILP
#'   models some nondominated points may in fact be dominated by a segment.
#' @return The ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @import ggplot2
#' @example inst/examples/ex_criterion.R
plotCriterion2D <- function(A,
                            b,
                            obj,
                            type = rep("c", ncol(A)),
                            nonneg = rep(TRUE, ncol(A)),
                            crit = "max",
                            addTriangles = FALSE,
                            addHull = TRUE,
                            plotFeasible = TRUE,
                            latex = FALSE,
                            labels = NULL)
{
   # Set Custom theme
   myTheme <- theme_bw()
   myTheme <-
      myTheme + theme(
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         #axis.line = element_blank(),
         axis.line = element_line(
            colour = "black",
            size = 0.5,
            arrow = arrow(length = unit(0.3, "cm"))
         ),
         #axis.ticks = element_blank()
         #axis.text.x = element_text(margin = margin(r = 30))
         # axis.ticks.length = unit(0.5,"mm"),
         #aspect.ratio=4/3,
         legend.position = "none"
      )

   # First find all relevant points
   if (all(type == "i")) {
      points <- integerPoints(A, b, nonneg)
   } else if (all(type == "c")) {
      points <- cornerPoints(A, b, type, nonneg)
   } else {
      points <- slices(A, b, type, nonneg, collapse = TRUE)
   }
   points <- criterionPoints(points, obj, crit, labels)
   if (all(type == "c")) { # if cont then no non-ext
      points$nd[points$nd & !points$se] <- FALSE
      points$se[points$nd & !points$se] <- FALSE
      points$sne[points$nd & !points$se] <- FALSE
   }
   #dat <<- points  # hack to get data as data frame

   if (crit=="max") points <- points[order(-points$z2,-points$z1),]
   if (crit=="min") points <- points[order(points$z2,points$z1),]

   # Initialize plot
   p <- ggplot(points, aes_q(x = quote(z1), y = quote(z2), col = "grey10") )
   if (latex) p <- p + xlab("$z_1$") + ylab("$z_2$")
   if (!latex) p <- p + xlab(expression(z[1])) + ylab(expression(z[2]))

   # Add hull plus rays
   if (addHull) {
      tmp<-points[points$se & !duplicated(cbind(points$z1,points$z2), MARGIN = 1),]
      delta <- max( (max(points$z1)-min(points$z1))/10, (max(points$z2)-min(points$z2))/10 )
      if (crit=="max") {
         tmp<-rbind(tmp[1:2,],tmp,tmp[1,]) # add rows
         tmp$z1[1] <- min(points$z1) - delta
         tmp$z2[1] <- min(points$z2) - delta
         tmp$z1[2] <- min(points$z1) - delta
         tmp$z2[2] <- max(points$z2)
         tmp$z1[length(tmp$z1)] <- max(points$z1)
         tmp$z2[length(tmp$z1)] <- min(points$z2)- delta
      }
      if (crit=="min") {
         tmp<-rbind(tmp[1,],tmp,tmp[1:2,]) # add rows
         tmp$z1[1] <- max(points$z1) + delta
         tmp$z2[1] <- min(points$z2)
         tmp$z1[length(tmp$z1)-1] <- min(points$z1)
         tmp$z2[length(tmp$z1)-1] <- max(points$z2) + delta
         tmp$z1[length(tmp$z1)] <- max(points$z1) + delta
         tmp$z2[length(tmp$z1)] <- max(points$z2) + delta
      }
      p <- p + geom_polygon(fill="gray95", col = NA, data=tmp)
   }

   if (plotFeasible) {
      # plot feasible areas
      if (all(type == "c")) {
         idx <- grDevices::chull(points[,c("z1","z2")])
         p <- p + geom_polygon(data = points[idx,], aes_string(x = 'z1', y = 'z2'),
                               fill=NA, size = 0.5, linetype = 1, col="grey80", alpha=0.6)
         p <- p + geom_point(aes_string(colour = 'nd', shape = 'se'), data = points) +
            scale_colour_grey(start = 0.6, end = 0)
      } else if (all(type == "i")) {
         #iPoints <- integerPoints(A, b, nonneg)
         #iPoints <- criterionPoints(iPoints, obj, crit, labels)
         p <- p + geom_point(aes_string(colour = 'nd', shape = 'se'), data = points) +
            #    #coord_fixed(ratio = 1) +
            scale_colour_grey(start = 0.6, end = 0)
      } else {
         pl <- slices(A, b, type, nonneg)
         pl <- lapply(pl, function(x) {
            x <- x %*% t(obj)
            if (is.vector(x)) x <- matrix(x, nrow=1)
            x[,1:2, drop = FALSE]
         })
         idx <- lapply(pl, grDevices::chull)
         #idx <- lapply(idx, function(x) c(x, x[1]))
         pl <- mapply(function(x,y){
            x[y,,drop = FALSE]
         },
         x = pl, y = idx, SIMPLIFY = FALSE)
         for (i in 1:length(pl)) pl[[i]] <- cbind(pl[[i]],i)
         # pl <- lapply(pl, function(x) {
         #    colnames(x) <- c("z1", "z2", "g")
         #    rownames(x) <- NULL
         #    x<-data.frame(x)
         # })
         pl <- do.call(rbind, pl)
         pl <- as.data.frame(pl)
         colnames(pl) <- c("z1", "z2", "g")
         rownames(pl) <- NULL
         p <- p + geom_polygon(
            data = pl,
            alpha = 0.6, fill = "grey",
            aes_string(
               x = "z1",
               y = "z2",
               group = "g"
            )
         ) + scale_fill_identity()
         points$co <- points$se | points$sne
         p <- p + geom_point(aes_string(colour = 'co', shape = 'se'), data = points) +
            scale_colour_grey(start = 0.8, end = 0)
      }
   }
   # Add triangles
   if (addTriangles) {
      tmp<-points[points$se | points$sne,]
      if (length(tmp$z1)>1) { # triangles
         for (r in 1:(dim(tmp)[1] - 1)) {
            p <- p +
               geom_segment(
                  x = tmp$z1[r],
                  y = tmp$z2[r],
                  xend = tmp$z1[r + 1],
                  yend = tmp$z2[r + 1],
                  colour = "gray"
               ) +
               geom_segment(
                  x = tmp$z1[r],
                  y = tmp$z2[r],
                  xend = tmp$z1[r],
                  yend = tmp$z2[r + 1],
                  colour = "gray"
               ) +
               geom_segment(
                  x = tmp$z1[r],
                  y = tmp$z2[r + 1],
                  xend = tmp$z1[r + 1],
                  yend = tmp$z2[r + 1],
                  colour = "gray"
               )
         }
      }
   }

   nudgeC=-(max(points$z1)-min(points$z1))/100
   if (!is.null(labels) & anyDuplicated(round(cbind(points$z1,points$z2),10), MARGIN = 1) > 0)
      p <- p + ggrepel::geom_text_repel(aes_string(label = 'lbl'), size=3,
                                        colour = "gray50", data=points)
   if (!is.null(labels) & anyDuplicated(round(cbind(points$z1,points$z2),10), MARGIN = 1) == 0)
      p <- p + geom_text(aes_string(label = 'lbl'), nudge_x = nudgeC, nudge_y = nudgeC,
                         hjust=1, size=3, colour = "gray50", data=points)
   p <- p + myTheme
   return(p)
}


#' Help function to save the view angle for the RGL 3D plot
#'
#' @param fname The file name of the view.
#' @param overwrite Overwrite existing file.
#' @param print Print the view so can be copied to R code (no file is saved).
#'
#' @note Only save if the file name don't exists.
#' @return NULL (invisible).
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @examples
#' view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0,
#' 0.910147845745087, -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183,
#' 0.97196090221405, 0.231208890676498, 0, 0, 0, 0, 1), nc = 4)
#'
#' loadView(v = view)
#' A <- matrix( c(3, 2, 5, 2, 1, 1, 1, 1, 3, 5, 2, 4), nc = 3, byrow = TRUE)
#' b <- c(55, 26, 30, 57)
#' obj <- c(20, 10, 15)
#' plotPolytope(A, b, plotOptimum = TRUE, obj = obj, labels = "coord")
#'
#' # Try to modify the angle in the RGL window
#' saveView(print = TRUE)  # get the viewangle to insert into R code
saveView <- function(fname = "view.RData", overwrite = FALSE, print = FALSE) {
   if (print) {
      view <- rgl::par3d()$userMatrix
      cat(paste0("view <- matrix( c(", paste0(view, collapse = ", "), "), nc = 4)"))
   } else if (!file.exists(fname) | overwrite) {
      view <- rgl::par3d()$userMatrix
      save(view, file = fname)
      message(paste0("RGL view saved to RData file ", fname, "."))
   }
   return(invisible(NULL))
}


#' Help function to load the view angle for the RGL 3D plot from a file or matrix
#'
#' @param fname The file name of the view.
#' @param v The view matrix.
#' @param clear Call \link{clear3d}.
#' @param close Call \link{rgl.close}.
#' @param zoom Zoom level.
#' @param ... Additional parameters passed to \link{view3d}.
#'
#' @return NULL
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @examples
#' view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0,
#' 0.910147845745087, -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183,
#' 0.97196090221405, 0.231208890676498, 0, 0, 0, 0, 1), nc = 4)
#'
#' loadView(v = view)
#' A <- matrix( c(3, 2, 5, 2, 1, 1, 1, 1, 3, 5, 2, 4), nc = 3, byrow = TRUE)
#' b <- c(55, 26, 30, 57)
#' obj <- c(20, 10, 15)
#' plotPolytope(A, b, plotOptimum = TRUE, obj = obj, labels = "coord")
#'
#' # Try to modify the angle in the RGL window
#' saveView(print = TRUE)  # get the viewangle to insert into R code
loadView <- function(fname = "view.RData", v = NULL, clear = TRUE, close = FALSE, zoom = 1, ...) {
   if (clear) rgl::clear3d()
   if (close) rgl::rgl.close()
   if (!is.null(v)) {
      rgl::view3d(userMatrix = v, zoom = zoom, ...)
   } else {
      if (file.exists(fname)) {
         load(fname)
         rgl::view3d(userMatrix = v)
      } else {
         warning(paste0("Can't load view in file ", fname, "!"))
      }
   }
}



#' Create a plot of a discrete non-dominated set.
#'
#' @param points Data frame with non-dominated points.
#' @param crit Either max or min (only used if add the iso profit line). A vector is currently not
#'   supported.
#' @param addTriangles Add search triangles defined by the non-dominated extreme points.
#' @param addHull Add the convex hull and the rays.
#' @param latex If true make latex math labels for TikZ.
#' @param labels If \code{NULL} don't add any labels. If 'n' no labels but show the points. If
#'   'coord' add coordinates to the points. Otherwise number all points from one.
#'
#' @note Currently only points are checked for dominance. That is, for MILP models some
#'   nondominated points may in fact be dominated by a segment.
#' @return The ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @import ggplot2
#' @examples
#' dat <- data.frame(z1=c(12,14,16,18,18,18,14,15,15), z2=c(18,16,12,4,2,6,14,14,16))
#' points <- addNDSet(dat, crit = "min", keepDom = TRUE)
#' plotNDSet2D(points, crit = "min", addTriangles = TRUE)
#' plotNDSet2D(points, crit = "min", addTriangles = FALSE)
#' plotNDSet2D(points, crit = "min", addTriangles = TRUE, addHull = FALSE)
#' points <- addNDSet(dat, crit = "max", keepDom = TRUE)
#' plotNDSet2D(points, crit = "max", addTriangles = TRUE)
#' plotNDSet2D(points, crit = "max", addHull = FALSE)
plotNDSet2D <- function(points,
                        crit,
                        addTriangles = FALSE,
                        addHull = TRUE,
                        latex = FALSE,
                        labels = NULL)
{
   # Set Custom theme
   myTheme <- theme_bw()
   myTheme <-
      myTheme + theme(
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         #axis.line = element_blank(),
         axis.line = element_line(
            colour = "black",
            size = 0.5,
            arrow = arrow(length = unit(0.3, "cm"))
         ),
         #axis.ticks = element_blank()
         #axis.text.x = element_text(margin = margin(r = 30))
         # axis.ticks.length = unit(0.5,"mm"),
         #aspect.ratio=4/3,
         legend.position = "none"
      )
   if (crit=="max") points <- points[order(-points$z2,-points$z1),]
   if (crit=="min") points <- points[order(points$z2,points$z1),]
   # Initialize plot
   p <- ggplot(points, aes_q(x = quote(z1), y = quote(z2)) )
   if (latex) p <- p + xlab("$z_1$") + ylab("$z_2$")
   if (!latex) p <- p + xlab(expression(z[1])) + ylab(expression(z[2]))

   # Add hull plus rays
   if (addHull) {
      di <- .mToDirection(crit, 2)
      p <- p +
         plotHull2D(points[points$nd, c("z1", "z2")],
                    addRays = TRUE, drawLines = TRUE, drawBBoxHull = FALSE,
                    direction = di, drawPlot = FALSE)
   }

   # Add triangles
   if (addTriangles) {
      tmp<-points[points$se | points$sne,]
      if (length(tmp$z1)>1) { # triangles
         for (r in 1:(dim(tmp)[1] - 1)) {
            p <- p +
               geom_segment(
                  x = tmp$z1[r],
                  y = tmp$z2[r],
                  xend = tmp$z1[r + 1],
                  yend = tmp$z2[r + 1],
                  colour = "gray"
               ) +
               geom_segment(
                  x = tmp$z1[r],
                  y = tmp$z2[r],
                  xend = tmp$z1[r],
                  yend = tmp$z2[r + 1],
                  colour = "gray"
               ) +
               geom_segment(
                  x = tmp$z1[r],
                  y = tmp$z2[r + 1],
                  xend = tmp$z1[r + 1],
                  yend = tmp$z2[r + 1],
                  colour = "gray"
               )
         }
      }
   }

   p <- p + geom_point(aes_string(colour = 'nd', shape = 'se'), data = points) +
      #    #coord_fixed(ratio = 1) +
      scale_colour_grey(start = 0.6, end = 0)

   nudgeC=-(max(points$z1)-min(points$z1))/100
   if (!is.null(labels) &
       anyDuplicated(round(cbind(points$z1, points$z2), 10), MARGIN = 1) > 0)
      p <- p + ggrepel::geom_text_repel(
         aes_string(label = 'lbl'),
         size = 3,
         colour = "gray50",
         data = points
      )
   if (!is.null(labels) &
       anyDuplicated(round(cbind(points$z1, points$z2), 10), MARGIN = 1) == 0)
      p <-
      p + geom_text(
         aes_string(label = 'lbl'),
         nudge_x = nudgeC,
         nudge_y = nudgeC,
         hjust = 1,
         size = 3,
         colour = "gray50",
         data = points
      )
   p <- p + myTheme
   return(p)
}


#' Plot a rectangle defined by two corner points.
#'
#' The rectangle is defined by {x|a <= x <= b} where a is the minimum values and
#' b is the maximum values.
#'
#' @param a A vector of length 3.
#' @param b A vector of length 3.
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlot3d`: A list of arguments for [`rgl::plot3d`].
#'   * `argsSegments3d`: A list of arguments for [`rgl::segments3d`][rgl::points3d].
#'   * `argsPolygon3d`: A list of arguments for [`rgl::polygon3d`].
#'   * `argsShade3d`: A list of arguments for [`rgl::shade3d`][rgl::mesh3d].
#'
#' @return Object ids (invisible).
#' @export
#'
#' @examples
#' ini3D()
#' plotRectangle3D(c(0,0,0), c(1,1,1))
#' plotRectangle3D(c(1,1,1), c(4,4,3), drawPoints = TRUE, drawLines = FALSE,
#'            argsPlot3d = list(size=2, type="s", alpha=0.3))
#' ids <- plotRectangle3D(c(2,2,2), c(3,3,2.5), argsPolygon3d = list(alpha = 1) )
#' finalize3D()
#' # rgl.pop(id = ids) remove last object
plotRectangle3D <- function(a, b, ...) {
   args <- list(...)
   argsSegments3d <- mergeLists(list(), args$argsSegments3d)
   argsPlot3d <- mergeLists(list(), args$argsPlot3d)
   argsShade3d <- mergeLists(list(), args$argsShade3d)
   argsPolygon3d <- mergeLists(list(), args$argsPolygon3d)

   if (any(a == b)) stop("Define different min and max values!")
   if (is.matrix(a)) a <- as.vector(a)
   if (is.matrix(b)) b <- as.vector(b)
   x<-matrix(c(a,b), ncol=3, byrow = TRUE)
   x<-expand.grid(x=c(x[1,1],x[2,1]), y=c(x[1,2],x[2,2]), z=c(x[1,3],x[2,3]))
   argsP <- list()
   if (!is.null(args$drawPoints)) argsP$drawPoints <- args$drawPoints
   if (!is.null(args$drawLines)) argsP$drawLines <- args$drawLines
   lst <- do.call(plotHull3D, args = c(list(x), list(argsSegments3d = argsSegments3d,
                                              argsPlot3d = argsPlot3d, argsShade3d = argsShade3d,
                                              argsPolygon3d = argsPolygon3d), argsP))
   return(invisible(lst$ids))
}


#' Plot a polygon.
#'
#' @param pts Vertices.
#' @param useShade Plot shade of the polygon.
#' @param useLines Plot lines inside the polygon.
#' @param usePoints Plot point shapes inside the polygon.
#' @param useFrame Plot a frame around the polygon.
#' @param ... Further arguments passed on the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsShade`: A list of arguments for [`rgl::polygon3d`] (n > 4 vertices),
#'                  [rgl::triangles3d] (n = 3 vertices) and [rgl::quads3d] (n = 4 vertices)
#'                  if `useShade = TRUE`.
#'   * `argsFrame`: A list of arguments for [`rgl::lines3d`] if `useFrame = TRUE`.
#'   * `argsPoints`: A list of arguments for [`rgl::shade3d`] if `usePoints = TRUE`. It is important
#'                   to give a texture using `texture`. A texture can be set using [getTexture].
#'   * `argsLines`: A list of arguments for [rgl::persp3d] when `useLines = TRUE`. Moreover, the list
#'                  may contain `lines`: number of lines.
#'
#' @return Object ids (invisible).
#' @importFrom stats lm predict
#' @export
#'
#' @examples
#' pts0 <- data.frame(x = c(1,0,0,0.4), y = c(0,1,0,0.3), z = c(0,0,1,0.3))
#' pts <- data.frame(x = c(1,0,0), y = c(0,1,0), z = c(0,0,1))
#'
#' ini3D()
#' plotPolygon3D(pts)
#' finalize3D()
#'
#' ini3D()
#' plotPolygon3D(pts, argsShade = list(color = "red", alpha = 1))
#' finalize3D()
#'
#' ini3D()
#' plotPolygon3D(pts, useFrame = TRUE, argsShade = list(color = "red", alpha = 0.5),
#'               argsFrame = list(color = "green"))
#' finalize3D()
#'
#' ini3D()
#' plotPolygon3D(pts, useFrame = TRUE, useLines = TRUE, useShade = TRUE,
#'               argsShade = list(color = "red", alpha = 0.2),
#'               argsLines = list(color = "blue"))
#' finalize3D()
#'
#' ini3D()
#' ids <- plotPolygon3D(pts, usePoints = TRUE, useShade = TRUE,
#'               argsPoints = list(color = "blue", texture = getTexture(pch = 16, cex = 20)))
#' finalize3D()
#' # rgl.pop(id = ids) # remove object again
#'
#' \donttest{
#' # In general you have to finetune size and numbers when you use textures
#' # Different pch
#' for (i in 0:3) {
#'   fname <- getTexture(pch = 15+i, cex = 30)
#'   ini3D(TRUE)
#'   plotPolygon3D(pts, usePoints = TRUE, argsPoints = list(texture = fname))
#'   finalize3D()
#' }
#'
#' # Size of pch
#' for (i in 1:4) {
#'   fname <- getTexture(pch = 15+i, cex = 10 * i)
#'   ini3D(TRUE)
#'   plotPolygon3D(pts, usePoints = TRUE, argsPoints = list(texture = fname))
#'   finalize3D()
#' }
#'
#' # Number of pch
#' fname <- getTexture(pch = 16, cex = 20)
#' for (i in 1:4) {
#'   ini3D(TRUE)
#'   plotPolygon3D(pts, usePoints = TRUE,
#'                 argsPoints = list(texture = fname, texcoords = rbind(pts$x, pts$y)*5*i))
#'   finalize3D()
#' }
#' }
plotPolygon3D <- function(pts, useShade = TRUE, useLines = FALSE, usePoints = FALSE,
                          useFrame = TRUE, ...) {
   args <- list(...)
   argsShade <- mergeLists(list(color = "black", col = "grey40",
                                    lwd = 2, alpha = 0.2, fill = TRUE,
                                    texcoords = rbind(pts[,1], pts[,2])*10 ),
                               args$argsShade)
   argsFrame <- mergeLists(list(color = "black", lwd = 1, alpha = 0.8), args$argsFrame)
   argsPoints <- mergeLists(list(color = "white", specular = "black", alpha = 0.5,
                                 texcoords = rbind(pts[,1], pts[,2])*10 ),
                           args$argsPoints)
   argsLines <- mergeLists(list(color = "black", lwd = 1, alpha = 0.4, lines = 50, back = 'lines',
                                front = 'lines', lit = FALSE),
                           args$argsLines)

   ids <- NULL
   pts <- .checkPts(pts, p = 3, asDF = TRUE)
   if (dimFace(pts) != 2) stop("Vertices don't define a polygon!")
   if (usePoints) useShade = FALSE

   if (useShade) {
      if (nrow(pts) > 3) ids <- c(ids, do.call(rgl::polygon3d, args = c(list(pts), argsShade)))
      if (nrow(pts) == 3) ids <- c(ids, do.call(rgl::triangles3d, args = c(list(pts), argsShade)))
   }
   if (useFrame) {
      n <- length(pts[,1])
      nas <- n+1
      prev <- 0L
      loop <- integer()
      for (i in seq_along(nas)) {
         loop <- c(loop, if (i > 1) NA, (prev + 1L):(nas[i] - 1L), prev + 1L)
         prev <- nas[i]
      }
      res <- cbind(pts[loop,1], pts[loop,2], pts[loop,3])
      ids <- c(ids, do.call(rgl::lines3d, args = c(list(res), argsFrame)))
   }
   if (usePoints) {
      if (nrow(pts) > 3)
         poly <- do.call(rgl::polygon3d, args = c(list(pts, plot = FALSE), argsPoints))
      if (nrow(pts) == 3)
         poly <- tmesh3d(rbind(pts[,1], pts[,2], pts[,3], 1), indices = 1:3)
      poly$texcoords <- argsPoints$texcoords
      ids <- c(ids, do.call(rgl::shade3d, args = c(list(poly), argsPoints)))
   }
   if (useLines) {
      if (!rgl::rgl.cur()) stop("Option useLines need an open rgl window!")
      limits <- rgl::par3d()$bbox
      m <- c(limits[1], limits[3], limits[5])
      M <- c(limits[2], limits[4], limits[6])
      if (all(m == M)) stop("The rgl window's bounding box is not valid!")
      # do the mesh
      x <- seq(m[1], M[1], length.out = argsLines$lines)
      y <- seq(m[2], M[2], length.out = argsLines$lines)
      xy <- expand.grid(x, y)
      colnames(pts) <- c("x", "y", "z")
      xy[sp::point.in.polygon(xy[,1], xy[,2], pts$x, pts$y) <= 0,] <- NA
      z <- predict(lm(z ~ x + y, data = pts), newdata = data.frame(x=xy[,1], y=xy[,2]))
      ids <- c(ids, do.call(rgl::persp3d, args = c(list(x, y, z, add = TRUE), argsLines)))
   }
   return(invisible(ids))
}


#' Save a pch symbol as a temporary file.
#'
#' @param pch Pch number/symbol.
#' @param cex Pch size
#' @param ...  Further arguments passed to `plot`.
#'
#' @return The file name.
#' @export
#'
#' @examples
#' # Pch shapes
#' generateRPointShapes<-function(){
#'    oldPar<-par()
#'    par(font=2, mar=c(0.5,0,0,0))
#'    y=rev(c(rep(1,6),rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
#'    x=c(rep(1:5,5),6)
#'    plot(x, y, pch = 0:25, cex=1.5, ylim=c(1,5.5), xlim=c(1,6.5),
#'         axes=FALSE, xlab="", ylab="", bg="blue")
#'    text(x, y, labels=0:25, pos=3)
#'    par(mar=oldPar$mar,font=oldPar$font )
#' }
#' generateRPointShapes()
#'
#' getTexture()
getTexture <- function(pch = 16, cex = 10, ...) {
   filename <- tempfile(fileext = ".png")
   grDevices::png(filename = filename)
   graphics::plot(1, 1, pch = pch, cex = cex, axes=FALSE, xlab="", ylab="", ...)
   grDevices::dev.off()
   return(filename)
}



#' Plot a cone defined by a point in 3D.
#'
#' The cones are defined as the point plus R3+.
#'
#' @param pts A matrix with a point in each row.
#' @param drawPoint Draw the points defining the cone.
#' @param drawLines Draw lines of the cone.
#' @param drawPolygons Draw polygons of the cone.
#' @param rectangle Draw the cone as a rectangle.
#' @param direction Ray direction. If i'th entry is positive, consider the i'th column of `pts`
#'   plus a value greater than on equal zero (minimize objective $i$). If negative, consider the
#'   i'th column of `pts` minus a value greater than on equal zero (maximize objective $i$).
#' @param useRGLBBox Use the RGL bounding box as ray limits for the cone.
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlot3d`: A list of arguments for [`rgl::plot3d`].
#'   * `argsSegments3d`: A list of arguments for [`rgl::segments3d`][rgl::points3d].
#'   * `argsPolygon3d`: A list of arguments for [`rgl::polygon3d`].
#'
#' @return Object ids (invisible).
#' @export
#'
#' @examples
#' ini3D(argsPlot3d = list(xlim = c(0,6), ylim = c(0,6), zlim = c(0,6)))
#' plotCones3D(c(4,4,4), drawLines = FALSE, drawPoint = TRUE,
#'            argsPlot3d = list(col = "red", size = 10),
#'            argsPolygon3d = list(alpha = 1), rectangle = TRUE)
#' plotCones3D(c(1,1,1), rectangle = FALSE)
#' plotCones3D(matrix(c(3,3,3,2,2,2), ncol = 3, byrow = TRUE))
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(xlim = c(0,6), ylim = c(0,6), zlim = c(0,6)))
#' plotCones3D(c(4,4,4), direction = 1)
#' plotCones3D(c(2,2,2), direction = -1)
#' plotCones3D(c(4,2,2), direction = c(1,-1,-1))
#' ids <- plotCones3D(c(2,2,4), direction = c(-1,-1,1))
#' finalize3D()
#' # rgl.pop(id = ids) # remove last cone
plotCones3D <-
   function(pts,
            drawPoint = TRUE,
            drawLines = TRUE,
            drawPolygons = TRUE,
            direction = 1,
            rectangle = FALSE,
            useRGLBBox = TRUE,
            ...) {
   args <- list(...)
   argsPlot3d <- mergeLists(list(), args$argsPlot3d)
   argsSegments3d <- mergeLists(list(lwd = 1, col = "grey40"), args$argsSegments3d)
   argsPolygon3d <- mergeLists(list(), args$argsPolygon3d)

   pts <- .checkPts(pts, p = 3)
   for (i in 1:dim(pts)[1]) {
      pt <- as.vector(pts[i, ])
      lst <- plotHull3D(pt, drawPoints = drawPoint,
                   drawLines = drawLines, drawPolygons = drawPolygons,
                   addRays = TRUE, direction = direction, drawBBoxHull = rectangle,
                   useRGLBBox = useRGLBBox,
                   argsPlot3d = argsPlot3d, argsSegments3d = argsSegments3d,
                   argsPolygon3d = argsPolygon3d
                 )
   }
   return(invisible(lst$ids))
   }



#' Plot a cone defined by a point in 2D.
#'
#' The cones are defined as the point plus/minus rays of R2.
#'
#' @param pts A matrix with a point in each row.
#' @param drawPoint Draw the points defining the cone.
#' @param drawLines Draw lines of the cone.
#' @param drawPolygons Draw polygons of the cone.
#' @param rectangle Draw the cone as a rectangle.
#' @param direction Ray direction. If i'th entry is positive, consider the i'th column of `pts`
#'   plus a value greater than on equal zero (minimize objective $i$). If negative, consider the
#'   i'th column of `pts` minus a value greater than on equal zero (maximize objective $i$).
#' @param drawPlot Draw the ggplot. Set to FALSE if you want to combine hulls in a single plot.
#' @param m Minimum values of the bounding box.
#' @param M Maximum values of the bounding box.
#' @param ... Further arguments passed to [plotHull2D]
#'
#' @return A ggplot object
#' @export
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' plotCones2D(c(4,4), drawLines = FALSE, drawPoint = TRUE,
#'            argsGeom_point = list(col = "red", size = 10),
#'            argsGeom_polygon = list(alpha = 0.5), rectangle = TRUE)
#' plotCones2D(c(1,1), rectangle = FALSE)
#' plotCones2D(matrix(c(3,3,2,2), ncol = 2, byrow = TRUE))
#'
#' ## The Danish flag
#' lst <- list(argsGeom_polygon = list(alpha = 0.85, fill = "red"),
#'             drawPlot = FALSE, drawPoint = FALSE, drawLines = FALSE)
#' p1 <- do.call(plotCones2D, args = c(list(c(2,4), direction = 1), lst))
#' p2 <- do.call(plotCones2D, args = c(list(c(1,2), direction = -1), lst))
#' p3 <- do.call(plotCones2D, args = c(list(c(2,2), direction = c(1,-1)), lst))
#' p4 <- do.call(plotCones2D, args = c(list(c(1,4), direction = c(-1,1)), lst))
#' ggplot() + p1 + p2 + p3 + p4 + theme_void()
plotCones2D <-
   function(pts,
            drawPoint = TRUE,
            drawLines = TRUE,
            drawPolygons = TRUE,
            direction = 1,
            rectangle = FALSE,
            drawPlot = TRUE,
            m = apply(pts,2,min)-5,
            M = apply(pts,2,max)+5,
            ...) {
   pts <- .checkPts(pts, p = 2)
   lst <- NULL
   for (i in 1:dim(pts)[1]) {
      pt <- as.vector(pts[i, ])
      lst <- c(lst,
               plotHull2D(pt, drawPoints = drawPoint,
                 drawLines = drawLines, drawPolygons = drawPolygons,
                 addRays = TRUE, direction = direction, drawPlot = FALSE,
                 drawBBoxHull = rectangle, m = m, M = M, ...))
   }
   if (drawPlot) lst <- ggplot() + lst
   return(lst)
}


#' Plot the convex hull of a set of points in 3D.
#'
#' @param pts A matrix with a point in each row.
#' @param drawPoints Draw the points.
#' @param drawLines Draw lines of the facets.
#' @param drawPolygons Fill the facets.
#' @param addText Add text to the points. Currently `coord` (coordinates), `rownames` (rownames)
#'   and `both` supported or a vector with text.
#' @param addRays Add the ray defined by `direction`.
#' @param useRGLBBox Use the RGL bounding box when add rays.
#' @param direction Ray direction. If i'th entry is positive, consider the i'th column of `pts`
#'   plus a value greater than on equal zero (minimize objective $i$). If negative, consider the
#'   i'th column of `pts` minus a value greater than on equal zero (maximize objective $i$).
#' @param drawBBoxHull If addRays then draw the hull areas hitting the bounding box also.
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlot3d`: A list of arguments for [`rgl::plot3d`].
#'   * `argsSegments3d`: A list of arguments for [`rgl::segments3d`][rgl::points3d].
#'   * `argsPolygon3d`: A list of arguments for [`rgl::polygon3d`].
#'   * `argsShade3d`: A list of arguments for [`rgl::shade3d`][rgl::mesh3d].
#'   * `argsText3d`: A list of arguments for [`rgl::text3d`].
#'
#' @return A list with hull, `pts` classified and object ids (invisible).
#' @export
#'
#' @examples
#' ini3D()
#' pts<-matrix(c(0,0,0), ncol = 3, byrow = TRUE)
#' plotHull3D(pts) # a point
#' pts<-matrix(c(1,1,1,2,2,2,3,3,3), ncol = 3, byrow = TRUE)
#' plotHull3D(pts, drawPoints = TRUE) # a line
#' pts<-matrix(c(1,0,0,1,1,1,1,2,2,3,1,1,3,3,3), ncol = 3, byrow = TRUE)
#' plotHull3D(pts, drawLines = FALSE, argsPolygon3d = list(alpha=0.6)) # a polygon
#' pts<-matrix(c(5,5,5,10,10,5,10,5,5,5,5,10), ncol = 3, byrow = TRUE)
#' lst <- plotHull3D(pts, argsPolygon3d = list(alpha=0.9), argsSegments3d = list(color="red"))
#' finalize3D()
#' # rgl.pop(id = lst$ids) # remove last hull
#'
#' ## Using addRays
#' pts <- data.frame(x = c(1,3), y = c(1,3), z = c(1,3))
#' ini3D(argsPlot3d = list(xlim = c(0,max(pts$x)+10),
#'   ylim = c(0,max(pts$y)+10),
#'   zlim = c(0,max(pts$z)+10)))
#' plotHull3D(pts, drawPoints = TRUE, addRays = TRUE, , drawBBoxHull = FALSE)
#' plotHull3D(c(4,4,4), drawPoints = TRUE, addRays = TRUE)
#' finalize3D()
#'
#' pts <- data.frame(x = c(4,2.5,1), y = c(1,2.5,4), z = c(1,2.5,4))
#' ini3D(argsPlot3d = list(xlim = c(0,max(pts$x)+10),
#'   ylim = c(0,max(pts$y)+10),
#'   zlim = c(0,max(pts$z)+10)))
#' plotHull3D(pts, drawPoints = TRUE, addRays = TRUE)
#' finalize3D()
#'
#' pts <- matrix(c(
#'   0, 4, 8,
#'   0, 8, 4,
#'   8, 4, 0,
#'   4, 8, 0,
#'   4, 0, 8,
#'   8, 0, 4,
#'   4, 4, 4,
#'   6, 6, 6
#'   ), ncol = 3, byrow = TRUE)
#' ini3D(FALSE, argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+10),
#'   ylim = c(min(pts[,2])-2,max(pts[,2])+10),
#'   zlim = c(min(pts[,3])-2,max(pts[,3])+10)))
#' plotHull3D(pts, drawPoints = TRUE, addText = "coord")
#' plotHull3D(pts, addRays = TRUE)
#' finalize3D()
#'
#' \donttest{
#' pts <- genNDSet(3, 100, dubND = FALSE)
#' pts <- as.data.frame(pts[,1:3])
#'
#' ini3D(argsPlot3d = list(
#'   xlim = c(0,max(pts[,1])+10),
#'   ylim = c(0,max(pts[,2])+10),
#'   zlim = c(0,max(pts[,3])+10)))
#' plotHull3D(pts, drawPoints = TRUE, addRays = TRUE)
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(
#'   xlim = c(0,max(pts[,1])+10),
#'   ylim = c(0,max(pts[,2])+10),
#'   zlim = c(0,max(pts[,3])+10)))
#' plotHull3D(pts, drawPoints = TRUE, drawPolygons = TRUE, addText = "coord", addRays = TRUE)
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(
#'   xlim = c(0,max(pts[,1])+10),
#'   ylim = c(0,max(pts[,2])+10),
#'   zlim = c(0,max(pts[,3])+10)))
#' plotHull3D(pts, drawPoints = TRUE, drawLines = FALSE,
#'   argsPolygon3d = list(alpha = 1), addRays = TRUE)
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(
#'   xlim = c(0,max(pts[,1])+10),
#'   ylim = c(0,max(pts[,2])+10),
#'   zlim = c(0,max(pts[,3])+10)))
#' plotHull3D(pts, drawPoints = TRUE, argsPolygon3d = list(color = "red"), addRays = TRUE)
#' plotCones3D(pts, argsPolygon3d = list(alpha = 1), rectangle = TRUE)
#' finalize3D()
#' }
plotHull3D <- function(pts,
                       drawPoints = FALSE,
                       drawLines = TRUE,
                       drawPolygons = TRUE,
                       addText = FALSE,
                       addRays = FALSE,
                       useRGLBBox = TRUE,
                       direction = 1,
                       drawBBoxHull = TRUE,
                       ...)
{
   args <- list(...)
   argsSegments3d <- mergeLists(list(lwd = 1, col = "grey40"), args$argsSegments3d)
   argsPlot3d <- mergeLists(list(size = 5, col = "black"), args$argsPlot3d)
   argsShade3d <- mergeLists(list(col = "grey100"), args$argsShade3d)
   argsPolygon3d <- mergeLists(list(color = "grey100", col = "grey40", lwd = 2, alpha = 0.2),
                               args$argsPolygon3d)
   argsText3d <- mergeLists(list(), args$argsText3d)

   ids <- NULL
   pts <- .checkPts(pts, p = 3)
   if (length(direction) != 3) direction = rep(direction[1],3)
   hull <- convexHull(pts, addRays = addRays, direction = direction, useRGLBBox = useRGLBBox)
   set <- hull$pts
   hull <- hull$hull
   d <- dimFace(set[,1:3, drop = FALSE])
   if (d==3) { # then poly define facets
      poly <- hull
      pN <- purrr::map_dfc(1:3, function(i) if (sign(direction[i]) > 0) max(set[,i]) else min(set[,i]))
      colnames(pN) <- colnames(set[,1:3])
      for (i in 1:dim(poly)[1]) {
         tri <- poly[i,!is.na(poly[i,])]
         pt <- set[tri,1:4]
         tri <- 1:nrow(pt)
         if (drawLines) {
            if (length(tri)>3) { # then have to find the vertex sequence
               tri <- convexHull(pt[,1:3])$hull
            }
            tri1 <- NULL # use tri1 to include the addRays=TRUE case
            for (j in 2:length(tri)){
               if (!(pt[tri[j-1],4] == 0 && pt[tri[j],4] == 0)) #  && !drawBBoxHull
                  tri1 <- c(tri1,NA,tri[j-1],tri[j])
            }
            if (!(pt[tri[1], 4] == 0 && pt[tri[length(tri)], 4] == 0)) {  #  && !drawBBoxHull
               tri1 <- c(tri1, NA, tri[1], tri[length(tri)])
            }
            ids <- c(ids, do.call(rgl::polygon3d, args = c(list(pt[tri1, 1], pt[tri1, 2], pt[tri1, 3], fill = FALSE),
                    argsSegments3d)))
         }
         if (drawPolygons) {
            if (drawBBoxHull | (!drawBBoxHull & nrow(dplyr::intersect(pt[,1:3],pN)) == 0)) {
               if (length(tri)==3) {
                  ids <- c(ids, do.call(rgl::triangles3d, args = c(list(x = pt[,1:3]), argsPolygon3d) ))
               } else if (length(tri)==4){
                  tri <- convexHull(pt[,1:3])$hull # then have to find the vertex sequence
                  obj <- rgl::qmesh3d(t(pt[,1:3]),tri, homogeneous = FALSE)
                  ids <- c(ids, do.call(rgl::shade3d, args = c(list(obj), argsPolygon3d)))
               } else {
                  # idx <- apply(pt[,1:3], 2, function(x) {return(length(unique(x))==1)})
                  # idx<-which(!idx)
                  # if (length(idx)==3) coords <- 1:2 else coords <- idx
                  #plotHull3D(pt[tri,1:3])
                  tri <- convexHull(pt[,1:3])$hull
                  comb <- t(utils::combn(3,2))
                  for (j in 1:3) {
                     res <- try(
                        ids <- c(ids, do.call(rgl::polygon3d, args = c(list(
                        pt[tri, 1], pt[tri, 2], pt[tri, 3], fill = TRUE, coords = comb[j,]
                     ), argsPolygon3d))), silent = TRUE)
                     if (!inherits(res, "try-error")) break #else {cat(i, " ", j, " ")}
                  }
               }
            }
         }
      }
   }
   if (d==0) {
      ids <- c(ids, do.call(rgl::plot3d, args = c(list(set[,1], set[,2], set[,3], add=TRUE), argsPlot3d)))
   }
   if (d==1) { #segments3d(set, col = "black", lwd=10, smooth= TRUE)
      cyl <- rgl::cylinder3d(set, radius = 0.01)
      ids <- c(ids, do.call(rgl::shade3d, args = c(list(cyl), argsShade3d)))
   }
   if (d==2) {
      idx <- apply(set[,1:3], 2, function(x) {return(length(unique(x))==1)})
      tri <- hull
      if (length(tri)==3) {
         if (drawPolygons) ids <- c(ids, do.call(rgl::triangles3d,
              args = c(list(set[tri,1], set[tri,2], set[tri,3]), argsPolygon3d) ))
         if (drawLines) ids <- c(ids, do.call(rgl::polygon3d,
              args = c(list(set[tri,1], set[tri,2], set[tri,3], coords = which(!idx), fill= FALSE),
                       argsSegments3d) ))
      } else {
         if (drawPolygons) ids <- c(ids, do.call(rgl::polygon3d,
              args = c(list(set[tri,1], set[tri,2], set[tri,3], coords = which(!idx), fill= TRUE),
                       argsPolygon3d) ))
         if (drawLines) ids <- c(ids, do.call(rgl::polygon3d,
              args = c(list(set[tri,1], set[tri,2], set[tri,3], coords = which(!idx), fill= FALSE),
                       argsSegments3d) ))
      }
   }
   if (drawPoints) ids <- c(ids, do.call(plotPoints3D, args = c(list(pts), list(argsPlot3d = argsPlot3d))))


   if (length(addText) > 1) {
      if (length(addText) == nrow(pts)) {
         do.call(rgl::text3d, args = c(list(x = pts, texts = addText), argsText3d) )
      }
   } else if (length(addText) == 1) {
      if (addText == TRUE | addText == "coord") {
         text <- paste0("(",pts[,1],",",pts[,2],",",pts[,3],")")
         ids <- c(ids, do.call(rgl::text3d, args = c(list(x = pts, texts = text), argsText3d) ))
      } else if (addText == "rownames") {
         text <- rownames(as.data.frame(pts))
         ids <- c(ids, do.call(rgl::text3d, args = c(list(x = pts, texts = text), argsText3d) ))
      } else if (addText == "both") {
         text <- paste0("(",pts[,1],",",pts[,2],",",pts[,3],")")
         text <- paste(text,rownames(as.data.frame(pts)))
         ids <- c(ids, do.call(rgl::text3d, args = c(list(x = pts, texts = text), argsText3d) ))
      } else if (addText != FALSE & length(addText) == nrow(pts)) {
         ids <- c(ids, do.call(rgl::text3d, args = c(list(x = pts, texts = addText), argsText3d) ))
      }
   }
   return(invisible(list(hull = hull, pts = set, ids = ids)))
   stop("Error in plotHull3D")
}


#' Plot points in 3D.
#'
#' @param pts A vector or matrix with the points.
#' @param addText Add text to the points. Currently `coord` (coordinates), `rownames` (rownames)
#'   and `both` supported or a vector with the text.
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlot3d`: A list of arguments for [`rgl::plot3d`].
#'   * `argsPch3d`: A list of arguments for [`rgl::pch3d`].
#'   * `argsText3d`: A list of arguments for [`rgl::text3d`].
#'
#' @return Object ids (invisible).
#' @export
#'
#' @examples
#' ini3D()
#' pts<-matrix(c(1,1,1,5,5,5), ncol = 3, byrow = TRUE)
#' plotPoints3D(pts)
#' plotPoints3D(c(2,3,3), argsPlot3d = list(col = "red", size = 10))
#' plotPoints3D(c(3,2,3), argsPlot3d = list(col = "blue", size = 10, type="p"))
#' plotPoints3D(c(1.5,1.5,1.5), argsPlot3d = list(col = "blue", size = 10, type="p"))
#' plotPoints3D(c(2,2,2, 1,1,1), addText = "coord")
#' ids <- plotPoints3D(c(3,3,3, 4,4,4), addText = "rownames")
#' finalize3D()
#' rgl::rglwidget()
#' # rgl.pop(ids) # remove the last again
plotPoints3D <- function(pts, addText = FALSE, ...) {
   args <- list(...)
   argsPlot3d <- mergeLists(list(size = 5, col = "black", type="p"), args$argsPlot3d)
   argsPch3d <- mergeLists(list(litt = TRUE), args$argsPch3d)
   argsText3d <- mergeLists(list(), args$argsText3d)

   ids <- NULL
   p <- pts
   if (is.vector(pts)) p <- matrix(pts, ncol = 3, byrow = TRUE)
   p <- as.matrix(p)

   if (is.null(args$argsPch3d)) ids <- c(ids, do.call(rgl::plot3d, args = c(list(p, add= TRUE), argsPlot3d) ))
   if (!is.null(args$argsPch3d)) ids <- c(ids, do.call(rgl::pch3d, args = c(list(p), argsPch3d) ))

   if (length(addText) > 1) {
      if (length(addText) == nrow(p)) {
         ids <- c(ids, do.call(rgl::text3d, args = c(list(x = p, texts = addText), argsText3d) ))
      }
   } else if (length(addText) == 1) {
      if (addText == TRUE | addText == "coord") {
         text <- paste0("(",p[,1],",",p[,2],",",p[,3],")")
         ids <- c(ids, do.call(rgl::text3d, args = c(list(x = p, texts = text), argsText3d) ))
      } else if (addText == "rownames") {
         text <- rownames(as.data.frame(p))
         ids <- c(ids, do.call(rgl::text3d, args = c(list(x = p, texts = text), argsText3d) ))
      } else if (addText == "both") {
         text <- paste0("(",p[,1],",",p[,2],",",p[,3],")")
         text <- paste(text,rownames(as.data.frame(p)))
         ids <- c(ids, do.call(rgl::text3d, args = c(list(x = p, texts = text), argsText3d) ))
      } else if (addText != FALSE & length(addText) == nrow(p)) {
         ids <- c(ids, do.call(rgl::text3d, args = c(list(x = p, texts = addText), argsText3d) ))
      }
   }
   return(invisible(ids))
}


#' Plot a plane in 3D.
#'
#' @param normal Normal to the plane.
#' @param point A point on the plane.
#' @param offset The offset of the plane (only used if `point = NULL`).
#' @param useShade Plot shade of the plane.
#' @param useLines Plot lines inside the plane.
#' @param usePoints Plot point shapes inside the plane.
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlanes3d`: A list of arguments for [rgl::planes3d] used when `useShade = TRUE`.
#'   * `argsLines`: A list of arguments for [rgl::persp3d] when `useLines = TRUE`. Moreover, the list
#'                  may contain `lines`: number of lines.
#'
#' @return NULL (invisible)
#' @export
#'
#' @examples
#' ini3D(argsPlot3d = list(xlim = c(-1,10), ylim = c(-1,10), zlim = c(-1,10)) )
#' plotPlane3D(c(1,1,1), point = c(1,1,1))
#' plotPoints3D(c(1,1,1))
#' plotPlane3D(c(1,2,1), point = c(2,2,2), argsPlanes3d = list(color="red"))
#' plotPoints3D(c(2,2,2))
#' plotPlane3D(c(2,1,1), offset = -6, argsPlanes3d = list(color="blue"))
#' plotPlane3D(c(2,1,1), argsPlanes3d = list(color="green"))
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(xlim = c(-1,10), ylim = c(-1,10), zlim = c(-1,10)) )
#' plotPlane3D(c(1,1,1), point = c(1,1,1), useLines = TRUE, useShade = TRUE)
#' ids <- plotPlane3D(c(1,2,1), point = c(2,2,2), argsLines = list(col="blue", lines = 100),
#'             useLines = TRUE)
#' finalize3D()
#' # rgl.pop(id = ids) # remove last plane
plotPlane3D <- function(normal, point = NULL, offset = 0, useShade = TRUE, useLines = FALSE,
                        usePoints = FALSE, ...) {
   args <- list(...)
   argsPlanes3d <- mergeLists(list(col = "grey100", alpha = 0.5), args$argsPlanes3d)
   argsLines <- mergeLists(list(back = 'lines', front = 'lines', add = TRUE, lines = 50),
                           args$argsLines)

   ids <- NULL
   if (!is.null(point)) offset <- -sum(normal * point)
   if (useShade) {
      ids <- c(ids, do.call(rgl::planes3d, args = c(list(normal, d = offset), argsPlanes3d) ))
   }
   # else use points or lines
   if (!rgl::rgl.cur()) stop("Option useLines or usePoints need an open rgl window!")
   limits <- rgl::par3d()$bbox
   m <- c(limits[1], limits[3], limits[5])
   M <- c(limits[2], limits[4], limits[6])
   # do the mesh
   x <- seq(m[1], M[1], length.out = argsLines$lines)
   y <- seq(m[2], M[2], length.out = argsLines$lines)
   f <- function(x,y){-(normal[1]/normal[3])*x - (normal[2]/normal[3])*y - offset/normal[3]}
   z <- outer(x,y,f)
   z[z < m[3] | z > M[3]] <- NA
   if (useLines) {
      ids <- c(ids, do.call(rgl::persp3d, args = c(list(x, y, z), argsLines) ))
   }
   if (usePoints) {
      return(invisible(ids))
      # do.call(rgl::plot3d, args = c(list(x, y, z, add = TRUE), argsPoints) )
      # persp3d(x, y, z, back = 'lines', front = 'lines', add = TRUE)
      #
      #
      # if (nrow(pts) > 3) poly <- do.call(rgl::polygon3d, args = c(list(pts, plot = FALSE),
      #                                                             argsPolygon3d))
      # if (nrow(pts) == 3) {
      #    # poly <- do.call(rgl::triangles3d, args = c(list(pts, plot = FALSE), argsPolygon3d))
      #    poly <- tmesh3d(rbind(pts[,1], pts[,2], pts[,3], 1), indices = 1:3)
      # }
      # poly$texcoords <- argsPolygon3d$texcoords
      # do.call(rgl::shade3d, args = c(list(poly, col = "white", specular = "black"), argsPolygon3d))
   }
   return(invisible(ids))
}


#' ggPlot theme for the package
#'
#' @param ... Further arguments parsed to [ggplot2::theme].
#'
#' @return The theme object.
#' @export
gMOIPTheme <- function(...) {
   return(
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            #axis.line = element_blank(),
            axis.line = element_line(colour = "black", size = 0.5,
                                     arrow = arrow(length = unit(0.3,"cm")) ),
            #axis.ticks = element_blank()
            #axis.text.x = element_text(margin = margin(r = 30))
            # axis.ticks.length = unit(0.5,"mm"),
            #aspect.ratio=4/3,
            legend.position="none",
            ...
      )
   )
}


#' Initialize the rgl window.
#'
#' @param new A new window is opened (otherwise the current is cleared).
#' @param clear Clear the current rgl window.
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists. Currently the following arguments are supported:
#'
#'   * `argsPlot3d`: A list of arguments for [`rgl::plot3d`].
#'   * `argsAspect3d`: A list of arguments for [`rgl::aspect3d`].
#'
#' @return NULL (invisible).
#' @export
#'
#' @examples
#' ini3D()
#' pts<-matrix(c(1,1,1,5,5,5), ncol = 3, byrow = TRUE)
#' plotPoints3D(pts)
#' finalize3D()
ini3D <- function(new = FALSE, clear = TRUE, ...){
   args <- list(...)
   argsPlot3d <-
      mergeLists(list(
         xlab = '',
         ylab = '',
         zlab = '',
         box = FALSE,
         axes = FALSE
      ), args$argsPlot3d)
   argsAspect3d <- mergeLists(list(x = "iso"), args$argsAspect3d)

   if (new) rgl::open3d()
   rgl::highlevel()
   if (clear) rgl::clear3d()
   do.call(rgl::plot3d, args = c(list(x = c(1,1), y = c(1,1), z = c(1,1), type = 'n'), argsPlot3d))
   do.call(rgl::aspect3d, args = argsAspect3d)
   return(invisible(NULL))
}

#' Finalize the rgl window.
#'
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists. Currently the following arguments are supported:
#'
#'   * `argsAxes3d`: A list of arguments for [`rgl::axes3d`].
#'   * `argsTitle3d`: A list of arguments for [`rgl::title3d`][rgl::axes3d].
#'
#' @return NULL (invisible).
#' @export
#'
#' @examples
#' ini3D()
#' pts<-matrix(c(1,1,1,5,5,5), ncol = 3, byrow = TRUE)
#' plotPoints3D(pts)
#' finalize3D()
#'
#' ini3D()
#' pts<-matrix(c(1,1,1,5,5,5), ncol = 3, byrow = TRUE)
#' plotPoints3D(pts)
#' finalize3D(argsAxes3d = list(edges = "bbox"))
finalize3D <- function(...){
   args <- list(...)
   argsAxes3d <- mergeLists(list(edges = c('x', 'y', 'z')), args$argsAxes3d)
   argsTitle3d <- mergeLists(list(xlab = expression(italic(z)[1]), ylab = expression(italic(z)[2]),
                                  zlab = expression(italic(z)[3])), args$argsTitle3d)

   do.call(rgl::axes3d, args = argsAxes3d)
   do.call(rgl::title3d, args = argsTitle3d)
   rgl.bringtotop()
   return(invisible(NULL))
}