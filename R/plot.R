## Functions for plotting

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
      stop("Only 2 or 3 variables supported!")
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
#' @param ... If 2D arguments passed to the \link{aes_string} function in
#'   \link{geom_point} or \link{geom_line}.
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
      if (!is.null(obj) & (!is.vector(obj) | !length(obj) == ncol(A)))
         stop("Arg. obj must be a vector of same length as the number of columns in A.")
      #if (is.null(points) & is.null(rangePoints) & is.null(cPoints)) stop("Arguments cPoints, points or rangePoints must be specified!")
      # Set Custom theme
      myTheme <- theme_bw()
   myTheme <- myTheme + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              #axis.line = element_blank(),
                              axis.line = element_line(colour = "black", size = 0.5,
                                                       arrow = arrow(length = unit(0.3,"cm")) ),
                              #axis.ticks = element_blank()
                              #axis.text.x = element_text(margin = margin(r = 30))
                              # axis.ticks.length = unit(0.5,"mm"),
                              #aspect.ratio=4/3,
                              legend.position="none"
   )

   # Create solution plot
   p<- ggplot() #+ coord_fixed(ratio = 1)
   if (latex) p <- p + xlab("$x_1$") + ylab("$x_2$")
   if (!latex) p <- p + xlab(expression(x[1])) + ylab(expression(x[2]))
   #coord_cartesian(xlim = c(-0.1, max(cPoints$x1)+1), ylim = c(-0.1, max(cPoints$x2)+1), expand = F) +

   if (plotFaces) {
      cPoints = cornerPoints(A, b, faces, nonneg)
      idx <- grDevices::chull(cPoints)
      cPoints <- cPoints[idx,]
      p <- p + geom_polygon(data = as.data.frame(cPoints), aes_string(x = 'x1', y = 'x2'),
                            fill="gray90", size = 0.5, linetype = 1, color="gray")
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
      if (all(type == "c")) {
         #p <- p + geom_point(aes_string(x = 'x1', y = 'x2'), data=points) #+ scale_colour_grey(start = 0.6, end = 0)
      }
      if (all(type == "i")) {
         p <- p + geom_point(aes_string(x = 'x1', y = 'x2'), data=points, ...) #+ scale_colour_grey(start = 0.6, end = 0)
      }
      if (length(which(type == "c"))==1) {
         # pl <- slices(A, b, type, nonneg)
         # pl <- lapply(pl, unique)
         # for (i in 1:length(pl)) pl[[i]] <- cbind(pl[[i]],i)
         # pl <- lapply(pl, function(x) {
         #    colnames(x) <- c("x1", "x2", "g")
         #    rownames(x) <- NULL
         #    x<-data.frame(x)
         # })
         # tmp <- do.call(rbind, pl)
         # points <- tmp[,1:2]
         p <- p + geom_line(aes_string(x = 'x1', y = 'x2', group='g', ...), data=points)
         idx <- sapply(pl, function(x) nrow(x)==1)
         pl <- pl[idx]
         if (length(pl)>0) {
            tmp <- do.call(rbind, pl)
            p <- p + geom_point(aes_string(x = 'x1', y = 'x2', ...), data=tmp)
         }
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
         p <- p + geom_point(aes_string(x = 'x1', y = 'x2'), data=tmp)
         nudgeS=-(max(tmp$x1)-min(tmp$x1))/100
         if (anyDuplicated(cbind(tmp$x1,tmp$x2), MARGIN = 1) > 0)
            p <- p + ggrepel::geom_text_repel(aes_string(x = 'x1', y = 'x2', label = 'lbl'),
                                              data=tmp, size=3, colour = "gray50")
         if (anyDuplicated(cbind(tmp$x1,tmp$x2), MARGIN = 1) == 0)
            p <- p + geom_text(aes_string(x = 'x1', y = 'x2', label = 'lbl'), data=tmp,
                               nudge_x = nudgeS, nudge_y = nudgeS, hjust=1, size=3,
                               colour = "gray50")
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
            p <- p + geom_abline(intercept = tmp$z[i]/obj[2], slope = -obj[1]/obj[2], lty="dashed")
         } else {
            p <- p + geom_vline(xintercept = tmp$x1[i], lty="dashed")
         }
         p <- p + geom_label(aes_string(x = 'x1', y = 'x2', label = 'str'), data = tmp[i,],
                             nudge_x = 1.0)
      }
   }
   p <- p + myTheme
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
#' @param ... Arguments passed to axes3d, plot3d, title3d. Parsed using lists argsAxes3d,
#'   argsPlot3d and argsTitle3d.
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
      argsPlot3d <- mergeLists(list(xlab = '', box = F, axes = F), args$argsPlot3d)
      argsTitle3d <- mergeLists(list(xlab = 'x1', ylab = 'x2', zlab = 'x3'), args$argsTitle3d)

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
         #print(cbind(vertices,val))
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
         if (length(which(type == "c"))<length(type) & length(which(type == "c"))>0) {
            #pch3d(iPoints[,1:3], col="black", cex = 0.1, pch = 16)
            points3d(points[,1:3], col="grey50", size = 7)
         }

         if (labels=="coord")
            points$lbl <- df2String(points)
         else if (labels == "n")
            points$lbl <- ""
         else points$lbl <- 1:nrow(points)
         text3d(points[,1:3], texts = points$lbl, cex = c(1.1,1.1), adj=2, font = 2)
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
      points$nD[points$nD & !points$ext] <- FALSE
      points$ext[points$nD & !points$ext] <- FALSE
      points$nonExt[points$nD & !points$ext] <- FALSE
   }
   #dat <<- points  # hack to get data as data frame
   # Initialize plot
   p <- ggplot(points, aes_q(x = quote(z1), y = quote(z2), col = "grey10") )
   if (latex) p <- p + xlab("$z_1$") + ylab("$z_2$")
   if (!latex) p <- p + xlab(expression(z[1])) + ylab(expression(z[2]))

   # Add hull plus rays
   if (addHull) {
      tmp<-points[points$ext & !duplicated(cbind(points$z1,points$z2), MARGIN = 1),]
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
         p <- p + geom_point(aes_string(colour = 'nD', shape = 'ext'), data = points) +
            scale_colour_grey(start = 0.6, end = 0)
      } else if (all(type == "i")) {
         #iPoints <- integerPoints(A, b, nonneg)
         #iPoints <- criterionPoints(iPoints, obj, crit, labels)
         p <- p + geom_point(aes_string(colour = 'nD', shape = 'ext'), data = points) +
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
         points$co <- points$ext | points$nonExt
         #print(points)
         p <- p + geom_point(aes_string(colour = 'co', shape = 'ext'), data = points) +
            scale_colour_grey(start = 0.8, end = 0)
      }
   }
   # Add triangles
   if (addTriangles) {
      tmp<-points[points$ext | points$nonExt,]
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
#' @return The ggplot2 object.
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
#' @param crit Either max or min (only used if add the iso profit line).
#' @param addTriangles Add search triangles defined by the non-dominated extreme
#'   points.
#' @param addHull Add the convex hull and the rays.
#' @param latex If true make latex math labels for TikZ.
#' @param labels If \code{NULL} don't add any labels. If 'n' no labels but show the points. If
#'   'coord' add coordinates to the points. Otherwise number all points from one.
#'
#' @note Currently only points are checked for dominance. That is, for MILP
#'   models some nondominated points may in fact be dominated by a segment.
#' @return The ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @examples
#' dat <- data.frame(z1=c(12,14,16,18,18,18,14,15,15), z2=c(18,16,12,4,2,6,14,14,16))
#' points <- addNDSet2D(dat, crit = "min", keepDom = TRUE)
#' plotNDSet2D(points, crit = "min", addTriangles = TRUE)
#' points <- addNDSet2D(dat, crit = "max", keepDom = TRUE)
#' plotNDSet2D(points, crit = "max", addTriangles = TRUE)
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
   # Initialize plot
   p <- ggplot(points, aes_q(x = quote(z1), y = quote(z2), col = "grey10") )
   if (latex) p <- p + xlab("$z_1$") + ylab("$z_2$")
   if (!latex) p <- p + xlab(expression(z[1])) + ylab(expression(z[2]))

   # Add hull plus rays
   if (addHull) {
      tmp<-points[points$ext & !duplicated(cbind(points$z1,points$z2), MARGIN = 1),]
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

   # Add triangles
   if (addTriangles) {
      tmp<-points[points$ext | points$nonExt,]
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

   p <- p + geom_point(aes_string(colour = 'nD', shape = 'ext'), data = points) +
      #    #coord_fixed(ratio = 1) +
      scale_colour_grey(start = 0.6, end = 0)

   nudgeC=-(max(points$z1)-min(points$z1))/100
   if (!is.null(labels) &
       anyDuplicated(round(cbind(points$z1, points$z2), 10), MARGIN = 1) > 0)
      p <-
      p + ggrepel::geom_text_repel(
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
#' @return The corner points of the rectangle (invisible).
#' @export
#'
#' @examples
#' ini3D()
#' plotRectangle3D(c(0,0,0), c(1,1,1))
#' plotRectangle3D(c(1,1,1), c(4,4,3), drawPoints = TRUE, drawLines = FALSE,
#'            argsPlot3d = list(size=2, type="s", alpha=0.3))
#' plotRectangle3D(c(2,2,2), c(3,3,2.5), argsPolygon3d = list(alpha = 1) )
#' finalize3D()
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
   do.call(plotHull3D, args = c(list(x), list(argsSegments3d = argsSegments3d,
                                              argsPlot3d = argsPlot3d, argsShade3d = argsShade3d,
                                              argsPolygon3d = argsPolygon3d), argsP))
   return(invisible(x))
}


#' Plot a cone defined by a point in 3D.
#'
#' The cones are defined as the point plus R3+.
#'
#' @param pts A matrix with a point in each row.
#' @param drawPoint Draw the points defining the cone.
#' @param drawLines Draw lines of the cone.
#' @param reverse Cones are defined as the point minus R3+
#' @param rectangle Draw the cone as a rectangle.
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlot3d`: A list of arguments for [`rgl::plot3d`].
#'   * `argsSegments3d`: A list of arguments for [`rgl::segments3d`][rgl::points3d].
#'   * `argsPolygon3d`: A list of arguments for [`rgl::polygon3d`].
#'
#' @return NULL (invisible)
#' @export
#'
#' @examples
#' ini3D(argsPlot3d = list(xlim = c(0,6), ylim = c(0,6), zlim = c(0,6)))
#' plotCones3D(c(4,4,4), drawLines = FALSE, drawPoint = TRUE,
#'            argsPlot3d = list(col = "red", size = 10),
#'            argsPolygon3d = list(alpha = 1), rectangle = TRUE)
#' plotCones3D(c(1,1,1), reverse = TRUE, rectangle = TRUE)
#' plotCones3D(matrix(c(3,3,3,2,2,2), ncol = 3, byrow = TRUE))
#' finalize3D()
plotCones3D <-
   function(pts,
            drawPoint = TRUE,
            drawLines = TRUE,
            reverse = FALSE,
            rectangle = FALSE,
            ...) {
      args <- list(...)
   argsPlot3d <- mergeLists(list(), args$argsPlot3d)
   argsSegments3d <- mergeLists(list(lwd = 1, col = "grey40"), args$argsSegments3d)
   argsPolygon3d <- mergeLists(list(), args$argsPolygon3d)

   if (is.vector(pts)) pts <- t(as.matrix(pts))
   eps <- 0.00001
   pts <- as.matrix(pts)
   limits <- rgl::par3d()$bbox
   p1 <- c(limits[2], limits[4], limits[6])
   p2 <- c(limits[1], limits[3], limits[5])
   for (i in 1:dim(pts)[1]) {
      p <- as.vector(pts[i, ])
      if (!(limits[1] < p[1] && p[1] < limits[2] &&
            limits[3] < p[2] && p[2] < limits[4] &&
            limits[5] < p[3] && p[3] < limits[6])) {
         stop("The cone will not be in the interior of the current bounding box. Resize your axes.")
      }
      if (reverse) {
         if (rectangle)
            do.call(plotRectangle3D, args = c(
               list(p, p2),
               list(argsPolygon3d = argsPolygon3d, argsSegments3d = argsSegments3d)
            ))
         else
            x <- matrix(c(p, p2), ncol = 3, byrow = TRUE)
      } else {
         if (rectangle)
            do.call(plotRectangle3D, args = c(
               list(p, p1),
               list(argsPolygon3d = argsPolygon3d, argsSegments3d = argsSegments3d)
            ))
         else
            x <- matrix(c(p, p1), ncol = 3, byrow = TRUE)
      }
      if (!rectangle) {
         x <- expand.grid(x=c(x[1,1],x[2,1]), y=c(x[1,2],x[2,2]), z=c(x[1,3],x[2,3]))
         x <- as.matrix(x)
         # text3d(x, texts = paste(x[,1],x[,2],x[,3], "r", 1:dim(x)[1]))
         face1 <- matrix( c(x[1,], x[2,], x[6,], x[5,]), ncol = 3, byrow = TRUE)
         face2 <- matrix( c(x[1,], x[3,], x[7,], x[5,]), ncol = 3, byrow = TRUE)
         face3 <- matrix( c(x[1,], x[3,], x[4,], x[2,]), ncol = 3, byrow = TRUE)
         if (drawLines) {
            do.call(rgl::segments3d, args = c(list(x[c(1,5),]), argsSegments3d) )
            do.call(rgl::segments3d, args = c(list(x[c(1,3),]), argsSegments3d) )
            do.call(rgl::segments3d, args = c(list(x[c(1,2),]), argsSegments3d) )
         }
         do.call(plotHull3D, args = c(
            list(face1, drawLines = FALSE),
            list(argsPolygon3d = argsPolygon3d)
         ))
         do.call(plotHull3D, args = c(
            list(face2, drawLines = FALSE),
            list(argsPolygon3d = argsPolygon3d)
         ))
         do.call(plotHull3D, args = c(
            list(face3, drawLines = FALSE),
            list(argsPolygon3d = argsPolygon3d)
         ))
      }
      if (drawPoint)
         do.call(plotPoints3D, args = c(list(pts[i, , drop = FALSE]), list(argsPlot3d = argsPlot3d)))
   }
   return(invisible(NULL))
}
# Old implementation using subscenes
# plotCones3D <- function(point, col = "grey100") {
#   p <- point
#   eps <- 0.00001
#   if (is.matrix(p)) points <- p else points <- t(as.matrix(p))
#   plot3d( # hack to expand bounding box
#     points-eps,
#     type = "p", size = 0.1, add = T, col = "white"
#   )
#   plot3d(
#     points,
#     type = "p",
#     col = "black",
#     size = 5, add = T
#   )
#   root <- currentSubscene3d()
#   for (i in 1:dim(points)[1]) {
#     p <- points[i,]
#     newSubscene3d(
#       "inherit",
#       "inherit",
#       "inherit",
#       copyShapes = T,
#       copyBBoxDeco = F,
#       parent = root
#     )
#     planes3d(1, 0, 0, -p[1], alpha = 0.5, col = col)
#     planes3d(0, 1, 0, -p[2], alpha = 0.5, col = col)
#     planes3d(0, 0, 1, -p[3], alpha = 0.5, col = col)
#     clipplanes3d(1, 0, 0,-p[1] + eps)
#     clipplanes3d(0, 1, 0,-p[2] + eps)
#     clipplanes3d(0, 0, 1,-p[3] + eps)
#   }
#   useSubscene3d(root)
# }


#' Plot the convex hull of a set of points.
#'
#' @param pts A matrix with a point in each row.
#' @param drawPoints Draw the points.
#' @param drawLines Draw lines of the facets.
#' @param drawPolygons Fill the facets.
#' @param addText Add text to the points. Currently `coord` (coordinates), `rownames` (rownames)
#'   and `both` supported.
#' @param addR3 Plot the hull of the points + R3+.
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlot3d`: A list of arguments for [`rgl::plot3d`].
#'   * `argsSegments3d`: A list of arguments for [`rgl::segments3d`][rgl::points3d].
#'   * `argsPolygon3d`: A list of arguments for [`rgl::polygon3d`].
#'   * `argsShade3d`: A list of arguments for [`rgl::shade3d`][rgl::mesh3d].
#'   * `argsText3d`: A list of arguments for [`rgl::text3d`].
#'
#' @return The convex hull (invisible).
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
#' plotHull3D(pts, argsPolygon3d = list(alpha=0.9), argsSegments3d = list(color="red"))
#' finalize3D()
#'
#' ## Using addR3
#' pts <- data.frame(x = c(1,3), y = c(1,3), z = c(1,3))
#' ini3D(argsPlot3d = list(xlim = c(0,max(pts$x)+10),
#'   ylim = c(0,max(pts$y)+10),
#'   zlim = c(0,max(pts$z)+10)))
#' plotHull3D(pts, drawPoints = TRUE, addR3 = TRUE)
#' finalize3D()
#'
#' pts <- data.frame(x = c(4,2.5,1), y = c(1,2.5,4), z = c(1,2.5,4))
#' ini3D(argsPlot3d = list(xlim = c(0,max(pts$x)+10),
#'   ylim = c(0,max(pts$y)+10),
#'   zlim = c(0,max(pts$z)+10)))
#' plotHull3D(pts, drawPoints = TRUE, addR3 = TRUE)
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
#' plotHull3D(pts, drawPoints = TRUE, drawPolygons = TRUE, addText = "coord", addR3 = TRUE)
#' finalize3D()
#'
#' \donttest{
#' pts <- genNDSet()
#' pts <- as.data.frame(pts)
#'
#' ini3D(argsPlot3d = list(
#'   xlim = c(0,max(pts$x)+10),
#'   ylim = c(0,max(pts$y)+10),
#'   zlim = c(0,max(pts$z)+10)))
#' plotHull3D(pts, drawPoints = TRUE, addR3 = TRUE)
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(xlim = c(0,max(pts[,1])+10),
#'   ylim = c(0,max(pts[,2])+10),
#'   zlim = c(0,max(pts[,3])+10)))
#' plotHull3D(pts, drawPoints = TRUE, drawPolygons = TRUE, addText = "coord", addR3 = TRUE)
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(xlim = c(0,max(pts$x)+10),
#'   ylim = c(0,max(pts$y)+10),
#'   zlim = c(0,max(pts$z)+10)))
#' plotHull3D(pts, drawPoints = TRUE, drawLines = FALSE,
#'   argsPolygon3d = list(alpha = 1), addR3 = TRUE)
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(xlim = c(0,max(pts$x)+2),
#'   ylim = c(0,max(pts$y)+2),
#'   zlim = c(0,max(pts$z)+2)))
#' plotHull3D(pts, drawPoints = TRUE, argsPolygon3d = list(color = "red"), addR3 = TRUE)
#' plotCones3D(pts, argsPolygon3d = list(alpha = 1), rectangle = TRUE)
#' finalize3D()
#' }
plotHull3D <- function(pts,
                       drawPoints = FALSE,
                       drawLines = TRUE,
                       drawPolygons = TRUE,
                       addText = FALSE,
                       addR3 = FALSE,
                       ...)
{
   args <- list(...)
   argsSegments3d <- mergeLists(list(lwd = 1, col = "grey40"), args$argsSegments3d)
   argsPlot3d <- mergeLists(list(size = 5, col = "black"), args$argsPlot3d)
   argsShade3d <- mergeLists(list(col = "grey100"), args$argsShade3d)
   argsPolygon3d <- mergeLists(list(color = "grey100", col = "grey40", lwd = 2, alpha = 0.2),
                               args$argsPolygon3d)
   argsText3d <- mergeLists(list(), args$argsText3d)

   if (is.vector(pts)) pts <- t(as.matrix(pts))
   set<- as.matrix(unique(pts[,1:3, drop = FALSE]))
   hull <- convexHull3D(set[,1:3], classify = TRUE, addR3 = addR3)
   set <- hull$pts
   d <- dimFace(set[,1:3, drop = FALSE])
   if (d==3) {
      poly <- hull$hull
      for (i in 1:dim(poly)[1]) {
         tri <- poly[i,!is.na(poly[i,])]
         p <- set[tri,1:4]
         tri <- convexHull3D(p[,1:3])
         tri1 <- NULL
         for (j in 2:length(tri)){
            if (!(p[tri[j-1],4] == 0 && p[tri[j],4] == 0))
               tri1 <- c(tri1,NA,tri[j-1],tri[j])
         }
         if (!(p[tri[1], 4] == 0 && p[tri[length(tri)], 4] == 0)) {
            tri1 <- c(tri1, NA, tri[1], tri[length(tri)])
         }
         if (drawLines) {
            do.call(rgl::polygon3d, args = c(list(p[tri1, 1], p[tri1, 2], p[tri1, 3], fill = FALSE),
                    argsSegments3d))
         }
         if (drawPolygons) {
            if (length(tri)==3) {
               do.call(rgl::triangles3d, args = c(list(p[tri,1], p[tri,2], p[tri,3]), argsPolygon3d) )
            } else if (length(tri)==4){
               obj <- rgl::qmesh3d(t(p[,1:3]),tri, homogeneous = FALSE)
               do.call(rgl::shade3d, args = c(list(obj), argsPolygon3d))
            } else {
               idx <- apply(p[,1:3], 2, function(x) {return(length(unique(x))==1)})
               # print(idx)
               idx<-which(!idx)
               if (length(idx)==3) coords <- 1:2 else coords <- idx
               # plotHull3D(p[tri,1:3])
               do.call(rgl::polygon3d, args = c(list(
                  p[tri, 1], p[tri, 2], p[tri, 3], fill = TRUE, coords = coords
               ), argsPolygon3d))
            }
         }
      }
   }
   if (d==0) {
      do.call(rgl::plot3d, args = c(list(set[,1], set[,2], set[,3], add=TRUE), argsPlot3d) )
   }
   if (d==1) { #segments3d(set, col = "black", lwd=10, smooth= TRUE)
      cyl <- rgl::cylinder3d(set, radius = 0.01)
      do.call(rgl::shade3d, args = c(list(cyl), argsShade3d) )
   }
   if (d==2) {
      idx <- apply(set[,1:3], 2, function(x) {return(length(unique(x))==1)})
      tri <- hull$hull
      if (length(tri)==3) {
         if (drawPolygons) do.call(rgl::triangles3d,
              args = c(list(set[tri,1], set[tri,2], set[tri,3]), argsPolygon3d) )
         if (drawLines) do.call(rgl::polygon3d,
              args = c(list(set[tri,1], set[tri,2], set[tri,3], coords = which(!idx), fill= FALSE),
                       argsSegments3d) )
      } else {
         if (drawPolygons) do.call(rgl::polygon3d,
              args = c(list(set[tri,1], set[tri,2], set[tri,3], coords = which(!idx), fill= TRUE),
                       argsPolygon3d) )
         if (drawLines) do.call(rgl::polygon3d,
              args = c(list(set[tri,1], set[tri,2], set[tri,3], coords = which(!idx), fill= FALSE),
                       argsSegments3d) )
      }
   }
   if (drawPoints) do.call(plotPoints3D, args = c(list(pts), list(argsPlot3d = argsPlot3d)))
   if (addText == TRUE | addText == "coord") {
      text <- paste0("(",pts[,1],",",pts[,2],",",pts[,3],")")
      do.call(rgl::text3d, args = c(list(x = pts, texts = text), argsText3d) )
   }
   if (addText == "rownames") {
      text <- rownames(as.data.frame(pts))
      do.call(rgl::text3d, args = c(list(x = pts, texts = text), argsText3d) )
   }
   if (addText == "both") {
      text <- paste0("(",pts[,1],",",pts[,2],",",pts[,3],")")
      text <- paste(text,rownames(as.data.frame(pts)))
      do.call(rgl::text3d, args = c(list(x = pts, texts = text), argsText3d) )
   }
   #if (drawCoordinates) rgl::text3d(pts, texts = paste0("(",pts[,1],",",pts[,2],",",pts[,3],")"))
   return(invisible(hull))
   stop("Error in plotHull3D")
}


#' Plot points in 3D.
#'
#' @param pts A vector or matrix with the points.
#' @param addText Add text to the points. Currently `coord` (coordinates), `rownames` (rownames)
#'   and `both` supported.
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlot3d`: A list of arguments for [`rgl::plot3d`].
#'   * `argsPch3d`: A list of arguments for [`rgl::pch3d`].
#'   * `argsText3d`: A list of arguments for [`rgl::text3d`].
#'
#' @return NULL (invisible)
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
#' plotPoints3D(c(3,3,3, 4,4,4), addText = "rownames")
#' finalize3D()
plotPoints3D <- function(pts, addText = F, ...) {
   args <- list(...)
   argsPlot3d <- mergeLists(list(size = 5, col = "black", type="p"), args$argsPlot3d)
   argsPch3d <- mergeLists(list(litt = TRUE), args$argsPch3d)
   argsText3d <- mergeLists(list(), args$argsText3d)

   p <- pts
   if (is.vector(pts)) p <- matrix(pts, ncol = 3, byrow = TRUE)
   p <- as.matrix(p)

   if (is.null(args$argsPch3d)) do.call(rgl::plot3d, args = c(list(p, add= TRUE), argsPlot3d) )
   if (!is.null(args$argsPch3d)) do.call(rgl::pch3d, args = c(list(p), argsPch3d) )

   if (addText == TRUE | addText == "coord") {
      text <- paste0("(",p[,1],",",p[,2],",",p[,3],")")
      do.call(rgl::text3d, args = c(list(x = p, texts = text), argsText3d) )
   }
   if (addText == "rownames") {
      text <- rownames(as.data.frame(p))
      do.call(rgl::text3d, args = c(list(x = p, texts = text), argsText3d) )
   }
   if (addText == "both") {
      text <- paste0("(",p[,1],",",p[,2],",",p[,3],")")
      text <- paste(text,rownames(as.data.frame(p)))
      do.call(rgl::text3d, args = c(list(x = p, texts = text), argsText3d) )
   }
   return(invisible(NULL))
}


#' Plot a plane in 3D.
#'
#' @param normal Normal to the plane.
#' @param point A point on the plane.
#' @param offset The offset of the plane (only used if `point = NULL`).
#' @param ... Further arguments passed on the the rgl plotting functions. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlanes3d`: A list of arguments for [rgl::planes3d].
#'
#' @return NULL (invisible)
#' @export
#'
#' @examples
#' ini3D(argsPlot3d = list(xlim = c(-1,10), ylim = c(-1,10), zlim = c(-1,10)) )
#' plotPlane3D(c(1,1,1), point = c(1,1,1))
#' plotPoints3D(c(1,1,1))
#' plotPlane3D(c(1,2,1), point = c(2,2,2), argsPlanes3d = list(col="red"))
#' plotPoints3D(c(2,2,2))
#' plotPlane3D(c(2,1,1), offset = -6, argsPlanes3d = list(col="blue"))
#' plotPlane3D(c(2,1,1), argsPlanes3d = list(col="green"))
#' finalize3D()
plotPlane3D <- function(normal, point = NULL, offset = 0, ...) {
   args <- list(...)
   argsPlanes3d <- mergeLists(list(col = "grey100", alpha = 0.5), args$argsPlanes3d)

   if (!is.null(point)) offset <- -sum(normal * point)
   do.call(rgl::planes3d, args = c(list(normal, d = offset), argsPlanes3d) )
   return(invisible(NULL))
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
         box = F,
         axes = F
      ), args$argsPlot3d)
   argsAspect3d <- mergeLists(list(x = "iso"), args$argsAspect3d)

   if (new) rgl::open3d()
   rgl::highlevel()
   if (clear) rgl::clear3d()
   do.call(rgl::plot3d, args = c(list(x = c(1,1), y = c(1,1), z = c(1,1), type = 'n'), argsPlot3d))
   do.call(rgl::aspect3d, args = argsAspect3d)
   # return(invisible(NULL))
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
finalize3D <- function(...){
   args <- list(...)
   argsAxes3d <- mergeLists(list(edges = c('x', 'y', 'z')), args$argsAxes3d)
   argsTitle3d <- mergeLists(list(xlab = expression(italic(x)[1]), ylab = expression(italic(x)[2]),
                                  zlab = expression(italic(x)[3])), args$argsTitle3d)

   do.call(rgl::axes3d, args = argsAxes3d)
   do.call(rgl::title3d, args = argsTitle3d)
   # return(invisible(NULL))
}