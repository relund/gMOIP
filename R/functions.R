

#' Calculate the criterion points of a set of points and ranges to find the set
#' of non-dominated points (pareto points) and classify them into extreme
#' supported, non-extreme supported, non-supported.
#'
#' @param points A data frame with a column for each variable in the solution
#'   space (can also be a rangePoints).
#' @param coeff A p x n matrix(one row for each criterion).
#' @param crit Either max or min.
#' @param labels If \code{NULL} or "n" don't add any labels (empty string). If
#'   'coord' labels are the solution space coordinates. Otherwise number all
#'   points from one based on the soluton space points.
#'
#' @return A data frame with columns x1, ..., xn, z1, ..., zp, lbl (label), nD
#'   (non-dominated), ext (extreme), nonExt (non-extreme supported).
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @examples
#' A <- matrix( c(3, -2, 1, 2, 4, -2, -3, 2, 1), nc = 3, byrow = TRUE)
#' b <- c(10,12,3)
#' points <- integerPoints(A, b)
#' coeff <- matrix( c(1,-3,1,-1,1,-1), byrow = TRUE, ncol = 3 )
#' criterionPoints(points, coeff, crit = "max", labels = "numb")
criterionPoints<-function(points, coeff, crit, labels = "coord") {
   n <- ncol(coeff)
   zVal <- points %*% t(coeff)
   zVal <- round(zVal,10)
   colnames(zVal) <- paste0("z", 1:nrow(coeff))
   rownames(zVal) <- NULL
   iP <- cbind(points,zVal)
   iP <- as.data.frame(iP)
   tol <- 1e-4
   iP$oldLbl <- 1:length(iP$x1)
   if (crit=="max") iP <- iP[order(-iP$z2,-iP$z1),]
   if (crit=="min") iP <- iP[order(iP$z2,iP$z1),]
   iP$lbl <- 1:length(iP$x1)  # note use in alg!
   # classify non dom
   iP$nD <- FALSE
   iP$nD[1] <- TRUE  # upper left point
   p1 <- iP$z1[1]; p2 <- iP$z2[1]  # current non dom point (due to sorting will p2 always be larger than or equal to current)
   for (r in 2:length(iP$x1)) { # current point under consideration
      if (abs(p2-iP$z2[r])<tol & abs(p1-iP$z1[r])<tol) {iP$nD[r] <- TRUE; p1 <- iP$z1[r]; p2 <- iP$z2[r]; next}
      if (crit=="max" & p2-iP$z2[r]>tol & iP$z1[r]>p1+tol) {iP$nD[r] <- TRUE; p1 <- iP$z1[r]; p2 <- iP$z2[r]; next}
      if (crit=="min" & iP$z2[r]-p2>tol & iP$z1[r]<p1-tol) {iP$nD[r] <- TRUE; p1 <- iP$z1[r]; p2 <- iP$z2[r]; next}
   }
   # classify extreme supported
   idx <- which(iP$nD & !duplicated(cbind(iP$z1,iP$z2), MARGIN = 1) )  # remove dublicated points
   iP$ext <- FALSE
   iP$ext[idx[1]] <- TRUE
   iP$ext[idx[length(idx)]] <- TRUE
   if (length(idx)<3) {
      iP$nonExt <- FALSE
      #return(iP)   # a single extreme
   } else {
      nD <- iP[idx,]
      ul<-1
      lr<-length(idx)
      while (ul<length(idx)) {
         # for (k in 1:1000) {
         slope <- (nD$z2[lr]-nD$z2[ul])/(nD$z1[lr]-nD$z1[ul])
         nD$val <- nD$z2-slope*nD$z1
         #cat("val:",nD$val[ul],"max:",max(nD$val),"min:",min(nD$val),"\n")
         if (crit=="max") {
            i <- which.max(nD$val)
            if (nD$val[ul]<nD$val[i] - tol) {
               iP$ext[nD$lbl[i]] <- TRUE
               lr <- i
            } else {
               ul <- lr
               lr<-length(idx)
            }
         }
         if (crit=="min") {
            i <- which.min(nD$val)
            if (nD$val[ul]>nD$val[i] + tol) {
               iP$ext[nD$lbl[i]] <- TRUE
               lr <- i
            } else {
               ul <- lr
               lr<-length(idx)
            }
         }
      }
      # classify nonextreme supported
      idxExt <- which(iP$ext)
      iP$nonExt <- FALSE
      if (length(idxExt)>1) {
         for (i in 2:length(idxExt)) {
            slope <- (iP$z2[idxExt[i]]-iP$z2[idxExt[i-1]])/(iP$z1[idxExt[i]]-iP$z1[idxExt[i-1]])
            nDCand <- iP[idxExt[i-1]:idxExt[i],]
            nDCand <- nDCand[nDCand$nD & !duplicated(cbind(nDCand$z1,nDCand$z2), MARGIN = 1),]
            nDCand <- nDCand[c(-1,-length(nDCand$nD)),]
            if (length(nDCand$nD)==0) next   # no points inbetween
            for (j in 1:length(nDCand$nD)) {
               slopeCur = (nDCand$z2[j]-iP$z2[idxExt[i-1]])/(nDCand$z1[j]-iP$z1[idxExt[i-1]])
               if (abs(slope - slopeCur) < tol) iP$nonExt[nDCand$lbl[j]==iP$lbl] <- TRUE
            }
         }
      }
      # classify dublicates
      idx <- which(iP$nD)
      if (!length(idx)<2) {
         for (i in 2:length(idx)) {
            if (iP$ext[i-1] & abs(iP$z2[i-1]-iP$z2[i])<tol & abs(iP$z1[i-1]-iP$z1[i])<tol) {
               iP$ext[i] = TRUE
               next
            }
            if (iP$nonExt[i-1] & abs(iP$z2[i-1]-iP$z2[i])<tol & abs(iP$z1[i-1]-iP$z1[i])<tol) {
               iP$nonExt[i] = TRUE
            }
         }
      }
   }
   # set correct labels
   iP$lbl <- iP$oldLbl
   iP$oldLbl <- NULL
   if (is.null(labels)) labels <- "n"
   if (labels == "coord") iP$lbl <- df2String(iP[,1:n])
   if (labels == "n") iP$lbl <- ""

   return(iP)
}


#' Calculate the corner points for the polytope Ax<=b assuming all variables are
#' continuous.
#'
#' @param A Constraint matrix.
#' @param b Right hand side.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#'
#' @return A data frame with a corner point in each row.
#' @author Lars Relund \email{lars@@relund.dk}
cornerPointsCont <- function (A, b, nonneg = rep(TRUE, ncol(A))) {
   planes <- cbind(A,-b)
   nneg <- NULL
   for (i in 1:ncol(A)) {
      if (nonneg[i]) {
         v <- rep(0, ncol(A))
         v[i] <- -1
         nneg <- rbind(nneg, c(v,0))
      }
   }
   planes <- rbind(planes,nneg)

   # Compute the vertices (all cont variables)
   n <- nrow(planes)
   m <- ncol(planes)
   vertices <- NULL
   tmp <- vector("list", m-1)
   tmp <- lapply(tmp, function(x) 1:n)
   tmp <- expand.grid(tmp)
   tmp <- tmp[apply(tmp, 1, function(x) all(diff(x) > 0)), ]
   tmp <- as.matrix(tmp)
   for( i in 1:nrow(tmp) )
      try( {
         # Intersection of the planes i, j, k
         vertex <- solve(planes[tmp[i,],-m], -planes[tmp[i,],m] )
         # Check that it is indeed in the polyhedron
         if( all( planes %*% c(vertex,1) <= 1e-6 ) ) {
            vertices <- rbind( vertices, vertex )
         }
      }, silent = TRUE)
   #vertices <- as.data.frame(vertices)
   colnames(vertices) <- paste0("x", 1:ncol(vertices))
   rownames(vertices) <- NULL
   #vertices$lbl <- df2String(vertices)
   return(vertices)
}


#' Calculate the corner points for the polytope Ax<=0.
#'
#' @param A Constraint matrix.
#' @param b Right hand side.
#' @param type A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#'
#' @return A data frame with a corner point in each row.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @examples
#' A <- matrix( c(3,-2, 1, 2, 4,-2,-3, 2, 1), nc = 3, byrow = TRUE)
#' b <- c(10, 12, 3)
#' cornerPoints(A, b, type = c("c", "c", "c"))
#' cornerPoints(A, b, type = c("i", "i", "i"))
#' cornerPoints(A, b, type = c("i", "c", "c"))
cornerPoints <- function (A, b, type = rep("c", ncol(A)), nonneg = rep(TRUE, ncol(A))) {
   if (length(type)!=ncol(A)) stop("Arg 'type' must be same length as columns in A!")

   if (all(type == "c")) return(cornerPointsCont(A, b, nonneg))
   if (all(type == "i")) {
      iPoints <- integerPoints(A, b, nonneg)
      tri <- t(geometry::convhulln(iPoints))
      idx <- unique(as.vector(tri))
      return(iPoints[idx,])
   }
   # else combination
   p <- slices(A, b, type, nonneg)
   p <- do.call(rbind, p)
   tri <- t(geometry::convhulln(p))
   #rgl.triangles(p[tri,1],p[tri,2],p[tri,3],col="gold2",alpha=.6)
   idx <- unique(as.vector(tri))
   p <- p[idx,]
   #points3d(p, col="blue", size = 15)
   #p <- as.data.frame(p[idx,])
   colnames(p) <- paste0("x",1:ncol(A))
   rownames(p) <- NULL
   #p$lbl <- df2String(p)
   return(p)
}


#' Integer points inside the feasible region (Ax<=b).
#'
#' @param A Constraint matrix.
#' @param b Right hand side.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#'
#' @return A data frame with all integer points inside the feasible region.
#' @note Do a simple enumeration of all integer points between min and max values found using the continuous polytope.
#' @author Lars Relund \email{lars@@relund.dk}.
#' @export
#' @examples
#' A <- matrix( c(3,-2, 1, 2, 4,-2,-3, 2, 1), nc = 3, byrow = TRUE)
#' b <- c(10, 12, 3)
#' integerPoints(A, b)
#'
#' A <- matrix(c(9, 10, 2, 4, -3, 2), ncol = 2, byrow = TRUE)
#' b <- c(90, 27, 3)
#' integerPoints(A, b)
integerPoints<-function(A, b, nonneg = rep(TRUE, ncol(A))) {
   vertices <- cornerPointsCont(A, b, nonneg = nonneg)
   iPoints <- apply(vertices, 2, range)
   iPoints <- split(iPoints, rep(1:ncol(iPoints), each = nrow(iPoints)))
   iPoints <- lapply(iPoints, function(x) x[1]:x[2])
   iPoints <- expand.grid(iPoints)
   tmp <- A %*% t(iPoints)
   idx <- which(apply(tmp, 2, function(x) all(x <= b + 1e-6)) )
   iPoints <- iPoints[idx,]
   colnames(iPoints) <- paste0("x",1:ncol(A))
   rownames(iPoints) <- NULL
   # iPoints$lbl <- df2String(iPoints)
   return(as.matrix(iPoints))
}

#' Convert each row to a string.
#'
#' @param df Data frame.
#' @param round How many digits to round
#'
#' @return A vector of strings.
df2String <- function(df, round = 2) {
   apply(df, 1, function(x) paste0("(", paste0(round(x,round), collapse = ", "), ")"))
}


#' Find all corner points in the slices define for each fixed integer combination.
#'
#' @param A The constraint matrix.
#' @param b Right hand side.
#' @param type A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#' @param collapse Collapse list to a data frame with unique points.
#'
#' @return A list with the corner points (one entry for each slice).
#' @export
#'
#' @examples
#' A <- matrix( c(3, -2, 1,2, 4, -2,-3, 2, 1), nc = 3, byrow = TRUE)
#' b <- c(10,12,3)
#' slices(A, b, type=c("i","c","i"))
#'
#' A <- matrix(c(9,10,2,4,-3,2), ncol = 2, byrow = TRUE)
#' b <- c(90,27,3)
#' slices(A, b, type=c("c","i"), collapse = TRUE)
slices<-function (A, b, type = rep("c", ncol(A)), nonneg = rep(TRUE, ncol(A)), collapse = FALSE) {
   if (length(type)!=ncol(A)) stop("Arg 'type' must be same length as columns in A!")
   if (sum(type=="i")==0) stop("One variable must be integer to generate slices!")
   if (all(type=="i")) stop("Cannot generate slices since all variables are integer!")

   planes <- cbind(A,-b)
   nneg <- NULL
   for (i in 1:ncol(A)) {
      if (nonneg[i]) {
         v <- rep(0, ncol(A))
         v[i] <- -1
         nneg <- rbind(nneg, c(v,0))
      }
   }
   planes <- rbind(planes,nneg)

   getV <- function(intCst) {
      nR <- nrow(intCst)
      planes <- rbind(planes,intCst)
      n <- nrow(planes)- nR
      m <- ncol(planes)
      tmp2 <- vector("list", m-1)
      tmp2 <- lapply(tmp2, function(x) 1:n)
      if (nR == 1) tmp2[[length(tmp2)]] <- n + nR
      if (nR == 2) {
         tmp2[[length(tmp2)-1]] <- n + nR - 1
         tmp2[[length(tmp2)]] <- n + nR
      }
      tmp2 <- expand.grid(tmp2)
      tmp2 <- tmp2[apply(tmp2, 1, function(x) all(diff(x) > 0)), ]
      tmp2 <- as.matrix(tmp2)
      vertices <- NULL
      for( i in 1:nrow(tmp2) )
         try( {
            # Intersection of the planes i, j, k
            vertex <- solve(planes[tmp2[i,],-m], -planes[tmp2[i,],m] )
            # Check that it is indeed in the polyhedron
            if( all( planes %*% c(vertex,1) <= 1e-6 ) ) {
               vertices <- rbind( vertices, vertex )
            }
         }, silent = TRUE)
      return(vertices)
      #Stop("Error in internal getV function!")
   }

   iPoints <- integerPoints(A, b, nonneg)
   idx <- which(type=="i")
   if (length(idx)==2) {
      minI <- apply(iPoints[,idx], 2, min)
      maxI <- apply(iPoints[,idx], 2, max)
      cases <- as.matrix(expand.grid(minI[1]:maxI[1], minI[2]:maxI[2]))
      #colnames(cases) <- names(maxI)
   } else {
      cases <- as.matrix(min(iPoints[,idx]):max(iPoints[,idx]))
   }

   lst <- vector("list", nrow(cases))
   for (i in 1:nrow(cases)) {
      intCst <- NULL
      tmp <- rep(0,ncol(A)+1)
      for (j in 1:length(cases[i,])) {
         tmp1 <- tmp
         tmp1[idx[j]] <- -1
         tmp1[ncol(A)+1] <- cases[i,j]
         intCst <- rbind(intCst, tmp1)
      }
      lst[[i]] <- getV(intCst)
   }
   lst <- lapply(lst, unique)
   lst <- plyr::compact(lst)
   lst <- lapply(lst, function(x) {
      colnames(x) <- paste0("x", 1:ncol(A))
      rownames(x) <- NULL
      x
   })
   if (collapse) {
      lst <- do.call(rbind, lst)
      lst <- unique(lst)
   }
   return(lst)
}










#' Efficient test for points inside a convex hull in n dimensions. Inspired by
#' the Matlab code by John D'Errico
#' http://www.mathworks.com/matlabcentral/fileexchange/10226-inhull and
#' https://tolstoy.newcastle.edu.au/R/e8/help/09/12/8784.html
#'
#' @param testPts A nxp array to test, n data points, in p dimensions If you
#'   have many points to test, it is most efficient to call this function once
#'   with the entire set.
#' @param vertices A mxp array of vertices of the convex hull, as used by
#'   convhulln.
#' @param hull Tessellation (or triangulation) generated by convhulln If hull is
#'   left empty or not supplied, then it will be generated.
#' @param tol Tolerance on the tests for inclusion in the convex hull. You can
#'   think of tol as the distance a point may possibly lie outside the hull, and
#'   still be perceived as on the surface of the hull. Because of numerical slop
#'   nothing can ever be done exactly here. I might guess a semi-intelligent
#'   value of tol to be
#'
#'   tol = 1.e-13*mean(abs(vertices(:)))
#'
#'   In higher dimensions, the numerical issues of floating point arithmetic
#'   will probably suggest a larger value of tol.
#'
#' @return An integer vector of length n
#       1 = inside hull
#      -1 = inside hull
#       0 = on hull (to precision indicated by tol)
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
#' @importFrom MASS Null
inHull <- function(testPts, vertices, hull=geometry::convhulln(vertices),
                   tol=mean(mean(abs(vertices)))*sqrt(.Machine$double.eps)) {
   # ensure arguments are matrices (not data frames) and get sizes
   if (is.vector(testPts)) testPts = t(testPts)
   vertices <- as.matrix(vertices)
   testPts <- as.matrix(testPts)
   p <- ncol(vertices)   # columns in vertices
   cx <- nrow(testPts)  # rows in testPts
   nt <- nrow(hull)    # number of simplexes in hull
   # find normal vectors to each simplex
   nrmls <- matrix(NA, nt, p)         # predefine each nrml as NA, degenerate
   degenflag <- matrix(TRUE, nt, 1)
   for (i in  1:nt) {
      nullsp <- t(Null(t(vertices[hull[i, -1], ] - matrix(vertices[hull[i, 1], ], p - 1, p, byrow =TRUE))))
      if (dim(nullsp)[1] == 1) {
         nrmls[i, ] <- nullsp
         degenflag[i] <- FALSE
      }
   }
   # Warn of degenerate faces, and remove corresponding normals
   if (sum(degenflag) > 0)
      warning(sum(degenflag), " degenerate faces in convex hull")
   nrmls <- nrmls[!degenflag, ]
   nt <- nrow(nrmls)
   # find center point in hull, and any (1st) point in the plane of each simplex
   center = colMeans(vertices)
   a <- vertices[hull[!degenflag, 1], ]
   # scale normal vectors to unit length and ensure pointing inwards
   nrmls <- nrmls / matrix(apply(nrmls, 1, function(x) sqrt(sum(x ^ 2))), nt, p)
   dp <- sign(apply((matrix(center, nt, p, byrow = TRUE) - a) * nrmls, 1, sum))
   nrmls <- nrmls * matrix(dp, nt, p)
   # if  min across all faces of dot((x - a),nrml) is
   #      +ve then x is inside hull
   #      0   then x is on hull
   #      -ve then x is outside hull
   # Instead of dot((x - a),nrml)  use dot(x,nrml) - dot(a, nrml)
   aN <- diag(a %*% t(nrmls))
   val <- apply(testPts %*% t(nrmls) - matrix(aN, cx, nt, byrow = TRUE), 1, min)
   # code  values inside 'tol' to zero, return sign as integer
   val[abs(val) < tol] <- 0
   as.integer(sign(val))
}







#' Efficient test for points inside a convex hull in n dimensions. Inspired by
#' the Matlab code by John D'Errico
#' http://www.mathworks.com/matlabcentral/fileexchange/10226-inhull and
#' https://tolstoy.newcastle.edu.au/R/e8/help/09/12/8784.html
#'
#' @param testPts A nxp array to test, n data points, in p dimensions If you
#'   have many points to test, it is most efficient to call this function once
#'   with the entire set.
#' @param vertices A mxp array of vertices of the convex hull, as used by
#'   convhulln.
#' @param hull Tessellation (or triangulation) generated by convhulln If hull is
#'   left empty or not supplied, then it will be generated.
#' @param tol Tolerance on the tests for inclusion in the convex hull. You can
#'   think of tol as the distance a point may possibly lie outside the hull, and
#'   still be perceived as on the surface of the hull. Because of numerical slop
#'   nothing can ever be done exactly here. I might guess a semi-intelligent
#'   value of tol to be
#'
#'   tol = 1.e-13*mean(abs(vertices(:)))
#'
#'   In higher dimensions, the numerical issues of floating point arithmetic
#'   will probably suggest a larger value of tol.
#'
#' @return An integer vector of length n
#       1 = inside hull
#      -1 = inside hull
#       0 = on hull (to precision indicated by tol)
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
hullSegment <- function(vertices, hull=geometry::convhulln(vertices),
                        tol=mean(mean(abs(vertices)))*sqrt(.Machine$double.eps)) {
   vertices <- as.matrix(vertices)
   p <- ncol(vertices)   # columns in vertices
   nt <- nrow(hull)    # number of simplexes in hull
   # find normal vectors to each simplex
   nrmls <- matrix(NA, nt, p)         # predefine each nrml as NA, degenerate
   degenflag <- matrix(TRUE, nt, 1)
   for (i in  1:nt) {
      nullsp <- t(Null(t(vertices[hull[i, -1], ] - matrix(vertices[hull[i, 1], ], p - 1, p, byrow =TRUE))))
      if (dim(nullsp)[1] == 1) {
         nrmls[i, ] <- nullsp
         degenflag[i] <- FALSE
      }
   }
   nt <- nrow(nrmls)
   # find center point in hull, and any (1st) point in the plane of each simplex
   center = colMeans(vertices)
   a <- vertices[hull[, 1], ]
   # scale normal vectors to unit length and ensure pointing inwards
   nrmls <- nrmls / matrix(apply(nrmls, 1, function(x) sqrt(sum(x ^ 2))), nt, p)
   dp <- sign(apply((matrix(center, nt, p, byrow = TRUE) - a) * nrmls, 1, sum))
   nrmls <- nrmls * matrix(dp, nt, p)
   idx <- !duplicated(round(nrmls,10))
   nrmls <- nrmls[idx,]
   a <- a[idx,]
   aN <- diag(a %*% t(nrmls))
   res <- NULL
   m <- nrow(vertices)
   for( i in 1:m )
      for( j in 1:m )
         if( i < j ) {
            # Middle of the segment
            p <- .5 * vertices[i,] + .5 * vertices[j,]
            # Check if it is at the intersection of two planes
            tmp <- p %*% t(nrmls) - aN #matrix(aN, 1, nrow(nrmls), byrow = TRUE)
            if(sum( abs( tmp ) < 1e-6 ) >= 2) res <- rbind(res, c(i,j))
         }
   return(as.matrix(res))
}




#' Plot the polytope (bounded convex set) of a linear matematical program.
#'
#' @param A The constraint matrix.
#' @param b Right hand side.
#' @param type A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#' @param crit Either max or min (only used if add the iso profit line)
#' @param faces A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#'   Usefull if e.g. want to show the linear relaxation of an IP.
#' @param plotFaces If \code{True} then plot the faces.
#' @param plotFeasible If \code{True} then plot the feasible points/segments
#'   (relevant for IPLP/MILP).
#' @param plotOptimum Show the optimum corner solution point (if alternative solutions
#'   only one is shown) and add the iso profit line.
#' @param latex If \code{True} make latex math labels for TikZ.
#' @param labels If \code{NULL} don't add any labels. If 'n' no labels but show the points. If 'coord' add
#'   coordinates to the points. Otherwise number all points from one.
#' @param ... If 2D arguments passed to the \link{aes} function in
#'   \link{geom_point} or \link{geom_line}.
#'
#' @return If 2D a ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
#' @import ggplot2 rgl
plotPolytope <-
   function(A,
            b,
            coeff = NULL,
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
      if (!is.null(coeff))
         if (length(coeff)!=ncol(A)) stop("Arg. 'coeff' must have the same columns as in A and be a single criterion!")
      if (ncol(A)==2) return(plotPolytope2D(A, b, coeff, type, nonneg, crit, faces, plotFaces, plotFeasible, plotOptimum, latex, labels, ...))
      if (ncol(A)==3) return(plotPolytope3D(A, b, coeff, type, nonneg, crit, faces, plotFaces, plotFeasible, plotOptimum, latex, labels, ...))
      stop("Only 2 or 3 variables supported!")
   }




#' Create a plot of a polytope (bounded convex set)
#'
#' If an iso profit line added then the max/min is obtained among the \code{points}.
#'
#' @param cPoints Corner points in the polytope.
#' @param points Points to plot (e.g integer points inside the polytope or corner points).
#' @param rangePoints Ranges to plot (e.g if one variable is continuous). Found using \code{ranges} function.
#' @param showLbl Add labels to the points (only if points have a \code{lbl} column).
#' @param iso NULL or if 2D vector add the iso profit line the the solution plot.
#' @param crit Either max or min (only used if add the iso profit line)
#' @param latex If true make latex math labels for TikZ.
#' @param feasible The points searched for max/min value (only used if add the iso profit line).
#' @param ... Arguments passed to the \link{aes} function in \link{geom_point}.
#'
#' @return The ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
#' @import ggplot2
plotPolytope2D<-function(A, b, coeff = NULL, type = rep("c", ncol(A)), nonneg = rep(TRUE, ncol(A)),
                         crit="max", faces = rep("c", ncol(A)), plotFaces = TRUE, plotFeasible = TRUE,
                         plotOptimum = FALSE, latex = FALSE, labels = NULL, ...)
{
   if (!is.null(coeff) & (!is.vector(coeff) | !length(coeff)==ncol(A))) stop("Arg. coeff must be a vector of same length as the number of columns in A.")
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
      idx <- chull(cPoints)
      cPoints <- cPoints[idx,]
      p <- p + geom_polygon(data = as.data.frame(cPoints), aes_string(x = 'x1', y = 'x2'), fill="gray90", size = 0.5,
                            linetype = 1, color="gray")
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
            p <- p + ggrepel::geom_text_repel(aes_string(x = 'x1', y = 'x2', label = 'lbl'), data=tmp, size=3,
                                              colour = "gray50")
         if (anyDuplicated(cbind(tmp$x1,tmp$x2), MARGIN = 1) == 0)
            p <- p + geom_text(aes_string(x = 'x1', y = 'x2', label = 'lbl'), data=tmp, nudge_x = nudgeS,
                               nudge_y = nudgeS, hjust=1, size=3, colour = "gray50")
      }
   }

   if (plotOptimum) {
      if (!is.null(coeff)) {    # add iso profit line
         tmp <- points
         tmp$lbl <- df2String(tmp)
         tmp$z <- as.matrix(points[,1:2]) %*% coeff
         if (crit=="max") i <- which.max(tmp$z)
         if (crit=="min") i <- which.min(tmp$z)
         # if (latex) str <- paste0("$x^* = (", tmp$x1[i], ",", tmp$x2[i], ")$")
         # if (!latex) str <- paste0("x* = ", tmp$lbl[1])
         if (latex) str <- paste0("$z^* = ", round(tmp$z[i], 2) , "$")
         if (!latex) str <- paste0("z* = ", round(tmp$z[i],2) )
         if (coeff[2]!=0) {
            p <- p + geom_abline(intercept = tmp$z[i]/coeff[2], slope = -coeff[1]/coeff[2], lty="dashed")
         } else {
            p <- p + geom_vline(xintercept = tmp$x1[i], lty="dashed")
         }
         p <- p + geom_label(aes_string(x = 'x1', y = 'x2', label = 'str'), data = tmp[i,], nudge_x = 1.0)
      }
   }
   p <- p + myTheme
   return(p)
}





#' Create a plot of a polytope (bounded convex set)
#'
#' If an iso profit line added then the max/min is obtained among the \code{points}.
#'
#'
#' @param A A matrix.
#' @param b Right hand side.
#' @param type A character vector of same length as number of variables. If entry k is 'i'
#'             variable k must be integer and if 'c' continuous.
#' @param nonneg A boolean vector of same length as number of variables. If entry k is TRUE then
#'             variable k must be non-negative.

#' @param points Points to plot (e.g integer points inside the polytope or corner points).
#' @param rangePoints Ranges to plot (e.g if one variable is continuous). Found using \code{ranges} function.
#' @param showLbl Add labels to the points (only if points have a \code{lbl} column).
#' @param iso NULL or if 2D vector add the iso profit line the the solution plot.
#' @param crit Either max or min (only used if add the iso profit line)
#' @param latex If true make latex math labels for TikZ.
#' @param feasible The points searched for max/min value (only used if add the iso profit line).
#' @param ... Arguments passed to the \link{aes} function in \link{geom_point}.
#'
#' @return The ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
#' @import ggplot2
plotPolytope3D <-
   function(A,
            b,
            coeff = NULL,
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
      argsPlot3d <- mergeLists(list(xlab = '', box = FALSE, axes = F), args$argsPlot3d)
      argsTitle3d <- mergeLists(list(xlab = 'x1', ylab = 'x2', zlab = 'x3'), args$argsTitle3d)

      #open3d()
      do.call(plot3d, args = c(list(x = replicate(2, 1:3), type = 'n'), argsPlot3d))
      aspect3d("iso")

      plotMat <- function(mat, col="black") {
         if (nrow(mat)==1) points3d(mat, col=col, size = 10)
         if (nrow(mat)==2) { #segments3d(mat, col = "black", lwd=10, smooth=T)
            cyl <- cylinder3d(mat, radius = 0.01)
            shade3d(cyl, col = col)
         }
         if (nrow(mat)==3) triangles3d(mat, col=col, alpha=0.6)
         #if (nrow(mat)==4) quads3d(mat, col="black", fill=T)
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
            points3d(iPoints[,1:3], col="black", size = 7)
         } else {
            pl <- slices(A, b, type, nonneg)
            for (i in 1:length(pl)) {
               mat <- pl[[i]]
               if (is.null(mat)) next
               if (nrow(mat)==1) points3d(mat, col="black", size = 10)
               if (nrow(mat)==2) { #segments3d(mat, col = "black", lwd=10, smooth=T)
                  cyl <- cylinder3d(mat, radius = 0.01)
                  shade3d(cyl, col = "black")
               }
               if (nrow(mat)==3) triangles3d(mat, col="grey100", alpha=0.6)
               #if (nrow(mat)==4) quads3d(mat, col="black", fill=T)
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
         if (is.null(coeff)) stop("You need to specify the objective coefficients when using argument plotOption = TRUE.")
         vertices <- cornerPoints(A, b, type, nonneg) #points3d(vertices[,1:3], col="blue", size = 10)
         val <- vertices[,1:3] %*% as.matrix(coeff)
         #print(cbind(vertices,val))
         idx <- which.max(val)
         val <- vertices[idx,1:3]
         points3d(val[1], val[2], val[3], col="red", size = 14)
         #spheres3d(val[1], val[2], val[3], col="black", radius = 1)
         #planes3d(coeff, d = -val, alpha = 0.6)
         #arrow3d(c(0,0,0), 0.25*coeff, type="lines", barblen = 0.01, col="red", lwd=5)
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
         if (length(which(type == "c"))<length(type) & length(which(type == "c"))>0)
            points3d(points[,1:3], col="grey50", size = 7)

         if (labels=="coord")
            points$lbl <- df2String(points)
         else if (labels == "n")
            points$lbl <- ""
         else points$lbl <- 1:nrow(points)
         text3d(points[,1:3], text = points$lbl, cex = c(1.1,1.1), adj=2, font = 2)
      }

      do.call(axes3d, args = argsAxes3d)
      do.call(title3d, args = argsTitle3d)
   }



mergeLists <- function (a,b) {
   c(a[setdiff(names(a), names(b))], b)
}



#' Create a plot of the criterion space of a bi-objective problem.
#'
#' @param A The constraint matrix.
#' @param b Right hand side.
#' @param coeff A p x n matrix(one row for each criterion).
#' @param type A character vector of same length as number of variables. If
#'   entry k is 'i' variable \eqn{k} must be integer and if 'c' continuous.
#' @param nonneg A boolean vector of same length as number of variables. If
#'   entry k is TRUE then variable k must be non-negative.
#' @param crit Either max or min (only used if add the iso profit line).
#' @param addTriangles Add seach triangles defined by the non-dominated extreme
#'   points.
#' @param addHull Add the convex hull and the rays.
#' @param plotFeasible If \code{True} then plot the criterion points/slices.
#' @param latex If true make latex math labels for TikZ.
#' @param labels If \code{NULL} don't add any labels. If 'n' no labels but show the points. If 'coord' add
#'   coordinates to the points. Otherwise number all points from one.
#'
#' @note Currently only points are checked for dominance. That is, for MILP
#'   models some nondominated points may infact be dominated by a segment.
#' @return The ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
plotCriterion2D <- function(A,
                            b,
                            coeff,
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
   points <- criterionPoints(points, coeff, crit, labels)
   if (all(type == "c")) { # if cont then no non-ext
      points$nD[points$nD & !points$ext] <- FALSE
      points$ext[points$nD & !points$ext] <- FALSE
      points$nonExt[points$nD & !points$ext] <- FALSE
   }
   dat <<- points  # hack to get data as data frame
   # Initialize plot
   p <- ggplot(points, aes(x = z1, y = z2, col = "grey10") )
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
      # plot feasiable areas
      if (all(type == "c")) {
         idx <- chull(points[,c("z1","z2")])
         p <- p + geom_polygon(data = points[idx,], aes_string(x = 'z1', y = 'z2'),
                               fill=NA, size = 0.5, linetype = 1, col="grey80", alpha=0.6)
         #points <- points[points$ext,] # remove all nonextreme (since LP model)
      } else if (all(type == "i")) {
         #iPoints <- integerPoints(A, b, nonneg)
         #iPoints <- criterionPoints(iPoints, coeff, crit, labels)
         p <- p + geom_point(aes_string(colour = 'nD', shape = 'ext'), data = points) +
         #    #coord_fixed(ratio = 1) +
            scale_colour_grey(start = 0.6, end = 0)
      } else {
         pl <- slices(A, b, type, nonneg)
         pl <- lapply(pl, function(x) {
            x <- x %*% t(coeff)
            if (is.vector(x)) x <- matrix(x, nrow=1)
            x[,1:2, drop = FALSE]
         })
         idx <- lapply(pl, chull)
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
            aes(
               x = z1,
               y = z2,
               group = g
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
               geom_segment(x=tmp$z1[r],y=tmp$z2[r],xend=tmp$z1[r+1],yend=tmp$z2[r+1], colour="gray") +
               geom_segment(x=tmp$z1[r],y=tmp$z2[r],xend=tmp$z1[r],yend=tmp$z2[r+1], colour="gray") +
               geom_segment(x=tmp$z1[r],y=tmp$z2[r+1],xend=tmp$z1[r+1],yend=tmp$z2[r+1], colour="gray")
         }
      }
   }

   nudgeC=-(max(points$z1)-min(points$z1))/100
   if (!is.null(labels) & anyDuplicated(round(cbind(points$z1,points$z2),10), MARGIN = 1) > 0)
      p <- p + ggrepel::geom_text_repel(aes_string(label = 'lbl'), size=3, colour = "gray50", data=points)
   if (!is.null(labels) & anyDuplicated(round(cbind(points$z1,points$z2),10), MARGIN = 1) == 0)
      p <- p + geom_text(aes_string(label = 'lbl'), nudge_x = nudgeC, nudge_y = nudgeC, hjust=1, size=3,
                         colour = "gray50", data=points)
   p <- p + myTheme
   return(p)
}









