

#' Calculate the criterion points of a set of points and ranges to find the set
#' of non-dominated points (Pareto points) and classify them into extreme
#' supported, non-extreme supported, non-supported.
#'
#' @param points A data frame with a column for each variable in the solution
#'   space (can also be a rangePoints).
#' @param obj A p x n matrix(one row for each criterion).
#' @param crit Either max or min.
#' @param labels If \code{NULL} or "n" don't add any labels (empty string). If
#'   'coord' labels are the solution space coordinates. Otherwise number all
#'   points from one based on the solution space points.
#'
#' @return A data frame with columns x1, ..., xn, z1, ..., zp, lbl (label), nD
#'   (non-dominated), ext (extreme), nonExt (non-extreme supported).
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @examples
#' A <- matrix( c(3, -2, 1, 2, 4, -2, -3, 2, 1), nc = 3, byrow = TRUE)
#' b <- c(10,12,3)
#' points <- integerPoints(A, b)
#' obj <- matrix( c(1,-3,1,-1,1,-1), byrow = TRUE, ncol = 3 )
#' criterionPoints(points, obj, crit = "max", labels = "numb")
criterionPoints<-function(points, obj, crit, labels = "coord") {
   n <- ncol(obj)
   zVal <- points %*% t(obj)
   zVal <- round(zVal,10)
   colnames(zVal) <- paste0("z", 1:nrow(obj))
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
      if (abs(p2 - iP$z2[r]) < tol &
          abs(p1 - iP$z1[r]) < tol) {
         iP$nD[r] <- TRUE
         p1 <- iP$z1[r]
         p2 <- iP$z2[r]
         next
      }
      if (crit == "max" &
          p2 - iP$z2[r] > tol &
          iP$z1[r] > p1 + tol) {
         iP$nD[r] <- TRUE
         p1 <- iP$z1[r]
         p2 <- iP$z2[r]
         next
      }
      if (crit == "min" &
          iP$z2[r] - p2 > tol &
          iP$z1[r] < p1 - tol) {
         iP$nD[r] <- TRUE
         p1 <- iP$z1[r]
         p2 <- iP$z2[r]
         next
      }
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


#' Calculate the corner points for the polytope Ax<=b.
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


#' Integer points in the feasible region (Ax<=b).
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


#' Binary (0-1) points in the feasible region (Ax<=b).
#'
#' @param A Constraint matrix.
#' @param b Right hand side.
#'
#' @return A data frame with all binary points inside the feasible region.
#' @note Do a simple enumeration of all binary points. Will not work if `ncol(A)` large.
#' @author Lars Relund \email{lars@@relund.dk}.
#' @export
#' @examples
#' A <- matrix( c(3,-2, 1, 2, 4,-2,-3, 2, 1), nc = 3, byrow = TRUE)
#' b <- c(10, 12, 3)
#' binaryPoints(A, b)
#'
#' A <- matrix(c(9, 10, 2, 4, -3, 2), ncol = 2, byrow = TRUE)
#' b <- c(90, 27, 3)
#' binaryPoints(A, b)
binaryPoints<-function(A, b) {
   iPoints <- rep(list(0:1), ncol(A))
   iPoints <- expand.grid(iPoints)
   tmp <- A %*% t(iPoints)
   idx <- which(apply(tmp, 2, function(x) all(x <= b + 1e-6)) )
   iPoints <- iPoints[idx,]
   colnames(iPoints) <- paste0("x",1:ncol(A))
   rownames(iPoints) <- NULL
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



#' Check if point input is okay
#'
#' @param pts Point input.
#' @param p Desired dimension of points.
#' @param warn Output warnings.
#'
#' @return Point input converted to a matrix.
.checkPts <- function(pts, p = NULL, warn = FALSE) {
   if (is.vector(pts)) {
      if (warn) warning("Point specified as a vector. Converting to a matrix with a single row!")
      pts <- t(matrix(pts))
   }
   nr <- nrow(pts)
   set <- as.matrix(unique(pts[,, drop = FALSE]))
   if (nrow(pts) != nrow(set)) {
      stop("Points specified should be unique!")
   }

   if (is.data.frame(pts)) {
      if (warn) warning("Points specified as a data frame. Converting to a matrix!")
   }
   if (!is.null(p))
      if (ncol(set) != p) {
      stop("The dimension of the poins should be ", p, "!")
      }
   if (is.null(rownames(set))) rownames(set) <- 1:nrow(set)
   if (is.null(colnames(set))) colnames(set) <- 1:ncol(set)
   return(set)
}
