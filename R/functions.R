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
#'
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


#' Convert min/max to direction (1/-1)
#'
#' @param m Min/max vector.
#' @param p Number of objectives.
#'
#' @return A direction vector (min = 1 and max = -1)
#' @keywords internal
.mToDirection <- function(m, p) {
   if (length(m) != p) m <- rep(m[1],p)
   return(dplyr::if_else(m == "min", 1, -1))
}


#' Check if point input is okay
#'
#' @param pts Point input.
#' @param p Desired dimension of points.
#' @param warn Output warnings.
#' @param stopUnique Stop if rows not are unique.
#' @param asDF Return as data frame otherwise as matrix.
#'
#' @return Point input converted to a matrix.
#' @keywords internal
.checkPts <- function(pts, p = NULL, warn = FALSE, stopUnique = TRUE, asDF = FALSE) {
   if (is.vector(pts)) {
      if (warn) warning("Point specified as a vector. Converting to a matrix with a single row!")
      pts <- t(matrix(pts))
   }
   if (stopUnique) {
      set <- as.matrix(unique(pts[,, drop = FALSE]))
      if (nrow(pts) != nrow(set)) {
         stop("Points specified should be unique!")
      }
   }
   if (!is.null(p))
      if (ncol(pts) != p) {
         stop("The dimension of the points should be ", p, "!")
      }
   if (!asDF) {
      if (is.data.frame(pts)) {
         if (warn) warning("Points specified as a data frame. Converting to a matrix!")
         pts <- as.matrix(pts)
      }
   } else pts <- as.data.frame(pts)
   if (is.null(rownames(pts))) rownames(pts) <- 1:nrow(pts)
   if (is.null(colnames(pts))) colnames(pts) <- paste0("p",1:ncol(pts))
   return(pts)
}
