
#' Add 2D discrete points to a non-dominated set and classify them into extreme
#' supported, non-extreme supported, non-supported.
#'
#' @param points A data frame. It is assumed that z1 and z2 are in the two first columns.
#' @param nDSet A data frame with current non-dominated set (NULL is none yet).
#' @param crit Either max or min.
#' @param keepDom Keep dominated points.
#'
#' @return A data frame with columns z1 and z2, nD (non-dominated),
#'         ext (extreme), nonExt (non-extreme supported).
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @examples
#' nDSet <- data.frame(z1=c(12,14,16,18), z2=c(18,16,12,4))
#' points <- data.frame(z1 = c(18,18,14,15,15), z2=c(2,6,14,14,16))
#' addNDSet(points, nDSet, crit = "max")
#' addNDSet(points, nDSet, crit = "max", keepDom = TRUE)
#' addNDSet(points, nDSet, crit = "min")
addNDSet<-function(points, nDSet = NULL, crit = "max", keepDom = FALSE) {
   nDSet$nD <- NULL; nDSet$ext <- NULL; nDSet$nonExt <- NULL
   if (!is.null(nDSet))
      if (ncol(points)!=ncol(nDSet))
         stop("Number of columns minus classification colunms must be the same!")
   iP = points
   colnames(iP)[1:2] <- paste0("z", 1:2)
   #iP <- round(iP,10)
   rownames(iP) <- NULL
   iP <- rbind(iP, nDSet)
   tol <- 1e-4
   iP$oldRowIdx <- 1:length(iP$z1)
   if (crit=="max") iP <- iP[order(-iP$z2,-iP$z1),]
   if (crit=="min") iP <- iP[order(iP$z2,iP$z1),]

   # classify non dom
   iP$nD <- FALSE
   iP$nD[1] <- TRUE  # upper left point
   p1 <- iP$z1[1]; p2 <- iP$z2[1]  # current non dom point (due to sorting will p2 always be larger than or equal to current)
   for (r in 2:length(iP$z1)) { # current point under consideration
      if (abs(p2-iP$z2[r])<tol & abs(p1-iP$z1[r])<tol) {iP$nD[r] <- TRUE; p1 <- iP$z1[r]; p2 <- iP$z2[r]; next}
      if (crit=="max" & p2-iP$z2[r]>tol & iP$z1[r]>p1+tol) {iP$nD[r] <- TRUE; p1 <- iP$z1[r]; p2 <- iP$z2[r]; next}
      if (crit=="min" & iP$z2[r]-p2>tol & iP$z1[r]<p1-tol) {iP$nD[r] <- TRUE; p1 <- iP$z1[r]; p2 <- iP$z2[r]; next}
   }
   # iP$nD <- TRUE
   # for (i in 2:nrow(iP)) { # remove non-dom z2
   #    if (iP$z2[i-1]==iP$z2[i]) iP$nD[i] <- FALSE
   # }
   # if (!keepDom) iP <- iP[iP$nD,]
   # for (i in 2:nrow(iP)) { # remove non-dom z1
   #    if (iP$z1[i-1]==iP$z1[i]) iP$nD[i] <- FALSE
   # }
   if (!keepDom) iP <- iP[iP$nD,]
   iP$rowIdx <- 1:nrow(iP)
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
               iP$ext[nD$rowIdx[i]] <- TRUE
               lr <- i
            } else {
               ul <- lr
               lr<-length(idx)
            }
         }
         if (crit=="min") {
            i <- which.min(nD$val)
            if (nD$val[ul]>nD$val[i] + tol) {
               iP$ext[nD$rowIdx[i]] <- TRUE
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
               if (abs(slope - slopeCur) < tol) iP$nonExt[nDCand$rowIdx[j]==iP$rowIdx] <- TRUE
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
   iP$rowIdx <- NULL
   iP$oldRowIdx <- NULL
   return(iP)
}


#' Generate a sample of nondominated points.
#'
#' @param n Number of samples generated (note only a subset of these points are nondominated).
#' @param random Random sampling.
#' @param onSphere Generate points on a sphere.
#' @param keep Keep dominated points also.
#' @param ... Arguments for generating the points based on method. Currently two lists may be added:
#'            argsRandom Arguments for random sampling.
#'            argsSphere Arguments for sampling on a sphere.
#'
#' @return A data frame with xyz columns and ...0
#' @export
#'
#' @examples
#' ini3D()
#' p <- genNDSet(1000, random = TRUE, keep = TRUE)
#' head(p)
#' plotPoints3D(p)
#' plotPoints3D(p[!p$dom,], argsPlot3d = list(col = "red", size = 10))
#' finalize3D()
#'
#' ini3D()
#' p <- genNDSet(1000, keep = TRUE)
#' rgl::spheres3d(c(52,52,52), radius=50, color = "grey100", alpha=0.1)
#' plotPoints3D(p)
#' plotPoints3D(p[!p$dom,], argsPlot3d = list(col = "red", size = 10))
#' rgl::planes3d(52,52,52,-8112, alpha = 0.5, col = "red")
#' finalize3D()
#'
#' ini3D()
#' cent <- c(100,100,100)
#' r <- 75
#' p <- genNDSet(1000, keep = TRUE, argsSphere = list(center = cent, radius = r))
#' rgl::spheres3d(cent, radius=r, color = "grey100", alpha=0.1)
#' plotPoints3D(p)
#' plotPoints3D(p[!p$dom,], argsPlot3d = list(col = "red", size = 10))
#' rgl::planes3d(cent[1],cent[2],cent[3],-sum(cent^2), alpha = 0.5, col = "red")
#' finalize3D()
#'
#' ini3D()
#' cent <- c(100,100,100)
#' r <- 75
#' planeC <- c(cent+r/3)
#' planeC <- c(planeC, -sum(planeC^2))
#' p <- genNDSet(1000, keep = TRUE,
#'   argsSphere = list(center = cent, radius = r, below = FALSE, plane = planeC))
#' rgl::spheres3d(cent, radius=r, color = "grey100", alpha=0.1)
#' plotPoints3D(p)
#' plotPoints3D(p[!p$dom,], argsPlot3d = list(col = "red", size = 10))
#' rgl::planes3d(planeC[1],planeC[2],planeC[3],planeC[4], alpha = 0.5, col = "red")
#' finalize3D()
genNDSet <- function(n = 100, random = FALSE, onSphere = TRUE, keep = F, ...) {
   args <- list(...)
   argsRandom <- mergeLists(list(xlim = 1:100, ylim = 1:100, zlim = 1:100), args$argsRandom)
   calcPlane = FALSE
   if (is.null(args$argsSphere$plane)) calcPlane <- TRUE
   argsSphere <-
      mergeLists(list(
         radius = 50,
         center = c(52, 52, 52),
         plane = c(52, 52, 52, -8112),
         below = TRUE
      ),
      args$argsSphere)
   if (calcPlane)
      argsSphere$plane <-
      c(
         argsSphere$center[1],
         argsSphere$center[2],
         argsSphere$center[3],
         -sum(argsSphere$center ^ 2)
      )

   if (random) onSphere = FALSE
   if (random) {
      set <- cbind(x = sample(argsRandom$xlim,n, replace = T),
                   y = sample(argsRandom$ylim,n, replace = T),
                   z = sample(argsRandom$zlim,n, replace = T))
   }
   if (onSphere) {
      sp <- sphereplot::pointsphere(n,c(0,360),c(-90,90),c(argsSphere$radius,argsSphere$radius))
      p <- sphereplot::sph2car(sp, deg = TRUE)
      p <- p + matrix(rep(argsSphere$center, dim(p)[1]), byrow = T, ncol=3)  # shift center
      set<- NULL
      for (i in 1:nrow(p)) {
         x <- p[i,1]
         y <- p[i,2]
         z <- p[i,3]
         if (argsSphere$below &&
             argsSphere$plane[1]*x + argsSphere$plane[2]*y +
             argsSphere$plane[3]*z <= -argsSphere$plane[4]) {
            set <- rbind(set, p[i,])
         }
         if (!argsSphere$below &&
             argsSphere$plane[1]*x + argsSphere$plane[2]*y +
             argsSphere$plane[3]*z >= -argsSphere$plane[4]) {
            set <- rbind(set, p[i,])
         }
      }
      set <- round(set)
   }

   set <- set[order(set[,1],set[,2],set[,3]),]
   set <- as.data.frame(set)
   set$dom <- FALSE
   for (i in 1:(dim(set)[1]-1)) {
      for (j in (i+1):dim(set)[1]) {
         if (!set$dom[j]) {
            if (set$x[i]<=set$x[j] && set$y[i]<=set$y[j] && set$z[i]<=set$z[j]) set$dom[j] <- TRUE
         }
      }
   }
   if (!keep) set <- set[!set$dom,1:3]
   row.names(set) <- NULL
   return(set)
}