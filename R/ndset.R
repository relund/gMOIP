
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
#' addNDSet2D(points, nDSet, crit = "max")
#' addNDSet2D(points, nDSet, crit = "max", keepDom = TRUE)
#' addNDSet2D(points, nDSet, crit = "min")
addNDSet2D<-function(points, nDSet = NULL, crit = "max", keepDom = FALSE) {
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


#' Generate a sample of points in dimension $p$.
#'
#' @param p Dimension of the points.
#' @param n Number of samples generated.
#' @param range The range of the points in each dimension (a vector or matrix with `p` rows).
#' @param random Random sampling.
#' @param sphere Generate points on a sphere.
#' @param box Generate points in boxes.
#' @param ... Further arguments passed on to the method for generating points. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsSphere`: A list of arguments for generating points on a sphere:
#'      - `radius`: The radius of the sphere.
#'      - `center`: The center of the sphere.
#'      - `plane`: The plane used.
#'      - `below`: Either true (generate points below the plane), false (generate points above the
#'                 plane) or `NULL` (generated on the whole sphere).
#'      - `factor`: If using af plane. Then the factor multiply `n` with so generate enough points
#'                  below/above the plane.
#'   * `argsBox`: A list of arguments for generating points inside boxes:
#'      - `intervals`: Number of intervals to split the length of the range into. That is, each
#'                     range is divided into `intervals` (sub)intervals and only the lowest/higest
#'                     subrange is used.
#'      - `cor`: How to correlate indices. If `'idxAlt'` then alternate the intervals (high/low)
#'               for each dimension. For instance if `p = 3` and the first dimension is in the high
#'               interval range then the second will be in the low interval range and third in the
#'               high interval range again. If `idxRand` then choose the low/high interval range
#'               for each dimension based on `prHigh`. If `idxSplit` then select
#'               `floor(p/2):ceiling(p/2)` dimensions for the high interval range and the other for
#'               the low interval range.
#'      - `prHigh`: Probablity for choosing the high interval range in each dimension.
#'
#' @details Note having ranges with different length when using the sphere method, doesn't make
#'   sense. The best option is proberly to use a center and radius here. Moreover, as for higher
#'   `p` you may have to use a larger radius than half of the desired interval range.
#'
#' @return A data frame with `p` columns
#' @export
#'
#' @examples
#' ### Using random
#' ## p = 2
#' range <- matrix(c(1,100, 50,100), ncol = 2, byrow = TRUE )
#' pts <- genSample(2, 1000, range = range, random = TRUE)
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plot(pts)
#'
#' ## p = 3
#' range <- matrix(c(1,100, 50,100, 10,50), ncol = 2, byrow = TRUE )
#' ini3D()
#' pts <- genSample(3, 1000, range = range, random = TRUE)
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plotPoints3D(pts)
#' finalize3D()
#'
#' ## other p
#' p <- 10
#' range <- c(1,100)
#' pts <- genSample(p, 1000, range = range, random = TRUE)
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#'
#'
#' ### Using sphere
#' ## p = 2
#' range <- c(1,100)
#' cent <- rep(range[1] + (range[2]-range[1])/2, 2)
#' pts <- genSample(2, 1000, range = range)
#' dim(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plot(pts, asp=1)
#' abline(sum(cent^2)/cent[1], -cent[2]/cent[1])
#'
#' cent <- c(100,100)
#' r <- 75
#' planeC <- c(cent+r/3)
#' planeC <- c(planeC, -sum(planeC^2))
#' pts <- genSample(2, 100,
#'   argsSphere = list(center = cent, radius = r, below = FALSE, plane = planeC, factor = 6))
#' dim(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plot(pts, asp=1)
#' abline(-planeC[3]/planeC[1], -planeC[2]/planeC[1])
#'
#' pts <- genSample(2, 100, argsSphere = list(center = cent, radius = r, below = NULL))
#' dim(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plot(pts, asp=1)
#'
#' ## p = 3
#' ini3D()
#' range <- c(1,100)
#' cent <- rep(range[1] + (range[2]-range[1])/2, 3)
#' pts <- genSample(3, 1000, range = range)
#' dim(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' rgl::spheres3d(cent, radius=49.5, color = "grey100", alpha=0.1)
#' plotPoints3D(pts)
#' rgl::planes3d(cent[1],cent[2],cent[3],-sum(cent^2), alpha = 0.5, col = "red")
#' finalize3D()
#'
#' ini3D()
#' cent <- c(100,100,100)
#' r <- 75
#' planeC <- c(cent+r/3)
#' planeC <- c(planeC, -sum(planeC^2))
#' pts <- genSample(3, 100,
#'   argsSphere = list(center = cent, radius = r, below = FALSE, plane = planeC, factor = 6))
#' rgl::spheres3d(cent, radius=r, color = "grey100", alpha=0.1)
#' plotPoints3D(pts)
#' rgl::planes3d(planeC[1],planeC[2],planeC[3],planeC[4], alpha = 0.5, col = "red")
#' finalize3D()
#'
#' ini3D()
#' pts <- genSample(3, 10000, argsSphere = list(center = cent, radius = r, below = NULL))
#' Rfast::colMinsMaxs(as.matrix(pts))
#' rgl::spheres3d(cent, radius=r, color = "grey100", alpha=0.1)
#' plotPoints3D(pts)
#' finalize3D()
#'
#' ## Other p
#' p <- 10
#' cent <- rep(0,p)
#' r <- 100
#' pts <- genSample(p, 100000, argsSphere = list(center = cent, radius = r, below = NULL))
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' apply(pts,1, function(x){sqrt(sum((x-cent)^2))}) # test should be approx. equal to radius
#'
#'
#' ### Using box
#' ## p = 2
#' range <- matrix(c(1,100, 50,100), ncol = 2, byrow = TRUE )
#' pts <- genSample(2, 1000, range = range, box = TRUE, argsBox = list(cor = "idxAlt"))
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plot(pts)
#'
#' pts <- genSample(2, 1000, range = range, box = TRUE, argsBox = list(cor = "idxAlt",
#'                  intervals = 6))
#' plot(pts)
#'
#' pts <- genSample(2, 1000, range = range, box = TRUE, argsBox = list(cor = "idxRand"))
#' plot(pts)
#' pts <- genSample(2, 1000, range = range, box = TRUE,
#'                  argsBox = list(cor = "idxRand", prHigh = c(0.1,0.6)))
#' points(pts, pch = 3, col = "red")
#' pts <- genSample(2, 1000, range = range, box = TRUE,
#'                  argsBox = list(cor = "idxRand", prHigh = c(0,0)))
#' points(pts, pch = 4, col = "blue")
#'
#' pts <- genSample(2, 1000, range = range, box = TRUE, argsBox = list(cor = "idxSplit"))
#' plot(pts)
#'
#' ## p = 3
#' range <- matrix(c(1,100, 1,200, 1,50), ncol = 2, byrow = TRUE )
#' ini3D(argsPlot3d = list(box = TRUE, axes = TRUE))
#' pts <- genSample(3, 1000, range = range, box = TRUE, , argsBox = list(cor = "idxAlt"))
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plotPoints3D(pts)
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(box = TRUE, axes = TRUE))
#' pts <- genSample(3, 1000, range = range, box = TRUE, ,
#'                  argsBox = list(cor = "idxAlt", intervals = 6))
#' plotPoints3D(pts)
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(box = TRUE, axes = TRUE))
#' pts <- genSample(3, 1000, range = range, box = TRUE, , argsBox = list(cor = "idxRand"))
#' plotPoints3D(pts)
#' pts <- genSample(3, 1000, range = range, box = TRUE, ,
#'                  argsBox = list(cor = "idxRand", prHigh = c(0.1,0.6,0.1)))
#' plotPoints3D(pts, argsPlot3d = list(col="red"))
#' finalize3D()
#'
#' ini3D(argsPlot3d = list(box = TRUE, axes = TRUE))
#' pts <- genSample(3, 1000, range = range, box = TRUE, , argsBox = list(cor = "idxSplit"))
#' plotPoints3D(pts)
#' finalize3D()
#'
#' ## other p
#' p <- 10
#' range <- c(1,100)
#' pts <- genSample(p, 1000, range = range, box = TRUE, argsBox = list(cor = "idxSplit"))
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
genSample <- function(p, n, range = c(1,100), random = FALSE, sphere = TRUE, box = FALSE, ...) {
   if (!is.matrix(range)) range <- matrix(range, ncol = 2)
   if (nrow(range) == 1) range <-  matrix(rep(range, each=p), nrow=p)
   args <- list(...)

   if (random) {
      sphere = FALSE
      argsRandom <- mergeLists(list(), args$argsRandom)
      set <- apply(range, 1, function(x) sample(x[1]:x[2], n, replace = TRUE) )
   }

   if (sphere) {
      argsSphere <-
         mergeLists(list(
            radius = min(Rfast::rowMins((range[,2] - range[,1])/2, value = TRUE)),
            center = range[,1] + (range[,2]-range[,1])/2,
            plane = NULL,
            below = TRUE,
            factor = 2 + 1/log(n)
         ), args$argsSphere)
      if (is.null(argsSphere$plane))
         argsSphere$plane <- c(argsSphere$center, -sum(argsSphere$center ^ 2))
      if (!is.null(argsSphere$below)) n1 <- floor(n * argsSphere$factor) else n1 <- n # increase so have n points below/above

      # generate points https://math.stackexchange.com/questions/87230/
      #                 picking-random-points-in-the-volume-of-sphere-with-uniform-probability
      pts <- matrix(stats::rnorm(p*n1), nrow = p)  # sample
      pts <- t(apply(pts, 2, function(x){x * argsSphere$radius/sqrt(sum(x^2))}))
      # old method
      # sp <- sphereplot::pointsphere(n,c(0,360),c(-90,90),c(argsSphere$radius,argsSphere$radius))
      # pts <- sphereplot::sph2car(sp, deg = TRUE)

      pts <- pts + matrix(rep(argsSphere$center, dim(pts)[1]), byrow = TRUE, ncol=p)  # shift center
      if (!is.null(argsSphere$below)) {
         set <- matrix(rep(0,p), nrow = 1)
         if(argsSphere$below) {
            for (i in 1:nrow(pts)) { # remove all points below/above the plane
               if (sum(argsSphere$plane[1:p] *  pts[i,]) <= -argsSphere$plane[p+1]) {
                  set <- rbind(set, pts[i,])
               }
               if (nrow(set) == n + 1) break
            }
         }
         if (!argsSphere$below) {
            for (i in 1:nrow(pts)) { # remove all points below/above the plane
               if (sum(argsSphere$plane[1:p] *  pts[i,]) >= -argsSphere$plane[p+1]) {
                  set <- rbind(set, pts[i,])
               }
               if (nrow(set) == n + 1) break
            }
         }
         set <- set[-1,]  # remove first dummy row
      } else set <- pts
      set <- round(set)
   }

   if (box) {
      argsBox <-
         mergeLists(list(
            intervals = 3,
            cor = "idxAlt",
            prHigh = rep(0.5, p)
         ), args$argsBox)
      invLength <- ceiling((range[,2]-range[,1])/argsBox$intervals)
      rngL <- cbind(range[,1], range[,1] + invLength)
      rngH <- cbind(range[,2] - invLength, range[,2])
      ptsH <- apply(rngH, 1, function(x) sample(x[1]:x[2], n, replace = TRUE) )
      ptsL <- apply(rngL, 1, function(x) sample(x[1]:x[2], n, replace = TRUE) )
      ptsL <- split(ptsL, seq(nrow(ptsL)))
      ptsH <- split(ptsH, seq(nrow(ptsH)))
      if (argsBox$cor == "idxAlt") {
         high <- stats::rbinom(n, 1, 0.5)
         high <- lapply(high, function(x) rep(c(x,!x),times = p)[1:p])
      }
      if (argsBox$cor == "idxRand") {
         high <-
            sapply(argsBox$prHigh, function(pr)
               sample(0:1, n, replace = TRUE, prob = c(1 - pr, pr)))
         high <- split(high, seq(nrow(high)))
      }
      if (argsBox$cor == "idxSplit") {
         if (p %% 2 == 0) r <- p/2  # even number
         if (p %% 2 == 1) r <- floor(p/2):ceiling(p/2)
         s <- expand.grid(as.data.frame(matrix(rep(0:1,p), ncol = p)))
         s <- s[apply(s, 1, function(x) sum(x) %in% r),]
         rownames(s) <- NULL
         high <- sample(1:nrow(s), n, replace = TRUE)
         high <- lapply(high, function(i) s[i,])
      }
      lst <- lapply(1:length(ptsL),function(i) list(pts = rbind(high[[i]], ptsL[[i]], ptsH[[i]]) ))
      set <- t(sapply(lst, function(x) apply(x$pts,2, function(x) x[x[1]+2])))
   }
   if (nrow(set) != n) warning("Only ", nrow(set), " samples generated!")
   return(set)
}




#' Generate a sample of nondominated points.
#'
#' @param p Dimension of the points.
#' @param n Number of samples generated.
#' @param range The range of the points in each dimension (a vector or matrix with `p` rows).
#' @param random Random sampling.
#' @param sphere Generate points on a sphere.
#' @param box Generate points in boxes.
#' @param keep Keep dominated points also.
#' @param ... Further arguments passed on to [`genSample`].
#'
#' @return A data frame with `p+1` columns (last one indicate if dominated or not).
#' @export
#'
#' @examples
#' range <- matrix(c(1,100, 50,100, 10,50), ncol = 2, byrow = TRUE )
#' ini3D()
#' pts <- genNDSet(3, 1000, range = range, random = TRUE, keep = TRUE)
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plotPoints3D(pts)
#' plotPoints3D(pts[!pts$dom,], argsPlot3d = list(col = "red", size = 10))
#' finalize3D()
#'
#' ini3D()
#' range <- c(1,100)
#' cent <- rep(range[1] + (range[2]-range[1])/2, 3)
#' pts <- genNDSet(3, 1000, range = range, sphere = TRUE, keep = TRUE,
#'        argsSphere = list(center = cent))
#' rgl::spheres3d(cent, radius=49.5, color = "grey100", alpha=0.1)
#' plotPoints3D(pts)
#' plotPoints3D(pts[!pts$dom,], argsPlot3d = list(col = "red", size = 10))
#' rgl::planes3d(cent[1],cent[2],cent[3],-sum(cent^2), alpha = 0.5, col = "red")
#' finalize3D()
#'
#' ini3D()
#' cent <- c(100,100,100)
#' r <- 75
#' planeC <- c(cent+r/3)
#' planeC <- c(planeC, -sum(planeC^2))
#' pts <- genNDSet(3, 100, keep = TRUE,
#'   argsSphere = list(center = cent, radius = r, below = FALSE, plane = planeC, factor = 6))
#' rgl::spheres3d(cent, radius=r, color = "grey100", alpha=0.1)
#' plotPoints3D(pts)
#' plotPoints3D(pts[!pts$dom,], argsPlot3d = list(col = "red", size = 10))
#' rgl::planes3d(planeC[1],planeC[2],planeC[3],planeC[4], alpha = 0.5, col = "red")
#' finalize3D()
genNDSet <-
   function(p,
            n,
            range = c(1, 100),
            random = FALSE,
            sphere = TRUE,
            box = FALSE,
            keep = FALSE,
            ...) {

   if (p!=3) stop("Currently only works for p = 3!")
   set <- genSample(p, n , range = range, random = random, sphere = sphere, box = box, ...)
   set <- set[order(set[,1],set[,2],set[,3]),]
   set <- as.data.frame(set)
   colnames(set) <- c("x","y","z")
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



#' Classify a set of nondominated points
#'
#' The classification is supported (true/false), extreme (true/false), supported non-extreme
#' (true/false)
#'
#' @param pts A set of non-dominated points. It is assumed that `ncol(pts)` equals the number of
#'   objectives ($p$).
#'
#' @note It is assumed that `pts` are nondominated.
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
#' pts <- genNDSet(3,50)
#' ini3D(argsPlot3d = list(xlim = c(0,max(pts$x)+2),
#'   ylim = c(0,max(pts$y)+2),
#'   zlim = c(0,max(pts$z)+2)))
#' plotHull3D(pts, addR3 = T, argsPolygon3d = list(alpha = 0.5))
#' pts <- classifyNDSet(pts[,1:3])
#' plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
#' plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
#' plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
#' finalize3D()
#'
classifyNDSet <- function(pts, direction = 1) {
   pts <- .checkPts(pts)
   # if (nrow(pts) == 1) return(cbind(pts))

   p <- ncol(pts)
   if (length(direction) != p) direction = rep(direction[1],p)
   d <- dimFace(pts)
   if (d != p) stop("The points including rays don't seem to define a hull of dimension ", p, "!")

   set <- convexHull3D(pts, classify = TRUE, addR3 = TRUE, direction = direction)
   hull <- set$hull
   set <- set$pts


   # set <- addRays(set, direction = direction)

   # ## Find extra dummy vertex
   # z <- dplyr::bind_rows(purrr::map_dfc(pts, min), purrr::map_dfc(pts, max))
   # idx <- purrr::map_dbl(obj, function(x) if (x == "min") 2 else 1)
   # z <- purrr::map_dbl(1:p, function(j) as.numeric(z[idx[j],j]))
   # pts <- rbind(pts,z)


   # hull <- geometry::convhulln(pts)
   # idx <- unique(as.vector(hull))
   # idx <- idx[!is.na(idx)]
   # idx <- sort(idx)
   # set <- as.data.frame(pts)
   set <- dplyr::mutate(set, se = ifelse(vtx,TRUE,FALSE))
   set <- dplyr::mutate(set, sne = FALSE, us = FALSE, id = 1:nrow(set))
   chk <- set %>% dplyr::filter(!vtx)
   if (nrow(chk) != 0) {
      val <- inHull(chk[,1:p], set[set$vtx,1:p])
      set$us[chk$id[which(val == 1)]] <- TRUE
      set$sne[chk$id[which(val == 0)]] <- TRUE
   }
   set <- set %>% dplyr::mutate(cls = if_else(se, "se", if_else(sne, "sne", "us")))
   return(set %>% dplyr::filter(pt == 1) %>% dplyr::select(1:p, c("se", "sne", "us", "cls")))
}