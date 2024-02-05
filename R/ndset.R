## Functions related to multi-objective optimization


#' Calculate the criterion points of a set of points and ranges to find the set
#' of non-dominated points (Pareto points) and classify them into extreme
#' supported, non-extreme supported, non-supported.
#'
#' @param pts A data frame with a column for each variable in the solution
#'   space (can also be a `rangePoints`).
#' @param obj A p x n matrix(one row for each criterion).
#' @param crit Either `max` or `min`.
#' @param labels If \code{NULL} or "n" don't add any labels (empty string). If
#'   equals `coord`, labels are the solution space coordinates. Otherwise number all
#'   points from one based on the solution space points.
#'
#' @return A data frame with columns `x1, ..., xn, z1, ..., zp, lbl (label), nD
#'   (non-dominated), ext (extreme), nonExt (non-extreme supported)`.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @examples
#' A <- matrix( c(3, -2, 1, 2, 4, -2, -3, 2, 1), nc = 3, byrow = TRUE)
#' b <- c(10,12,3)
#' pts <- integerPoints(A, b)
#' obj <- matrix( c(1,-3,1,-1,1,-1), byrow = TRUE, ncol = 3 )
#' criterionPoints(pts, obj, crit = "max", labels = "numb")
criterionPoints<-function(pts, obj, crit, labels = "coord") {
   n <- ncol(obj)
   p <- nrow(obj)
   pts <- .checkPts(pts, stopUnique = FALSE)
   zVal <- pts %*% t(obj)
   zVal <- round(zVal,10)
   zNames <- paste0("z", 1:p)
   colnames(zVal) <- zNames
   iP <- bind_cols(as.data.frame(pts), as.data.frame(zVal))
   iP$lbl <- as.character(1:nrow(iP))
   zVal <- addNDSet(zVal, crit = crit, keepDom = TRUE, dubND = TRUE)
   iP <- full_join(iP, zVal, by = zNames)

   if (is.null(labels)) {
      iP$lbl <- ""
      return(iP)
   }
   if (labels == "n") iP$lbl <- ""
   if (labels == "coord") iP$lbl <- df2String(iP[,1:n])
   return(iP)


#    iP <- as.data.frame(iP)
#    tol <- 1e-4
#    iP$oldLbl <- 1:length(iP$x1)
#    if (crit=="max") iP <- iP[order(-iP$z2,-iP$z1),]
#    if (crit=="min") iP <- iP[order(iP$z2,iP$z1),]
#    iP$lbl <- 1:length(iP$x1)  # note use in alg!
#    # classify non dom
#    iP$nD <- FALSE
#    iP$nD[1] <- TRUE  # upper left point
#    p1 <- iP$z1[1]; p2 <- iP$z2[1]  # current non dom point (due to sorting will p2 always be larger than or equal to current)
#    for (r in 2:length(iP$x1)) { # current point under consideration
#       if (abs(p2 - iP$z2[r]) < tol &
#           abs(p1 - iP$z1[r]) < tol) {
#          iP$nD[r] <- TRUE
#          p1 <- iP$z1[r]
#          p2 <- iP$z2[r]
#          next
#       }
#       if (crit == "max" &
#           p2 - iP$z2[r] > tol &
#           iP$z1[r] > p1 + tol) {
#          iP$nD[r] <- TRUE
#          p1 <- iP$z1[r]
#          p2 <- iP$z2[r]
#          next
#       }
#       if (crit == "min" &
#           iP$z2[r] - p2 > tol &
#           iP$z1[r] < p1 - tol) {
#          iP$nD[r] <- TRUE
#          p1 <- iP$z1[r]
#          p2 <- iP$z2[r]
#          next
#       }
#    }
#    # classify extreme supported
#    idx <- which(iP$nD & !duplicated(cbind(iP$z1,iP$z2), MARGIN = 1) )  # remove duplicated points
#    iP$ext <- FALSE
#    iP$ext[idx[1]] <- TRUE
#    iP$ext[idx[length(idx)]] <- TRUE
#    if (length(idx)<3) {
#       iP$nonExt <- FALSE
#       #return(iP)   # a single extreme
#    } else {
#       nD <- iP[idx,]
#       ul<-1
#       lr<-length(idx)
#       while (ul<length(idx)) {
#          # for (k in 1:1000) {
#          slope <- (nD$z2[lr]-nD$z2[ul])/(nD$z1[lr]-nD$z1[ul])
#          nD$val <- nD$z2-slope*nD$z1
#          #cat("val:",nD$val[ul],"max:",max(nD$val),"min:",min(nD$val),"\n")
#          if (crit=="max") {
#             i <- which.max(nD$val)
#             if (nD$val[ul]<nD$val[i] - tol) {
#                iP$ext[nD$lbl[i]] <- TRUE
#                lr <- i
#             } else {
#                ul <- lr
#                lr<-length(idx)
#             }
#          }
#          if (crit=="min") {
#             i <- which.min(nD$val)
#             if (nD$val[ul]>nD$val[i] + tol) {
#                iP$ext[nD$lbl[i]] <- TRUE
#                lr <- i
#             } else {
#                ul <- lr
#                lr<-length(idx)
#             }
#          }
#       }
#       # classify nonextreme supported
#       idxExt <- which(iP$ext)
#       iP$nonExt <- FALSE
#       if (length(idxExt)>1) {
#          for (i in 2:length(idxExt)) {
#             slope <- (iP$z2[idxExt[i]]-iP$z2[idxExt[i-1]])/(iP$z1[idxExt[i]]-iP$z1[idxExt[i-1]])
#             nDCand <- iP[idxExt[i-1]:idxExt[i],]
#             nDCand <- nDCand[nDCand$nD & !duplicated(cbind(nDCand$z1,nDCand$z2), MARGIN = 1),]
#             nDCand <- nDCand[c(-1,-length(nDCand$nD)),]
#             if (length(nDCand$nD)==0) next   # no points inbetween
#             for (j in 1:length(nDCand$nD)) {
#                slopeCur = (nDCand$z2[j]-iP$z2[idxExt[i-1]])/(nDCand$z1[j]-iP$z1[idxExt[i-1]])
#                if (abs(slope - slopeCur) < tol) iP$nonExt[nDCand$lbl[j]==iP$lbl] <- TRUE
#             }
#          }
#       }
#       # classify duplicates
#       idx <- which(iP$nD)
#       if (!length(idx)<2) {
#          for (i in 2:length(idx)) {
#             if (iP$ext[i-1] & abs(iP$z2[i-1]-iP$z2[i])<tol & abs(iP$z1[i-1]-iP$z1[i])<tol) {
#                iP$ext[i] = TRUE
#                next
#             }
#             if (iP$nonExt[i-1] & abs(iP$z2[i-1]-iP$z2[i])<tol & abs(iP$z1[i-1]-iP$z1[i])<tol) {
#                iP$nonExt[i] = TRUE
#             }
#          }
#       }
#    }
#    # set correct labels
#    iP$lbl <- iP$oldLbl
#    iP$oldLbl <- NULL
}



#' Add discrete points to a non-dominated set and classify them into extreme supported, non-extreme
#' supported, non-supported.
#'
#' @param pts A data frame with points to add (a column for each objective).
#' @param nDSet A data frame with current non-dominated set (NULL if none yet). Column names of the
#'   p objectives must be `z1, ..., zp`.
#' @param crit A max or min vector. If length one assume all objectives are optimized in the same
#'   direction.
#' @param keepDom Keep dominated points in output.
#' @param dubND Duplicated non-dominated points are classified as non-dominated.
#' @param classify Non-dominated points are classified into supported extreme (`se`), supported
#'   non-extreme (`sne`) and unsupported (`us`).
#'
#' @return A data frame with a column for each objective (`z` columns) and `nd` (non-dominated).
#'   Moreover if `classify` then columns `se`, `sne`, `us` and `cls`.
#'
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#'
#' @examples
#' nDSet <- data.frame(z1=c(12,14,16,18), z2=c(18,16,12,4))
#' pts <- data.frame(z1 = c(18,18,14,15,15), z2=c(2,6,14,14,16))
#' addNDSet(pts, nDSet, crit = "max")
#' addNDSet(pts, nDSet, crit = "max", keepDom = TRUE)
#' addNDSet(pts, nDSet, crit = "min")
#' addNDSet(c(2,2), nDSet, crit = "max")
#' addNDSet(c(2,2), nDSet, crit = "min")
#'
#' \donttest{
#' nDSet <- data.frame(z1=c(12,14,16,18), z2=c(18,16,12,4), z3 = c(1,7,0,6))
#' pts <- data.frame(z1=c(12,14,16,18), z2=c(18,16,12,4), z3 = c(2,2,2,6))
#' crit = c("min", "min", "max")
#' di <- c(1,1,-1)
#' li <- c(-1,20)
#' ini3D(argsPlot3d = list(xlim = li, ylim = li, zlim = li))
#' plotCones3D(nDSet, direction = di, argsPolygon3d = list(color = "green", alpha = 1),
#'             drawPoint = FALSE)
#' plotHull3D(nDSet, addRays = TRUE, direction = di)
#' plotPoints3D(nDSet, argsPlot3d = list(col = "red"), addText = "coord")
#' plotPoints3D(pts, addText = "coord")
#' finalize3D()
#' addNDSet(pts, nDSet, crit, dubND = FALSE)
#' addNDSet(pts, nDSet, crit, dubND = TRUE)
#' addNDSet(pts, nDSet, crit, dubND = TRUE, keepDom = TRUE)
#' addNDSet(pts, nDSet, crit, dubND = TRUE, keepDom = TRUE, classify = FALSE)
#' }
addNDSet<-function(pts, nDSet = NULL, crit = "max", keepDom = FALSE, dubND = FALSE,
                   classify = TRUE)
{
   if (is.data.frame(nDSet))
      nDSet$nd <- NULL; nDSet$se <- NULL; nDSet$sne <- NULL; nDSet$us <- NULL
   if (!is.data.frame(pts)) pts <- as.data.frame(.checkPts(pts, stopUnique = FALSE))
   p <- ncol(pts)
   direction <- .mToDirection(crit, p)
   colnames(pts) <- paste0("z",1:p)
   if (!is.null(nDSet)) {
      if (ncol(pts)!= ncol(nDSet))
         stop("Number of columns (not including classification colunms) must be the same!")
   } else {
      if (nrow(pts) == 1)
         return(mutate(pts, nd = TRUE, se = TRUE, sne = FALSE, us = FALSE, cls = "se"))
      nDSet <- pts[1,]
      pts <- pts[-1,]
   }
   set <- bind_rows(nDSet, pts)
   idx <- eaf::is_nondominated(set, maximise = (direction == -1), keep_weakly = dubND)

   # pf <- dplyr::if_else(direction == 1, "quo(", "quo(desc(")
   # sf <- dplyr::if_else(direction == 1, ")", "))")
   # args <- paste0("list(", paste0(pf, colnames(nDSet), sf, collapse = ", "), ")")
   # args <- eval(parse(text=args))
   # set <- set %>% dplyr::arrange(!!!args)  # sort based on direction, i.e. i<j => j cannot dom i
   # set$nd <- TRUE
   # if (dubND) {
   #    for (i in 1:(nrow(set)-1)) {
   #       for (j in (i+1):nrow(set)) {
   #          if (set$nd[j]) { # must check if i dom j and not eq
   #             if (all(set[i, 1:p]*direction <= set[j, 1:p]*direction)) { # i may dom j
   #                if (!(all(set[i, 1:p]*direction == set[j, 1:p]*direction))) set$nd[j] <- FALSE
   #             }
   #          }
   #       }
   #    }
   # } else {
   #    for (i in 1:(nrow(set)-1)) {
   #       for (j in (i+1):nrow(set)) {
   #          if (set$nd[j]) { # must check if i dom j
   #             if (all(set[i, 1:p]*direction <= set[j, 1:p]*direction)) set$nd[j] <- FALSE
   #          }
   #       }
   #    }
   # }

   set$nd <- FALSE
   set$nd[idx] <- TRUE
   if (!keepDom) set <- set[idx, ]
   if (classify) {
      set1 <- classifyNDSet(set[set$nd,1:p], direction)
      set <- set %>% tibble::rownames_to_column(var = "id")
      ids <- set$id[set$nd]
      set1 <- set1 %>% tibble::add_column(id = set$id[set$nd], .before = 1)
      set <- left_join(x = set, y = set1, by = c("id", colnames(pts)))
      if (keepDom) set <- set %>%
         mutate_if(is.factor, as.character) %>%
         tidyr::replace_na(list(se = FALSE, sne = FALSE, us = TRUE, cls = "d"))
      set <- set %>% select(-id)
   }
   return(set)
}




#' Add 2D discrete points to a non-dominated set and classify them into extreme
#' supported, non-extreme supported, non-supported.
#'
#' @param pts A data frame. It is assumed that z1 and z2 are in the two first columns.
#' @param nDSet A data frame with current non-dominated set (NULL is none yet).
#' @param crit Either max or min.
#' @param keepDom Keep dominated points.
#'
#' @return A data frame with columns z1 and z2, `nD` (non-dominated),
#'         `ext` (extreme), `nonExt` (non-extreme supported).
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @keywords internal
#'
#' @examples
#' nDSet <- data.frame(z1=c(12,14,16,18), z2=c(18,16,12,4))
#' pts <- data.frame(z1 = c(18,18,14,15,15), z2=c(2,6,14,14,16))
#' addNDSet2D(pts, nDSet, crit = "max")
#' addNDSet2D(pts, nDSet, crit = "max", keepDom = TRUE)
#' addNDSet2D(pts, nDSet, crit = "min")
addNDSet2D<-function(pts, nDSet = NULL, crit = "max", keepDom = FALSE) {
   nDSet$nD <- NULL; nDSet$ext <- NULL; nDSet$nonExt <- NULL
   if (!is.null(nDSet))
      if (ncol(pts)!=ncol(nDSet))
         stop("Number of columns minus classification colunms must be the same!")
   iP = pts
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
   idx <- which(iP$nD & !duplicated(cbind(iP$z1,iP$z2), MARGIN = 1) )  # remove duplicated points
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
      # classify duplicates
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
#' @param planes Generate points between two planes.
#' @param sphere Generate points on a sphere.
#' @param box Generate points in boxes.
#' @param ... Further arguments passed on to the method for generating points. This must be done as
#'   lists (see examples). Currently the following arguments are supported:
#'
#'   * `argsPlanes`: A list of arguments for generating points between planes and in the cube
#'      defined by the range:
#'      - `center`: A point between the planes (default `rowMeans(range)`).
#'      - `planeU`: The upper plane (default `c(rep(1, p), -1.2*sum(center))`).
#'      - `planeL`: The lower plane (default `c(rep(1, p), -0.8*sum(center))`).
#'   * `argsSphere`: A list of arguments for generating points on a sphere:
#'      - `radius`: The radius of the sphere.
#'      - `center`: The center of the sphere.
#'      - `plane`: The plane used.
#'      - `below`: Either true (generate points below the plane), false (generate points above the
#'                 plane) or `NULL` (generated on the whole sphere).
#'      - `factor`: If using a plane. Then the factor to multiply `n` with, so generate enough points
#'                  below/above the plane.
#'      - `closeToPlane`: If TRUE only return points close to the plane.
#'   * `argsBox`: A list of arguments for generating points inside boxes:
#'      - `intervals`: Number of intervals to split the length of the range into. That is, each
#'                     range is divided into `intervals` (sub)intervals and only the lowest/highest
#'                     subrange is used.
#'      - `cor`: How to correlate indices. If `'idxAlt'` then alternate the intervals (high/low)
#'               for each dimension. For instance if `p = 3` and the first dimension is in the high
#'               interval range then the second will be in the low interval range and third in the
#'               high interval range again. If `idxRand` then choose the low/high interval range
#'               for each dimension based on `prHigh`. If `idxSplit` then select
#'               `floor(p/2):ceiling(p/2)` dimensions for the high interval range and the other for
#'               the low interval range.
#'      - `prHigh`: Probability for choosing the high interval range in each dimension.
#'
#' @details Note having ranges with different length when using the sphere method, doesn't make
#'   sense. The best option is properly to use a center and radius here. Moreover, as for higher
#'   `p` you may have to use a larger radius than half of the desired interval range.
#'
#' @return A matrix with `p` columns.
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
#' \donttest{
#' ## p = 3
#' range <- matrix(c(1,100, 50,100, 10,50), ncol = 2, byrow = TRUE )
#' ini3D()
#' pts <- genSample(3, 1000, range = range, random = TRUE)
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plotPoints3D(pts)
#' finalize3D()
#' }
#'
#' ## other p
#' p <- 10
#' range <- c(1,100)
#' pts <- genSample(p, 1000, range = range, random = TRUE)
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#'
#'
#' ### Using planes
#' ## p = 2
#' range <- matrix(c(1,100, 50,100), ncol = 2, byrow = TRUE )
#' center <- rowMeans(range)
#' planeU <- c(rep(1, 2), -1.5*sum(rowMeans(range)))
#' planeL <- c(rep(1, 2), -0.7*sum(rowMeans(range)))
#' pts <- genSample(2, 1000, range = range, planes = TRUE,
#'    argsPlanes = list(center = center, planeU = planeU, planeL = planeL))
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' plot(pts)
#'
#' \donttest{
#' ## p = 3
#' range <- matrix(c(1,100, 50,100, 10, 50), ncol = 2, byrow = TRUE )
#' center <- rowMeans(range)
#' planeU <- c(rep(1, 3), -1.2*sum(rowMeans(range)))
#' planeL <- c(rep(1, 3), -0.6*sum(rowMeans(range)))
#' pts <- genSample(3, 1000, range = range, planes = TRUE,
#'    argsPlanes = list(center = center, planeU = planeU, planeL = planeL))
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
#' ini3D(argsPlot3d = list(box = TRUE, axes = TRUE))
#' plotPoints3D(pts)
#' rgl::planes3d(planeL[1], planeL[2], planeL[3], planeL[4], alpha = 0.5)
#' rgl::planes3d(planeU[1], planeU[2], planeU[3], planeU[4], alpha = 0.5)
#' finalize3D()
#' }
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
#' \donttest{
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
#' }
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
#' \donttest{
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
#' }
#'
#' ## other p
#' p <- 10
#' range <- c(1,100)
#' pts <- genSample(p, 1000, range = range, box = TRUE, argsBox = list(cor = "idxSplit"))
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts))
genSample <- function(p, n, range = c(1,100), random = FALSE, sphere = TRUE, planes = FALSE, box = FALSE, ...) {
   if (!is.matrix(range)) range <- matrix(range, ncol = 2)
   if (nrow(range) == 1) range <-  matrix(rep(range, each=p), nrow=p)
   args <- list(...)

   if (random) {
      sphere = FALSE
      argsRandom <- mergeLists(list(), args$argsRandom)
      set <- apply(range, 1, function(x) sample(x[1]:x[2], n, replace = TRUE) )
   }

   if (planes) {
      sphere = FALSE
      argsPlanes <- mergeLists(list(
         center = rowMeans(range),
         planeU = c(rep(1,p), -1.2*sum(rowMeans(range))),
         planeL = c(rep(1,p), -0.8*sum(rowMeans(range)))
      ), args$argsPlanes)

      set <- apply(range, 1, function(x) sample(x[1]:x[2], n, replace = TRUE))
      set <- set[
         set %*% argsPlanes$planeL[1:p] >= -argsPlanes$planeL[p+1] &
            set %*% argsPlanes$planeU[1:p] <= -argsPlanes$planeU[p+1],
         ]
      while (nrow(set) < n) {
         set1 <- apply(range, 1, function(x) sample(x[1]:x[2], n, replace = TRUE))
         set1 <- set1[
            set1 %*% argsPlanes$planeL[1:p] >= -argsPlanes$planeL[p+1] &
               set1 %*% argsPlanes$planeU[1:p] <= -argsPlanes$planeU[p+1], ]
         set <- rbind(set,set1)
      }
      set <- set[1:n,]
   }

   if (sphere) {
      argsSphere <-
         mergeLists(list(
            radius = min(Rfast::rowMins((range[,2] - range[,1])/2, value = TRUE)),
            center = range[,1] + (range[,2]-range[,1])/2,
            plane = NULL,
            below = TRUE,
            factor = 2 + 1/log(n),
            closeToPlane = FALSE
         ), args$argsSphere)
      if (is.null(argsSphere$plane))
         argsSphere$plane <- c(argsSphere$center, -sum(argsSphere$center ^ 2))
      if (!is.null(argsSphere$below)) n1 <- floor(n * argsSphere$factor) else n1 <- n # increase so have n points below/above
      if (!is.null(argsSphere$closeToPlane))
         if (argsSphere$closeToPlane) n1 <- floor(n * n * (2 + 1/n * 1000))

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
      } else if (argsSphere$closeToPlane) {
         set <- matrix(rep(0,p), nrow = 1)
         delta <- argsSphere$radius/25
         for (i in 1:nrow(pts)) { # remove all points not close to the plane
            if (sum(argsSphere$plane[1:p] *  pts[i,]) >= -argsSphere$plane[p+1] - delta &
                sum(argsSphere$plane[1:p] *  pts[i,]) <= -argsSphere$plane[p+1] + delta) {
               set <- rbind(set, pts[i,])
            }
            if (nrow(set) == n + 1) break
         }
         set <- set[-1,]
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
   colnames(set) <- paste0("z", 1:p)
   return(set)
}




#' Generate a sample of nondominated points.
#'
#' @param p Dimension of the points.
#' @param n Number nondominated points generated.
#' @param range The range of the points in each dimension (a vector or matrix with `p` rows).
#' @param random Random sampling.
#' @param planes Generate points between two planes.
#' @param sphere Generate points on a sphere.
#' @param box Generate points in boxes.
#' @param keepDom Keep dominated points also.
#' @param crit Criteria used (a vector of min/max).
#' @param dubND Should duplicated non-dominated points be considered as non-dominated.
#' @param classify Non-dominated points are classified into supported extreme (`se`), supported
#'   non-extreme (`sne`) and unsupported (`us`)
#' @param ... Further arguments passed on to [`genSample`].
#'
#' @return A data frame with `p+1` columns (last one indicate if dominated or not).
#' @export
#'
#' @examples
#' \donttest{
#' ## Random
#' range <- matrix(c(1,100, 50, 100, 10, 50), ncol = 2, byrow = TRUE)
#' pts <- genNDSet(3, 5, range = range, random = TRUE, keepDom = TRUE)
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts[, 1:3]))
#' ini3D(FALSE, argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+10),
#'   ylim = c(min(pts[,2])-2,max(pts[,2])+10),
#'   zlim = c(min(pts[,3])-2,max(pts[,3])+10)))
#' plotPoints3D(pts[,1:3])
#' plotPoints3D(pts[pts$nd,1:3], argsPlot3d = list(col = "red", size = 10))
#' plotCones3D(pts[pts$nd,1:3], argsPolygon3d = list(alpha = 1))
#' finalize3D()
#'
#'
#' ## Between planes
#' range <- matrix(c(1,10000, 1,10000), ncol = 2, byrow = TRUE)
#' pts <- genNDSet(2, 50, range = range, planes = TRUE, classify = TRUE)
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts[, 1:2]))
#' plot(pts[, 1:2])
#'
#' range <- matrix(c(1,100, 50,100, 10, 50), ncol = 2, byrow = TRUE)
#' center <- rowMeans(range)
#' planeU <- c(rep(1, 3), -1.2*sum(rowMeans(range)))
#' planeL <- c(rep(1, 3), -0.8*sum(rowMeans(range)))
#' pts <- genNDSet(3, 50, range = range, planes = TRUE, keepDom = TRUE, classify = TRUE,
#'    argsPlanes = list(center = center, planeU = planeU, planeL = planeL))
#' head(pts)
#' Rfast::colMinsMaxs(as.matrix(pts[, 1:3]))
#' ini3D(FALSE, argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+10),
#'   ylim = c(min(pts[,2])-2,max(pts[,2])+10),
#'   zlim = c(min(pts[,3])-2,max(pts[,3])+10),
#'   box = TRUE, axes = TRUE))
#' plotPoints3D(pts[,1:3])
#' plotPoints3D(pts[pts$nd,1:3], argsPlot3d = list(col = "red", size = 10))
#' rgl::planes3d(planeL[1], planeL[2], planeL[3], planeL[4], alpha = 0.5)
#' rgl::planes3d(planeU[1], planeU[2], planeU[3], planeU[4], alpha = 0.5)
#' finalize3D()
#'
#'
#' ## On a sphere
#' ini3D()
#' range <- c(1,100)
#' cent <- rep(range[1] + (range[2]-range[1])/2, 3)
#' pts <- genNDSet(3, 20, range = range, sphere = TRUE, keepDom = TRUE,
#'        argsSphere = list(center = cent))
#' rgl::spheres3d(cent, radius=49.5, color = "grey100", alpha=0.1)
#' plotPoints3D(pts)
#' plotPoints3D(pts[pts$nd,], argsPlot3d = list(col = "red", size = 10))
#' rgl::planes3d(cent[1],cent[2],cent[3],-sum(cent^2), alpha = 0.5, col = "red")
#' finalize3D()
#'
#' ini3D()
#' cent <- c(100,100,100)
#' r <- 75
#' planeC <- c(cent+r/3)
#' planeC <- c(planeC, -sum(planeC^2))
#' pts <- genNDSet(3, 20, keepDom = TRUE,
#'   argsSphere = list(center = cent, radius = r, below = FALSE, plane = planeC, factor = 6))
#' rgl::spheres3d(cent, radius=r, color = "grey100", alpha=0.1)
#' plotPoints3D(pts)
#' plotPoints3D(pts[pts$nd,], argsPlot3d = list(col = "red", size = 10))
#' rgl::planes3d(planeC[1],planeC[2],planeC[3],planeC[4], alpha = 0.5, col = "red")
#' finalize3D()
#' }
#' @importFrom rlang .data
genNDSet <-
   function(p,
            n,
            range = c(1, 100),
            random = FALSE,
            sphere = TRUE,
            planes = FALSE,
            box = FALSE,
            keepDom = FALSE,
            crit = "min",
            dubND = FALSE,
            classify = FALSE,
            ...) {
   set <- genSample(p, 2 * n, range = range, random = random, planes = planes, sphere = sphere, box = box, ...)
   nDSet <- addNDSet(set, crit = crit, keepDom = keepDom, dubND = dubND, classify = FALSE)
   ctr <- 0
   while (sum(nDSet$nd) < n) {
      before <- sum(nDSet$nd)
      set <- genSample(p, 2 * n, range = range, random = random, planes = planes, sphere = sphere, box = box, ...)
      nDSet <- addNDSet(set, nDSet, crit = crit, keepDom = keepDom, dubND = dubND, classify = FALSE)
      now <- sum(nDSet$nd)
      if (before >= now) ctr <- ctr + 1
      if (ctr == 10) {
         warning("Tried to generate ", n, " ND points. However did only find ", now, "!")
         break
      }
   }
   if (keepDom) {
      datD <- nDSet %>%
         filter(!.data$nd)
      datND <- nDSet %>%
         filter(.data$nd) %>%
         slice_sample(n = n)
      nDSet <- bind_rows(datD, datND) %>%
         tibble::remove_rownames()
   } else {
      nDSet <- nDSet %>%
         slice_sample(n = n) %>%
         tibble::remove_rownames()
   }
   if (classify) nDSet <- addNDSet(nDSet[, 1:p], crit = crit, keepDom = keepDom, dubND = dubND, classify = classify)
   return(nDSet)
}



#' Classify a set of nondominated points
#'
#' The classification is supported (true/false), extreme (true/false), supported non-extreme
#' (true/false)
#'
#' @param pts A set of non-dominated points. It is assumed that `ncol(pts)` equals the number of
#'   objectives ($p$).
#' @param direction Ray direction. If i'th entry is positive, consider the i'th column of the `pts`
#'   plus a value greater than on equal zero (minimize objective $i$). If negative, consider the
#'   i'th column of the `pts` minus a value greater than on equal zero (maximize objective $i$).
#'
#' @note It is assumed that `pts` are nondominated.
#'
#' @return The ND set with classification columns.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \donttest{
#' pts <- matrix(c(0,0,1, 0,1,0, 1,0,0, 0.5,0.2,0.5, 0.25,0.5,0.25), ncol = 3, byrow = TRUE)
#' ini3D(argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+2),
#'   ylim = c(min(pts[,2])-2,max(pts[,2])+2),
#'   zlim = c(min(pts[,3])-2,max(pts[,3])+2)))
#' plotHull3D(pts, addRays = TRUE, argsPolygon3d = list(alpha = 0.5), useRGLBBox = TRUE)
#' pts <- classifyNDSet(pts[,1:3])
#' plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
#' plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
#' plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
#' plotCones3D(pts[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1))
#' finalize3D()
#' pts
#'
#' pts <- matrix(c(0,0,1, 0,1,0, 1,0,0, 0.2,0.1,0.1, 0.1,0.45,0.45), ncol = 3, byrow = TRUE)
#' di <- -1 # maximize
#' ini3D(argsPlot3d = list(xlim = c(min(pts[,1])-1,max(pts[,1])+1),
#'   ylim = c(min(pts[,2])-1,max(pts[,2])+1),
#'   zlim = c(min(pts[,3])-1,max(pts[,3])+1)))
#' plotHull3D(pts, addRays = TRUE, argsPolygon3d = list(alpha = 0.5), direction = di,
#'            addText = "coord")
#' pts <- classifyNDSet(pts[,1:3], direction = di)
#' plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
#' plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
#' plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
#' plotCones3D(pts[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1), direction = di)
#' finalize3D()
#' pts
#'
#' pts <- matrix(c(0,0,1, 0,0,1, 0,1,0, 0.5,0.2,0.5, 1,0,0, 0.5,0.2,0.5, 0.25,0.5,0.25), ncol = 3,
#'               byrow = TRUE)
#' classifyNDSet(pts)
#'
#' pts <- genNDSet(3,25)[,1:3]
#' ini3D(argsPlot3d = list(xlim = c(0,max(pts$z1)+2),
#'   ylim = c(0,max(pts$z2)+2),
#'   zlim = c(0,max(pts$z3)+2)))
#' plotHull3D(pts, addRays = TRUE, argsPolygon3d = list(alpha = 0.5))
#' pts <- classifyNDSet(pts[,1:3])
#' plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
#' plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
#' plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
#' finalize3D()
#' pts
#' }
classifyNDSet <- function(pts, direction = 1) {
   pts <- .checkPts(pts, stopUnique = FALSE)
   p <- ncol(pts)
   colnames(pts) <- paste0("z", 1:p)
   idx <- duplicated(pts)
   pts <- pts %>% dplyr::as_tibble() %>% dplyr::mutate(id = 1:nrow(pts)) #%>%  tibble::rownames_to_column(var = "rn")
   if (nrow(pts) == 1) {
      pts <- as.data.frame(pts)
      return(cbind(select(pts,-id), se = TRUE, sne = FALSE, us = FALSE, cls = "se"))
   }
   if (length(direction) != p) direction = rep(direction[1],p)

   # find hull of the unique points and classify
   set <- convexHull(pts[!idx,1:p], addRays = TRUE, direction = direction)
   hull <- set$hull
   set <- set$pts
   d <- dimFace(set[,1:p])
   if (d != p) stop("The points including rays don't seem to define a hull of dimension ", p, "!")
   set <- dplyr::mutate(set, se = dplyr::if_else(.data$vtx,TRUE,FALSE))
   set <- dplyr::mutate(set, sne = FALSE, us = FALSE, id = 1:nrow(set))
   chk <- set %>% dplyr::filter(!.data$vtx)
   if (nrow(chk) != 0) {
      val <- inHull(chk[,1:p], set[set$vtx,1:p])
      set$us[chk$id[which(val == 1)]] <- TRUE
      set$sne[chk$id[which(val == 0)]] <- TRUE
   }
   set <- set %>% # tidy and add old id
      dplyr::filter(.data$pt == 1) %>%
      dplyr::mutate(cls = dplyr::if_else(.data$se, "se", dplyr::if_else(.data$sne, "sne", "us"))) %>%
      dplyr::select(tidyselect::all_of(1:p), c("se", "sne", "us", "cls")) %>%
      dplyr::mutate(id = which(!idx))
   set1 <- set %>% left_join(x = set, y = pts[idx,], by = paste0("z", 1:p)) # match id of duplicates
   set1 <- set1 %>%
      dplyr::filter(!is.na(.data$id.y)) %>%
      dplyr::mutate(id.x = .data$id.y) %>% dplyr::select("z1":"id.x")
   set <- dplyr::bind_rows(set, pts[idx,]) %>% dplyr::arrange(id) %>% dplyr::select(-id)
   if (nrow(set1) > 0) for (i in 1:nrow(set1)) set[set1$id.x[i],] <- set1[i, 1:(p+4)]
   return(set)
}