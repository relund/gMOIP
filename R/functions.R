## functions for 2D plots for bounded MO-MILP problems in criterion and solution space.


#' Integer points inside the feasible region (Ax<=b).
#'
#' @param A A matrix.
#' @param b Right hand side.
#'
#' @return A data frame with all integer points inside the feasible region (columns x1, x2, lbl).
#' @note Do a simple enumeration of all integer points between min and max.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
integerPoints<-function(A, b) {
   cPoints <- cornerPoints(A, b)
   eps <- rep(0.00001,length(b))
   maxX1<-floor(max(cPoints$x1))
   maxX2<-floor(max(cPoints$x2))
   minX1 <- ceiling(min(cPoints$x1))
   minX2 <- ceiling(min(cPoints$x2))
   const <- rep('<=',dim(A)[1])
   ans<-matrix(0, 0, 2)
   for (x1 in minX1:maxX1) {
      for (x2 in minX2:maxX2) {
         x <- c(x1,x2)
         a <- A %*% x
         expr <- paste(a,const,b,"+ eps")
         if (all(sapply(expr, function(x) eval(parse(text=x))))) ans <- rbind(ans,c(x1,x2))
      }
   }
   ans <- as.data.frame(ans)
   colnames(ans)<-c("x1","x2")
   ans$lbl<-1:length(ans$x1)
   return(ans)
}




#' Calculate the corner points for the polytope Ax<=b, x>=0 (used some code from the \code{intpoint} package)
#'
#' @param A A matrix.
#' @param b Right hand side.
#'
#' @return A data frame with a corner point in each row.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
cornerPoints<-function (A, b) {
   m <- nrow(A)
   R <- matrix(0, 1, 2)
   # corner points not on axis
   if (m > 1) {
      for (i in 1:(m - 1)) {
         for (j in (i + 1):m) {
            B <- rbind(A[i, ], A[j, ])
            bb <- c(b[i], b[j])
            if ((abs(det(B)) > 1e-07)) {
               x <- solve(B, bb)
               if ((x[1] >= 0) & (x[2] >= 0)) {
                  b.prima <- A %*% x
                  y <- b - b.prima
                  count <- 0
                  for (s in 1:length(y)) {
                     if (y[s] >= 0)
                        count <- count + 1
                     else {
                        if (abs(y[s]) < 1e-08)
                           count <- count + 1
                        else count <- count
                     }
                  }
                  if (count == length(y)) {
                     R <- rbind(R, x)
                  }
               }
            }
         }
      }
   }
   # corner point between axis
   x <- c(0, 0)
   b.prima <- A %*% x
   y <- b - b.prima
   count <- 0
   for (s in 1:length(y)) {
      if (y[s] >= 0)
         count <- count + 1
      else {
         if (abs(y[s]) < 1e-08)
            count <- count + 1
         else count <- count
      }
   }
   if (count == length(y)) {
      R <- rbind(R, x)
   }
   # corner points on axis
   for (i in 1:m) {
      if (A[i, 2] != 0) {
         x <- c(0, b[i]/A[i, 2])
         if ((x[1] >= 0) & (x[2] >= 0)) {
            b.prima <- A %*% x
            y <- b - b.prima
            count <- 0
            for (s in 1:length(y)) {
               if (y[s] >= 0)
                  count <- count + 1
               else {
                  if (abs(y[s]) < 1e-08)
                     count <- count + 1
                  else count <- count
               }
            }
            if (count == length(y)) {
               R <- rbind(R, x)
            }
         }
      }
      if (A[i, 1] != 0) {
         x <- c(b[i]/A[i, 1], 0)
         if ((x[1] >= 0) & (x[2] >= 0)) {
            b.prima <- A %*% x
            y <- b - b.prima
            count <- 0
            for (s in 1:length(y)) {
               if (y[s] >= 0)
                  count <- count + 1
               else {
                  if (abs(y[s]) < 1e-08)
                     count <- count + 1
                  else count <- count
               }
            }
            if (count == length(y)) {
               R <- rbind(R, x)
            }
         }
      }
   }
   R<-as.data.frame(R)
   R<-R[-1,]
   rownames(R)<-NULL
   colnames(R)<-c("x1","x2")
   # sort points so can be used for plots
   R$slope <- R$x2/R$x1
   R<-R[order(-R$slope),]
   R$slope<-NULL
   return(R)
}



#' Calculate the criterion points of a set of points and find the set of non-dominated points
#' (pareto points) and classify them into extreme supported, non-extreme supported, non-supported.
#'
#' @param points A data frame with columns x1 and x2.
#' @param c1 2D vector for first criterion.
#' @param c2 2D vector for second criterion.
#' @param crit Either max or min.
#'
#' @return A data frame with columns x1, x2, z1 = c1*x, z2 = c2*x, z = z1 + z2, lbl = label, nD =
#'   non-dominated, ext = extreme, nonExt = non-extreme supported.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
criterionPoints<-function(points, c1, c2, crit) {
   iP <- points
   iP$z1 <- c1[1]*iP$x1 + c1[2]*iP$x2
   iP$z2 <- c2[1]*iP$x1 + c2[2]*iP$x2
   iP$z <- NULL #iP$z1 + iP$z2
   tol <- 1e-4
   if (crit=="max") iP <- iP[order(-iP$z2,-iP$z1),]
   if (crit=="min") iP <- iP[order(iP$z2,iP$z1),]
   iP$lbl <- 1:length(iP$x1)
   # classify non dom
   iP$nD <- FALSE
   iP$nD[1] <- TRUE  # upper left point
   p1 <- iP$z1[1]; p2 <- iP$z2[1]  # current non dom point (due to sorting will p2 always be larger then or equal to current)
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
      return(iP)   # a single extreme
   }
   nD <- iP[idx,]
   ul<-1
   lr<-length(idx)
   while (ul<length(idx)) {
   # for (k in 1:1000) {
      slope <- (nD$z2[lr]-nD$z2[ul])/(nD$z1[lr]-nD$z1[ul])
      nD$val <- nD$z2-slope*nD$z1
      # cat("val:",nD$val[ul],"max:",max(nD$val),"min:",min(nD$val),"\n")
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
   if (length(idx)<2) return(iP)
   for (i in 2:length(idx)) {
      if (iP$ext[i-1] & abs(iP$z2[i-1]-iP$z2[i])<tol & abs(iP$z1[i-1]-iP$z1[i])<tol) {
         iP$ext[i] = TRUE
         next
      }
      if (iP$nonExt[i-1] & abs(iP$z2[i-1]-iP$z2[i])<tol & abs(iP$z1[i-1]-iP$z1[i])<tol) {
         iP$nonExt[i] = TRUE
      }
   }
   return(iP)
}



#' Create a plot of a polytope (bounded convex set)
#'
#' If an iso profit line added then the max/min is obtained among the \code{points}.
#'
#' @param cPoints Corner points in the polytope.
#' @param points Points to plot (e.g integer points inside the polytope or corner points).
#' @param showLbl Add labels to the points (only if points have a \code{lbl} column).
#' @param iso NULL or if 2D vector add the iso profit line the the solution plot.
#' @param crit Either max or min (only used if add the iso profit line)
#' @param ... Arguments passed to the \link{aes} function in \link{geom_point}.
#'
#' @return The ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
#' @import ggplot2
plotPolytope<-function(cPoints = NULL, points = NULL, showLbl=FALSE, iso=NULL, crit="max", ...)
{
   # Set Custom theme
   myTheme <- theme_set(theme_bw())
   myTheme <- theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      #axis.line = element_blank(),
      axis.line = element_line(colour = "black", size = 0.5,
                               arrow = arrow(length = unit(0.3,"cm")) ),
      #axis.ticks = element_blank()
      #axis.text.x = element_text(margin = margin(r = 30))
      # axis.ticks.length = unit(0.5,"mm"),
      legend.position="none"
   )

   # Create solution plot
   p<- ggplot() + coord_fixed(ratio = 1) + xlab("$x_1$") + ylab("$x_2$")
   #coord_cartesian(xlim = c(-0.1, max(cPoints$x1)+1), ylim = c(-0.1, max(cPoints$x2)+1), expand = F) +
   if (!is.null(cPoints))
      p <- p + geom_polygon(data = cPoints, aes_string(x = 'x1', y = 'x2'), fill="gray90", size = 0.5,
                            linetype = 1, color="black")
   # axes
   # p <- p +
   #    geom_segment(aes(x=0, xend = max(cPoints$x1)+1 , y=0, yend = 0), size=1, arrow = arrow(length = unit(0.3,"cm"))) +
   #    geom_segment(aes(x=0, xend = 0 , y=0, yend = max(cPoints$x2)+1), size=1, arrow = arrow(length = unit(0.3,"cm")))
   # integer points
   if (!is.null(points)) {
      p <- p + geom_point(aes_string(x = 'x1', y = 'x2', ...), data=points)
      if (showLbl & length(points$lbl)>0) {
         nudgeS=-(max(points$x1)-min(points$x1))/100
         if (anyDuplicated(cbind(points$x1,points$x2), MARGIN = 1) > 0)
            p <- p + ggrepel::geom_text_repel(aes_string(x = 'x1', y = 'x2', label = 'lbl'), data=points, size=3,
                                     colour = "gray50")
         if (anyDuplicated(cbind(points$x1,points$x2), MARGIN = 1) == 0)
            p <- p + geom_text(aes_string(x = 'x1', y = 'x2', label = 'lbl'), data=points, nudge_x = nudgeS,
                               nudge_y = nudgeS, hjust=1, size=3, colour = "gray50")
      }
   # add iso profit line
      if (!is.null(iso)) {
         c <- iso
         points$z <- c[1]*points$x1 + c[2]*points$x2
         if (crit=="max") i <- which.max(points$z)
         if (crit=="min") i <- which.min(points$z)
         str <- paste0("$x^* = (", points$x1[i], ",", points$x2[i], ")$")
         if (c[2]!=0) {
            p <- p + geom_abline(intercept = points$z[i]/c[2], slope = -c[1]/c[2], lty="dashed")
         } else {
            p <- p + geom_vline(xintercept = points$x1[i], lty="dashed")
         }
         p <- p + geom_label(aes_string(x = 'x1', y = 'x2', label = 'str'), data = points[i,], nudge_x = 1.5)
      }
   }
   return(p)
}



#' Create a plot of criterion space
#'
#' @param points Data frame with criterion points
#' @param showLbl Add labels to the points.
#' @param addTriangles Add triangles to the non-dominated points
#' @param addHull Add the convex hull of the non-dominated points and rays.
#' @param crit Either min or max. The objective the criterion points are classified as. Note must
#'    be the same as used in \link{criterionPoints}.
#'
#' @return The ggplot2 object.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
#' @example inst/examples/examples.R
plotCriterion<-function(points, showLbl=FALSE, addTriangles = FALSE, addHull = TRUE, crit="max") {
   # Set Custom theme
   myTheme <- theme_set(theme_bw())
   myTheme <- theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.border = element_blank(),
                           #axis.line = element_blank(),
                           axis.line = element_line(colour = "black", size = 0.5, arrow = arrow(length = unit(0.3,"cm")) ),
                           #axis.ticks = element_blank()
                           #axis.text.x = element_text(margin = margin(r = 30))
                           # axis.ticks.length = unit(0.5,"mm"),
                           legend.position="none"
   )

   # Create criterion plot
   p <- ggplot(points, aes_string(x = 'z1', y = 'z2') ) +
      #coord_cartesian(xlim = c(min(points$z1)-delta, max(points$z1)+delta), ylim = c(min(points$z2)-delta, max(points$z2)+delta), expand = F) +
      xlab("$z_1$") +
      ylab("$z_2$")
   if (addHull) {
      tmp<-points[points$ext & !duplicated(cbind(points$z1,points$z2), MARGIN = 1),]
      delta=max( (max(points$z1)-min(points$z1))/10, (max(points$z2)-min(points$z2))/10 )
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
      p <- p + geom_polygon(fill="gray90", data=tmp)
   }
   if (addTriangles) {
      tmp<-points[points$ext,]
      if (length(tmp$z1)>1) { # triangles
         for (r in 1:(dim(tmp)[1] - 1)) {
            p <- p +
               geom_segment(x=tmp$z1[r],y=tmp$z2[r],xend=tmp$z1[r+1],yend=tmp$z2[r+1], colour="gray50") +
               geom_segment(x=tmp$z1[r],y=tmp$z2[r],xend=tmp$z1[r],yend=tmp$z2[r+1], colour="gray50") +
               geom_segment(x=tmp$z1[r],y=tmp$z2[r+1],xend=tmp$z1[r+1],yend=tmp$z2[r+1], colour="gray0")
         }
      }
   }

   p <- p + geom_point(aes_string(colour = 'nD', shape = 'ext')) +
      coord_fixed(ratio = 1) +
      scale_colour_grey(start = 0.6, end = 0)
   nudgeC=-(max(points$z1)-min(points$z1))/100
   if (showLbl & anyDuplicated(cbind(points$z1,points$z2), MARGIN = 1) > 0)
      p <- p + ggrepel::geom_text_repel(aes_string(label = 'lbl'), size=3, colour = "gray50")
   if (showLbl & anyDuplicated(cbind(points$z1,points$z2), MARGIN = 1) == 0)
      p <- p + geom_text(aes_string(label = 'lbl'), nudge_x = nudgeC, nudge_y = nudgeC, hjust=1, size=3,
                         colour = "gray50")
   return(p)
}


