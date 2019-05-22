# Define the LP max/min coeff*x st. Ax<=b, x>=0
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
coeff <- c(7.75, 10)


## LP model
# The polytope with the corner points
plotPolytope(
   A,
   b,
   coeff,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = F,
   labels = NULL
)
# With optimum and labels:
plotPolytope(
   A,
   b,
   coeff,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)
# Minimize:
plotPolytope(
   A,
   b,
   coeff,
   type = rep("c", ncol(A)),
   crit = "min",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = T,
   labels = "n"
)
# Note return a ggplot so can e.g. add other labels on e.g. the axes:
p <- plotPolytope(
   A,
   b,
   coeff,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)
p + xlab("x") + ylab("y")


## LP-model with no non-negativity constraints
A <- matrix(c(-3, 2, 2, 4, 9, 10, 1, -2), ncol = 2, byrow = TRUE)
b <- c(3, 27, 90, 2)
coeff <- c(7.75, 10)
plotPolytope(
   A,
   b,
   coeff,
   type = rep("c", ncol(A)),
   nonneg = rep(F, ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = F,
   labels = NULL
)



## The package don't plot feasible regions that are unbounded e.g if we drop the 2 and 3 constraint
A <- matrix(c(-3,2), ncol = 2, byrow = TRUE)
b <- c(3)
coeff <- c(7.75, 10)


plotPolytope(
   A,
   b,
   coeff,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = F,
   labels = NULL
)

# One solution is to add a bounding box and check if the bounding box is binding
A <- rbind(A, c(1,0), c(0,1))
b <- c(b, 10, 10)

plotPolytope(
   A,
   b,
   coeff,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = F,
   labels = NULL
)






# # Load lpSolve
# require(lpSolve)
# A <- matrix(c(-3,2), ncol = 2, byrow = TRUE)
# b <- c(3)
# coeff <- c(7.75, 10)
#
#
# # Direction of the constraints
# constranints_direction  <- c("<=")
#
# # Find the optimal solution
# optimum <-  lp(direction="max",
#                objective.in = coeff,
#                const.mat = A,
#                const.dir = constranints_direction,
#                const.rhs = b)
#
# # Print status: 0 = success, 2 = no feasible solution, 3 = unbounded (http://lpsolve.sourceforge.net/5.5/solve.htm)
# print(optimum$status)
#
# # Display the optimum values for x_4p, x_3p and x_w
# best_sol <- optimum$solution
# names(best_sol) <- c("x_4p", "x_3p", "x_w")
# print(best_sol)
#
# # Check the value of objective function at optimal point
# print(paste("Total cost: ", optimum$objval, sep=""))
















### ILP model
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
coeff <- c(7.75, 10)
# ILP model with LP faces:
plotPolytope(
   A,
   b,
   coeff,
   type = rep("i", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)
#ILP model with IP faces:
plotPolytope(
   A,
   b,
   coeff,
   type = rep("i", ncol(A)),
   crit = "max",
   faces = rep("i", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)


## MILP model
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
coeff <- c(7.75, 10)
# Second coordinate integer
plotPolytope(
   A,
   b,
   coeff,
   type = c("c", "i"),
   crit = "max",
   faces = c("c", "i"),
   plotFaces = F,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)
# First coordinate integer and with faces:
plotPolytope(
   A,
   b,
   coeff,
   type = c("i", "c"),
   crit = "max",
   faces = c("i", "c"),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)









## MILP model
cPoints<-cornerPoints(A, b)
rngPoints <- ranges(A, b, x1="int", x2="cont")
plotPolytope(cPoints, rangePoints = rngPoints)
plotPolytope(cPoints, points = rngPoints, rangePoints = rngPoints)
plotPolytope(cPoints, rngPoints, rngPoints, iso = coeff, crit = "max", feasible = rngPoints)
plotPolytope(cPoints, rngPoints, rngPoints, iso = c(3,-3), crit = "min", feasible = rngPoints)
cPoints<-cornerPoints(A, b, x1="int", x2="cont")
plotPolytope(cPoints, rangePoints = rngPoints)
plotPolytope(cPoints, points = rngPoints, rangePoints = rngPoints)
plotPolytope(cPoints, rngPoints, rngPoints, iso = coeff, crit = "max", feasible = rngPoints)
plotPolytope(cPoints, rngPoints, rngPoints, iso = c(3,-3), crit = "min", feasible = rngPoints)
rngPoints <- ranges(A, b, x1="cont", x2="int")
cPoints<-cornerPoints(A, b, x1="cont", x2="int")
plotPolytope(cPoints, rngPoints, rngPoints)

### Bi-objective problem: Plot of criterion space given a bi-objective vector
## LP model
cPoints<-cornerPoints(A, b)
zPoints<-criterionPoints(cPoints, c1 = c(coeff[1], -10), c2 = c(-10, coeff[2]), crit = "max")
zPoints<-zPoints[zPoints$ext,]  # remove all nonextreme (since LP model)
plotCriterion(zPoints)

## IP model
iPoints<-integerPoints(A, b)
zPoints<-criterionPoints(iPoints, c1 = c(coeff[1], 0), c2 = c(0, coeff[2]), crit = "max")
head(zPoints)
plotCriterion(zPoints)
plotCriterion(zPoints, addHull = FALSE, addTriangles = TRUE)
# other criteria
zPoints<-criterionPoints(iPoints, c1 = c(coeff[1], -10), c2 = c(-5, coeff[2]), crit = "max")
plotCriterion(zPoints, addTriangles = TRUE)
# mimimize
zPoints<-criterionPoints(zPoints, c1 = c(-1, 1), c2 = c(1, -1), crit = "min")
plotCriterion(zPoints, addHull = TRUE, addTriangles = TRUE, crit = "min")
zPoints<-criterionPoints(iPoints, c1 = c(coeff[1], -10), c2 = c(-5, coeff[2]), crit = "min")
plotCriterion(zPoints, addHull = TRUE, addTriangles = TRUE, crit = "min")
# identify solutions (x1,x2) corresponding to (z1,z2)
plotPolytope(cPoints, zPoints, showLbl = TRUE)
plotCriterion(zPoints, addHull = TRUE, addTriangles = TRUE, crit = "min", showLbl = TRUE)
# other examples (max and min with shapes)
zPoints<-criterionPoints(iPoints, c1 = c(-2, -1), c2 = c(1, 4), crit = "max")
plotPolytope(cPoints, zPoints, showLbl = TRUE, shape = zPoints$nD)
plotCriterion(zPoints, addHull = TRUE, addTriangles = TRUE, crit = "max", showLbl = TRUE)
zPoints<-criterionPoints(iPoints, c1 = c(-2, -1), c2 = c(1, 4), crit = "min")
plotCriterion(zPoints, addHull = TRUE, addTriangles = TRUE, crit = "min", showLbl = TRUE)



grid.arrange(p1, p2, nrow = 1)


## MILP model
# x1 integer
cPoints<-cornerPoints(A, b, x1="int", x2="cont")
rngPoints <- ranges(A, b, x1="int", x2="cont")
plotPolytope(cPoints, points = rngPoints, rangePoints = rngPoints)
zRngPoints<-criterionPoints(rngPoints, c1 = c(coeff[1], 0), c2 = c(0, coeff[2]), crit = "max")
plotCriterion(zRngPoints, rangePoints = zRngPoints, addTriangles = TRUE)
zRngPoints<-criterionPoints(rngPoints, c1 = c(coeff[1], -10), c2 = c(-5, coeff[2]), crit = "max")
plotCriterion(zRngPoints, rangePoints = zRngPoints, addTriangles = TRUE)
zRngPoints<-criterionPoints(rngPoints, c1 = c(coeff[1], -10), c2 = c(-5, coeff[2]), crit = "min")
plotCriterion(zRngPoints, rangePoints = zRngPoints, addTriangles = TRUE, crit = "min")
# x2 integer
cPoints<-cornerPoints(A, b, x1="cont", x2="int")
rngPoints <- ranges(A, b, x1="cont", x2="int")
plotPolytope(cPoints, points = rngPoints, rangePoints = rngPoints)
zRngPoints<-criterionPoints(rngPoints, c1 = c(coeff[1], 0), c2 = c(0, coeff[2]), crit = "max")
plotCriterion(zRngPoints, rangePoints = zRngPoints, addTriangles = TRUE)
zRngPoints<-criterionPoints(rngPoints, c1 = c(coeff[1], -10), c2 = c(-5, coeff[2]), crit = "max")
plotCriterion(zRngPoints, rangePoints = zRngPoints, addTriangles = TRUE)
zRngPoints<-criterionPoints(rngPoints, c1 = c(coeff[1], -10), c2 = c(-5, coeff[2]), crit = "min")
plotCriterion(zRngPoints, rangePoints = zRngPoints, addTriangles = TRUE, crit = "min")

# x2 integer (one plot)
layout<-theme(plot.title = element_text(size = 5, face = "bold"))
cPoints<-cornerPoints(A, b, x1="cont", x2="int")
rngPoints <- ranges(A, b, x1="cont", x2="int")
p1<-plotPolytope(cPoints, points = rngPoints, rangePoints = rngPoints) + ggtitle("Decision space") + layout
zRngPoints<-criterionPoints(rngPoints, c1 = c(coeff[1], 0), c2 = c(0, coeff[2]), crit = "max")
p2<-plotCriterion(zRngPoints, rangePoints = zRngPoints, addTriangles = TRUE) +
   ggtitle(paste0("max c1 = (",coeff[1],",0), c2 = (0,",coeff[2],")")) + layout
zRngPoints<-criterionPoints(rngPoints, c1 = c(coeff[1], -10), c2 = c(-5, coeff[2]), crit = "max")
p3<-plotCriterion(zRngPoints, rangePoints = zRngPoints, addTriangles = TRUE) +
   ggtitle(paste0("max c1 = (",coeff[1],",-10), c2 = (-5,",coeff[2],")")) + layout
zRngPoints<-criterionPoints(rngPoints, c1 = c(coeff[1], -10), c2 = c(-5, coeff[2]), crit = "min")
p4<-plotCriterion(zRngPoints, rangePoints = zRngPoints, addTriangles = TRUE, crit = "min") +
   ggtitle(paste0("min c1 = (",coeff[1],",-10), c2 = (-5,",coeff[2],")")) + layout
library(gridExtra)

grid.arrange(p1, p2, p3, p4, nrow = 2)

}

\dontrun{
# Generate tikz file for LaTeX
library(tikzDevice)
tikz(file = "plot_polytope.tex", standAlone=F, width = 7, height = 6)
plotPolytope(cPoints, zPoints, showLbl = TRUE, latex = TRUE)
dev.off()
}

