# Define the LP max/min coeff*x st. Ax<=b, x>=0
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
obj <- c(7.75, 10)


## LP model
# The polytope with the corner points
plotPolytope(
   A,
   b,
   obj,
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
   obj,
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
   obj,
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
   obj,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)
p + ggplot2::xlab("x") + ggplot2::ylab("y")


## LP-model with no non-negativity constraints
A <- matrix(c(-3, 2, 2, 4, 9, 10, 1, -2), ncol = 2, byrow = TRUE)
b <- c(3, 27, 90, 2)
obj <- c(7.75, 10)
plotPolytope(
   A,
   b,
   obj,
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
obj <- c(7.75, 10)
# Wrong plot
plotPolytope(
   A,
   b,
   obj,
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
   obj,
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
# obj <- c(7.75, 10)
#
#
# # Direction of the constraints
# constranints_direction  <- c("<=")
#
# # Find the optimal solution
# optimum <-  lp(direction="max",
#                objective.in = obj,
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
obj <- c(7.75, 10)
# ILP model with LP faces:
plotPolytope(
   A,
   b,
   obj,
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
   obj,
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
obj <- c(7.75, 10)
# Second coordinate integer
plotPolytope(
   A,
   b,
   obj,
   type = c("c", "i"),
   crit = "max",
   faces = c("c", "i"),
   plotFaces = F,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)
# First coordinate integer and with LP faces:
plotPolytope(
   A,
   b,
   obj,
   type = c("i", "c"),
   crit = "max",
   faces = c("c", "c"),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)
# First coordinate integer and with LP faces:
plotPolytope(
   A,
   b,
   obj,
   type = c("i", "c"),
   crit = "max",
   faces = c("i", "c"),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = T,
   labels = "coord"
)




\dontrun{
   # Generate tikz file for LaTeX
   library(tikzDevice)
   tikz(file = "polytope.tex", standAlone=F, width = 7, height = 6)
   plotPolytope(
      A,
      b,
      obj,
      type = c("i", "c"),
      crit = "max",
      faces = c("i", "c"),
      plotFaces = T,
      plotFeasible = T,
      plotOptimum = T,
      labels = "coord",
      latex = TRUE
   )
   dev.off()
}





### Bi-objective problem: Plot of criterion space given a bi-objective vector
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
library(grid)
library(gridExtra)

# function for plotting solution and criterion space
plotBiObj <- function(A, b, obj,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = T,
   plotFeasible = T,
   plotOptimum = F,
   labels = "numb",
   addTriangles = T,
   addHull = T)
{
   p1 <- plotPolytope(A, b, type = type, crit = crit, faces = faces, plotFaces = plotFaces,
                      plotFeasible = plotFeasible, plotOptimum = plotOptimum, labels = labels)
   p2 <- plotCriterion2D(A, b, obj, type = type, crit = crit, addTriangles = addTriangles,
                         addHull = addHull, plotFeasible = plotFeasible, labels = labels)
   grid.arrange(p1, p2, nrow = 1)
}

## LP model
obj <- matrix(
   c(7, -10, # first criterion
     -10, -10), # second criterion
   nrow = 2)
plotBiObj(A, b, obj, addTriangles = F)

## ILP models with different criteria (maximize)
obj <- matrix(c(7, -10, -10, -10), nrow = 2)
plotBiObj(A, b, obj, type = rep("i", ncol(A)))
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj(A, b, obj, type = rep("i", ncol(A)))
obj <- matrix(c(-7, -1, -5, 5), nrow = 2)
plotBiObj(A, b, obj, type = rep("i", ncol(A)))
obj <- matrix(c(-1, -1, 2, 2), nrow = 2)
plotBiObj(A, b, obj, type = rep("i", ncol(A)))

## ILP models with different criteria (minimize)
obj <- matrix(c(7, -10, -10, -10), nrow = 2)
plotBiObj(A, b, obj, type = rep("i", ncol(A)), crit = "min")
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj(A, b, obj, type = rep("i", ncol(A)), crit = "min")
obj <- matrix(c(-7, -1, -5, 5), nrow = 2)
plotBiObj(A, b, obj, type = rep("i", ncol(A)), crit = "min")
obj <- matrix(c(-1, -1, 2, 2), nrow = 2)
plotBiObj(A, b, obj, type = rep("i", ncol(A)), crit = "min")

## MILP model (x1 integer) with different criteria (maximize)
obj <- matrix(c(7, -10, -10, -10), nrow = 2)
plotBiObj(A, b, obj, type = c("i", "c"))
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj(A, b, obj, type = c("i", "c"))
obj <- matrix(c(-7, -1, -5, 5), nrow = 2)
plotBiObj(A, b, obj, type = c("i", "c"))
obj <- matrix(c(-1, -1, 2, 2), nrow = 2)
plotBiObj(A, b, obj, type = c("i", "c"))

## MILP model (x2 integer) with different criteria (minimize)
obj <- matrix(c(7, -10, -10, -10), nrow = 2)
plotBiObj(A, b, obj, type = c("c", "i"), crit = "min")
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj(A, b, obj, type = c("c", "i"), crit = "min")
obj <- matrix(c(-7, -1, -5, 5), nrow = 2)
plotBiObj(A, b, obj, type = c("c", "i"), crit = "min")
obj <- matrix(c(-1, -1, 2, 2), nrow = 2)
plotBiObj(A, b, obj, type = c("c", "i"), crit = "min")







### 3D examples
saveView <- function(fname = "view.RData") {
   if (!file.exists(fname)) {
      view <- rgl::par3d()$userMatrix
      save(view, file = fname)
      message(paste0("RGL view saved to RData file ", fname, "."))
   }
}
loadView <- function(fname = "view.RData", v = NULL) {
   if (!is.null(v)) {
      rgl::view3d(userMatrix = v)
   } else {
      if (file.exists(fname)) {
         load(fname)
         rgl::view3d(userMatrix = view)
      } else {
         warning(paste0("Can't load view in file ", fname, "!"))
      }
   }
}
 printView <- function() {
    view <- par3d()$userMatrix
    cat(paste0("view <- matrix( c(", paste0(view, collapse = ", "), "), nc = 4)"))
 }

# Ex 1
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087, -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405, 0.231208890676498, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)
A <- matrix( c(
3, 2, 5,
2, 1, 1,
1, 1, 3,
5, 2, 4
), nc = 3, byrow = TRUE)
b <- c(55, 26, 30, 57)
obj <- c(20, 10, 15)
# LP model
plotPolytope(A, b, plotOptimum = T, obj = obj, labels = "coord")
# ILP model
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","i"), plotOptimum = T, obj = obj)
# MILP model
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","i"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","i"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","c"), plotOptimum = T, obj = obj, plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","c"), plotOptimum = T, obj = obj, plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","c","i"), plotOptimum = T, obj = obj, plotFaces = F)

view <- matrix( c(-0.812462985515594, -0.029454167932272, 0.582268416881561, 0, 0.579295456409454, -0.153386667370796, 0.800555109977722, 0, 0.0657325685024261, 0.987727105617523, 0.14168381690979, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)
A <- matrix( c(
   1, 1, 1,
   3, 0, 1
), nc = 3, byrow = TRUE)
b <- c(10, 24)
obj <- c(20, 10, 15)
plotPolytope(A, b, plotOptimum = T, obj = obj, labels = "coord")
# ILP model
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","i"), plotOptimum = T, obj = obj)
# MILP model
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","i"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","i"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","c"), plotOptimum = T, obj = obj, plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","c"), plotOptimum = T, obj = obj, plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","c","i"), plotOptimum = T, obj = obj, plotFaces = F)

view <- matrix( c(0.976349174976349, -0.202332556247711, 0.0761845782399178, 0, 0.0903248339891434, 0.701892614364624, 0.706531345844269, 0, -0.196427255868912, -0.682940244674683, 0.703568696975708, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)
A <- matrix( c(
   -1, 1, 0,
    1, 4, 0,
    2, 1, 0,
    3, -4, 0,
    0, 0, 4
), nc = 3, byrow = TRUE)
b <- c(5, 45, 27, 24, 10)
obj <- c(5, 45, 15)
plotPolytope(A, b, plotOptimum = T, obj = obj, labels = "coord")
# ILP model
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","i"), plotOptimum = T, obj = obj)
# MILP model
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","i"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","i"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","c"), plotOptimum = T, obj = obj, plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","c"), plotOptimum = T, obj = obj, plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","c","i"), plotOptimum = T, obj = obj, plotFaces = F)

view <- matrix( c(-0.452365815639496, -0.446501553058624, 0.77201122045517, 0, 0.886364221572876, -0.320795893669128, 0.333835482597351, 0, 0.0986008867621422, 0.835299551486969, 0.540881276130676, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)
Ab <- matrix( c(
   1, 1, 2, 5,
   2, -1, 0, 3,
   -1, 2, 1, 3,
   0, -3, 5, 2
   #   0, 1, 0, 4,
   #   1, 0, 0, 4
), nc = 4, byrow = TRUE)
A <- Ab[,1:3]
b <- Ab[,4]
obj = c(1,1,3)
plotPolytope(A, b, plotOptimum = T, obj = obj, labels = "coord")
# ILP model
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","i"), plotOptimum = T, obj = obj)
# MILP model
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","i"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","i"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotOptimum = T, obj = obj)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","c"), plotOptimum = T, obj = obj, plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","c"), plotOptimum = T, obj = obj, plotFaces = F)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","c","i"), plotOptimum = T, obj = obj)



