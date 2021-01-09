testthat::context("Testing convex hull functions ...")
library(dplyr)

test_that("Hull in 1D", {
   pts <- matrix(c(1,2,3), ncol = 1, byrow = TRUE)
   d <- dimFace(pts) # a line
   expect_equal(d, 1)

   hull <- convexHull(pts)
   expect_equal(hull$hull, matrix(c(1,3), nrow=1))

   hull <- convexHull(pts, addRays = TRUE)
   set <- hull$pts %>% mutate(id = 1:nrow(hull$pts)) %>% slice(hull$hull) %>% filter(pt == 1)
   expect_equal(set$id, c(1))

   hull <- convexHull(pts, addRays = TRUE, direction = -1)
   set <- hull$pts %>% mutate(id = 1:nrow(hull$pts)) %>% slice(hull$hull) %>% filter(pt == 1)
   expect_equal(set$id, c(3))
})

test_that("Hull in 2D", {
   pts<-matrix(c(2,2), ncol = 2, byrow = TRUE)
   d <- dimFace(pts)
   expect_equal(d, 0)

   pts<-matrix(c(1,1, 2,2), ncol = 2, byrow = TRUE)
   d <- dimFace(pts) # a line
   expect_equal(d, 1)

   hull <- convexHull(pts)
   expect_equal(hull$hull, matrix(c(1,2), nrow=1))

   hull <- convexHull(pts, addRays = TRUE)
   set <- hull$pts %>% mutate(id = 1:nrow(hull$pts)) %>% slice(hull$hull) %>% filter(pt == 1)
   expect_equal(set$id, c(1))

   hull <- convexHull(pts, addRays = TRUE, direction = -1)
   set <- hull$pts %>% mutate(id = 1:nrow(hull$pts)) %>% slice(hull$hull) %>% filter(pt == 1)
   expect_equal(set$id, c(2))

   pts<-matrix(c(1,1, 2,2, 0,1), ncol = 2, byrow = TRUE)
   d <- dimFace(pts) # a polygon
   expect_equal(d, 2)

   hull <- convexHull(pts)
   expect_equal(hull$hull, matrix(c(1,3,2), nrow=1))

   hull <- convexHull(pts, addRays = TRUE)
   set <- hull$pts %>% mutate(id = 1:nrow(hull$pts)) %>% slice(hull$hull) %>% filter(pt == 1)
   expect_equal(set$id, c(3))
})

test_that("Hull in 3D", {
   pts<-matrix(c(1,1,1), ncol = 3, byrow = TRUE)
   d <- dimFace(pts) # a point
   expect_equal(d, 0)

   hull <- convexHull(pts)
   expect_equal(hull$hull, matrix(c(1), nrow=1))

   pts<-matrix(c(0,0,0,1,1,1,2,2,2,3,3,3), ncol = 3, byrow = TRUE)
   d <- dimFace(pts) # a line
   expect_equal(d, 1)

   hull <- convexHull(pts)
   expect_equal(hull$hull, matrix(c(1,4), nrow=1))

   pts<-matrix(c(0,0,0,0,1,1,0,2,2,0,0,2), ncol = 3, byrow = TRUE)
   d <- dimFace(pts) # a polygon
   expect_equal(d, 2)

   hull <- convexHull(pts)
   expect_equal(hull$hull, matrix(c(1,4,3), nrow=1))

   hull <- convexHull(pts, addRays = TRUE)
   set <- hull$pts %>% mutate(id = 1:nrow(hull$pts)) %>% slice(unique(as.vector(hull$hull))) %>% filter(pt == 1)
   expect_equal(set$id, c(1))


   pts<-matrix(c(1,1,1,2,2,1,2,1,1,1,1,2), ncol = 3, byrow = TRUE)
   hull <- convexHull(pts)
   expect_equal(matrix(hull$hull, ncol=3), matrix(c(3,2,1, 4,2,1, 4,3,1, 4,3,2), ncol=3, byrow = TRUE))

   hull <- convexHull(pts, addRays = TRUE)
   set <- hull$pts %>% mutate(id = 1:nrow(hull$pts)) %>% slice(unique(as.vector(hull$hull))) %>% filter(pt == 1)
   expect_equal(set$id, c(1))
})


test_that("Hull in 5D", {
   pts<-matrix(c(1,1,1,2,3), ncol = 5, byrow = TRUE)
   d <- dimFace(pts) # a point
   expect_equal(d, 0)

   hull <- convexHull(pts)
   expect_equal(hull$hull, matrix(c(1), nrow=1))

   pts<-matrix(c(0,0,0,0,0, 0,0,0,0,1, 0,0,0,1,0, 0,0,1,0,0, 0,1,0,0,0, 1,0,0,0,0), ncol = 5, byrow = TRUE)
   d <- dimFace(pts)
   expect_equal(d, 5)

   hull <- convexHull(pts)
   expect_equal(matrix(hull$hull, ncol=5), matrix(c(3,4,5,6,1, 2,4,5,6,1, 2,3,5,6,1, 2,3,4,6,1, 2,3,4,5,1, 2,3,4,5,6), ncol=5, byrow = TRUE))

   ## May give different results on different machines
   # pts<-matrix(c(0,0,0,0,1, 0,0,0,1,0, 0,0,1,0,0, 0,1,0,0,0, 1,0,0,0,0), ncol = 5, byrow = TRUE)
   # d <- dimFace(pts)
   # expect_equal(d, 4)
   # expect_warning(hull <- convexHull(pts))
   # expect_equal(matrix(hull$hull, ncol=4), matrix(c(2,5,1,3, 4,5,1,3, 4,2,1,3, 4,2,5,3, 4,2,5,1), ncol=4, byrow = TRUE))
})

test_that("In hull (1D)", {
   vertices <- matrix(4, ncol = 1)
   pt <- matrix(c(2,4), ncol = 1, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(-1,0))

   vertices <- matrix(c(1,4), ncol = 1)
   pt <- matrix(c(1,3,4,5), ncol = 1, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,1,0,-1))

   vertices <- matrix(4, ncol = 1)
   pt <- matrix(c(1,3.99,4,4.01), ncol = 1, byrow = TRUE)
   res <- inHull(pt, vertices, tol = 0.011)
   expect_equal(res, c(-1,0,0,0))
})

test_that("In hull (2D)", {
   vertices <- matrix(c(2,4), ncol = 2) # a pt
   pt <- matrix(c(2,4, 1,1), ncol = 2, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,-1))

   vertices <- matrix(c(0,0, 3,3), ncol = 2, byrow = TRUE) # a line
   pt <- matrix(c(0,0, 1,1, 2,2, 3,3, 4,4), ncol = 2, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,0,0,0,-1))

   vertices <- matrix(c(1,1, 1,2, 1,3), ncol = 2, byrow = TRUE) # vert. line
   pt <- matrix(c(1,2, 1,3, 1,4), ncol = 2, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,0,-1))

   vertices <- matrix(c(1,1, 2,1, 3,1), ncol = 2, byrow = TRUE) # horiz. line
   pt <- matrix(c(2,1, 3,1, 4,1), ncol = 2, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,0,-1))

   vertices <- matrix(c(0,0, 0,3, 3,0), ncol = 2, byrow = TRUE) # a polygon
   pt <- matrix(c(0,0, 1,1, 4,4), ncol = 2, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,1,-1))

   vertices <- matrix(c(3,3, 8,8, 9,9), ncol = 2, byrow = TRUE)
   pts <- matrix(c(3,3.1, 4,4.001, 9,9.01, 5,5), ncol = 2, byrow = TRUE)
   res <- inHull(pts, vertices, tol = 0.015)
   expect_equal(res, c(-1,0,0,0))

   vertices <- matrix(c(3,3, 8,8, 9,9), ncol = 2, byrow = TRUE)
   pts <- matrix(c(3,3.1, 4,5, 9,8, 5,5), ncol = 2, byrow = TRUE)
   res <- inHull(pts, vertices, tol = 10)
   expect_equal(res, c(0,0,0,0))
})

test_that("In hull (3D)", {
   vertices <- matrix(c(2,2,2), ncol = 3, byrow = TRUE) # a point
   pt <- matrix(c(1,1,1, 3,3,3, 2,2,2, 3,3,2), ncol = 3, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(-1,-1,0,-1))

   vertices <- matrix(c(2,2,2, 4,4,4), ncol = 3, byrow = TRUE) # a line
   pt <- matrix(c(1,1,1, 2,2,2, 3,3,3, 4,4,4, 3,3,2), ncol = 3, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(-1,0,0,0,-1))

   vertices <- matrix(c(1,1,5, 1,2,5, 1,3,5), ncol = 3, byrow = TRUE) # vert. line
   pt <- matrix(c(1,2,5, 1,3,5, 1,4,5, 1,7,7), ncol = 3, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,0,-1,-1))

   vertices <- matrix(c(1,1,6, 2,1,6, 3,1,6), ncol = 3, byrow = TRUE) # horiz. line
   pt <- matrix(c(2,1,6, 3,1,6, 4,1,6), ncol = 3, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,0,-1))

   vertices <- matrix(c(1,1,6, 2,2,6, 4,4,6), ncol = 3, byrow = TRUE) # line
   pt <- matrix(c(2,2,6, 1,1,6, 3,3,6, 2,1,2, 1,2,6), ncol = 3, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,0,0,-1,-1))

   vertices <- matrix(c(1,0,0, 1,1,0, 1,0,1), ncol = 3, byrow = TRUE)
   pt <- matrix(c(1,0.1,0.2, 3,3,2), ncol = 3, byrow = TRUE)
   expect_warning(res <- inHull(pt, vertices))
   expect_equal(res, c(0,-1))

   vertices <- matrix(c(2,2,2, 2,4,4, 2,2,4, 4,4,2, 4,2,2, 2,4,2, 4,2,4, 4,4,4), ncol = 3,
                      byrow = TRUE)
   pt <- matrix(c(1,1,1, 3,3,3, 2,2,2, 3,3,2), ncol = 3, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(-1,1,0,0))
})

test_that("In hull (5D)", {
   vertices <- matrix(c(1,1,1,6,7, 2,2,2,6,7, 5,5,5,6,7), ncol = 5, byrow = TRUE) # line
   pt <- matrix(c(1,1,1,6,7, 2,2,2,6,7, 3,3,3,6,7, 3,3,2,6,7), ncol = 5, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(0,0,0,-1))

   vertices <- matrix(c(4,0,0,0,0, 0,4,0,0,0, 0,0,4,0,0, 0,0,0,4,0, 0,0,0,0,4, 0,0,0,0,0),
                      ncol = 5, byrow = TRUE)
   pt <- matrix(c(0.1,0.1,0.1,0.1,0.1, 3,3,3,3,3, 2,0,0,0,0), ncol = 5, byrow = TRUE)
   res <- inHull(pt, vertices)
   expect_equal(res, c(1,-1,0))
})


