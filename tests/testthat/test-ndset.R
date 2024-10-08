testthat::context("Testing NDSet functions ...")

test_that("Correct classification of ND points", {
   pts <- matrix(c(0,0,1, 0,1,0, 1,0,0, 0.5,0.2,0.5, 0.25,0.5,0.25), ncol = 3, byrow = TRUE)
   pts <- classifyNDSet(pts[,1:3])
   expect_equal(pts$cls, c("se","se","se","us","sne"))

   pts <- matrix(c(0,0,1, 0,1,0, 1,0,0, 0.5,0.2,0.5, 0.25,0.5,0.25), ncol = 3, byrow = TRUE)
   pts <- classifyNDSetExtreme(pts[,1:3])
   expect_equal(pts$cls, c("se","se","se", NA_character_, NA_character_))

   pts <- matrix(c(0,0,1, 0,1,0, 1,0,0, 0.2,0.1,0.1, 0.1,0.45,0.45), ncol = 3, byrow = TRUE)
   pts <- classifyNDSet(pts[,1:3], direction = -1)
   expect_equal(pts$cls, c("se","se","se","us","sne"))
})
