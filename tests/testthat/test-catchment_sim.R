
test_that("catchment_sim creates a data frame of 16 rows", {
  expect_equal(dim(catchment_sim(16, 4.313320, 3.026894, 20)), c(16,6))
})


test_that("catchment_sim creates a data frame of 20 rows", {
  expect_equal(dim(catchment_sim(20, 4.313320, 3.026894, 20)), c(20,6))
})


