
#simulate catchment area
catch_df <- catchment_sim(16, 4.313320, 3.026894, 20)

#simulate elementary schools for each area
elementary_df <- elementary_pop(catch_df, 5.27426341, 0.01427793)

catch_id <- aggregate(elementary_df$schoolPop, FUN = "sum", by = list(elementary_df$catchID))[,1]


test_that("catchment_sim creates a data frame of 16 rows", {
  expect_equal(dim(catch_df), c(16,6))
})

test_that("elementary schools are assigned to catchment areas", {
  expect_equal(catch_df$catchID, catch_id)
})



