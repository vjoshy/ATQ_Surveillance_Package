# tests/testthat/test-elementary_pop.R

test_that("elementary_pop generates correct output structure", {
  set.seed(123)
  catch_df <- catchment_sim(16, 20, shape = 3.5, rate = 2.8)
  elem_df <- elementary_pop(catch_df, shape = 5.1, rate = 0.015)

  expect_equal(ncol(elem_df), 7)
  expect_equal(names(elem_df), c("catchID", "schoolID", "schoolPop", "xStart", "xEnd", "yStart", "yEnd"))
  expect_true(all(elem_df$schoolPop > 0))
  expect_equal(nrow(elem_df), sum(catch_df$num.schools))
  expect_true(all(elem_df$catchID %in% catch_df$catchID))
})

test_that("elementary_pop works with different distribution functions", {
  set.seed(123)
  catch_df <- catchment_sim(16, 20, shape = 3.5, rate = 2.8)

  elem_df_norm <- elementary_pop(catch_df, dist_func = stats::rnorm, mean = 300, sd = 50)
  elem_df_pois <- elementary_pop(catch_df, dist_func = stats::rpois, lambda = 250)

  expect_equal(nrow(elem_df_norm), sum(catch_df$num.schools))
  expect_equal(nrow(elem_df_pois), sum(catch_df$num.schools))
  expect_true(all(elem_df_norm$schoolPop > 0))
  expect_true(all(elem_df_pois$schoolPop > 0))
})

test_that("elementary_pop handles edge cases", {
  catch_df_single <- catchment_sim(1, 20, shape = 3.5, rate = 2.8)
  elem_df_single <- elementary_pop(catch_df_single, shape = 5.1, rate = 0.015)

  expect_equal(nrow(elem_df_single), catch_df_single$num.schools)
  expect_equal(unique(elem_df_single$catchID), catch_df_single$catchID)

})
