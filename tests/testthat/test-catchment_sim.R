# tests/testthat/test-catchment_sim.R


test_that("catchment_sim generates correct output structure", {
  set.seed(123)
  catch_df <- catchment_sim(16, 20, shape = 4.1, rate = 2.7)

  expect_equal(nrow(catch_df), 16)
  expect_equal(ncol(catch_df), 6)
  expect_equal(names(catch_df), c("catchID", "num.schools", "xStart", "xEnd", "yStart", "yEnd"))
  expect_true(all(catch_df$num.schools >= 1))
  expect_true(all(catch_df$xEnd - catch_df$xStart == 20))
  expect_true(all(catch_df$yEnd - catch_df$yStart == 20))
})

test_that("catchment_sim works with different distribution functions", {
  set.seed(123)
  catch_df_norm <- catchment_sim(16, 20, dist_func = stats::rnorm, mean = 5, sd = 1)
  catch_df_pois <- catchment_sim(16, 20, dist_func = stats::rpois, lambda = 3)

  expect_equal(nrow(catch_df_norm), 16)
  expect_equal(nrow(catch_df_pois), 16)
  expect_true(all(catch_df_norm$num.schools >= 1))
  expect_true(all(catch_df_pois$num.schools >= 1))
})
