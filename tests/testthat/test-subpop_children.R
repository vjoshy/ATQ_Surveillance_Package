# tests/testthat/test-subpop_children.R


# Helper function to create sample data
create_sample_data <- function() {
  catch_df <- catchment_sim(4, 5, shape = 3.5, rate = 2.8)
  elementary_pop(catch_df, shape = 5.1, rate = 0.015)
}

test_that("subpop_children generates correct output structure", {
  set.seed(123)
  df <- create_sample_data()
  result <- subpop_children(df, n = 2,
                            prop_parent_couple = 0.7,
                            prop_children_couple = c(0.3, 0.5, 0.2),
                            prop_children_lone = c(0.4, 0.4, 0.2),
                            prop_elem_age = 0.6)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("houseID", "num_parent", "num_child", "num_elem_child", "schoolID",
                    "catchID", "schoolPop", "xStart", "xEnd", "yStart", "yEnd", "num_people") %in% names(result)))
  expect_true(all(result$num_parent %in% c(1, 2)))
  expect_true(all(result$num_child >= 1))
  expect_true(all(result$num_elem_child <= result$num_child))
})



test_that("subpop_children handles custom distribution functions", {
  set.seed(123)
  df <- create_sample_data()
  result <- subpop_children(df, n = 2,
                            prop_parent_couple = 0.7,
                            prop_children_couple = c(0.3, 0.5, 0.2),
                            prop_children_lone = c(0.4, 0.4, 0.2),
                            prop_elem_age = 0.6,
                            parent_dist = stats::rnorm, mean = 0.5, sd = 0.1,
                            child_dist = stats::rbeta, shape1 = 2, shape2 = 2,
                            age_dist = stats::runif)

  expect_s3_class(result, "data.frame")
  expect_true(all(result$num_parent %in% c(1, 2)))
  expect_true(all(result$num_child >= 1))
  expect_true(all(result$num_elem_child <= result$num_child))
})
