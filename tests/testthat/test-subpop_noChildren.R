# tests/testthat/test-subpop_noChildren.R

# Helper function to create sample data
create_sample_data <- function() {
  catch_df <- catchment_sim(4, 5, shape = 3.5, rate = 2.8)
  elementary_df <- elementary_pop(catch_df, shape = 5.1, rate = 0.015)
  house_children <- subpop_children(elementary_df, n = 2,
                                    prop_parent_couple = 0.7,
                                    prop_children_couple = c(0.3, 0.5, 0.2),
                                    prop_children_lone = c(0.4, 0.4, 0.2),
                                    prop_elem_age = 0.6)
  list(house_children = house_children, elementary_df = elementary_df)
}

test_that("subpop_noChildren generates correct output structure", {
  set.seed(123)
  sample_data <- create_sample_data()
  result <- subpop_noChildren(sample_data$house_children, sample_data$elementary_df,
                              prop_house_size = c(0.2, 0.3, 0.25, 0.15, 0.1),
                              prop_house_Children = 0.3)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("catchID", "houseID", "num_people", "schoolPop",
                    "xStart", "xEnd", "yStart", "yEnd") %in% names(result)))
  expect_true(all(result$num_people >= 1))
  expect_true(all(result$houseID > max(sample_data$house_children$houseID)))
})


