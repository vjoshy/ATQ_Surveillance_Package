# tests/testthat/test-simulate_households.R

# Helper function to create sample data
create_sample_data <- function() {
  catch_df <- catchment_sim(4, 5, shape = 3.5, rate = 2.8)
  elementary_df <- elementary_pop(catch_df, shape = 5.1, rate = 0.015)
  house_children <- subpop_children(elementary_df, n = 2,
                                    prop_parent_couple = 0.7,
                                    prop_children_couple = c(0.3, 0.5, 0.2),
                                    prop_children_lone = c(0.4, 0.4, 0.2),
                                    prop_elem_age = 0.2)
  house_nochildren <- subpop_noChildren(house_children, elementary_df,
                                        prop_house_size = c(0.2, 0.3, 0.25, 0.15, 0.1),
                                        prop_house_Children = 0.3)
  list(children_df = house_children, noChildren_df = house_nochildren)
}

test_that("simulate_households generates correct output structure", {
  set.seed(123)
  sample_data <- create_sample_data()
  result <- simulate_households(sample_data$children_df, sample_data$noChildren_df)

  expect_type(result, "list")
  expect_named(result, c("household_sim", "individual_sim"))


  expect_true(all(c("houseID", "catchID", "schoolID", "num_people",
                    "num_elem_child","xStart", "xEnd", "yStart",
                    "yEnd", "loc.x", "loc.y") %in% names(result$household_sim)))

  expect_true(all(c("houseID", "catchID", "schoolID", "num_people", "num_elem_child",
                    "xStart", "xEnd", "yStart", "yEnd", "loc.x", "loc.y",
                    "individualID", "elem_child_ind") %in% names(result$individual_sim)))
})

test_that("simulate_households correctly assigns individuals to households", {
  set.seed(123)
  sample_data <- create_sample_data()
  result <- simulate_households(sample_data$children_df, sample_data$noChildren_df)

  expect_equal(sum(result$household_sim$num_people), nrow(result$individual_sim))
  expect_equal(sum(result$household_sim$num_elem_child), sum(result$individual_sim$elem_child_ind))

  # Check if each household has the correct number of individuals
  household_counts <- table(result$individual_sim$houseID)
  expect_equal(sum(as.vector(household_counts)), sum(result$household_sim$num_people))
})
