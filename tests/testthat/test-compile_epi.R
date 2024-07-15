test_that("compile_epi handles input validation", {
  # Create dummy data
  epidemic <- list(year1 = list(new_inf = rnorm(300), reported_cases = rnorm(300)))
  individual_data <- data.frame(elem_child_ind = sample(0:1, 1000, replace = TRUE),
                                schoolID = sample(1:10, 1000, replace = TRUE))

  # Test invalid inputs
  expect_error(compile_epi("not a list", individual_data),
               "epidemic must be a list")
  expect_error(compile_epi(epidemic, "not a dataframe"),
               "individual_data must be a dataframe")
  expect_error(compile_epi(epidemic, individual_data, lags = "16"),
               "lags must be a single positive numeric value")
  expect_error(compile_epi(epidemic, individual_data, lags = -1),
               "lags must be a single positive numeric value")
})

test_that("compile_epi produces correct output structure", {
  # Create dummy data
  set.seed(123)
  epidemic <- list(
    year1 = list(new_inf = rpois(300, 5), reported_cases = rpois(300, 2)),
    year2 = list(new_inf = rpois(300, 7), reported_cases = rpois(300, 3))
  )
  individual_data <- data.frame(
    elem_child_ind = sample(0:1, 1000, replace = TRUE, prob = c(0.3, 0.7)),
    schoolID = sample(1:10, 1000, replace = TRUE)
  )

  # Run function
  result <- compile_epi(epidemic, individual_data)

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 600)  # 300 days * 2 years

  expected_columns <- c("Date", "ScYr", "pct_absent", "absent", "absent_sick",
                        "new_inf", "lab_conf", "Case", "sinterm", "costerm",
                        "window", "ref_date")
  expect_true(all(expected_columns %in% names(result)))

  # Check for lag columns
  lag_columns <- paste0("lag", 0:15)
  expect_true(all(lag_columns %in% names(result)))
})

test_that("compile_epi calculates derived variables correctly", {
  # Create dummy data with known values
  set.seed(456)
  epidemic <- list(
    year1 = list(
      new_inf = c(rep(0, 50), rep(5, 10), rep(0, 240)),
      reported_cases = c(rep(0, 52), rep(2, 8), rep(0, 240))
    )
  )
  individual_data <- data.frame(
    elem_child_ind = rep(1, 1000),
    schoolID = rep(1:10, each = 100)
  )

  # Run function
  result <- compile_epi(epidemic, individual_data)

  # Check Case variable
  expect_equal(sum(result$Case), 10)  # Should be 1 for each day with new infections

  # Check seasonal terms
  expect_equal(result$sinterm[1], sin((2*pi*1)/365.25), tolerance = 1e-6)
  expect_equal(result$costerm[1], cos((2*pi*1)/365.25), tolerance = 1e-6)

  # Check reference date
  expect_equal(sum(result$ref_date), 1)  # Should be only one reference date

})

test_that("compile_epi handles multiple years correctly", {
  # Create dummy data for multiple years
  set.seed(789)
  epidemic <- list(
    year1 = list(new_inf = rpois(300, 5), reported_cases = rpois(300, 2)),
    year2 = list(new_inf = rpois(300, 7), reported_cases = rpois(300, 3)),
    year3 = list(new_inf = rpois(300, 6), reported_cases = rpois(300, 2.5))
  )
  individual_data <- data.frame(
    elem_child_ind = sample(0:1, 1000, replace = TRUE, prob = c(0.3, 0.7)),
    schoolID = sample(1:10, 1000, replace = TRUE)
  )

  # Run function
  result <- compile_epi(epidemic, individual_data)

  # Check years
  expect_equal(length(unique(result$ScYr)), 3)
  expect_equal(nrow(result), 900)  # 300 days * 3 years
})

test_that("compile_epi handles edge cases", {
  # Test with no infections
  epidemic_no_inf <- list(year1 = list(new_inf = rep(0, 300), reported_cases = rep(0, 300)))
  individual_data <- data.frame(elem_child_ind = rep(1, 1000), schoolID = rep(1:10, each = 100))

  result_no_inf <- compile_epi(epidemic_no_inf, individual_data)
  expect_equal(sum(result_no_inf$Case), 0)
  expect_equal(sum(result_no_inf$ref_date), 0)
  expect_equal(sum(result_no_inf$window), 0)

  # Test with all children infected
  epidemic_all_inf <- list(year1 = list(new_inf = rep(1000, 300), reported_cases = rep(20, 300)))
  result_all_inf <- compile_epi(epidemic_all_inf, individual_data)
  expect_true(all(result_all_inf$Case == 1))
})
