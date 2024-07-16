create_sample_data <- function(n_individuals = 1000, n_years = 2, T = 300) {
  individual_data <- data.frame(
    elem_child_ind = sample(0:1, n_individuals, replace = TRUE, prob = c(0.3, 0.7)),
    schoolID = sample(1:10, n_individuals, replace = TRUE)
  )

  epidemic <- ssir(N = n_individuals, T = T, alpha = 0.3, inf_period = 4,
                   inf_init = 32, report = 0.02, lag = 7, rep = n_years)

  list(epidemic = epidemic, individual_data = individual_data)
}


test_that("compile_epi handles input validation", {
  # Create dummy data
  sample_data <- create_sample_data()
  result <- compile_epi(sample_data$epidemic, sample_data$individual_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 600)  # 300 days * 2 years

  expected_columns <- c("Date", "ScYr", "pct_absent", "absent", "absent_sick",
                        "new_inf", "lab_conf", "Case", "sinterm", "costerm",
                        "window", "ref_date")
  expect_true(all(expected_columns %in% names(result)))

  lag_columns <- paste0("lag", 0:15)
  expect_true(all(lag_columns %in% names(result)))
})

test_that("compile_epi produces correct output structure", {
  # Create dummy data
  set.seed(123)
  sample_data <- create_sample_data()

  epidemic <- sample_data$epidemic
  individual_data <- sample_data$individual_data

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



test_that("compile_epi handles multiple years correctly", {
  # Create dummy data for multiple years
  sample_data <- create_sample_data(n_years = 3)

  epidemic <- sample_data$epidemic
  individual_data <- sample_data$individual_data

  # Run function
  result <- compile_epi(epidemic, individual_data)

  # Check years
  expect_equal(length(unique(result$ScYr)), 3)
  expect_equal(nrow(result), 900)  # 300 days * 3 years
})

