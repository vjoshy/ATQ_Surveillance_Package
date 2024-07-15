# tests/testthat/test-ssir.R

test_that("ssir generates correct output structure for single simulation", {
  set.seed(123)
  result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4,
                 inf_init = 32, report = 0.02, lag = 7)

  expect_s3_class(result, "ssir_epidemic")
  expect_named(result, c("new_inf", "reported_cases", "S", "I", "R", "parameters"))
  expect_length(result$new_inf, 300)
  expect_length(result$reported_cases, 307)  # T + lag
  expect_length(result$S, 300)
  expect_length(result$I, 300)
  expect_length(result$R, 300)
  expect_type(result$parameters, "list")
  expect_equal(result$parameters$N, 10000)
})

test_that("ssir generates correct output structure for multiple simulations", {
  set.seed(123)
  result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4,
                 inf_init = 32, report = 0.02, lag = 7, rep = 5)

  expect_s3_class(result, "ssir_epidemic_multi")
  expect_length(result, 6)  # 5 simulations + parameters
  expect_type(result[[1]], "list")
  expect_named(result[[1]], c("new_inf", "reported_cases", "S", "I", "R"))
  expect_type(result$parameters, "list")
  expect_equal(result$parameters$rep, 5)
})

test_that("ssir handles edge cases and errors", {
  expect_error(ssir(N = -1000, alpha = 0.3))
  expect_error(ssir(N = 10000, T = -300, alpha = 0.3))
  expect_error(ssir(N = 10000, T = 300, alpha = 1.5))
  expect_error(ssir(N = 10000, T = 300, alpha = 0.3, inf_period = -4))
  expect_error(ssir(N = 10000, T = 300, alpha = 0.3, report = 1.5))
  expect_error(ssir(N = 10000, T = 300, alpha = 0.3, lag = -7))
  expect_error(ssir(N = 10000, T = 300, alpha = 0.3, rep = -5))

  # Test with extreme values
  expect_error(ssir(N = 10, T = 10, alpha = 0.99, inf_period = 1,
                    inf_init = 1, report = 0.99, lag = 0))
})
