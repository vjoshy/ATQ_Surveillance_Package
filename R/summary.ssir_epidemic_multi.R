#' S3 Summary method for multiple epidemic simulations
#'
#' This function provides a summary of multiple SSIR epidemic simulations.
#' It calculates and displays average statistics across all simulations.
#'
#' @param object An object of class "ssir_epidemic_multi", typically the result of
#' calling ssir() with rep > 1.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
#' @examples
#' # Run multiple simulations
#' multi_result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4,
#'                      inf_init = 32, report = 0.02, lag = 7, rep = 100)
#'
#' # Display summary
#' summary(multi_result)
#'

summary.ssir_epidemic_multi <- function(object, ...) {
  cat("SSIR Epidemic Summary (Multiple Simulations):\n")
  cat("Number of simulations:", object$parameters$rep, "\n\n")

  # Calculate average statistics across all simulations
  avg_total_infected <- mean(sapply(object[1:object$parameters$rep],
                                    function(sim) sum(sim$new_inf)))
  avg_total_reported <- mean(sapply(object[1:object$parameters$rep],
                                    function(sim) sum(sim$reported_cases, na.rm = TRUE)))
  avg_peak_infected <- mean(sapply(object[1:object$parameters$rep],
                                   function(sim) max(sim$I)))

  cat("Average total infected:", round(avg_total_infected, 2), "\n")
  cat("Average total reported cases:", round(avg_total_reported, 2), "\n")
  cat("Average peak infected:", round(avg_peak_infected, 2), "\n")

  cat("\nModel parameters:\n")
  print(object$parameters)
}
