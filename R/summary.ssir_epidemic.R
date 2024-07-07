#' S3 Summary method for epidemic Simulation
#'
#'This function provides a summary of SSIR epidemic simulation.
#' It calculates and displays the  statistics.
#'
#' @param object An object of class "ssir_epidemic", typically the result of
#' calling ssir() with rep = 1 or NULL.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
#' @examples
#' # Run multiple simulations
#' result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4,
#'                      inf_init = 32, report = 0.02, lag = 7)
#'
#' # Display summary
#' summary(result)
#'

summary.ssir_epidemic <- function(object, ...) {

  total_infected <- sum(object$new_inf)
  total_reported <- sum(object$reported_cases, na.rm = TRUE)
  peak_infected <- max(object$I)
  peak_day <- which.max(object$I)
  final_susceptible <- tail(object$S, 1)
  final_removed <- tail(object$R, 1)

  cat("SSIR Epidemic Summary:\n")
  cat("Total infected:", total_infected, "\n")
  cat("Total reported cases:", total_reported, "\n")
  cat("Peak infected:", peak_infected, "\n")
  cat("Peak day:", peak_day, "\n")
  cat("Final susceptible:", final_susceptible, "\n")
  cat("Final removed:", final_removed, "\n")
  cat("\nModel parameters:\n")
  print(object$parameters)

}
