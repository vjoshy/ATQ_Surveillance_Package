#' Plot method for a single SSIR epidemic simulation
#'
#' This function creates plots for a single SSIR (Stochastic Susceptible-Infectious-Removed)
#' epidemic simulation.
#' It generates two plots: one for new infections and one for reported cases.
#'
#' @param x An object of class "ssir_epidemic", typically the result of calling ssir() with
#' rep = NULL or 1.
#' @param ... Additional arguments passed to the underlying plotting function (currently unused).
#'
#' @return A grid arrangement of two ggplot objects: new infections and reported cases.
#' @export
#'
#' @seealso \code{\link{plot_single_epidemic}} for the underlying plotting function
#'
#' @examples
#' # Run a single simulation
#' result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4, inf_init = 32,
#' report = 0.02, lag = 7)
#'
#' # Plot the results
#' plot(result)
plot.ssir_epidemic <- function(x, ...) {
  plot_single_epidemic(x, epidemic_num = 1)
}
