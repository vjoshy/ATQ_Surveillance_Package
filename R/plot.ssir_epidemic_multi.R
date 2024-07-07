#' Plot method for multiple SSIR epidemic simulations
#'
#' This function creates plots for multiple SSIR (Stochastic Susceptible-Infectious-Removed) epidemic simulations.
#' It generates two plots (new infections and reported cases) for each simulation in the multi-simulation object.
#'
#' @param x An object of class "ssir_epidemic_multi", typically the result of calling ssir() with rep > 1.
#' @param ... Additional arguments passed to the underlying plotting function (currently unused).
#'
#' @return A list of grid arrangements, each containing two ggplot objects (new infections and reported cases) for each epidemic simulation.
#' @export
#'
#' @seealso \code{\link{plot_single_epidemic}} for the underlying plotting function
#'
#' @examples
#' # Run multiple simulations
#' multi_result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4,
#'                      inf_init = 32, report = 0.02, lag = 7, rep = 5)
#'
#' # Plot the results
#' plots <- plot(multi_result)
#'
#' # Display the first simulation's plots
#' plots[[1]]
plot.ssir_epidemic_multi <- function(x, ...) {
  num_epidemics <- x$parameters$rep
  plot_list <- lapply(1:num_epidemics, function(i) plot_single_epidemic(x[[i]], epidemic_num = i))
  return(plot_list)
}
