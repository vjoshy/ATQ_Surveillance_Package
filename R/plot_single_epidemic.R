#' Plot a single epidemic simulation
#'
#' This function creates two plots for a single epidemic simulation: one for new infections and
#' one for reported cases.
#'
#' @param epidemic A list containing the results of a single epidemic simulation, typically an
#' element from an "ssir_epidemic_multi" object.
#' @param epidemic_num An integer indicating the epidemic number, used in the plot titles.
#'
#' @return A grid arrangement of two ggplot objects: new infections and reported cases.
#' @export
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' # Assuming you have run a simulation:
#' result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4, inf_init = 32,
#' report = 0.02, lag = 7)
#' plot_single_epidemic(result, 1)
#'
#' # For multiple simulations:
#' multi_result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4, inf_init = 32,
#' report = 0.02, lag = 7, rep = 5)
#' plot_single_epidemic(multi_result[[1]], 1)
#'

plot_single_epidemic <- function(epidemic, epidemic_num) {

  # Calculate start date
  start_date <- which(epidemic$new_inf > 0)[1]

  # Create data frames for new infections and reported cases
  df_new_inf <- data.frame(
    time = 1:length(epidemic$new_inf),
    value = epidemic$new_inf,
    type = "New Infections"
  )

  df_reported <- data.frame(
    time = 1:length(epidemic$reported_cases),
    value = epidemic$reported_cases,
    type = "Reported Cases"
  )

  # Plot for new infections
  p1 <- ggplot(df_new_inf, aes(x = .data$time, y = .data$value)) +
    geom_line(color = "blue") +
    labs(title = paste("Epidemic", epidemic_num, "- New Infections (Start: Day", start_date, ")"),
         x = "Time",
         y = "Number of New Infections") +
    theme_bw()

  # Plot for reported cases
  p2 <- ggplot(df_reported, aes(x = .data$time, y = .data$value)) +
    geom_line(color = "red") +
    labs(title = paste("Epidemic", epidemic_num, "- Reported Cases (Start: Day", start_date, ")"),
         x = "Time",
         y = "Number of Reported Cases") +
    theme_bw()

  # Arrange plots for a single epidemic
  gridExtra::grid.arrange(p1, p2, ncol = 1)
}
