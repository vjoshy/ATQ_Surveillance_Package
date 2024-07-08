#' Create Alarm Plot Data Object
#'
#' This function creates an object of class "alarm_plot_data" from epidemic data and best models.
#' It ensures that the input data is in the correct format for plotting.
#'
#' @param epidemic_data A data frame or list of data frames containing epidemic data.
#' @param best_models A list of data frames containing the best models for each metric.
#'
#' @return An object of class "alarm_plot_data" containing the epidemic data and best models.
#' @export
#'
#' @examples
#' # Generate sample epidemic data
#' set.seed(123)
#' epidemic_data <- data.frame(
#'   Date = rep(1:300, 3),
#'   ScYr = rep(1:3, each = 300),
#'   pct_absent = runif(900, 0, 0.1),
#'   lab_conf = rpois(900, lambda = 5),
#'   ref_date = rep(c(rep(0, 299), 1), 3),
#'   Case = rbinom(900, 1, 0.1),
#'   window = rep(c(rep(0, 280), rep(1, 20)), 3)
#' )
#'
#' # Generate sample best models data
#' generate_best_model <- function() {
#'   data.frame(
#'     Date = sample(1:300, 50),
#'     ScYr = sample(1:3, 50, replace = TRUE),
#'     Alarm = sample(0:1, 50, replace = TRUE),
#'     lag = sample(1:15, 50, replace = TRUE),
#'     thres = runif(50, 0, 1)
#'   )
#' }
#'
#' best_models <- list(
#'   AATQ = generate_best_model(),
#'   FATQ = generate_best_model(),
#'   FAR = generate_best_model(),
#'   ADD = generate_best_model(),
#'   WFATQ = generate_best_model(),
#'   WAATQ = generate_best_model()
#' )
#'
#' # Create alarm_plot_data object
#' plot_data <- alarm_plot_data(epidemic_data, best_models)
#'
#' # Check the structure of the resulting object
#' str(plot_data)
alarm_plot_data <- function(epidemic_data, best_models) {
  # Function body remains the same
}
alarm_plot_data <- function(epidemic_data, best_models) {

  # Ensure epidemic_data is a data frame
  if (is.list(epidemic_data) && !is.data.frame(epidemic_data)) {
    epidemic_data <- do.call(rbind, epidemic_data)
  }

  # Ensure best_models is a list of data frames
  best_models <- lapply(best_models, function(model) {
    if (is.list(model) && !is.data.frame(model)) {
      return(do.call(rbind, model))
    }
    return(model)
  })

  structure(
    list(
      epidemic_data = epidemic_data,
      best_models = best_models
    ),
    class = "alarm_plot_data"
  )
}

