#' Create Best Model
#'
#' This function sets the class of the given model data to "best_model".
#'
#' @param model_data A data frame containing the model data.
#'
#' @return An object of class "best_model" containing the model data.
#' @export
#'
#' @examples
#' # Generate sample model data
#' sample_model_data <- data.frame(
#'   ScYr = rep(1:3, each = 100),
#'   Date = rep(1:100, 3),
#'   Alarm = sample(c(0, 1), 300, replace = TRUE, prob = c(0.9, 0.1)),
#'   lag = sample(1:15, 300, replace = TRUE),
#'   thres = runif(300, 0.1, 0.6)
#' )
#'
#' # Create best model
#' best_model_data <- best_model(sample_model_data)
#'
#' # Print the class of the best model
#' class(best_model_data)
best_model <- function(model_data) {
  class(model_data) <- c("best_model", class(model_data))
  model_data
}
