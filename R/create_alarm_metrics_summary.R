#' Create Alarm Metrics Summary
#'
#' This function creates a summary of alarm metrics, including statistics for each metric,
#' best model parameters, reference dates, and best prediction dates for each epidemic year.
#'
#' @param metrics An object of class "alarm_metrics" containing matrices for different metrics.
#' @param best_models A list of data frames, each containing the best model for a specific metric.
#' @param epidemic_data A data frame containing the epidemic data, including ScYr, Date, and ref_date columns.
#'
#' @return An object of class "alarm_metrics_summary" containing summary statistics, best values,
#'         reference dates, and best prediction dates for each metric and epidemic year.
#' @export
#'
#' @importFrom stats var
#'
#' @examples
#' # Generate sample data
#' set.seed(123)
#'
#' # Generate sample metrics
#' generate_metric_matrix <- function() {
#'   matrix(runif(15 * 11), nrow = 15, ncol = 11,
#'          dimnames = list(paste("Lag", 1:15),
#'                          paste("Threshold", seq(0.1, 0.6, by = 0.05))))
#' }
#'
#' sample_metrics <- list(
#'   FAR = generate_metric_matrix(),
#'   ADD = generate_metric_matrix(),
#'   AATQ = generate_metric_matrix(),
#'   FATQ = generate_metric_matrix(),
#'   WAATQ = generate_metric_matrix(),
#'   WFATQ = generate_metric_matrix()
#' )
#'
#' metrics <- structure(sample_metrics, class = c("alarm_metrics", "list"))
#'
#' # Generate sample best models
#' generate_best_model <- function() {
#'   data.frame(
#'     ScYr = rep(1:3, each = 100),
#'     time = rep(1:100, 3),
#'     Alarm = sample(c(0, 1), 300, replace = TRUE, prob = c(0.9, 0.1)),
#'     lag = sample(1:15, 300, replace = TRUE),
#'     thres = runif(300, 0.1, 0.6)
#'   )
#' }
#'
#' best_models <- list(
#'   best.FAR = generate_best_model(),
#'   best.ADD = generate_best_model(),
#'   best.AATQ = generate_best_model(),
#'   best.FATQ = generate_best_model(),
#'   best.WAATQ = generate_best_model(),
#'   best.WFATQ = generate_best_model()
#' )
#'
#' # Generate sample epidemic data
#' epidemic_data <- data.frame(
#'   ScYr = rep(1:3, each = 365),
#'   time = rep(1:365, 3),
#'   ref_date = c(rep(0, 364), 1)
#' )
#'
#' # Create alarm metrics summary
#' summary <- create_alarm_metrics_summary(metrics, best_models, epidemic_data)
#'
#' # Print summary
#' print(summary)
#'
create_alarm_metrics_summary <- function(metrics, best_models, epidemic_data) {

  # Extract metric matrices
  metric_names <- c("FAR", "ADD", "AATQ", "FATQ", "WAATQ", "WFATQ")
  metric_matrices <- metrics[metric_names]

  # Calculate mean and variance for each metric
  summary_stats <- lapply(metric_matrices, function(mat) {
    data.frame(
      mean = mean(mat, na.rm = TRUE),
      variance = stats::var(as.vector(mat), na.rm = TRUE)
    )
  })

  # Extract best lag and threshold for each metric
  best_values <- lapply(metric_names, function(metric) {
    best_model <- best_models[[paste0("best.", metric)]]
    data.frame(
      best_lag = best_model$lag[1],
      best_threshold = best_model$thres[1],
      best_value = min(metric_matrices[[metric]], na.rm = TRUE)
    )
  })
  names(best_values) <- metric_names
  epidemic_years <- unique(epidemic_data$ScYr)

  # Extract reference dates and best prediction dates
  ref_dates <- sapply(epidemic_years, function(year) {
    dates <- epidemic_data$time[epidemic_data$ScYr == year & epidemic_data$ref_date == 1]

    if(length(dates) > 0) min(dates) else NA
  })

  best_prediction_dates <- lapply(metric_names, function(metric) {
    sapply(epidemic_years, function(year) {
      best_model <- best_models[[paste0("best.", metric)]]
      dates <- best_model$time[best_model$ScYr == year & best_model$Alarm == 1 & best_model$window == 1]

      if(length(dates) > 0) min(dates) else NA
    })
  })
  names(best_prediction_dates) <- metric_names

  ref_dates <- data.frame(epidemic_years, ref_dates)

  # Create the summary object
  summary <- list(
    summary_stats = summary_stats,
    best_values = best_values,
    ref_dates = ref_dates,
    best_prediction_dates = best_prediction_dates
  )

  class(summary) <- "alarm_metrics_summary"
  return(summary)
}
