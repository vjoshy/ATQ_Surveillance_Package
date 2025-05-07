#' Summary of Alarm Metrics
#'
#' This function provides a summary of the alarm metrics, including mean and variance for each metric,
#' the best lag and threshold values, and lists of reference dates and best prediction dates.
#'
#' @param object A list of class `alarm_metrics_summary` containing the alarm metrics summary data.
#' @param ... Additional arguments passed to the function (currently unused).
#'
#' @return A printed summary of the alarm metrics.
#' @export
#'
#' @examples
# Generate sample data
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
#'   Date = rep(1:365, 3),
#'   ref_date = c(rep(0, 364), 1)
#' )
#'
#' # Create alarm metrics summary
#' data <- create_alarm_metrics_summary(metrics, best_models, epidemic_data)
#'
#' # Print summary
#' summary(data)
#'
summary.alarm_metrics_summary <- function(object, ...) {

  cat("Alert Metrics Summary\n")
  cat("=====================\n\n")

  # Display summary statistics for each metric
  for (metric in names(object$summary_stats)) {
    cat(metric, ":\n")
    # cat("  Mean:", round(object$summary_stats[[metric]]$mean, 4), "\n")
    # cat("  Variance:", round(object$summary_stats[[metric]]$variance, 4), "\n")
    cat("  Optimal lag:", object$best_values[[metric]]$best_lag, "\n")
    cat("  Optimal threshold:", round(object$best_values[[metric]]$best_threshold, 4), "\n")
    cat("  Minimum value:", round(object$best_values[[metric]]$best_value, 4), "\n\n")
  }

  ref_dates <- object$ref_dates
  metrics <- names(object$best_prediction_dates)

  combined_df <- data.frame(
    year = ref_dates$epidemic_years,
    ref_date = ref_dates$ref_dates
  )

  for(metric in metrics){
    combined_df[[metric]] <- object$best_prediction_dates[[metric]]
  }

  cat("Reference Dates and Model Selected Alert Dates:\n")
  cat("=====================\n\n")
  print(combined_df)

}

