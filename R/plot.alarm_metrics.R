#' Plot Heatmap of Alarm Metrics
#'
#' This function creates a heatmap visualization of the specified alarm metric
#' across different lags and thresholds.
#'
#' @param x An object of class "alarm_metrics" containing matrices of metric values.
#' @param metric A character string specifying which metric to plot.
#'        Default is "AATQ". Options include "FAR", "ADD", "AATQ", "FATQ", "WAATQ", "WFATQ".
#' @param col Set heat map color profile, default is heat.colors(15)
#' @param ... Additional arguments passed to the image function.
#'
#' @return A heatmap plot of the specified metric.
#' @export
#'
#' @importFrom graphics image
#' @importFrom grDevices heat.colors
#' @examples
#' # Generate sample alarm metrics data
#' set.seed(123)
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
#'   WFATQ = generate_metric_matrix(),
#'  lags = 1:15,
#'  thresholds = seq(0.1, 0.6, by = 0.05)
#' )
#'
#' # Create an alarm_metrics object
#' alarm_metrics_obj <- structure(sample_metrics, class = c("alarm_metrics", "list"))
#'
#' # Plot the heatmap for AATQ (default)
#' plot(alarm_metrics_obj)
#'
#' # Plot the heatmap for FAR
#' plot(alarm_metrics_obj, metric = "FAR")
#'
#' # Customize the plot
#' plot(alarm_metrics_obj, metric = "FATQ", col = heat.colors(12))
#'
plot.alarm_metrics <- function(x, metric = "AATQ", col = heat.colors(12), ...) {
  # Check if 'col' is provided in ...
  dots <- list(...)
  if ("col" %in% names(dots)) {
    col <- dots$col
    dots$col <- NULL
  }

  # Create a heatmap of the chosen metric
  do.call(image, c(list(
    z = x[[metric]],
    x = x$lags,
    y = x$thresholds,
    main = paste(metric, "by lag and threshold"),
    xlab = 'Lags',
    ylab = 'Thresholds',
    col = col
  ), dots))
}
