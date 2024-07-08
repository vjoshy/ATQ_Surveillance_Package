#' Plot Epidemic Data with Alarm Metrics
#'
#' This function creates a series of plots, one for each epidemic year,
#' showing the absenteeism percentage, laboratory confirmed cases,
#' reference dates, and alarm points for different models.
#'
#' @param x An object of class "alarm_plot_data" containing epidemic data and best models.
#' @param ... Additional arguments passed to the plotting function (currently unused).
#'
#' @return A list of ggplot objects, one for each epidemic year.
#' @export
#'
#' @import ggplot2
#' @import purrr
#'
#' @importFrom dplyr left_join  distinct mutate group_by group_map
#'
#' @examples
#' # Generate sample data
#' # Generate simulated epidemic data
#' n_rows <- 7421
#' n_houses <- 1000
#'
#' epidemic_new <- ssir(n_rows, T = 300, alpha =0.298, inf_init = 32, rep = 3)
#'
#'  individual_data <- data.frame(
#' houseID = rep(1:n_houses, each = ceiling(n_rows / n_houses))[1:n_rows],
#' catchID = sample(1:10, n_rows, replace = TRUE),
#' schoolID = sample(1:10, n_rows, replace = TRUE),
#' num_people = round(rnorm(n_rows, mean = 4, sd = 1)),  # Normal distribution for num_people
#' num_elem_child = round(rnorm(n_rows, mean = 1, sd = 1)),  # Normal distribution for num_elem_child
#' xStart = 0,
#' xEnd = 5,
#' yStart = 0,
#' yEnd = 5,
#' loc.x = rnorm(n_rows, mean = 2.5, sd = 1),  # Normal distribution for loc.x
#' loc.y = rnorm(n_rows, mean = 2.5, sd = 1),  # Normal distribution for loc.y
#' individualID = 1:n_rows,
#' elem_child_ind = sample(0:1, n_rows, replace = TRUE)
#' )
#'
#' compiled_data <- compile_epi(epidemic_new, individual_data)
#'
#' # Evaluate alarm metrics
#' results <- eval_metrics(compiled_data, thres = seq(0.1,0.6,by = 0.05),
#'                   ScYr = c(2:3), yr.weights = c(1:2)/sum(c(1:2)))
#'
#' # Generate plots
#' plots <- plot(results$plot_data)
#'
#' # Display the first plot
#' if(interactive()) print(plots[[1]])
#'
plot.alarm_plot_data <- function(x, ...) {
  # Extract data
  data <- x$epidemic_data
  best_models <- x$best_models

  # Prepare alert data
  alert_data <- purrr::map_dfr(names(best_models), function(model_name) {
    best_models[[model_name]] %>%
      dplyr::filter(Alarm == 1) %>%
      dplyr::mutate(Model = model_name)
  }) %>%
    dplyr::distinct(Date, ScYr, Model, .keep_all = TRUE)  # Remove any duplicates

  # Assign consistent y-positions to each model
  unique_models <- unique(alert_data$Model)
  model_positions <- setNames(seq(-1, -length(unique_models), by = -1), unique_models)

  alert_data <- alert_data %>%
    dplyr::mutate(y_position = model_positions[Model])

  # Combine epidemic data with alert data
  plot_data <- data %>%
    dplyr::left_join(alert_data, by = c("Date", "ScYr"), suffix = c("", ".alert")) %>%
    dplyr::distinct(Date, ScYr, Model, .keep_all = TRUE)  # Remove any duplicates that might have been created

  # Create a plot for each year
  plots <- plot_data %>%
    dplyr::group_by(ScYr) %>%
    dplyr::group_map(function(data, key) {
      year <- key$ScYr

      # Calculate y-axis limits
      y_max <- max(data$lab_conf, data$pct_absent * 100, na.rm = TRUE)
      y_min <- min(model_positions) - 1  # Adjust y_min to accommodate all model points

      p <- ggplot(data, aes(x = Date)) +
        # Absenteeism percentage
        geom_col(aes(y = pct_absent * 100), fill = "grey70", alpha = 0.7) +
        # Lab confirmed cases
        geom_col(aes(y = lab_conf), fill = "black", alpha = 0.7) +
        # Reference date
        geom_vline(data = dplyr::filter(data, ref_date == 1),
                   aes(xintercept = Date),
                   linetype = "dashed", color = "orange") +
        # Alert points (stacked)
        geom_point(data = dplyr::filter(data, !is.na(Model)),
                   aes(y = y_position, color = Model), shape = 15,
                   size = 3) +
        scale_color_brewer(palette = "Set1") +
        #scale_x_continuous(breaks = seq(0, 300, by = 50)) +
        scale_y_continuous(
          name = "Average Absenteeism Percentage",
          sec.axis = sec_axis(~., name = "Confirmed Influenza Cases"),
          limits = c(y_min, y_max * 1.1)  # Extend y-axis below 0 to accommodate stacked points
        ) +
        labs(title = paste("Epidemic Data with Alerts - Year", year),
             x = "Time") +
        theme_bw() +
        theme(legend.position = "bottom",
              axis.title.y.right = element_text(color = "black"),
              axis.title.y.left = element_text(color = "grey50"))

      return(p)
    })

  # Return the list of plots
  return(plots)
}
