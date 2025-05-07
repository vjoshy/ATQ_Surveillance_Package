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
#' @importFrom stats setNames
#' @importFrom rlang .data
#' @importFrom scales hue_pal
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
#' results <- eval_metrics(compiled_data, thres = seq(0.1,0.6,by = 0.05))
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
  alert_data <- do.call(rbind, lapply(names(best_models), function(model_name) {
    model_data <- best_models[[model_name]]
    subset(model_data, model_data$Alarm == 1, select = c("time", "ScYr"))
  }))
  alert_data$Model <- rep(names(best_models), sapply(best_models, function(x) sum(x$Alarm == 1)))

  alert_data <- unique(alert_data)

  # Assign consistent y-positions to each model
  unique_models <- unique(alert_data$Model)
  model_positions <- setNames(seq(-1, -length(unique_models), by = -1), unique_models)

  alert_data$y_position <- model_positions[alert_data$Model]

  # Combine epidemic data with alert data
  plot_data <- merge(data, alert_data, by = c("time", "ScYr"), all.x = TRUE)
  plot_data <- unique(plot_data)

  # Create a plot for each year
  plots <- lapply(split(plot_data, plot_data$ScYr), function(year_data) {

    year <- unique(year_data$ScYr)

    # Calculate y-axis limits
    y_max <- max(c(year_data$reported_cases, year_data$pct_absent * 100), na.rm = TRUE)
    y_min <- min(model_positions) - 1

    p <- ggplot(year_data, aes(x = .data$time)) +
      # Absenteeism percentage
      geom_area(aes(y = .data$pct_absent * 100, fill = "Absenteeism (%)"), alpha = 0.7,
               show.legend = FALSE) +
      # Lab confirmed cases
      geom_area(aes(y = .data$reported_cases, fill = "Lab Confirmed Cases"), alpha = 0.7,
               show.legend = FALSE) +
      # Reference date
      geom_vline(data = dplyr::filter(year_data, .data$ref_date == 1),
                 aes(xintercept = .data$time, color = "Reference Date"),
                 linetype = "dashed") +
      # Alert points (stacked)
      geom_point(data = dplyr::filter(year_data, !is.na(.data$Model)),
                 aes(y = .data$y_position, color = .data$Model), shape = 15,
                 size = 3) +
      scale_fill_manual(values = c("Absenteeism (%)" = "grey70",
                                   "Lab Confirmed Cases" = "black")) +

      scale_color_manual(values = c("Reference Date" = "orange",
                                    setNames(scales::hue_pal()(length(unique_models)),
                                             unique_models))) +
      scale_y_continuous(
        name = "Average Absenteeism Percentage",
        sec.axis = sec_axis(~., name = "Confirmed Influenza Cases"),
        limits = c(y_min, y_max * 1.1)) + # Extend y-axis below 0 for stacked points

        labs(title = paste("Epidemic Data with Alerts - Year", year),
             x = "Time",
             fill = "Data Type",
             color = "Alerts and Reference") +

        theme_bw() +
        theme(legend.position = "bottom",
              axis.title.y.right = element_text(color = "black"),
              axis.title.y.left = element_text(color = "grey50"),
              legend.box.background = element_rect(),
              axis.title = element_text(size = 10),
              plot.title = element_text(size = 10))


    return(p)
  })

  return(plots)
}
