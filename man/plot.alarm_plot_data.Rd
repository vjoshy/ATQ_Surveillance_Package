% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.alarm_plot_data.R
\name{plot.alarm_plot_data}
\alias{plot.alarm_plot_data}
\title{Plot Epidemic Data with Alarm Metrics}
\usage{
\method{plot}{alarm_plot_data}(x, ...)
}
\arguments{
\item{x}{An object of class "alarm_plot_data" containing epidemic data and best models.}

\item{...}{Additional arguments passed to the plotting function (currently unused).}
}
\value{
A list of ggplot objects, one for each epidemic year.
}
\description{
This function creates a series of plots, one for each epidemic year,
showing the absenteeism percentage, laboratory confirmed cases,
reference dates, and alarm points for different models.
}
\examples{
# Generate sample data
# Generate simulated epidemic data
n_rows <- 7421
n_houses <- 1000

epidemic_new <- ssir(n_rows, T = 300, alpha =0.298, inf_init = 32, rep = 3)

 individual_data <- data.frame(
houseID = rep(1:n_houses, each = ceiling(n_rows / n_houses))[1:n_rows],
catchID = sample(1:10, n_rows, replace = TRUE),
schoolID = sample(1:10, n_rows, replace = TRUE),
num_people = round(rnorm(n_rows, mean = 4, sd = 1)),  # Normal distribution for num_people
num_elem_child = round(rnorm(n_rows, mean = 1, sd = 1)),  # Normal distribution for num_elem_child
xStart = 0,
xEnd = 5,
yStart = 0,
yEnd = 5,
loc.x = rnorm(n_rows, mean = 2.5, sd = 1),  # Normal distribution for loc.x
loc.y = rnorm(n_rows, mean = 2.5, sd = 1),  # Normal distribution for loc.y
individualID = 1:n_rows,
elem_child_ind = sample(0:1, n_rows, replace = TRUE)
)

compiled_data <- compile_epi(epidemic_new, individual_data)

# Evaluate alarm metrics
results <- eval_metrics(compiled_data, thres = seq(0.1,0.6,by = 0.05))

# Generate plots
plots <- plot(results$plot_data)

# Display the first plot
if(interactive()) print(plots[[1]])

}
