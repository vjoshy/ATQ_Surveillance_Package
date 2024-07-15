#' Create Elementary Schools population size
#'
#' Function to simulate elementary school size and assigns it to catchments.
#' The school population is simulated using a specified distribution function,
#' with gamma distribution as the default.
#'
#' @param df output data frame from catchment_sim function
#' @param dist_func distribution function to simulate school population, default is stats::rgamma
#' @param ... additional arguments passed to the distribution function
#'
#' @return A data frame with the following columns:
#'   \item{catchID}{Identifier for the catchment area}
#'   \item{schoolID}{Unique identifier for each school}
#'   \item{schoolPop}{Simulated population of the school}
#'   \item{xStart}{Starting x-coordinate of the catchment area}
#'   \item{xEnd}{Ending x-coordinate of the catchment area}
#'   \item{yStart}{Starting y-coordinate of the catchment area}
#'   \item{yEnd}{Ending y-coordinate of the catchment area}
#'
#' @export
#'
#' @examples
#' # Simulate catchment areas
#' catch_df <- catchment_sim(16, 20, shape = 3.5, rate = 2.8)
#'
#' # Simulate elementary schools using default gamma distribution
#' elementary_df1 <- elementary_pop(catch_df, shape = 5.1, rate = 0.015)
#'
#' # Simulate elementary schools using normal distribution
#' elementary_df2 <- elementary_pop(catch_df, dist_func = stats::rnorm,
#'                                  mean = 300, sd = 50)
#'
#' # Simulate elementary schools using Poisson distribution
#' elementary_df3 <- elementary_pop(catch_df, dist_func = stats::rpois,
#'                                  lambda = 250)
elementary_pop <- function(df, dist_func = stats::rgamma, ...){


  # gamma distribution of number of students in elementary schools
  # Generate catchment area sizes using the provided distribution function
  school_pop <- tryCatch({
    round(dist_func(sum(df$num.schools), ...)) |> (\(x) replace(x, x == 0, 1))()
  }, error = function(e) {
    stop(paste("Error in distribution function:", e$message))
  })

  school_id <- seq.int(length(school_pop))


  school_to_catchment_map <- c()

  # for loop to assign simulated catchment areas to number of elementary
  # schools in each area
  for(i in 1:nrow(df)){
    school_to_catchment_map <- c(school_to_catchment_map,
                                 rep(df$catchID[i], df$num.schools[i]))
  }


  # merge catchment data frame with simulated elementary school data frame
  sim_school_df <- data.frame(catchID = school_to_catchment_map,
                              schoolID = school_id, schoolPop = school_pop) |>
                    merge(df[,c("catchID", "xStart", "xEnd", "yStart", "yEnd")],
                              by="catchID", all.y = TRUE)

  return(sim_school_df)

}
