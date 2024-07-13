#' Simulating catchment areas
#'
#' Function to simulate specified catchments of square area (a x a).
#' The number of schools in each catchment area is simulated via a specified distribution function,
#' with gamma distribution as the default.
#'
#' @param n number of catchments to be simulated
#' @param area square dimension for catchment (if area = 20, then each catchment
#' dimensions will be 20 x 20)
#' @param dist_func distribution function to simulate number of schools, default is stats::rgamma
#' @param ... additional arguments passed to the distribution function
#'
#' @return A data frame with n rows and the following columns:
#'   \item{catchID}{Unique identifier for each catchment area}
#'   \item{num.schools}{Number of schools in the catchment area}
#'   \item{xStart}{Starting x-coordinate of the catchment area}
#'   \item{xEnd}{Ending x-coordinate of the catchment area}
#'   \item{yStart}{Starting y-coordinate of the catchment area}
#'   \item{yEnd}{Ending y-coordinate of the catchment area}
#'
#' @export
#'
#' @examples
#' # Using default gamma distribution
#' catch_df1 <- catchment_sim(16, 20, shape = 4.1, rate = 2.7)
#'
#' # Using normal distribution
#' catch_df2 <- catchment_sim(16, 20, dist_func = stats::rnorm, mean = 5, sd = 1)
#'
#' # Using Poisson distribution
#' catch_df3 <- catchment_sim(16, 20, dist_func = stats::rpois, lambda = 3)
catchment_sim <- function(n, area, dist_func = stats::rgamma, ...){

  # Generate catchment area sizes using the provided distribution function
  size <- round(dist_func(n, ...)) |>
    (\(x) replace(x, x == 0, 1))()


  # creating empty vectors to hold number catchment areas of size n
  xStart <- c()
  xEnd <- c()
  yStart <- c()
  yEnd <- c()


  # for loop that populates the start and end values for x and y coordinates
  # of the boundaries of catchment areas
  for(i in 1:n){

    xStart[i] <- (i-1) %/% (n/4) * area
    xEnd[i] <- (i-1) %/% (n/4) * area + area
    yStart[i] <- (i-1) %% (n/4) * area
    yEnd[i] <- (i-1) %% (n/4) * area + area

  }


  # populating a data frame with simulated data
  sim_catchment_df <- data.frame(catchID = c(1:n),
                                 num.schools = size,
                                 xStart, xEnd,
                                 yStart, yEnd)

  return(sim_catchment_df)
}
