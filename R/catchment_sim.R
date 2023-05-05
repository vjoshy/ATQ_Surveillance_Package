
#' Simulating catchment areas
#'
#' Function to simulate specified catchment square area (a x a).
#' Catchment areas are simulated via gamma distribution
#'
#' @param n number of observations to be simulated
#' @param alpha shape parameter of gamma distribution
#' @param beta rate parameter of gamma distribution
#' @param area square dimension for catchment (if area = 20, then each catchment dimensions will be 20 x 20)
#'
#' @return data frame of n observations
#' @export
#'
#' @examples catch_df <- catchment_sim(16, 4.313320, 3.026894, 20)
#'
#'
catchment_sim <- function(n, alpha, beta, area){

  #gamma distribution of catchment area
  size <- round(stats::rgamma(n, alpha, beta)) |>
    (\(x) replace(x, x == 0, 1))()


  # creating empty vectors to hold number catchment areas of size n
  xStart <- c()
  xEnd <- c()
  yStart <- c()
  yEnd <- c()


  # for loop that populates the start and end values for x and y coordinates of the boundaries of catchment areas
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
