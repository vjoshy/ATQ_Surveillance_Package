
#' Create Elementary Schools population size
#'
#' Function to simulation elementary school size and then assigned to catchment areas.
#'
#' @param df output data frame from catchment_sim function
#' @param alpha shape parameter of gamma distribution
#' @param beta rate parameter of gamma distribution
#'
#' @return a data frame of simulated elementary school population
#' @export
#'
#' @examples catch_df <- catchment_sim(16, 4.313320, 3.026894, 20)
#' elementary_df <- elementary_pop(catch_df, 5.27426341, 0.01427793)
#'
#'
elementary_pop <- function(df, alpha, beta){


  # gamma distribution of number of elementary schools
  school_pop <- round(stats::rgamma(sum(df$num.schools), alpha, beta))
  school_id <- seq.int(length(school_pop))


  school_to_catchment_map <- c()

  # for loop to assign simulated catchment areas to number of elementary schools in each area
  for(i in 1:nrow(df)){
    school_to_catchment_map <- c(school_to_catchment_map, rep(df$catchID[i], df$num.schools[i]))
  }


  # merge catchment data frame with simulated elementary school data frame
  sim_school_df <- data.frame(catchID = school_to_catchment_map, schoolID = school_id,
                              schoolPop = school_pop) |>
    merge(df[,c("catchID", "xStart", "xEnd", "yStart", "yEnd")],
          by="catchID", all.y = TRUE)

  return(sim_school_df)

}
