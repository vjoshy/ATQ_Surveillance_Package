
elementary_pop <- function(df, shape, rate){


  # gamma distribution of number of elementary schools
  school_pop <- round(stats::rgamma(sum(df$num.schools), shape, rate))
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
