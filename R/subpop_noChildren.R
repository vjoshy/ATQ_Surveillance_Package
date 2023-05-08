#' Simulate households without children
#'
#' Simulation of households without children via random uniform distribution using data frames from elementary_pop() and house_children() functions
#'
#'
#' @param df simulated output data frame from subpop_children function
#' @param df2 simulated output data frame from elementary_pop function
#'
#' @return simulated population of households without children
#' @export
#'
#' @examples
#' \dontrun{
#' #simulate catchment area
#' catch_df <- catchment_sim(16, 4.313320, 3.026894, 20)
#'
#' #simulate elementary schools for each area
#' elementary_df <- elementary_pop(catch_df, 5.27426341, 0.01427793)
#'
#' # simulate household with children and assign them to elementary school
#' house_children <- subpop_children(elementary_df)
#'
#' # simulate household with no children and assign them to elementary school
#' house_noChild <- subpop_noChildren(house_children, elementary_df)
#' }
#'
subpop_noChildren <- function(df, df2){

  # connection for standard input from users
  con <- getOption("usr_con", stdin())


  # user input for proportion
  cat("Please enter proportion of households with 1, 2, 3, 4, 5+ members separted by space: ")

  prop_household_size <- scan(con, n = 5, what = double())

  cat("Please enter proportion of households with children:  ")

  prop_household_Children <- scan(con, n = 1, what = double())

  prop_household_size_nochildren <- c(prop_household_size[1]/(1-prop_household_Children)) # proportion households of size 1


  # calculate proportion of households of sizes 2-5
  for(i in 1:4){
    y <- (prop_household_size[i+1]-prop_household_Children*table(df$num_people)[i]/nrow(df))/(1-prop_household_Children)
    prop_household_size_nochildren <- c(prop_household_size_nochildren, y)
  }


  #warning message
  if(round(sum(prop_household_size_nochildren)) != 1){
    warning("Sum of proportions of household without children do not equal 1")
  }


  # Calculate the number of households without children per catchment area.
  # Based on the proportion of households with children.
  catchment_household_count <- data.frame(table(df$catchID))

  names(catchment_household_count) <- c("catchID", "num_household_with_children")

  catchment_household_count$total_households <- round(catchment_household_count$num_household_with_children/prop_household_Children)

  catchment_household_count$num_household_noChildren <- catchment_household_count$total_households - catchment_household_count$num_household_with_children


  #total number of households with no children
  total <- sum(catchment_household_count$num_household_noChildren)


  # simulating household size
  unif_household_size <- stats::runif(total)

  household_size <- ifelse(unif_household_size <= prop_household_size_nochildren[1], 1,
                           ifelse(unif_household_size <= sum(prop_household_size_nochildren[1:2]), 2,
                                  ifelse(unif_household_size <= sum(prop_household_size_nochildren[1:3]), 3,
                                         ifelse(unif_household_size <= sum(prop_household_size_nochildren[1:4]), 4, 5))))

  house_no_children <- data.frame(houseID = nrow(df) + seq.int(total),
                                  num_people = household_size,
                                  catchID = rep(0, total))

  # Assign households without children to catchment areas
  stop <- 0
  for(i in 1:nrow(catchment_household_count)){
    start <- stop + 1
    stop <- cumsum(catchment_household_count$num_household_noChildren)[i]
    house_no_children$catchID[start:stop] <- catchment_household_count$catchID[i]
  }

  house_no_children <- merge(house_no_children, df2[,-2], by="catchID")

  return(house_no_children)

}
