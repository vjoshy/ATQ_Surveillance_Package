#' Simulate households without children
#'
#' Simulation of households without children via random uniform distribution using data frames from elementary_pop() and house_children() functions
#'
#'
#' @param df simulated output data frame from subpop_children function
#' @param df2 simulated output data frame from elementary_pop function
#'
#' @details This is an interactive function where user has to enter 6 population proportion values as prompted.
#' To automate entering these prompts, establish a file connection where a vector of the eight population proportion values are written to an open file. Refer to examples section.
#'
#' @return simulated population of households without children
#' @export
#'
#' @examples
#' # simulate catchment area
#'  catch_df <- catchment_sim(8, 4.30, 3.6, 5)
#'
#' # simulate elementary schools for each area
#'  elementary_df <- elementary_pop(catch_df, 5.7, 0.014)
#'
#' # Establish a file connection for population proportion values when prompted by subpop_children()
#'  f <- file()
#'  lines <- c(0.7668901,0.3634045, 0.4329440, 0.2036515,0.5857832, 0.3071523, 0.1070645,0.4976825)
#'  ans <- paste(lines, collapse = "\n")
#'  write(ans, f)
#'  options("usr_con" = f)#'
#'
#' # simulate household with children and assign them to elementary school
#'  house_children <- subpop_children(elementary_df)
#'
#' # Overwriting connection with population proportion values for households without children
#'  lines <- c(0.23246269, 0.34281716, 0.16091418, 0.16427239, 0.09953358, 0.4277052)
#'  ans <- paste(lines, collapse = "\n")
#'  write(ans, f)
#'
#' # simulate household with no children and assign them to elementary school
#'  house_noChild <- subpop_noChildren(house_children, elementary_df)
#'
#' # close the file
#'  close(f)
#' # reset connection option
#'  options("usr_con" = stdin())
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
