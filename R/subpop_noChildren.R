#' Simulate households without children
#'
#' Simulation of households without children using data frames from elementary_pop() and subpop_children() functions.
#'
#' @param df simulated output data frame from subpop_children function
#' @param df2 simulated output data frame from elementary_pop function
#' @param prop_house_size vector of proportions for households with 1, 2, 3, 4, 5+ members (optional)
#' @param prop_house_Children proportion of households with children (optional)
#' @param house_size vector of random numbers for household size simulation (optional)
#'
#' @details This function can be used interactively or with pre-specified parameters.
#' If proportions are not provided, the user will be prompted to enter them.
#' The function calculates the number of households without children for each catchment area
#' based on the proportion of households with children.
#'
#' @return A data frame representing the simulated population of households without children, including:
#'   \item{catchID}{Catchment area ID}
#'   \item{houseID}{Unique identifier for each household}
#'   \item{num_people}{Number of people in the household}
#'   \item{schoolPop}{Total population of elementary school assigned for the household}
#'   \item{xStart}{Starting X-coordindate for assigned catchment}
#'   \item{xEnd}{End X-coordindate for assigned catchment}
#'   \item{yStart}{Starting Y-coordindate for assigned catchment}
#'   \item{yEnd}{End Y-coordindate for assigned catchment}
#'
#' @export
#'
#' @examples
#' # Simulate catchment areas
#' catch_df <- catchment_sim(16, 5, shape = 3.5, rate = 2.8)
#'
#' # Simulate elementary schools using default gamma distribution
#' elementary_df <- elementary_pop(catch_df, shape = 5.1, rate = 0.015)
#'
#' # Simulate households with children
#' house_children <- subpop_children(elementary_df, n = 3,
#'                                   prop_parent_couple = 0.7,
#'                                   prop_children_couple = c(0.3, 0.5, 0.2),
#'                                   prop_children_lone = c(0.4, 0.4, 0.2),
#'                                   prop_elem_age = 0.6)
#'
#' # Simulate households without children using pre-specified proportions
#' house_noChild <- subpop_noChildren(house_children, elementary_df,
#'                                    prop_house_size = c(0.2, 0.3, 0.25, 0.15, 0.1),
#'                                    prop_house_Children = 0.3)
#'

subpop_noChildren <- function(df, df2,
                              prop_house_size = NULL,
                              prop_house_Children = NULL,
                              house_size = NULL){

  # connection for standard input from users
  con <- getOption("usr_con", stdin())


  # user input for proportion
  if(is.null(prop_house_size)){
    cat("Please enter proportion of households with 1, 2, 3, 4, 5+ members separted by space: ")
    prop_house_size <- scan(con, n = 5, what = double())
  }

  if(is.null(prop_house_Children)){
    cat("Please enter proportion of households with children:  ")
    prop_house_Children <- scan(con, n = 1, what = double())
  }

  prop_house_size_nochildren <- c(prop_house_size[1]/(1-prop_house_Children))


  # calculate proportion of households of sizes 2-5
  for(i in 1:4){

    y <- (prop_house_size[i+1] -
            prop_house_Children*table(df$num_people)[i]/nrow(df))/
            (1-prop_house_Children)

    prop_house_size_nochildren <- c(prop_house_size_nochildren, y)
  }


  #warning message
  if(round(sum(prop_house_size_nochildren)) != 1){
    warning("Sum of proportions of household without children do not equal 1")
  }


  # Calculate the number of households without children per catchment area.
  # Based on the proportion of households with children.
  catch_house_count <- data.frame(table(df$catchID))

  names(catch_house_count) <- c("catchID",
                                        "num_house_with_children")

  catch_house_count$total <- round(catch_house_count$num_house_with_children/
                                     prop_house_Children)

  catch_house_count$num_house_noChildren <- catch_house_count$total-
                  catch_house_count$num_house_with_children


  #total number of households with no children
  total <- sum(catch_house_count$num_house_noChildren)


  # simulating household size if not specified
  if(is.null(house_size)){
    house_size <- stats::runif(total)
  }

  household_size <- ifelse(house_size <= prop_house_size_nochildren[1],1,
              ifelse(house_size <= sum(prop_house_size_nochildren[1:2]),
              2,ifelse(house_size <= sum(prop_house_size_nochildren[1:3]),
              3,ifelse(house_size <= sum(prop_house_size_nochildren[1:4]),
              4, 5))))

  house_no_children <- data.frame(houseID = nrow(df) + seq.int(total),
                                  num_people = household_size,
                                  catchID = rep(0, total))

  # Assign households without children to catchment areas
  stop <- 0
  for(i in 1:nrow(catch_house_count)){

    start <- stop + 1
    stop <- cumsum(catch_house_count$num_house_noChildren)[i]

    house_no_children$catchID[start:stop] <- catch_house_count$catchID[i]
  }

  house_no_children <- merge(house_no_children, df2[,-2], by="catchID")

  return(house_no_children)

}
