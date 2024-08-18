#' Simulate households with children
#'
#' Simulation of households with children using specified random distributions.
#' Number of observations is multiplied by a default value of five.
#'
#' @param df simulated output data frame from elementary_pop function
#' @param n population multiplier, default value = 5
#' @param prop_parent_couple proportion of parents as a couple (optional)
#' @param prop_children_couple vector of proportions for coupled parents with 1, 2, 3+ children (optional)
#' @param prop_children_lone vector of proportions for single parents with 1, 2, 3+ children (optional)
#' @param prop_elem_age proportion of children that are of elementary school age (optional)
#' @param parent_dist distribution function for parent type, default is stats::runif
#' @param child_dist distribution function for number of children, default is stats::runif
#' @param age_dist distribution function for child age, default is stats::runif
#' @param ... additional arguments passed to the distribution functions
#'
#' @details This function can be used interactively or with pre-specified parameters.
#' If proportions are not provided, the user will be prompted to enter them.
#' Custom distribution functions can be specified for parent type, number of children, and child age.
#' The total number of simulations (n * sum of school populations) is automatically passed as the first argument to each distribution function.
#'
#' @return A data frame representing the simulated population of households with children, including:
#'   \item{schoolID}{Assigned school ID for the household}
#'   \item{houseID}{Unique identifier for each household}
#'   \item{num_parent}{Number of parents in the household (1 or 2)}
#'   \item{num_child}{Total number of children in the household}
#'   \item{num_elem_child}{Number of elementary school-aged children in the household}
#'   \item{catchID}{Assigned catchment ID for the household}
#'   \item{schoolPop}{Total population of elementary school assigned for the household}
#'   \item{xStart}{Starting X-coordindate for assigned catchment}
#'   \item{xEnd}{End X-coordindate for assigned catchment}
#'   \item{yStart}{Starting Y-coordindate for assigned catchment}
#'   \item{yEnd}{End Y-coordindate for assigned catchment}
#'   \item{schoolID}{Assigned school ID for the household}
#'   \item{num_people}{Total number of people in the household}
#'
#' @export
#'
#' @examples
#' # Simulate catchment area
#' catch_df <- catchment_sim(4, 5, shape = 2.5, rate = 1.3)
#'
#' # Simulate elementary schools using default gamma distribution
#' elementary_df <- elementary_pop(catch_df, shape = 3.3, rate = 0.015)
#'
#' # Simulate households with children
#' house_children <- subpop_children(elementary_df, n = 2,
#'                                   prop_parent_couple = 0.7,
#'                                   prop_children_couple = c(0.3, 0.5, 0.2),
#'                                   prop_children_lone = c(0.4, 0.4, 0.2),
#'                                   prop_elem_age = 0.6)
#'
#' # Using custom distributions
#' house_children2 <- subpop_children(elementary_df, n = 3,
#'                                   prop_parent_couple = 0.7,
#'                                   prop_children_couple = c(0.3, 0.5, 0.2),
#'                                   prop_children_lone = c(0.4, 0.4, 0.2),
#'                                   prop_elem_age = 0.6,
#'                                   parent_dist = stats::rnorm, mean = 0.5, sd = 0.1,
#'                                   child_dist = stats::rbeta, shape1 = 2, shape2 = 2,
#'                                   age_dist = stats::runif)
#'
subpop_children <- function(df, n = 5,
                            prop_parent_couple = NULL,
                            prop_children_couple = NULL,
                            prop_children_lone = NULL,
                            prop_elem_age = NULL,
                            parent_dist = stats::runif,
                            child_dist = stats::runif,
                            age_dist = stats::runif,
                            ...) {

  if(n <= 1 ){
    stop("Please enter an integer greater than 1")
  }


  # n is multiplier for extra population
  total <- sum(df$schoolPop) * n


  # Collect all additional arguments
  args <- list(...)

  # Function to apply distribution with total as first argument
  apply_dist <- function(dist_func, args) {
    dist_args <- args[names(args) %in% names(formals(dist_func))]
    do.call(dist_func, c(list(n = total), dist_args))
  }


  # use provided distributions for parent types and children categories
  unif_parent_type <- apply_dist(parent_dist, args)
  unif_child_num <- apply_dist(child_dist, args)
  unif_childAge1 <- apply_dist(age_dist, args)
  unif_childAge2 <- apply_dist(age_dist, args)
  unif_childAge3 <- apply_dist(age_dist, args)

  # setting connection for standard input from users
  con <- getOption("usr_con", stdin())


  # user input for proportions
  if (is.null(prop_parent_couple)) {
    cat("Please enter proportion of parents as a couple: ")
    prop_parent_couple <- scan(con, n = 1, what = double())
  }


  # parent type: 2 = coupled parent, 1 = lone parent
  parent_type <- ifelse(unif_parent_type <= prop_parent_couple, 2, 1)


  # user input for proportions
  if (is.null(prop_children_couple)) {
    cat("Enter proportion of coupled parents with 1, 2, 3+ children separated by space:")
    prop_children_couple <- scan(con, n = 3, what = double())
  }


  if (is.null(prop_children_lone)) {
    cat("Enter proportion of single parents with 1, 2, 3+ children separated by space:")
    prop_children_lone <- scan(con, n = 3, what = double())
  }


  # simulating number of children based on parent type

  # number of children if couple with children
  numChildren <- ifelse(parent_type == 2,
                    ifelse(unif_child_num <= prop_children_couple[1], 1,
                      ifelse(unif_child_num <= sum(prop_children_couple[1:2]),
                             2,3)),
                    # number of children if lone-parent with children
                       ifelse(unif_child_num <= prop_children_lone[1], 1,
                          ifelse(unif_child_num <= sum(prop_children_lone[1:2]),
                                 2, 3)))


  # user input for proportions
  if (is.null(prop_elem_age)) {
    cat("Please enter proportion of children that are of elementary school age:")
    prop_elem_age <- scan(con, n = 1, what = double())
  }


  # Simulating whether child is of elementary school age
  child1_elemAge <- ifelse(unif_childAge1 <= prop_elem_age,1,0)
  child2_elemAge <- ifelse(unif_childAge2 <= prop_elem_age,1,0)
  child3_elemAge <- ifelse(unif_childAge3 <= prop_elem_age,1,0)


  # total number of children in each household
  num_elemChild <- ifelse(numChildren == 3,
                     child1_elemAge + child2_elemAge + child3_elemAge,
                      ifelse(numChildren == 2, child1_elemAge + child2_elemAge,
                             child1_elemAge))

  house_children <- data.frame(houseID = seq.int(length(parent_type))
                                     , num_parent = parent_type
                                     , num_child = numChildren
                                     , num_elem_child = num_elemChild
                                     , schoolID = rep(0, length(parent_type)))

  # Assign elementary aged children to elementary schools
  #     such that the school population is met
  start <- 0
  stop <-0
  school_assignment <- c()

  for(i in 1:nrow(df)){

    start <- start + stop

    #households remaining to be assigned
    temp_house <- house_children[(start+1):nrow(house_children),]

    #cumulative sum of elementary aged children yet to be assigned
    cumul_sum_students <- cumsum(temp_house$num_elem_child)

    # find the row in which the school population is satisfied
    stop <- which(cumul_sum_students >= df$schoolPop[i])[1]

    if (is.na(stop)){
      stop <- 0
    }

    # assign all those households to that school
    school_assignment <- c(school_assignment, rep(df$schoolID[i], stop))
  }


  house_children <- house_children[1:length(school_assignment),]

  house_children$schoolID <- school_assignment


  # update school populations to include the possible extra 1-2 children
  # in case there aren't any students assigned to a schoolwwwwwww
  school_pops <- stats::aggregate(house_children$num_elem_child ~ house_children$schoolID, FUN="sum")
  df$schoolPop <- ifelse(df$schoolID %in% school_pops[,1],
                         school_pops[match(df$schoolID, school_pops[,1]), 2],
                         0)

  # include catchment area information into household data frame
  house_children <- merge(house_children, df, by = "schoolID")

  house_children$num_people <- house_children$num_child +
                                    house_children$num_parent

  return(house_children)
}
