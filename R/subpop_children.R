#' Simulate households with children
#'
#' Simulation of households with children via random uniform distribution.
#' Number of observations is multiplied by a default value of five.
#'
#' @param df simulated output data frame from elementary_pop function
#' @param n population multiplier, default value = 5,
#'
#' @details This is an interactive function where user has to enter eight
#' population proportion values as prompted.To automate entering these prompts,
#' establish a file connection where a vector of the eight population
#' proportion values are written to an open file. Refer to examples section.
#'
#' @return simulated population of households with children
#' @export
#'
#' @examples
#'  # simulate catchment area
#'  catch_df <- catchment_sim(4, 4.30, 3.6, 5)
#'
#'  # simulate elementary schools for each area
#'  elementary_df <- elementary_pop(catch_df, 5.7, 0.014)
#'
#'  # Establish a file connection for population proportion values when
#'  # prompted by subpop_children()
#'  f <- file()
#'
#'  lines <- c(0.77, 0.36, 0.43, 0.21,0.59, 0.31, 0.10, 0.49)
#'
#'  ans <- paste(lines, collapse = "\n")
#'  write(ans, f)
#'
#'  options("usr_con" = f)#'
#'
#'  # simulate household with children and assign them to elementary school
#'  house_children <- subpop_children(elementary_df, n = 3)
#'
#'  # close the file
#'  close(f)
#'
#'  # reset connection option
#'  options("usr_con" = stdin())
#'
#'
subpop_children <- function(df, n = 5){

  if(n <= 1 ){
    stop("Please enter an integer greater than 1")
  }


  # n is multiplier for extra population
  total <- sum(df$schoolPop)*n


  # random uniform distributions for parent types and children categories
  unif_parent_type <- stats::runif(total)
  unif_child_num <- stats::runif(total)
  unif_childAge1 <- stats::runif(total)
  unif_childAge2 <- stats::runif(total)
  unif_childAge3 <- stats::runif(total)

  # setting connection for standard input from users
  con <- getOption("usr_con", stdin())


  # user input for proportions
  cat("Please enter proportion of parents as a couple: ")
  prop_parent_couple <- scan(con, n = 1, what = double())


  # parent type: 2 = coupled parent, 1 = lone parent
  parent_type <- ifelse(unif_parent_type <= prop_parent_couple, 2, 1)


  # user input for proportions
  cat("Enter proportion of coupled parents with 1, 2, 3+ children separated by space:")

  prop_children_couple <- scan(con, n = 3, what = double())

  cat("Enter proportion of single parents with 1, 2, 3+ children separated by space:")

  prop_children_lone <- scan(con, n = 3, what = double())


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
  cat("Please enter proportion of children that are of elementary school age:")
  prop_elem_age <- scan(con, n = 1, what = double())


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
  df$schoolPop <- stats::aggregate(house_children$num_elem_child ~
                                  house_children$schoolID, FUN="sum")[,2]

  # include catchment area information into household data frame
  house_children <- merge(house_children, df, by = "schoolID")

  house_children$num_people <- house_children$num_child +
                                    house_children$num_parent

  return(house_children)
}
