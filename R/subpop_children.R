

#' Simulate households with children
#'
#' Simulation of households with children via random uniform distribution. Number of observations is multiplied by a default value of five.
#'
#' @param df simulated output data frame from elementary_pop function
#' @param n population multiplier, default value = 5,
#'
#' @return simulated population of households with children
#' @export
#'
#' @examples
#' \dontrun{#simulate catchment area
#'  catch_df <- catchment_sim(16, 4.313320, 3.026894, 20)
#'
#'  #simulate elementary schools for each area
#'  elementary_df <- elementary_pop(catch_df, 5.27426341, 0.01427793)
#'
#'  # simulate household with children and assign them to elementary school
#'  house_children <- subpop_children(elementary_df)
#'  }
#'
#'
#'
subpop_children <- function(df, n = 5){

  if(n < 1 ){
    stop("Please enter an integer greater than or equal to 1")
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
  cat("Please enter proportion of coupled parents with 1, 2, 3+ children separated by space:")

  prop_children_couple <- scan(con, n = 3, what = double())

  cat("Please enter proportion of single parents with 1, 2, 3+ children separated by space:")

  prop_children_lone <- scan(con, n = 3, what = double())


  # simulating number of children based on parent type
  numChildren <- ifelse(parent_type == 2, # number of children if couple with children
                        ifelse(unif_child_num <= prop_children_couple[1], 1,
                               ifelse(unif_child_num <= sum(prop_children_couple[1:2]),2,3))
                        , ifelse(unif_child_num <= prop_children_lone[1], 1, # number of children if lone-parent with children
                                 ifelse(unif_child_num <= sum(prop_children_lone[1:2]), 2, 3)))


  # user input for proportions
  cat("Please enter proportion of children that are of elementary school age: ")
  prop_elem_age <- scan(con, n = 1, what = double())


  # Simulating whether child is of elementary school age
  child1_elemAge <- ifelse(unif_childAge1 <= prop_elem_age,1,0)
  child2_elemAge <- ifelse(unif_childAge2 <= prop_elem_age,1,0)
  child3_elemAge <- ifelse(unif_childAge3 <= prop_elem_age,1,0)


  # total number of children in each household
  num_elemChild <- ifelse(numChildren == 3, child1_elemAge + child2_elemAge + child3_elemAge
                          , ifelse(numChildren == 2, child1_elemAge + child2_elemAge, child1_elemAge))

  household_W_children <- data.frame(houseID = seq.int(length(parent_type))
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

    temp_house <- household_W_children[(start+1):nrow(household_W_children),] #households remaining to be assigned

    cumul_sum_students <- cumsum(temp_house$num_elem_child) #cumulative sum of elementary aged children yet to be assigned

    stop <- which(cumul_sum_students >= df$schoolPop[i])[1] # find the row in which the school population is satisfied

    if (is.na(stop)){
      stop <- 0
    }

    school_assignment <- c(school_assignment, rep(df$schoolID[i], stop)) # assign all those households to that school
  }

  household_W_children <- household_W_children[1:length(school_assignment),] #keep all assigned households.

  household_W_children$schoolID <- school_assignment


  # check to see if simulated population is similar
  #print(cbind(aggregate(household_W_children$num_elem_child ~ household_W_children$schoolID, FUN="sum"), df$schoolPop))


  # update school populations to include the possible extra 1-2 children
  df$schoolPop <- stats::aggregate(household_W_children$num_elem_child ~ household_W_children$schoolID, FUN="sum")[,2]

  # include catchment area information into household data frame
  household_W_children <- merge(household_W_children, df, by = "schoolID")
  household_W_children$num_people <- household_W_children$num_child + household_W_children$num_parent

  return(household_W_children)
}
