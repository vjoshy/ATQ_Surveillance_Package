#' Simulate total households and individuals data
#'
#' Creates two simulated populations, households and individuals
#'
#' @param children_df data frame output from house_children() function
#' @param noChildren_df data frame output from house_noChildren() function
#'
#' @return list of two data frames; simulated individuals data
#' and simulated households data
#' @export
#'
#' @examples
#' # Simulate catchment areas
#' catch_df <- catchment_sim(4, 5, shape = 2.5, rate = 1.8)
#'
#' # Simulate elementary schools using default gamma distribution
#' elementary_df <- elementary_pop(catch_df, shape = 4.1, rate = 0.019)
#'
#' # Simulate households with children
#' house_children <- subpop_children(elementary_df, n = 2,
#'                                   prop_parent_couple = 0.7,
#'                                   prop_children_couple = c(0.3, 0.5, 0.2),
#'                                   prop_children_lone = c(0.4, 0.4, 0.2),
#'                                   prop_elem_age = 0.2)
#'
#' # Simulate households without children using pre-specified proportions
#' house_nochildren <- subpop_noChildren(house_children, elementary_df,
#'                                    prop_house_size = c(0.2, 0.3, 0.25, 0.15, 0.1),
#'                                    prop_house_Children = 0.3)
#'
#' # simulate households and individuals data
#' simulation <- simulate_households(house_children, house_nochildren)
#'
simulate_households <- function(children_df, noChildren_df){

  noChildren_df$schoolID <- 0
  noChildren_df$num_elem_child <- 0

  households1 <- children_df[,c("houseID", "catchID", "schoolID", "num_people",
                                "num_elem_child", "xStart", "xEnd",
                                "yStart", "yEnd")]

  households2 <- noChildren_df[,c("houseID", "catchID", "schoolID",
                                  "num_people", "num_elem_child", "xStart",
                                  "xEnd", "yStart", "yEnd")]

  # combining households with and without children
  households <- rbind(households1, households2)

  # Generate house locations within catchment areas
  households$loc.x <- stats::runif(nrow(households),
                                   households$xStart, households$xEnd)

  households$loc.y <- stats::runif(nrow(households),
                                   households$yStart, households$yEnd)

  # expanding rows for each individual
  individuals <- households[rep(row.names(households), households$num_people),]

  rownames(individuals) <- c() #remove row names
  individuals$individualID <- seq.int(nrow(individuals))

  # Create elementary school child indicator
  individuals$elem_child_ind <- 0

  for(i in unique(individuals$houseID)){
    num_elem <- individuals[which(individuals$houseID == i),
                            "num_elem_child"][1]

    if(num_elem > 0){
      individuals[which(individuals$houseID == i),][1:num_elem,
                                                    "elem_child_ind"] <- 1
    }
  }

  # TEST: make sure the number of elem.child.ind is equal to the
  #  number of elementary school children
  if(sum(individuals$elem_child_ind) != sum(households$num_elem_child)){
    warning("number of elementary school children do not match")
  }

  return(list(household_sim = households, individual_sim = individuals))
}
