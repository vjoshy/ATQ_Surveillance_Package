#' Simulate total households and individuals data
#'
#' Creates two simulated populations, households and individuals
#'
#' @param children_df data frame output from house_children() function
#' @param noChildren_df data frame output from house_noChildren() function
#'
#' @return list of two data frames; simulated individuals data and simulated households data
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
#'  lines <- c(0.77, 0.36, 0.43, 0.21,0.59, 0.31, 0.10, 0.49)
#'  ans <- paste(lines, collapse = "\n")
#'  write(ans, f)
#'  options("usr_con" = f)
#'
#' # simulate household with children and assign them to elementary school
#'  house_children <- subpop_children(elementary_df, n = 2)
#'
#' # Overwriting connection with population proportion values for households without children
#'  lines <- c(0.23, 0.34, 0.17, 0.16, 0.1, 0.43)
#'  ans <- paste(lines, collapse = "\n")
#'  write(ans, f)
#'
#' # simulate household with no children and assign them to elementary school
#'  house_nochildren <- subpop_noChildren(house_children, elementary_df)
#'
#' # close the file
#'  close(f)
#' # reset connection option
#'  options("usr_con" = stdin())
#'
#'  # simulate households and individuals data
#'  simulation <- simulate_households(house_children, house_nochildren)
#'
simulate_households <- function(children_df, noChildren_df){

  noChildren_df$schoolID <- 0
  noChildren_df$num_elem_child <- 0

  households1 <- children_df[,c("houseID", "catchID", "schoolID", "num_people", "num_elem_child", "xStart", "xEnd", "yStart", "yEnd")]
  households2 <- noChildren_df[,c("houseID", "catchID", "schoolID", "num_people", "num_elem_child", "xStart", "xEnd", "yStart", "yEnd")]

  # combining households with and without children
  households <- rbind(households1, households2)

  # Generate house locations within catchment areas
  households$loc.x <- stats::runif(nrow(households), households$xStart, households$xEnd)
  households$loc.y <- stats::runif(nrow(households), households$yStart, households$yEnd)

  # expanding rows for each individual
  individuals <- households[rep(row.names(households), households$num_people),]

  rownames(individuals) <- c() #remove row names
  individuals$individualID <- seq.int(nrow(individuals))

  # Create elementary school child indicator
  individuals$elem_child_ind <- 0
  for(i in unique(individuals$houseID)){
    num_elem <- individuals[which(individuals$houseID == i), "num_elem_child"][1]
    if(num_elem > 0){
      individuals[which(individuals$houseID == i),][1:num_elem, "elem_child_ind"] <- 1
    }
  }

  # TEST: make sure the number of elem.child.ind is equal to the number of elementary school children
  if(sum(individuals$elem_child_ind) != sum(households$num_elem_child)){
    warning("number of elementary school children do not match")
  }

  return(list(household_sim = households, individual_sim = individuals))


}
