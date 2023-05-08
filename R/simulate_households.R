#' Simulate total households and individuals data
#'
#' Creates two simulated populations, households and individuals
#'
#' @param children_df data frame output from house_children() function
#' @param noChildren_df data frame output from house_noChildren() function
#'
#' @return list of two data frames
#' @export
#'
#' @examples
simulate_households <- function(children_df, noChildren_df){

  noChildren_df$schoolID <- 0
  noChildren_df$num_elem_child <- 0

  households1 <- children_df[,c("houseID", "catchID", "schoolID", "num_people", "num_elem_child", "xStart", "xEnd", "yStart", "yEnd")]
  households2 <- noChildren_df[,c("houseID", "catchID", "schoolID", "num_people", "num_elem_child", "xStart", "xEnd", "yStart", "yEnd")]

  # combining households with and without children
  households <- rbind(households1, households2)

  # Generate house locations within catchment areas
  households$loc.x <- runif(nrow(households), households$xStart, households$xEnd)
  households$loc.y <- runif(nrow(households), households$yStart, households$yEnd)

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
