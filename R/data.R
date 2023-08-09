#' ATQ Data
#'
#' @name data
NULL

#' Simulated catchment area data
#'
#' A data frame simulated from code catchment_sim(16, 0.5, 1.5, 5)
#'
#'  \itemize{
#'    \item catchID, ID associated with each catchment
#'    \item num.schools, Number of schools in each catchment
#'    \item xStart, Start x coordinate of catchment grid
#'    \item xEnd, End x coordinate of catchment grid
#'    \item yStart, Start y coordinate of catchment grid
#'    \item yEnd, End y coordinate of catchment grid
#'}
#'
#' @docType data
#' @name catchment
#' @usage data(catchment)
#' @format A data frame with 16 rows and 6 variables
NULL

#' Simulated elementary school size data
#'
#' A data frame simulated from code elementary_pop(catchment, 0.5, 0.015)
#'
#'  \itemize{
#'    \item catchID,ID associated with each catchment
#'    \item schoolID, ID associated with schools in each catchment
#'    \item schoolPop, Population of students in each school generated from a gamma distribution
#'    \item xStart, Start x coordinate of catchment grid
#'    \item xEnd, End x coordinate of catchment grid
#'    \item yStart, Start y coordinate of catchment grid
#'    \item yEnd, End y coordinate of catchment grid
#'}
#'
#' @docType data
#' @name elementary
#' @usage data(elementary)
#' @format A data frame with 18 rows and 7 variables
NULL

#' Simulated households with children data
#'
#' A data frame simulated from code subpop_children(elementary)
#'     Proportion of parents as a couple = 0.7668901
#'     proportion of coupled parents with 1, 2, 3+ children = 0.3634045, 0.4329440, 0.2036515
#'     proportion of single parents with 1, 2, 3+ children = 0.5857832, 0.3071523, 0.1070645
#'     proportion of children that are of elementary school age = 0.4976825
#'
#'  \itemize{
#'    \item schoolID, ID associated with schools in each catchment
#'    \item houseID, ID associated with each household
#'    \item num_parent, Number of parents in each household
#'    \item num_child, Number of children in each household
#'    \item num_elem_child, Number of children who are of elementary school age
#'    \item catchID, ID associated with each catchment
#'    \item schoolPop, Population of students in each school generated from a gamma distribution
#'    \item xStart, Start x coordinate of catchment grid
#'    \item xEnd, End x coordinate of catchment grid
#'    \item yStart, Start y coordinate of catchment grid
#'    \item yEnd, End y coordinate of catchment grid
#'    \item num_people, Total number of people in each household
#'}
#'
#' @docType data
#' @name house_children
#' @usage data(house_children)
#' @format A data frame with 874 rows and 12 variables
NULL

#' Simulated households with no children data
#'
#' A data frame simulated from code subpop_noChildren(house_children, elementary)
#'     proportion of households with 1, 2, 3, 4, 5+ members = 0.23246269, 0.34281716, 0.16091418, 0.16427239, 0.09953358
#'     proportion of households with children = 0.4277052
#'
#'  \itemize{
#'    \item catchID, ID associated with each catchment
#'    \item houseID, ID associated with each household
#'    \item num_people, Total number of people in each household
#'    \item schoolPop, Population of students in each school generated from a gamma distribution
#'    \item xStart, Start x coordinate of catchment grid
#'    \item xEnd, End x coordinate of catchment grid
#'    \item yStart, Start y coordinate of catchment grid
#'    \item yEnd, End y coordinate of catchment grid
#'}
#'
#' @docType data
#' @name house_nochildren
#' @usage data(house_nochildren)
#' @format A data frame with 1,386 rows and 8 variables
NULL

#' Simulated individuals data
#'
#' A data frame simulated from code simulate_households(house_children, house_nochildren)
#' individuals is one of two data frames returned within a list from the simulate_household() function
#'
#'  \itemize{
#'    \item houseID, ID associated with each household
#'    \item catchID, ID associated with each catchment
#'    \item school, ID, ID associated with schools in each catchment
#'    \item num_people, Total number of people in each household
#'    \item num_elem_child, Number of children who are of elementary school age
#'    \item schoolPop, Population of students in each school generated from a gamma distribution
#'    \item xStart, Start x coordinate of catchment grid
#'    \item xEnd, End x coordinate of catchment grid
#'    \item yStart, Start y coordinate of catchment grid
#'    \item yEnd, End y coordinate of catchment grid
#'    \item loc.x, x coordinate of household
#'    \item loc.y, y coordinate of household
#'    \item individualID, ID associated with each individual
#'    \item elem_child_ind, binary variable indicating whether individual is of elementary school age
#'}
#'
#' @docType data
#' @name individuals
#' @usage data(individuals)
#' @format A data frame with 5,599 rows and 13 variables
NULL

#' Simulated epidemics data
#'
#' A list of 10 lists simulated from code simepi(individuals, b=3, sus=.0019, spark=0, num_inf = 2)
#' Simulated through epidata() from EpiILM package. Each data frame follows the format below:
#'
#'  \itemize{
#'    \item XYcoordinates, A data frame of x and y coordinates associated with each household
#'    \item inftime, Day in which each individual got infected
#'    \item remtime, Day in which each individual is removed from infection
#'    \item start, Day epidemic started
#'}
#'
#' @docType data
#' @name epidemic
#' @usage data(epidemic)
#' @format A data frame with 5,599 rows and 13 variables
NULL

#' Simulated region data
#'
#' A data frame simulated from code model_data(epidemic, individuals)
#' region is one of two data frames contained within output of model_data() function.
#' region contains information about confirmed cases for the region.
#'
#'  \itemize{
#'    \item Date, Day (1-300)
#'    \item ScYr, School year
#'    \item Actual.case, Total number of true cases (unconfirmed)
#'    \item case.elem, Number of true cases that are elementary school children
#'    \item pct.absent, Percent of students absent in total school population
#'    \item absent, Total number of students absent
#'    \item absent.sick, Total number of students absent due to illness
#'    \item Case.No, Number of lab confirmed cases
#'    \item Case, Binary variable to indicate whether there has been a lab confirmed case for the day
#'    \item sinterm, Seasonal sine term
#'    \item costerm, Seasonal cosine term
#'    \item window, 14 day true alarm window for ADD and FAR calculations (binary variable)
#'    \item ref.date, reference date (first day where there are two lab confirmed cases in a 7 day period)
#'    \item lag0, lag1, ..., lag15, Lagged absenteeism values
#'}
#'
#' @docType data
#' @name region
#' @usage data(region)
#' @format A data frame with 3,000 rows and 29 variables
NULL
