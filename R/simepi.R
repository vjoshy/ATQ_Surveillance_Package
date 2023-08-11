#' Simulate epidemic
#'
#' This function under Susceptible-Infectious-Removed (SIR) compartment framework. The time period for simulation is 270 days
#'
#' @param df individuals data frame from household data simulation
#' @param b Spatial parameter(s) (>0) or network parameter (s) (>0) if contact network is used.
#' @param sus Susceptibility parameter (>0)
#' @param spark Sparks parameter (>=0), representing infections unexplained by other parts of the model.
#' @param num_inf number of individuals that are infected initially in each catchment
#'
#' @importFrom dplyr %>%
#'
#' @return list of 10 simulated epidemics
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
#'  options("usr_con" = f)#'
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
#' # simulate households and individuals data
#'  simulation <- simulate_households(house_children, house_nochildren)
#'
#' # randomly sampling 1000 rows to reduce simulation times
#'  individuals <- simulation$individual_sim[sample(nrow(simulation$individual_sim),1000),]
#'
#'  # simulate epidemic
#'  epidemic <- simepi(individuals, b=3, sus=.0019, spark=0, num_inf = 2)
#'
simepi <- function(df, b, sus, spark, num_inf){

  catchID <- individualID <- loc.x <- loc.y <- NULL

  runs <- list()
  num_catchment <- length(unique(df$catchID))

  for(i in 1:10){

    #   Select individuals from each catchment area for initial infections.
    #   Number of initial infections per catchment area = numinf.catch
    sample <- (df %>%
                 dplyr::group_by(catchID) %>%
                 dplyr::slice_sample(n = 2))$individualID

    #   Set Initial infection time for the sampled individuals
    #   Start is the "epidemic start time"
    #   Individuals are randomly infected within 14 days after the start time
    first.inf <- rep(0, nrow(df))

    # mean and standard deviation can change with infection types
    # infection period can also change
    start <- max(round(stats::rnorm(1, mean = 45, sd=15)), 20)
    first.inf[df$individualID %in% sample] <- start + floor(stats::runif(num_inf*num_catchment, 0, 15))

    # simulate epidemic
    # epidata parameters will also have to be included in function arguments
    SIR <- EpiILM::epidata(type="SIR"
                   , n=nrow(df)
                   , tmin=1
                   , tmax=270
                   , sus.par= sus
                   , beta= b
                   , spark = spark
                   , x=df$loc.x
                   , y=df$loc.y
                   , inftime = first.inf
                   , infperiod=rep(4, nrow(df)))


    # calculate infection rate by taking ratio of sum of infection time to
    # total number of infections
    infection.rate <- sum(SIR$inftime > 0) / length(SIR$inftime)

    if(infection.rate <= 0.02){

      SIR <- EpiILM::epidata(type="SIR"
                     , n=nrow(df)
                     , tmin=1
                     , tmax=270
                     , sus.par= sus
                     , beta= b
                     , spark = spark
                     , x=df$loc.x
                     , y=df$loc.y
                     , inftime = first.inf
                     , infperiod=rep(4, nrow(df)))
    }


    runs[[i]]<- list(XYcoordinates = SIR$XYcoordinates, inftime = SIR$inftime, remtime = SIR$remtime, start = start)
  }


  return(runs)

}
