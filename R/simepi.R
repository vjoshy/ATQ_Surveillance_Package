#' Simulate epidemics
#'
#' This function simulates epidemics under a Susceptible-Infectious-Removed (SIR) compartment framework. The time period for simulation is 270 days
#'
#' @param df individuals data frame from household data simulation
#' @param b Spatial parameter(s) (>0) or network parameter (s) (>0) if contact network is used.
#' @param sus Susceptibility parameter (>0)
#' @param spark Sparks parameter (>=0), representing infections unexplained by other parts of the model.
#' @param num_inf number of individuals that are infected initially in each catchment.
#' @param rep number of replications of epidemics, default is 10.
#'
#' @importFrom dplyr %>%
#'
#' @return List of simulated epidemics
#' @export
#'
#' @examples
#' #Simulate catchment data
#' catch_df <- catchment_sim(4, 1.2, 3.01, 5)
#'
#' #simulate elementary schools for each area
#' elementary_df <- elementary_pop(catch_df, 0.5, 0.015)
#'
#' # Enters values for prompts from subpop_children() function
#' f <- file()
#' lines <- c(0.7668901,0.3634045, 0.4329440, 0.2036515,0.5857832, 0.3071523, 0.1070645,0.4976825)
#' ans <- paste(lines, collapse = "\n")
#' write(ans, f)
#' options("usr_con" = f) # set connection option
#'
#' # simulating households without children
#' house_children <- subpop_children(elementary_df, n = 2)
#'
#' # Enters values for prompts from subpop_nochildren() function
#' lines <- c(0.23246269, 0.34281716, 0.16091418, 0.16427239, 0.09953358, 0.4277052)
#' ans <- paste(lines, collapse = "\n")
#' write(ans, f)
#'
#' # simulate households and individuals data
#' house_nochildren <- subpop_noChildren(house_children, elementary_df)
#'
#' close(f) # close the file
#' options("usr_con" = stdin()) # reset connection option
#'
#' # simulate households and individuals data
#' simulation <- simulate_households(house_children, house_nochildren)
#'
#' # simulate epidemic
#' epidemic <- simepi(simulation$individual_sim, b=3, sus=.0019, spark=0, num_inf = 2, rep = 4)
#'
simepi <- function(df, b, sus, spark, num_inf, rep = 10){

  catchID <- individualID <- loc.x <- loc.y <- NULL

  runs <- list()
  num_catchment <- length(unique(df$catchID))

  for(i in 1:rep){

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
