#' Simulate epidemic
#'
#' simulate epidemic
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
#' \dontrun{I need to update this}
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
