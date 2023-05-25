#' Simulate epidemic
#'
#' simulate epidemic
#'
#' @param df dataframe
#' @param b parameter
#' @param sus parameter
#' @param spar parameter
#' @param num_inf parameter
#'
#' @importFrom dplyr %>%
#'
#' @return list of simulated epidemic
#' @export
#'
#' @examples
#' \dontrun{whaever }
sim12epiNew <- function(df, b, sus, spar, num_inf){

  catchID <- individualID <- loc.x <- loc.y <- NULL

  runs <- list()
  num_catchment <- length(unique(df$catchID))

  for(i in 1:10){

    # Select individuals from each catchment area for initial infections.
    #   Number of initial infections per cathcment area = numinf.catch
    sample <- (df %>%
                 dplyr::group_by(catchID) %>%
                 dplyr::slice_sample(n = 2))$individualID

    # Set Initial infection time for the sampled individuals
    #   Start is the "epidemic start time"
    #   Individuals are randomly infected within 14 days after the start time
    first.inf <- rep(0, nrow(df))
    start <- max(round(stats::rnorm(1, mean = 45, sd=15)), 20)
    first.inf[df$individualID %in% sample] <- start + floor(stats::runif(num_inf*num_catchment, 0, 15))

    # simulate epidemic
    SIR <- EpiILM::epidata(type="SIR"
                   , n=nrow(df)
                   , tmin=1
                   , tmax=270
                   , sus.par= sus
                   , beta= b
                   , spark = spar
                   , x=df$loc.x*2
                   , y=df$loc.y*2
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
                     , spark = spar
                     , x=df$loc.x*2
                     , y=df$loc.y*2
                     , inftime = first.inf
                     , infperiod=rep(4, nrow(df)))
    }


    runs[[i]]<- list(XYcoordinates = SIR$XYcoordinates, inftime = SIR$inftime, remtime = SIR$remtime, first.inf = first.inf, start = start)
  }


  return(runs)

}
