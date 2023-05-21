library(EpiILM)
library(splitstackshape)

sim12epiNew <- function(b, sus, spar, num_inf){

  runs <- list()
  num_catchment <- length(unique(individuals$catchID))

  for(i in 1:10){

    # Select individuals from each catchment area for initial infections.
    #   Number of initial infections per cathcment area = numinf.catch
    sample <- stratified(individuals, "catchID", size = num_inf)$individualID

    # Set Initial infection time for the sampled individuals
    #   Start is the "epidemic start time"
    #   Individuals are randomly infected within 14 days after the start time
    first.inf <- rep(0, nrow(individuals))
    start <- max(round(rnorm(1, mean = 45, sd=15)), 20)
    first.inf[individuals$individualID %in% sample] <- start + floor(runif(num_inf*num_catchment, 0, 15))

    # simulate epidemic
    SIR <- epidata(type="SIR"
                   , n=nrow(individuals)
                   , tmin=1
                   , tmax=270
                   , sus.par= sus
                   , beta= b
                   , spark = spar
                   , x=individuals$loc.x*2
                   , y=individuals$loc.y*2
                   , inftime = first.inf
                   , infperiod=rep(4, nrow(individuals)))


    # calculate infection rate by taking ratio of sum of infection time to
    # total number of infections
    infection.rate <- sum(SIR$inftime > 0) / length(SIR$inftime)

    if(infection.rate <= 0.02){

      SIR <- epidata(type="SIR"
                     , n=nrow(individuals)
                     , tmin=1
                     , tmax=270
                     , sus.par= sus
                     , beta= b
                     , spark = spar
                     , x=individuals$loc.x*2
                     , y=individuals$loc.y*2
                     , inftime = first.inf
                     , infperiod=rep(4, nrow(individuals)))
    }


    runs[[i]]<- list(XYcoordinates = SIR$XYcoordinates, inftime = SIR$inftime, remtime = SIR$remtime, first.inf = first.inf, start = start)
  }


  return(runs)

}
