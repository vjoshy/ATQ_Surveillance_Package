#' Simulate laboratory confirmed cases and school absenteeism data
#'
#' Simulates lab confirmed cases and absenteeism information using epidemics
#' and individuals data. Simulation can be performed at the regional
#' or catchment level.
#'
#' @param epi.series Simulated epidemic data frame
#'  (output from simepi() function)
#' @param individual.data Simulated individuals data frame
#' (output from simulate_households() function)
#' @param  type Type of analysis (regional or catchment)
#' @param no.lags  Maximum number of lags, default = 16 (note that lag of zero 0 is included)
#'
#' @importFrom stats aggregate filter runif time
#' @importFrom zoo rollapply
#' @importFrom dplyr mutate rename
#'
#' @return returns data frame
#' @export
#'
#' @examples
#' #Simulate catchment data
#' catch_df <- catchment_sim(4, 4.12, 3.01, 5)
#'
#' #simulate elementary schools for each area
#' elementary_df <- elementary_pop(catch_df, 4.8, 0.015)
#'
#' # Enters values for prompts from subpop_children() function
#' f <- file()
#' lines <- c(0.7668901,0.3634045, 0.4329440, 0.2036515, 0.5857832,
#'               0.3071523, 0.1070645,0.4976825)
#'
#' ans <- paste(lines, collapse = "\n")
#' write(ans, f)
#' options("usr_con" = f) # set connection option
#'
#' # simulating households without children
#' house_children <- subpop_children(elementary_df, n = 2)
#'
#' # Enters values for prompts from subpop_nochildren() function
#' lines <- c(0.23246269, 0.34281716, 0.16091418, 0.16427239,
#'                0.09953358, 0.4277052)
#'
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
#' # randomly sampling 1000 rows to reduce simulation times
#' individuals <- simulation$individual_sim[sample(nrow(
#'                                           simulation$individual_sim),200),]
#'
#' # simulate epidemic
#' epidemic <- simepi(individuals, b=3, sus=.0019, spark=0,
#'                      num_inf = 2, rep = 1)
#'
#' # simulate laboratory confirmed cases, and school absenteeism data sets
#' data <- model_data(epidemic, individuals, type = "r")
#'
model_data <- function(epi.series, individual.data, type = 'r', no.lags = 16){

  #catchment area datasets
  actual.cases.catch <- data.frame(time=c(), catchID = c(), case.no = c(),
                                   case.elem = c(), ScYr = c())

  absent.catch <- data.frame(time = c(), catchID = c(), pct.absent = c(),
                             absent = c(), absent.sick = c(), ScYr = c())

  labconf.catch <- data.frame(time = c(), catchID = c(), labconf = c(),
                              ScYr = c())

  #Region-wide datasets
  actual.cases.region <- data.frame(time=c(), case.no = c(),
                                    case.elem = c(), ScYr = c())

  absent.region <- data.frame(time = c(), pct.absent = c(), absent = c(),
                              absent.sick = c(), ScYr = c())

  labconf.region <- data.frame(time = c(), labconf = c(), ScYr = c())

  #create dataframes consisting of all years
  for(i in 1:length(epi.series)){
    # catchment datasets
    # Actual cases (both lab confirmed, and non lab confirmed)
    #   This is not needed for modelling, but is good additional information
    actual.cases <- sim.actual.case(epi.series[[i]], individual.data)
    actual.cases <- actual.cases[actual.cases$time >0,]
    actual.cases$ScYr <- i
    actual.cases.catch <- rbind(actual.cases.catch, actual.cases)

    # Laboratory confirmed cases
    lab.conf <- sim.lab.confirm(epi.series[[i]], individual.data)
    lab.conf$ScYr <- i
    labconf.catch <- rbind(labconf.catch, lab.conf)

    # Absenteeism
    absent <- sim.absent(epi.series[[i]], individual.data)
    absent$ScYr <- i
    absent.count <- aggregate(cbind(absent, absent.sick) ~ time + catchID +
                                ScYr, data=absent, FUN=sum)

    absent.pct <- aggregate(pct.absent ~ time + catchID + ScYr,
                            data = absent, FUN=mean)

    absent.merge <- merge(absent.pct, absent.count,
                          by = c("time", "catchID", "ScYr"))

    absent.catch <- rbind(absent.catch, absent.merge)

    # Region datasets
    # Actual cases (both lab confirmed, and non lab confirmed)
    #   This is not needed for modelling, but is good additional information
    actual.cases <- aggregate(cbind(case, case.elem) ~ time + ScYr,
                              data=actual.cases, FUN = sum)

    actual.cases.region <- rbind(actual.cases.region, actual.cases)

    # Laboratory confirmed cases
    lab.conf <- aggregate(labconf ~ time + ScYr, data = lab.conf, FUN = sum)
    labconf.region <- rbind(labconf.region, lab.conf)

    # Absenteeism
    absent.count <- aggregate(cbind(absent, absent.sick) ~ time + ScYr,
                              data=absent, FUN = sum)
    absent.pct <- aggregate(pct.absent ~ time + ScYr,
                            data=absent, FUN = mean)
    absent.merge <- merge(absent.pct, absent.count,
                          by = c("time", "ScYr"))
    absent.region <- rbind(absent.region, absent.merge)
  }


  if(type == 'r'){

    # Region wide datasets
    #   Combine lab confirmed cases, absenteeism and actual cases
    master.region <- merge(actual.cases.region, absent.region,
                           by=c("time", "ScYr"), all=TRUE)

    master.region <- merge(master.region, labconf.region,
                           by=c("time", "ScYr"), all=TRUE)

    master.region <- rename(master.region, c("Actual.case" = "case",
                                      "Case.No" = "labconf", "Date" = "time"))

    # indicator for lab confirmed flu case that day
    master.region$Case <- 1*(master.region$Case.No > 0)

    # seasonal terms
    master.region$sinterm <- sin((2*pi*master.region$Date)/365.25)
    master.region$costerm <- cos((2*pi*master.region$Date)/365.25)

    # "True Alarm" window (to be calculated)
    master.region$window <- 0

    # reference date indicator (to be calculated)
    master.region$ref.date <- 0

    master.region <- master.region[order(master.region$ScYr,
                                         master.region$Date),]

    region.eval <- data.frame()

    for(k in unique(master.region$ScYr)){
      tmp.data <- master.region[master.region$ScYr == k,]

      # Calculate region wide reference date
      # # number of cases within a rolling window of 7 days
      weekly.cases <- rollapply(tmp.data$Case.No, width = 7,
                                sum, partial = TRUE, align = "right")

      # first day in the year where 2 confirmed influenza cases within 7 days
      region.ref <- suppressWarnings(min(which(weekly.cases > 1)))

      # If a reference date is defined for the region,
      #   create an indicator to specify that day as the reference date,
      #   and indicators for every day that is in the "True Alarm" window,
      #   that is the 14 days prior to the reference date
      #   (The "True Alarm" window is used for FAR and ADD calculations)
      if(region.ref != Inf){
        ref.row.start <- max(0, (region.ref-14))

        # 14 day window for ADD calculations
        tmp.data[tmp.data$Date >= ref.row.start & tmp.data$Date <= region.ref,
                 "window"] <- 1
        tmp.data[tmp.data$Date == region.ref, "ref.date"] <- 1
      }

      region.eval <- rbind(region.eval, tmp.data)

    }

    master.region <- region.eval

    # Create lagged absenteeism columns
    region.lag <- data.frame()

    # lagged absenteeism values for the entire region
    x_t <- master.region[order(master.region$ScYr, master.region$Date),
                         "pct.absent"]

    n <- length(x_t)
    lagmatrix <- matrix(0, nrow = n, ncol = no.lags)

    colnames(lagmatrix) <- paste("lag", c(0:(no.lags-1)), sep="")

    for(k in 1:no.lags){
      lagmatrix[,k] <- dplyr::lag(x_t, k-1)
    }

    master.region <- cbind(master.region, lagmatrix)

    return(master.region)


  } else if (type == 'c'){

    # Catchment area datasets
    #   Combine lab confirmed cases, absenteeism and actual
    #   cases into one dataframe
    master.catch <- merge(actual.cases.catch, absent.catch,
                          by=c("time", "catchID", "ScYr"), all=TRUE)

    master.catch <- merge(master.catch, labconf.catch,
                          by=c("time", "catchID", "ScYr"), all=TRUE)

    master.catch <- rename(master.catch, c("Actual.case" = "case",
                                      "Case.No" = "labconf", "Date" = "time"))

    # indicator for lab confirmed flu case in the catchment area on that day
    master.catch$Case <- 1*(master.catch$Case.No > 0)

    # seasonal terms
    master.catch$sinterm <- sin((2*pi*master.catch$Date)/365.25)
    master.catch$costerm <- cos((2*pi*master.catch$Date)/365.25)

    # "True Alarm" window (to be calculated)
    master.catch$window <- 0

    # reference date indicator (to be calculated)
    master.catch$ref.date <- 0
    master.catch$catchID <- as.factor(master.catch$catchID)

    master.catch <- master.catch[order(master.catch$ScYr,
                                    master.catch$Date, master.catch$catchID),]


    # Calculate reference date for each catchment area and year
    # region ref date = 2nd lab confirmed case within 7 days
    # catchment area ref date = 2nd lab confirmed case within 10 days (Option 1)
    #     OR = first lab confirmed case in catchment area after region ref date
    #      (Option 2 - this option is currently commented out)
    catchment.eval <- data.frame()

    # Calculate catchment area reference date
    for(j in unique(master.catch$catchID)){
      tmp.data <- master.catch[master.catch$catchID == j &
                                 master.catch$ScYr == k,]

      # Option 1 - 2nd lab confirmed case within 10 days
      # number of cases within a rolling window of 10 days
      weekly.cases <- rollapply(tmp.data$Case.No, width = 10,
                                sum, partial = TRUE, align = "right")

      # first day in the catchment area and year
      # where 2 confirmed influenza cases within 10 days
      catch.ref <- suppressWarnings(min(which(weekly.cases > 1)))

      # (Option 2 - first lab confirmed case in catchment area
      #  after region ref date)
      # catch.ref <- min(tmp.data[tmp.data$Case.No >0 &
      #  tmp.data$Date >= region.ref,'Date'])

      if(catch.ref != Inf){
        ref.row.start <- max(0, (catch.ref-14))

        # 14 day window for ADD calculations
        tmp.data[tmp.data$Date >= ref.row.start & tmp.data$Date <= catch.ref,
                 "window"] <- 1
        tmp.data[tmp.data$Date == catch.ref, "ref.date"] <- 1
      }
      catchment.eval <- rbind(catchment.eval, tmp.data)
    }

    master.catch <- catchment.eval

    # Create lagged absenteeism columns

    catch.lag <- data.frame()


    # lagged absenteeism values for each catchment catchment area
    for(j in unique(master.catch$catchID)){
      tmp.catch <- master.catch[master.catch$catchID == j,]
      tmp.catch <- tmp.catch[order(tmp.catch$ScYr, tmp.catch$Date),]
      x_t <- tmp.catch[, "pct.absent"]
      n <- length(x_t)
      lagmatrix <- matrix(0, nrow = n, ncol = no.lags)
      colnames(lagmatrix) <- paste("lag", c(0:(no.lags-1)), sep="")

      for(k in 1:no.lags){
        lagmatrix[,k] <- dplyr::lag(x_t, k-1)
      }

      tmp.catch <- cbind(tmp.catch, lagmatrix)
      catch.lag <- rbind(catch.lag, tmp.catch)
    }

    master.catch <- catch.lag

  }

  return(catchment = master.catch)
}


sim.lab.confirm <- function(epidata, individual.data){

  #combine dataframes to have individual and infection information
  epidata.df <- cbind(individual.data, inftime = epidata$inftime,
                      remtime = epidata$remtime)

  # Simulate labortoary case confirmation
  #   0.5% chance that an infected individual will
  #   have a medical visit each day they are sick (4 days)
  lab.unif <- runif(length(epidata$inftime))

  epidata.df$labtime <- ifelse(epidata.df$inftime == 0, 0,
                        # day 1
                         ifelse(lab.unif < .005, epidata.df$inftime,
                          # day 2
                          ifelse(lab.unif < .01, epidata.df$inftime+1,
                           # day 3
                           ifelse(lab.unif < .015, epidata.df$inftime+2,
                            # day4
                            ifelse(lab.unif < .02, epidata.df$inftime+3, 0)))))

  #indicator for lab confirmed case
  epidata.df$labconf <- (epidata.df$labtime > 0)*1

  # Aggregate data, such that we have the number of lab confirmed
  #  cases each day of the study period for each catchment area
  time.labconf <- aggregate(labconf ~ catchID + labtime,
                            data=epidata.df, FUN=sum)

  # all combinations of catchment area and days in the year
  time.catchment <- expand.grid(time=c(1:300),
                                catchID = unique(epidata.df$catchID))

  time.labconf <- merge(time.catchment, time.labconf, by.x=c("time", "catchID")
                        , by.y=c("labtime", "catchID"), all=TRUE)

  time.labconf <- time.labconf[time.labconf$time >0,]
  time.labconf[is.na(time.labconf)] <- 0

  return(time.labconf)
}

#### Create Absenteeism Data set ####
# Output: Mean absenteeism percentages for each school and day
# Input: simulated epidemic, individual information
sim.absent <- function(epidata, individual.data){

  #combine dataframes to have individual and infection information
  epidata.df <- cbind(individual.data, inftime = epidata$inftime,
                      remtime = epidata$remtime)

  #school information
  schoolkids <- epidata.df[epidata.df$elem_child_ind == 1,]
  num.schoolkids <- nrow(schoolkids)
  num.schools <- length(unique(schoolkids$schoolID))
  school.pop <- aggregate(elem_child_ind ~ schoolID, data=schoolkids, FUN=sum)
  names(school.pop)[2] <- "school.population"

  # For every day in the school year, simulate if each child is absent or not
  #   given the students infection status
  time.school.absent <- data.frame(time = c(), schoolID = c(),
                                   catchID =c(), absent = c())

  for(i in 1:300){
    a <- runif(num.schoolkids)
    absent <- ifelse(schoolkids$inftime == 0 | !(schoolkids$inftime <= i &
                                                   i < schoolkids$remtime),
                #if healthy, kids go to school 95% of time
                # (month of September has average 5% absenteeism in actual data)
                ifelse(a<.95,0,1),
                #if sick, kids go to school 5% of time
                ifelse(a<.95,1,0))

    #keep track of absenteeism due to illness
    absent.sick <- ifelse(schoolkids$inftime == 0 | !(schoolkids$inftime <= i &
                              i < schoolkids$remtime), 0, ifelse(a<.95,1,0))

    schoolabsent <- aggregate(cbind(absent, absent.sick) ~ schoolID + catchID,
                              data=schoolkids, FUN=sum)

    time.school.absent <- rbind(time.school.absent, cbind(time=rep(i,
                                          times=num.schools), schoolabsent))
  }

  # Aggregate data such that for each day and school, we have the percent absent
  time.school.absent <- merge(time.school.absent, school.pop,
                              by="schoolID", all=TRUE)

  time.school.absent$pct.absent <- time.school.absent$absent/time.school.absent$school.population

  time.absent.count <- aggregate(cbind(absent, absent.sick) ~ time + catchID
                                 + schoolID, data=time.school.absent, FUN=sum)

  time.absent.pct <- aggregate(pct.absent ~ time + catchID + schoolID,
                               data = time.school.absent, FUN=mean)

  time.absent <- merge(time.absent.pct, time.absent.count,
                       by = c("time", "catchID", "schoolID"))

  return(time.absent)
}

#### Total Infulenza Case Data (including non-lab confirmed cases) ####
# Output: The number of influenza cases each day, per catchment area
# Note: This includes cases that were not laboraotry confirmed.
#   This is not required for modelling, but can provide useful information.
# Input: simulated epidemic, indiviudal information

sim.actual.case <- function(epidata, individual.data){

  #combine dataframes to have individual and infection information
  epidata.df <- cbind(individual.data, inftime = epidata$inftime,
                      remtime = epidata$remtime)

  epidata.df$case <- (epidata.df$inftime > 0)*1 #case indicator

  actualcases <- aggregate(case ~ inftime + catchID, data=epidata.df, FUN=sum)

  # number of students that had influenza
  #note: this is not needed for modelling, this is just additional information
  elemkids <- epidata.df[epidata.df$elem_child_ind == 1,]
  cases.elemkids <- aggregate(case ~ inftime + catchID, data=elemkids, FUN=sum)

  # Aggregate data, such that we have the number of influenza cases
  #  each day of the study period for each catchment area
  time.catchment <- expand.grid(time=c(1:300),
                                catchID = unique(epidata.df$catchID))
  actualcases <- merge(actualcases, cases.elemkids,
                       by = c("inftime", "catchID"), suffixes = c("", ".elem"))
  actualcases <- merge(time.catchment, actualcases,
                       by.y=c("inftime", "catchID"), by.x=c("time", "catchID"),
                       all=TRUE)

  actualcases[is.na(actualcases)] <- 0

  return(actualcases)
}
