#' Compile Epidemic Data
#'
#' Compiles and processes epidemic data, simulating laboratory confirmed cases
#' and school absenteeism information using epidemic and individual data.
#'
#' @param epidemic A list of simulated epidemic data
#'   (output from simepi() or similar function).
#' @param individual_data A data frame of simulated individuals data
#'   (output from simulate_households() or similar function).
#' @param lags Maximum number of lags for absenteeism, default = 16
#'   (note that lag of zero is included).
#' @param inf_period Infection period of epidemic, default = 4.
#' @param T Time period, default = 300 days.
#'
#' @importFrom stats aggregate
#' @importFrom zoo rollapply
#' @importFrom dplyr rename
#'
#'
#' @return A data frame containing compiled epidemic data, including:
#'   \item{Date}{Day of the epidemic}
#'   \item{ScYr}{School year}
#'   \item{pct_absent}{Percentage of students absent}
#'   \item{absent}{Number of students absent}
#'   \item{absent_sick}{Number of students absent due to illness}
#'   \item{new_inf}{Number of new infections}
#'   \item{reported_cases}{Number of reported cases}
#'   \item{Case}{Indicator for lab confirmed flu case that day}
#'   \item{sinterm, costerm}{Seasonal terms}
#'   \item{window}{Indicator for "True Alarm" window}
#'   \item{ref_date}{Indicator for reference date}
#'   \item{lag0, lag1, ..., lag15}{Lagged absenteeism values}
#'
#' @export
#'
#' @examples
#' # Assuming you have previously simulated epidemic and individual data:
#' epidemic <- ssir(1000, alpha = 0.4)
#' individual_data <- data.frame(elem_child_ind = sample(0:1, 1000, replace = TRUE),
#'                               schoolID = sample(1:10, 1000, replace = TRUE))
#'
#' compiled_data <- compile_epi(epidemic, individual_data)
compile_epi <- function(epidemic, individual_data, lags = 16, inf_period = 4, T = 300){

  # Input validation
  if (!inherits(epidemic, c("ssir_epidemic", "ssir_epidemic_multi"))) {
    stop("epidemic must be of ssir_epidemic or ssir_epidemic_multi class, please use ssir() function to simulate epidemic.")
  }
  if (!is.data.frame(individual_data)) {
    stop("individual_data must be a dataframe")
  }
  if (!is.numeric(lags) || length(lags) != 1 || lags < 1) {
    stop("lags must be a single positive numeric value")
  }

  if(inherits(epidemic, "ssir_epidemic")){
    years <- 1
  } else {
    years <- length(epidemic) - 1
  }



  #Region-wide datasets
  actual_cases_region <- data.frame(time=c(), new_inf = c(),
                                    ScYr = c())

  absent_region <- data.frame(time = c(), pct_absent = c(), absent = c(),
                              absent_sick = c(), ScYr = c())

  labconf_region <- data.frame(time = c(), reported_cases = c(), ScYr = c(), new_inf = c())

  for(i in 1:years){

    if(years == 1){
      ssir_data <- epidemic
    } else {
      ssir_data <- epidemic[[i]]
    }

    tryCatch({
      cases_df <- create_ssir_df(ssir_data, i)
      absent <- sim_absent_ssir(ssir_data, individual_data, i, inf_period, T)

      absent_region <- rbind(absent_region, absent)
      labconf_region <- rbind(labconf_region, cases_df)
    }, error = function(e) {
      warning(paste("Error processing epidemic data for year", i, ":", e$message))
    })
  }

  if (nrow(absent_region) == 0 || nrow(labconf_region) == 0) {
    stop("No valid data processed from epidemic list")
  }

  master_region <- merge(absent_region, labconf_region, by=c("time", "ScYr"), all=TRUE)

  master_region <- rename(master_region, c("lab_conf" = "reported_cases", "Date" = "time"))

  # indicator for lab confirmed flu case that day
  master_region$Case <- 1*(master_region$new_inf > 0)

  # seasonal terms
  master_region$sinterm <- sin((2*pi*master_region$Date)/365.25)
  master_region$costerm <- cos((2*pi*master_region$Date)/365.25)

  # "True Alarm" window (to be calculated)
  master_region$window <- 0

  # reference date indicator (to be calculated)
  master_region$ref_date <- 0

  master_region <- master_region[order(master_region$ScYr,
                                       master_region$Date),]

  region_eval <- data.frame()

  for(k in unique(master_region$ScYr)){

    tmp_data <- master_region[master_region$ScYr == k,]

    # Calculate region wide reference date
    # # number of cases within a rolling window of 7 days
    weekly_cases <- rollapply(tmp_data$new_inf, width = 7,
                              sum, partial = TRUE, align = "right")

    # first day in the year where 2 confirmed influenza cases within 7 days
    region_ref <- suppressWarnings(min(which(weekly_cases > 1)))

    # If a reference date is defined for the region,
    #   create an indicator to specify that day as the reference date,
    #   and indicators for every day that is in the "True Alarm" window,
    #   that is the 14 days prior to the reference date
    #   (The "True Alarm" window is used for FAR and ADD calculations)
    if(region_ref != Inf){
      ref_row_start <- max(0, (region_ref-14))

      # 14 day window for ADD calculations
      tmp_data[tmp_data$Date >= ref_row_start & tmp_data$Date <= region_ref,
               "window"] <- 1
      tmp_data[tmp_data$Date == region_ref, "ref_date"] <- 1
    }

    region_eval <- rbind(region_eval, tmp_data)

  }

  master_region <- region_eval

  # Create lagged absenteeism columns
  region_lag <- data.frame()

  # lagged absenteeism values for the entire region
  x_t <- master_region[order(master_region$ScYr, master_region$Date),
                       "pct_absent"]

  n <- length(x_t)
  lagmatrix <- matrix(0, nrow = n, ncol = lags)

  colnames(lagmatrix) <- paste("lag", c(0:(lags-1)), sep="")

  for(k in 1:lags){
    lagmatrix[,k] <- dplyr::lag(x_t, k-1)
  }

  master_region <- cbind(master_region, lagmatrix)

  return(master_region)


}


create_ssir_df <- function(ssir_data, school_year) {

  # input validation
  if (!is.list(ssir_data) || !all(c("new_inf", "reported_cases") %in% names(ssir_data))) {
    stop("ssir_data must be a list containing 'new_inf' and 'reported_cases'")
  }
  if (!is.numeric(school_year) || length(school_year) != 1) {
    stop("school_year must be a single numeric value")
  }


  # Extract relevant data
  new_inf <- ssir_data$new_inf
  reported_cases <- ssir_data$reported_cases[1:length(new_inf)]  # Truncate to match new_inf length
  time <- 1:length(new_inf)
  ScYr <- rep(school_year, length(new_inf))

  # Create dataframe
  ssir_df <- data.frame(
    time = time,
    new_inf = new_inf,
    reported_cases = reported_cases,
    ScYr = ScYr
  )
  return(ssir_df)
}

sim_absent_ssir <- function(ssir_data, individual_data, school_year, inf_period, T = 300) {

  # Input validation
  if (!is.list(ssir_data) || !all(c("new_inf", "reported_cases") %in% names(ssir_data))) {
    stop("ssir_data must be a list containing 'new_inf' and 'reported_cases'")
  }
  if (!is.data.frame(individual_data) || !"elem_child_ind" %in% names(individual_data)) {
    stop("individual_data must be a dataframe containing 'elem_child_ind'")
  }
  if (!is.numeric(school_year) || length(school_year) != 1) {
    stop("school_year must be a single numeric value")
  }

  # Extract relevant data
  new_inf <- ssir_data$new_inf
  reported_cases <- ssir_data$reported_cases

  # Prepare individual data
  schoolkids <- individual_data[individual_data$elem_child_ind == 1, ]
  num_schoolkids <- nrow(schoolkids)
  num_schools <- length(unique(schoolkids$schoolID))

  if (num_schoolkids == 0) {
    stop("No school children found in individual_data")
  }

  school_pop <- aggregate(elem_child_ind ~ schoolID, data = schoolkids, FUN = sum)
  names(school_pop)[2] <- "school_pop"

  # Initialize output dataframe
  time_school_absent <- data.frame(time = integer(), schoolID = integer(),
                                   absent = integer(), absent_sick = integer())

  # Simulate absenteeism for each day
  for (i in 1:T) {
    # Calculate proportion of infected individuals
    prop_infected <- sum(new_inf[max(1, i - inf_period):i]) / num_schoolkids

    # Simulate absenteeism
    a <- runif(num_schoolkids)
    absent <- ifelse(a < prop_infected,
                     ifelse(a < 0.95, 1, 0),  # 95% of infected stay home
                     ifelse(a < 0.95, 0, 1))  # 5% of healthy are absent

    absent_sick <- ifelse(a < prop_infected & a < 0.95, 1, 0)

    # Aggregate by school
    school_absent <- aggregate(cbind(absent, absent_sick) ~ schoolID,
                               data = schoolkids, FUN = sum)

    time_school_absent <- rbind(time_school_absent,
                                cbind(time = rep(i, times = num_schools), school_absent))
  }

  # Calculate percentages and merge data
  time_school_absent <- merge(time_school_absent, school_pop, by = "schoolID", all = TRUE)
  time_school_absent$pct_absent <- time_school_absent$absent / time_school_absent$school_pop
  time_school_absent$ScYr <- school_year

  # Aggregate final results
  time_absent <- aggregate(cbind(pct_absent, absent, absent_sick) ~ time + schoolID,
                           data = time_school_absent, FUN = mean)

  time_absent$ScYr <- school_year

  absent_count <- aggregate(cbind(absent, absent_sick) ~ time + ScYr, data = time_absent, FUN = sum)
  absent_pct <- aggregate(pct_absent ~ time + ScYr, data = time_absent, FUN = mean)

  absent_df <- merge(absent_pct, absent_count, by = c("time", "ScYr"))

  return(absent_df)

}
