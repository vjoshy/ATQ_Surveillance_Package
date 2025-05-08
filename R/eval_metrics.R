#' Evaluate Alarm Metrics for Epidemic Models
#'
#' This function calculates various performance metrics for epidemic alarm
#' systems across different lags and thresholds. It evaluates
#' False Alarm Rate (FAR),Added Days Delayed (ADD), Average Alarm Time
#' Quality (AATQ), First Alarm Time Quality (FATQ), and their weighted
#' versions (WAATQ, WFATQ).
#'
#' @param data A data frame containing the epidemic data with lagged variables,
#'             typically output from the compile_epi function.
#' @param maxlag The maximum number of lags to consider (default: 15).
#' @param thres A vector of threshold values to evaluate
#' (default: seq(0.1, 0.6, by = 0.05)).
#' @param topt Optimal alarm day (default = 14).
#'
#' @return A list containing three elements:
#'   \item{metrics}{class "alarm_metrics" with the following components:
#'     \itemize{
#'       \item FAR: Matrix of False Alarm Rates for each lag and threshold
#'       \item ADD: Matrix of Added Days Delayed for each lag and threshold
#'       \item AATQ: Matrix of Average Alarm Time Quality for each lag and
#'       threshold
#'       \item FATQ: Matrix of First Alarm Time Quality for each lag and
#'       threshold
#'       \item WAATQ: Matrix of Weighted Average Alarm Time Quality for
#'        each lag and threshold
#'       \item WFATQ: Matrix of Weighted First Alarm Time Quality for each
#'       lag and threshold
#'       \item best.AATQ: Best model according to AATQ
#'       \item best.FATQ: Best model according to FATQ
#'       \item best.FAR: Best model according to FAR
#'       \item best.ADD: Best model according to ADD
#'       \item best.WFATQ: Best model according to WFATQ
#'       \item best.WAATQ: Best model according to WAATQ
#'     }
#'   }
#'   \item{plot_data}{class "alarm_plot_data" for generating plots}
#'   \item{summary}{class "alarm_metrics_summary" containing summary statistics}
#'
#' @importFrom stats as.formula glm predict
#'
#' @export
#'
#' @examples
#' # Generate simulated epidemic data
#' n_rows <- 7421
#' n_houses <- 1000
#'
#' epidemic_new <- ssir(n_rows, T = 300, alpha = 0.298, inf_init = 32, rep = 3)
#'
#' individual_data <- data.frame(
#'   houseID = rep(1:n_houses, each = ceiling(n_rows / n_houses))[1:n_rows],
#'   catchID = sample(1:10, n_rows, replace = TRUE),
#'   schoolID = sample(1:10, n_rows, replace = TRUE),
#'   num_people = round(rnorm(n_rows, mean = 4, sd = 1)),
#'   num_elem_child = round(rnorm(n_rows, mean = 1, sd = 1)),
#'   xStart = 0,
#'   xEnd = 5,
#'   yStart = 0,
#'   yEnd = 5,
#'   loc.x = rnorm(n_rows, mean = 2.5, sd = 1),
#'   loc.y = rnorm(n_rows, mean = 2.5, sd = 1),
#'   individualID = 1:n_rows,
#'   elem_child_ind = sample(0:1, n_rows, replace = TRUE)
#' )
#'
#' compiled_data <- compile_epi(epidemic_new, individual_data)
#'
#' # Evaluate alarm metrics
#' alarm_metrics <- eval_metrics(compiled_data,
#'                               thres = seq(0.1, 0.3, by = 0.05))
#'
#' # Access the results
#' summary(alarm_metrics$summary)
#' @seealso \code{\link{ssir}}, \code{\link{compile_epi}}
#'
#'
eval_metrics <- function(data,
                         maxlag = 15,
                         thres = seq(0.1,0.6,by = 0.05),
                         topt = 14) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_cols <- c("time","ScYr","pct_absent","absent","absent_sick",
                     "new_inf","reported_cases",
                     "Case","sinterm","costerm","window","ref_date")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:",
               paste(missing_cols, collapse = ", ")))
  }

  if (!is.numeric(maxlag) || maxlag < 1) {
    stop("maxlag must be a positive integer")
  }

  if (!is.numeric(thres) || any(thres < 0) || any(thres > 1)) {
    stop("thres must be a numeric vector with values between 0 and 1")
  }

  ScYr <- unique(data$ScYr)[-1]

  yr.weights <- 1:length(ScYr)/sum(1:length(ScYr))


  mod_resp <- (log_reg(data, maxlag, area = "region"))$resp

  lags <- seq.int(1,maxlag)

  #subset data do evaluation years
  lagdata <- data[data$ScYr %in% ScYr,]

  if (nrow(lagdata) == 0) {
    stop("No data left after filtering for specified ScYr values")
  }

  lagdata <- rep(list(lagdata), length(thres))

  # Matrices of each metric to determine which lag and
  # threshold minimizes the respective metric
  modmat <- matrix(nrow = length(lags), ncol = length(thres),
                   dimnames = list(paste("Lag", lags),
                                   c(paste("thres", thres))))

  FAR.mat <- ADD.mat <- AATQ.mat <- FATQ.mat <- WAATQ.mat <- WFATQ.mat <- modmat
  daily.results <- data.frame() # detailed daily data for output

  for(i in seq_along(lags)){

    for(t in seq_along(thres)){

      combined_resp <- do.call(rbind, mod_resp[[i]][ScYr])


      if(is.null(combined_resp) || nrow(combined_resp) == 0) {
        stop("combined_resp is empty. Check mod_resp structure and ScYr values")
      }

      if(nrow(combined_resp) != nrow(lagdata[[t]])) {
        stop(paste("Mismatch in dimensions: combined_resp has",
                   nrow(combined_resp), "rows,
                     lagdata has", nrow(lagdata[[t]]), "rows"))
      }

      lagdata[[t]]$Alarm <- ifelse(combined_resp$resp > thres[t], 1, 0)

      # calculate metrics
      performance.metric <- calc.metric.region(lagdata[[t]],
                                               ScYr, yr.weights, topt)

      #update lag and threshold metric matrices
      FAR.mat[i,t] <- performance.metric$FAR
      ADD.mat[i,t] <- performance.metric$ADD
      AATQ.mat[i,t] <- performance.metric$AATQ
      FATQ.mat[i,t] <- performance.metric$FATQ
      WAATQ.mat[i,t] <- performance.metric$WAATQ
      WFATQ.mat[i,t] <- performance.metric$WFATQ

      # Update detailed daily results
      tmp <- data.frame(lag = lags[i]
                        , thres = thres[t]
                        , performance.metric$daily.results)
      daily.results <- rbind(daily.results, tmp)
    }
  }


  #output selected models according to each metric
  best.FAR <- best.mod(FAR.mat, lags = lags, thres = thres,
                       daily.results = daily.results)
  best.ADD <- best.mod(ADD.mat, lags = lags, thres = thres,
                       daily.results = daily.results)
  best.AATQ <- best.mod(AATQ.mat, lags = lags, thres = thres,
                        daily.results = daily.results)
  best.FATQ <- best.mod(FATQ.mat, lags = lags, thres = thres,
                        daily.results = daily.results)
  best.WAATQ <- best.mod(WAATQ.mat, lags = lags, thres = thres,
                         daily.results = daily.results)
  best.WFATQ <- best.mod(WFATQ.mat, lags = lags, thres = thres,
                         daily.results = daily.results)

  result <- list(
    "FAR" = FAR.mat,
    "ADD" = ADD.mat,
    "AATQ" = AATQ.mat,
    "FATQ" = FATQ.mat,
    "WAATQ" = WAATQ.mat,
    "WFATQ" = WFATQ.mat,
    "best.AATQ" = best_model(best.AATQ),
    "best.FATQ" = best_model(best.FATQ),
    "best.FAR" = best_model(best.FAR),
    "best.ADD" = best_model(best.ADD),
    "best.WFATQ" = best_model(best.WFATQ),
    "best.WAATQ" = best_model(best.WAATQ),
    lags = 1:maxlag,
    thresholds = thres
  )

  metrics <- alarm_metrics(result)

  # Create alarm_plot_data object
  plot_data <- alarm_plot_data(
    epidemic_data = lagdata,
    best_models = list(
      "AATQ" = best.AATQ,
      "FATQ" = best.FATQ,
      "FAR" = best.FAR,
      "ADD" = best.ADD,
      "WFATQ" = best.WFATQ,
      "WAATQ" = best.WAATQ
    )
  )

  summary <- create_alarm_metrics_summary(metrics, result, data)

  list(metrics = metrics, plot_data = plot_data, results = summary)
}



#### Region-Wide Alarm - metric calculation ####
calc.metric.region <- function(lagdata, ScYr, yr.weights, topt) {

  # evaluation metric vectors, each entry represents a year
  FAR <- ADD <- AATQ <- FATQ <- c()
  daily.results <- data.frame()

  # Calculate evaluation metrics for each school year
  for(y in seq_along(ScYr)){

    #subset data
    ScYr.data <- lagdata[lagdata$ScYr == ScYr[y],
                         c("time", "ScYr", "new_inf",
                           "reported_cases", "Case", "pct_absent", "absent",
                           "absent_sick",  "window", "ref_date", "Alarm")]

    # Reference date
    refdate <- suppressWarnings(min(ScYr.data[ScYr.data$ref_date == 1,"time"]))

    # only consider alarms prior to the reference date
    ScYr.data <- ScYr.data[ScYr.data$time <= refdate,]

    #calculate evaluation metrics
    FAR[y] <- calc.FAR(ScYr.data)
    ADD[y] <- calc.ADD(ScYr.data, topt)

    ScYr.data$ATQ <- calc.ATQ(ScYr.data, topt)
    AATQ[y] <- calc.AATQ(ScYr.data)
    FATQ[y] <- calc.FATQ(ScYr.data)

    tmp <- data.frame(ScYr.data
                      , FAR = FAR[y]
                      , ADD = ADD[y]
                      , AATQ = AATQ[y]
                      , FATQ = FATQ[y])

    daily.results <- rbind(daily.results, tmp)
  }

  # Mean of metrics for each year
  return(list("ADD" = mean(ADD)
              , "FAR" = mean(FAR)
              , "AATQ" = mean(AATQ)
              , "FATQ" = mean(FATQ)
              , "WAATQ" = sum(AATQ * yr.weights)
              , "WFATQ" = sum(FATQ * yr.weights)
              , "daily.results" = daily.results))
}

#### False alarm rate (FAR) ####
calc.FAR <- function(data){

  TrueAlarm <- ifelse(data$window == 1 & data$Alarm == 1, 1, 0)
  FalseAlarm <- ifelse(data$window == 0 & data$Alarm == 1, 1, 0)

  num.true.alarm <- sum(TrueAlarm)
  num.false.alarm <-  sum(FalseAlarm)

  FAR <- ifelse(num.true.alarm > 0,
                num.false.alarm/(num.false.alarm + 1), 1)

  return(FAR)
}

#### Accumulated Days Delayed (ADD) ####
calc.ADD <- function(data, topt){

  refdate <- suppressWarnings(min(data[data$ref_date == 1,"time"]))

  TrueAlarm <- ifelse(data$window == 1 & data$Alarm == 1, 1, 0)

  first.true.alarm.date <- suppressWarnings(min(data[which(TrueAlarm == 1),
                                                     "time"]))

  # number of days advance notice
  advnot <- as.numeric(refdate - first.true.alarm.date)

  # NA for years with no true alarms
  advnot <- ifelse(advnot == "-Inf", NA ,advnot)
  tmax <- as.numeric(refdate - 1)
  ADD <- ifelse(is.na(advnot), tmax, abs(topt - advnot))

  return(ADD)
}

#### Alarm Time Quality (ATQ) ####
calc.ATQ <- function(data, topt){

  #ATQ metric powers and denominator
  # "True Alarm" power - power for alarms raised before optimal alarm day
  ta.pow <- 4

  # "False Alarm" power - power for alarms raised after optimal alarm day
  fa.pow <- 2
  denom <- 21

  refdate <- suppressWarnings(min(data[data$ref_date == 1,"time"]))
  RefDateDiff <- as.numeric(refdate - data$time)
  OptDateDiff <- topt - RefDateDiff

  #calculate ATQ values for every day
  ATQ <- ifelse(OptDateDiff < 0,ifelse((abs(OptDateDiff)/denom)^fa.pow > 1,1,
                                       (abs(OptDateDiff)/denom)^fa.pow),
                (abs(OptDateDiff)/denom)^ta.pow)

  ATQ <- ifelse(data$Alarm == 1, ATQ, NA) # if no alarm raised, NA

  return(ATQ)
}

#### Average Alarm Time Quality (AATQ) ####
calc.AATQ <- function(data){

  AATQ <- mean(data$ATQ, na.rm = TRUE)

  if (is.na(AATQ)) AATQ <- 1 # If no alarms, then AATQ = 1 for that year

  return(AATQ)
}

#### First Alarm Time Quality (FATQ) ####
calc.FATQ <- function(data){
  num.alarms <- sum(data$Alarm)
  first.alarm.date <- suppressWarnings(min(data[which(data$Alarm == 1),
                                                "time"]))
  #find index of first alarm
  first.alarm.index <- which(data$time == first.alarm.date)

  # If no alarms, then FATQ = 1 for that year
  FATQ <- ifelse(num.alarms == 0, 1, data$ATQ[first.alarm.index])

  return(FATQ)
}

#### Select the best lag and threshold value for a given metric ####
best.mod <- function(mat, lags = lags, thres = thres,
                     daily.results = daily.results){

  best.index <- which(mat == min(mat))[1]

  best.lag <- ifelse(best.index %% length(lags) != 0 ,
                     lags[best.index %% length(lags)], lags[length(lags)])

  best.thres <- thres[ceiling(best.index/length(lags))]

  best <- daily.results[daily.results$lag == best.lag
                        & daily.results$thres == best.thres,]
  return(best)
}


#### Logistic regression model fitting function for simulated data ####
log_reg <- function(lagdata, maxlag = 15, area = "region"){
  lags <- seq.int(1, maxlag)
  forms <- lapply(0:maxlag, function(lag) {
    lag_formula <- as.formula(paste("Case ~ sinterm + costerm +",
                                    paste0("lag", 0:lag, collapse = "+")))
    return(lag_formula)
  })

  train <- list()
  pred <- list()
  for(yr in unique(lagdata$ScYr)[-1]){
    train[[yr]] <- lagdata[lagdata$ScYr < yr,]
    pred[[yr]] <- lagdata[which(lagdata$ScYr == yr),]
  }

  mod <- rep(list(list()), length(lags))
  resp <- rep(list(list()), length(lags))

  for(i in seq_along(lags)){
    for(yr in unique(lagdata$ScYr)[-1]){
      mod[[i]][[yr]] <- suppressWarnings(glm(forms[[i]], data = train[[yr]],
                                             family = "binomial"))
      resp[[i]][[yr]] <- data.frame(resp = predict(mod[[i]][[yr]], pred[[yr]],
                                                   type = "response"))
    }
  }

  return(list(mod = mod, resp = resp))
}

