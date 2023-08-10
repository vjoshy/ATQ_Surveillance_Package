#' Calculate evaluation metrics for lag, threshold, and yearly model
#'
#' @param type 'c' for catchment area models, 'r' for region-wide models. Default is region-wide
#' @param lagdata lagged dataframe used for modelling
#' @param mod_resp output from "log_reg" function
#' @param ScYr vector of all school years to be used for model evaluation (range of natural numbers)
#' @param yr.weights weights for WFATQ and WAATQ metrics
#' @param lags lags evaluated in model
#' @param thres thresholds evaluated
#'
#' @return List of matrices for FAR, ADD, AATQ, FATQ, WAATQ, WFATQ
#' @export
#'
#' @examples
#' #Simulate catchment data
#' catch_df <- catchment_sim(16, 4.12, 3.01, 20)
#'
#' #simulate elementary schools for each area
#' elementary_df <- elementary_pop(catch_df, 4.8, 0.015)
#'
#' # Enters values for prompts from subpop_children() function
#' f <- file()
#' lines <- c(0.7668901,0.3634045, 0.4329440, 0.2036515,0.5857832, 0.3071523, 0.1070645,0.4976825)
#' ans <- paste(lines, collapse = "\n")
#' write(ans, f)
#' options("usr_con" = f) # set connection option
#'
#' # simulating households without children
#' house_children <- subpop_children(elementary_df)
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
#' # randomly sampling 1000 rows to reduce simulation times
#' individuals <- simulation$individual_sim[sample(nrow(simulation$individual_sim),1000),]
#'
#' # simulate epidemic
#' epidemic <- simepi(individuals, b=3, sus=.0019, spark=0, num_inf = 2)
#'
#' # simulate laboratory confirmed cases, and school absenteeism data sets
#' data <- model_data(epidemic, individuals)
#'
#' # Fit Logistic Regression Models to Lagged Absenteeism Data
#' regression <- log_reg(lagdata = data$region, 15, area = "region")
#'
#' # calculate and return metrics in a list
#' region_metric <- eval_metrics(lagdata = data$region, mod_resp = regression$resp,
#'                        type = "r", lags = 1:15, thres = seq(0.1,0.6,by = 0.05),
#'                        ScYr = c(2:10), yr.weights = c(1:9)/sum(c(1:9)))
#'
eval_metrics <- function(type = "r", lagdata, mod_resp, ScYr, yr.weights, lags = seq.int(1,15), thres = seq(0.1,0.6,by = 0.05)){

  lagdata <- lagdata[lagdata$ScYr %in% ScYr,] #subset data do evaluation years
  lagdata <- rep(list(lagdata), length(lags)) # lists of lagdata so that you can compare with model responses for each lag and year (also in list format)

  # Matrices of each metric to determine which lag and threshold minimizes the respective metric
  modmat <- matrix(nrow = length(lags)
                   , ncol = length(thres)
                   , dimnames = list(paste("Lag", lags), c(paste("thres", thres))))
  FAR.mat <- ADD.mat <- AATQ.mat <- FATQ.mat <- WAATQ.mat <- WFATQ.mat <- modmat
  daily.results <- data.frame() # detailed daily data for output

  # For each lag model, and threshold value, calculate evaluation metrics to aid model selection.
  for(i in seq_along(lags)){
    for(t in seq_along(thres)){
      # Determine if alarm was raised based on threshold (if predicted probability > threshold ==> alarm raised)
      lagdata[[t]]$Alarm <- ifelse(mod_resp[[i]]$resp > thres[t], 1,0)

      # Calculate performance metrics
      if(type == "c"){ #catchment area
        performance.metric <- calc.metric.catchment(lagdata[[t]], ScYr, yr.weights)
      } else if(type == "r") { # region wide model
        performance.metric <- calc.metric.region(lagdata[[t]], ScYr, yr.weights)
      } else {
        stop("type = c for cathcment area models, type = r for region-wide models")
      }

      #update lag and threshold metric matricies
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
  best.FAR <- best.mod(FAR.mat, lags = lags, thres = thres, daily.results = daily.results)
  best.ADD <- best.mod(ADD.mat, lags = lags, thres = thres, daily.results = daily.results)
  best.AATQ <- best.mod(AATQ.mat, lags = lags, thres = thres, daily.results = daily.results)
  best.FATQ <- best.mod(FATQ.mat, lags = lags, thres = thres, daily.results = daily.results)
  best.WAATQ <- best.mod(WAATQ.mat, lags = lags, thres = thres, daily.results = daily.results)
  best.WFATQ <- best.mod(WFATQ.mat, lags = lags, thres = thres, daily.results = daily.results)

  return(list("FAR" = FAR.mat
           , "ADD" = ADD.mat
           , "AATQ" = AATQ.mat
           , "FATQ" = FATQ.mat
           , "WAATQ" = WAATQ.mat
           , "WFATQ" = WFATQ.mat
           , "best.AATQ" = best.AATQ
           , "best.FATQ" = best.FATQ
           , "best.FAR" = best.FAR
           , "best.ADD" = best.ADD
           , "best.WFATQ" = best.WFATQ
           , "best.WAATQ" = best.WAATQ))
}

#### Catchment Area Alarm - metric calculation ####
calc.metric.catchment <- function(lagdata, ScYr, yr.weights) {

  allcatchment <- unique(lagdata$catchID)
  daily.results <- data.frame()
  FAR <- ADD <- AATQ <- FATQ <-  matrix(nrow=length(ScYr)
                                        , ncol=length(allcatchment)
                                        , dimnames = list(ScYr, allcatchment))

  # for each catchment area and for each school year calculate evaluation metrics
  for(c in seq_along(allcatchment)){
    for(y in seq_along(ScYr)){

      ScYr.data <- lagdata[which(lagdata$catchID == allcatchment[c] & lagdata$ScYr == ScYr[y])
                           ,c("Date", "catchID", "ScYr", "Actual.case", "case.elem", "Case.No", "Case", "pct.absent", "absent","absent.sick",  "window", "ref.date", "Alarm")]
      refdate <- suppressWarnings(min(ScYr.data[ScYr.data$ref.date == 1,"Date"])) # beginning of influenza season date

      if(is.infinite(refdate)){ # Metric values if no reference date was defined in this catchment area during the given year
        num.alarm <- sum(ScYr.data$Alarm)
        if(num.alarm > 0){ # assign values of 1 if alarms were raised
          FAR[y,c] <- ADD[y,c] <- AATQ[y,c]<- FATQ[y,c]<- 1
          ScYr.data$ATQ <- NA
        } else { # assign values of 0 if no alarms were raised
          FAR[y,c] <- ADD[y,c] <- AATQ[y,c]<- FATQ[y,c]<- 0
          ScYr.data$ATQ <- NA
        }
      } else { # If a reference date was defined in this catchment area during the given year, proceed normally

        ScYr.data <- ScYr.data[ScYr.data$Date <= refdate,] # only consider alarms prior to the reference date

        #calculate evaluation metrics
        FAR[y,c] <- calc.FAR(ScYr.data)
        ADD[y,c] <- calc.ADD(ScYr.data)

        ScYr.data$ATQ <- calc.ATQ(ScYr.data)
        AATQ[y,c] <- calc.AATQ(ScYr.data)
        FATQ[y,c] <- calc.FATQ(ScYr.data)
      }

      # Output detailed result
      tmp <- data.frame(ScYr.data
                        , FAR = FAR[y,c]
                        , ADD = ADD[y,c]
                        , AATQ = AATQ[y,c]
                        , FATQ = FATQ[y,c])
      daily.results <- rbind(daily.results, tmp)
    }
  }
  # Mean of catchment area metrics for each year
  return(list("ADD" = mean(ADD)
              , "FAR" = mean(FAR)
              , "AATQ" = mean(rowMeans(AATQ))
              , "FATQ" = mean(rowMeans(FATQ))
              , "WAATQ" = sum(rowMeans(AATQ) * yr.weights)
              , "WFATQ" = sum(rowMeans(FATQ) * yr.weights)
              , "daily.results" = daily.results))
}

#### Region-Wide Alarm - metric calculation ####
calc.metric.region <- function(lagdata, ScYr, yr.weights) {

  # evaluation metric vectors, each entry represents a year
  FAR <- ADD <- AATQ <- FATQ <- c()
  daily.results <- data.frame()

  # Calculate evaluation metrics for each school year
  for(y in seq_along(ScYr)){

    #subset data
    ScYr.data <- lagdata[lagdata$ScYr == ScYr[y]
                         ,c("Date", "ScYr", "Actual.case", "case.elem", "Case.No", "Case", "pct.absent", "absent","absent.sick",  "window", "ref.date", "Alarm")]
    refdate <- suppressWarnings(min(ScYr.data[ScYr.data$ref.date == 1,"Date"])) # Reference date
    ScYr.data <- ScYr.data[ScYr.data$Date <= refdate,] # only consider alarms prior to the reference date

    #calculate evaluation metrics
    FAR[y] <- calc.FAR(ScYr.data)
    ADD[y] <- calc.ADD(ScYr.data)

    ScYr.data$ATQ <- calc.ATQ(ScYr.data)
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
  FAR <- ifelse(num.true.alarm > 0
                , num.false.alarm/(num.false.alarm + 1)
                , 1)
  return(FAR)
}

#### Added Days Delayed (ADD) ####
calc.ADD <- function(data){
  topt <- 14 # Optimal alarm day
  refdate <- suppressWarnings(min(data[data$ref.date == 1,"Date"]))
  TrueAlarm <- ifelse(data$window == 1 & data$Alarm == 1, 1, 0)
  first.true.alarm.date <- suppressWarnings(min(data[which(TrueAlarm == 1), "Date"]))

  advnot <- as.numeric(refdate - first.true.alarm.date) # number of days advance notice
  advnot <- ifelse(advnot == "-Inf", NA ,advnot) # NA for years with no true alarms
  tmax <- as.numeric(refdate - 1)
  ADD <- ifelse(is.na(advnot), tmax, topt - advnot)
  return(ADD)
}

#### Alarm Time Quality (ATQ) ####
calc.ATQ <- function(data){
  #ATQ metric powers, denominator, and optimal day
  topt <- 14 # Optimal alarm day
  ta.pow <- 4 # "True Alarm" power - ie power for alarms raised before optimal alarm day
  fa.pow <- 2 # "False Alarm" power - ie power for alarms raised after optimal alarm day
  denom <- 21

  refdate <- suppressWarnings(min(data[data$ref.date == 1,"Date"]))
  RefDateDiff <- as.numeric(refdate - data$Date)
  OptDateDiff <- topt - RefDateDiff

  ATQ <- ifelse(OptDateDiff < 0 #calculae ATQ values for every day
                , ifelse((abs(OptDateDiff)/denom)^fa.pow > 1, 1, (abs(OptDateDiff)/denom)^fa.pow)
                , (abs(OptDateDiff)/denom)^ta.pow)

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
  first.alarm.date <- suppressWarnings(min(data[which(data$Alarm == 1), "Date"]))
  first.alarm.index <- which(data$Date == first.alarm.date) #find index of first alarm
  FATQ <- ifelse(num.alarms == 0
                 , 1 # If no alarms, then FATQ = 1 for that year
                 , data$ATQ[first.alarm.index])
  return(FATQ)
}

#### Select the best lag and threshold value for a given metric ####
best.mod <- function(mat, lags = lags, thres = thres, daily.results = daily.results){

  best.index <- which(mat == min(mat))[1]
  best.lag <- ifelse(best.index %% length(lags) != 0 , lags[best.index %% length(lags)], lags[length(lags)])
  best.thres <- thres[ceiling(best.index/length(lags))]
  best <- daily.results[daily.results$lag == best.lag
                        & daily.results$thres == best.thres,]
  return(best)
}


