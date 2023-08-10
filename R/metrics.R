#' Metrics Data
#'
#' A list of length 12 contains 6 matrices and 6 data frames
#'
#' @name metrics
NULL

#' Alarm Metrics Data
#'
#' List contains 6 matrices for 6 metrics and 6 data frames of .
#' Each matrix contains lag and threshold information whereas each data frames describes information about confirmed cases, reference date, etc associated with best selected lag and threshold values that minimizes specified metric.
#' metrics is returned by running code metrics <- eval_metrics(region, mod_resp = regression$resp, type = 'r', lags = 1:15, thres = seq(0.1,0.6,by = 0.05), ScYr = c(2:10), yr.weights = c(1:9)/sum(c(1:9)))
#'
#'  \itemize{
#'    \item FAR, ID associated with each catchment
#'    \item ADD, Number of schools in each catchment
#'    \item AATQ, Start x coordinate of catchment grid
#'    \item FATQ, End x coordinate of catchment grid
#'    \item WAATQ, Start y coordinate of catchment grid
#'    \item WFATQ, End y coordinate of catchment grid
#'}
#'
#' Each data frame is named "best.metric", eg: best.FAR and contains the following variables
#'
#'  \itemize{
#'    \item lag, Best selected lagged absenteeism value
#'    \item thres, Best selected threshold value
#'    \item Data, Day (1-300)
#'    \item ScYr, School year
#'    \item Actual.case, Total number of true cases (unconfirmed)
#'    \item case.elem, Number of true cases that are elementary school children
#'    \item Case.No, Number of lab confirmed cases
#'    \item Case, Binary variable to indicate whether there has been a lab confirmed case for the day
#'    \item pct.absent, Percent of students absent in total school population
#'    \item absent, Total number of students absent
#'    \item absent.sick, Total number of students absent due to illness
#'    \item window, 14 day true alarm window for ADD and FAR calculations (binary variable)
#'    \item ref.date, Reference date (first day where there are two lab confirmed cases in a 7 day period)
#'    \item Alarm, Binary variable indicating whether alarm was raised (1 - raised, 0 - not raised)
#'    \item ATQ, Alert time quality (metric)
#'    \item FAR, False alarm rate (metric)
#'    \item ADD, Accumulated days delayed (metric)
#'    \item AATQ, Average alarm time quality (metric)
#'    \item FATQ, First alarm time quality (metric)
#'
#'  }
#'
#' @docType data
#' @name metrics
#' @usage data(metrics)
#' @format A list of length 12 contains 6 matrices and 6 data frames (401 x 19)
NULL
