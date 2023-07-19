#' Logistic regression model fitting function for simulated data
#'
#' @param lagdata Data frame with lagged absenteeism data
#' @param maxlag maximum lagged day to be assessed
#'
#' @return list of lists of linear models (mod) and model responses (resp)
#' mod is a list of list of yearly models created using all data temporarily preceded that year (i.e, list length is equal to number of years)
#'
#' resp contains responses for each year. Length of list is the number of models fitted (number of lags).
#'
#' @importFrom stats as.formula glm predict
#'
#' @export
#'
#' @examples
#'  \dontrun{I need to update this}
#'
#'
log_reg <- function(lagdata, maxlag = 15){


  lags = seq.int(1, maxlag)

  #creates list of regression formulas for each iteration of lagged value from 0 to 15
  forms <- lapply(0:maxlag, function(lag) {
    lag_formula <- as.formula(paste("Case ~ sinterm + costerm +", paste0("lag", 0:lag, collapse = "+")))
    lag_formula
  })

  # Create lists of training and prediction datasets
  # Training datasets - each year uses all data that temporally preceded that year
  train <- list()
  pred <- list()
  for(yr in unique(lagdata$ScYr)[-1]){ # no predictions for the first year, it is only used for model training
    train[[yr]] <- lagdata[lagdata$ScYr < yr,]
    pred[[yr]] <- lagdata[which(lagdata$ScYr == yr),]
  }

  # Fit a model for each lag value and each year, then predict response
  mod <- rep(list(list()), length(lags))
  resp <- list()

  for(i in seq_along(lags)){
    for(yr in unique(lagdata$ScYr)[-1]){ # remove the first year since it was only used for training
      mod[[i]][[yr]] <- glm(forms[[i]], data = train[[yr]], family = "binomial") # train model
      tmp.resp <- data.frame(resp = predict(mod[[i]][[yr]], pred[[yr]], type = "response")) # predict

      if(yr == unique(lagdata$ScYr)[2]){ #fixes error if trying to rbind() first element in list
        resp[[i]] <- tmp.resp
      } else {
        resp[[i]] <- rbind(resp[[i]], tmp.resp)
      }
    }
  }

  return(list(mod = mod, resp = resp))
}
