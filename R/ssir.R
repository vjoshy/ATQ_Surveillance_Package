#' Stochastic SIR simulation
#'
#' This function runs a stochastic Susceptible-Infectious-Removed (SIR) epidemic simulation.
#' It can run a single simulation or multiple replications.
#'
#' @param N Numeric. The total population size.
#' @param T Numeric. The duration of the simulation in time steps. Default is 300.
#' @param avg_start Numeric. The average start day of the epidemic. Default is 45.
#' @param min_start Numeric. The minimum start day of the epidemic. Default is 20.
#' @param alpha Numeric. The transmission rate. Must be a number between 0 and 1.
#' @param inf_period Numeric. The duration of the infectious period in time steps. Default is 4.
#' @param inf_init Numeric. The initial number of infected individuals. Default is 32.
#' @param report Numeric. The proportion of cases that are reported. Default is 0.02.
#' @param lag Numeric. The average delay in reporting cases. Default is 7.
#' @param rep Numeric or NULL. The number of simulation replications to run.
#' If NULL or 1, a single simulation is run.
#'
#' @return If rep is NULL or 1, returns an object of class "ssir_epidemic" containing:
#'   \item{new_inf}{Vector of new infections at each time step}
#'   \item{reported_cases}{Vector of reported cases at each time step}
#'   \item{S}{Vector of susceptible individuals at each time step}
#'   \item{I}{Vector of infectious individuals at each time step}
#'   \item{R}{Vector of removed individuals at each time step}
#'   \item{parameters}{List of input parameters}
#'
#'   If rep > 1, returns an object of class "ssir_epidemic_multi" containing
#'   multiple simulation results.
#'
#' @importFrom stats rnorm runif rbinom rexp
#' @import utils
#'
#' @export
#'
#' @examples
#' # Run a single simulation
#' result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4,
#' inf_init = 32, report = 0.02, lag = 7)
#'
#' # Run multiple simulations
#' multi_result <- ssir(N = 10000, T = 300, alpha = 0.3, inf_period = 4, inf_init = 32,
#' report = 0.02, lag = 7, rep = 100)
#'
ssir <- function(N,
                 T = 300,
                 alpha,
                 avg_start = 45,
                 min_start = 20,
                 inf_period = 4,
                 inf_init = 32,
                 report = 0.02,
                 lag = 7,
                 rep = NULL) {
  # Parameter validation
  if (!is.numeric(N) || N <= 0 || N %% 1 != 0) {
    stop("N ")
  }
  if (!is.numeric(T) || T <= 0 || T %% 1 != 0) {
    stop("T ")
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha > 1) {
    stop("alpha must be a number between 0 and 1.")
  }
  if (!is.numeric(inf_period) || inf_period <= 0 || inf_period %% 1 != 0) {
    stop("inf_period ")
  }
  if (!is.numeric(inf_init) || inf_init <= 0 || inf_init %% 1 != 0) {
    stop("inf_init ")
  }
  if (!is.numeric(report) || report < 0 || report > 1) {
    stop("report must be a number between 0 and 1.")
  }
  if (!is.numeric(lag) || lag < 0 || lag %% 1 != 0) {
    stop("lag must be a non-negative integer.")
  }
  if (!is.null(rep) && (!is.numeric(rep) || rep <= 0 || rep %% 1 != 0)) {
    stop("rep must be NULL or a positive integer.")
  }

  run_simulation <- function() {
    tryCatch({
      epidemic <- list()

      # Random start date
      start <- max(round(stats::rnorm(1, mean = avg_start, sd = 15)),
                   min_start) +
              floor(stats::runif(1, 0, 15))

      # Initialize vectors
      dS <- dI <- dR <- new_inf <- S <- I <- R <- vector(length = T)

      # Initial conditions
      new_inf[1] <- I[1] <- dI[1] <- 0
      S[1] <- N
      R[1] <- 0

      # Initial infected
      new_inf[start] <- I[start] <- dI[start] <- inf_init

      # Case reporting
      reported_cases <- vector(length = T + lag)
      reported_cases[start + lag] <- rbinom(1, new_inf[start], report)

      # To hold delayed cases
      reporting_queue <- vector("list", length = T + lag)

      for (t in (start+1):T) {
        new_inf[t] <- rbinom(1, S[t-1], 1 - exp(-alpha * I[t-1] / N))

        # Simulate reported cases
        cases_to_report <- rbinom(1, new_inf[t], report)

        if (cases_to_report > 0) {
          delays <- round(rexp(cases_to_report, rate = 1/lag))
          for (delay in delays) {
            report_day <- t + delay
            if (report_day <= length(reported_cases)) {
              reporting_queue[[report_day]] <- c(reporting_queue[[report_day]], 1)
            }
          }
        }

        if (!is.null(reporting_queue[[t]])) {
          reported_cases[t] <- sum(reporting_queue[[t]])
        }

        # Update compartments
        dR[t] <- if (t < inf_period + 1) 0 else new_inf[t - inf_period]
        dI[t] <- new_inf[t] - dR[t]
        dS[t] <- -dI[t]

        I[t] <- I[t-1] + dI[t]
        R[t] <- R[t-1] + dR[t]
        S[t] <- N - R[t] - I[t]

        # Check for negative values
        if (any(c(S[t], I[t], R[t]) < 0)) {
          stop("Negative values encountered in S, I, or R compartments.")
        }
      }

      list(new_inf = new_inf, reported_cases = reported_cases, S = S, I = I, R = R)
    }, error = function(e) {
      message("Error in simulation: ", e$message)
      NULL
    })
  }

  # Run simulations
  if (is.null(rep) || rep == 1) {
    result <- run_simulation()
    if (is.null(result)) {
      stop("Simulation failed.")
    }
    result$parameters <- list(N = N, T = T, alpha = alpha, inf_period = inf_period,
                              inf_init = inf_init, report = report, lag = lag, rep = 1)
    class(result) <- "ssir_epidemic"
    return(result)
  } else {
    results <- replicate(rep, run_simulation(), simplify = FALSE)
    if (all(sapply(results, is.null))) {
      stop("All simulations failed.")
    }
    results <- results[!sapply(results, is.null)]
    results$parameters <- list(N = N, T = T, alpha = alpha, inf_period = inf_period,
                               inf_init = inf_init, report = report, lag = lag,
                               rep = length(results))
    class(results) <- "ssir_epidemic_multi"
    return(results)
  }
}
