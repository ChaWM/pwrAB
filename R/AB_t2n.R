#' @title Two-Sample t-Test Power Analysis
#'
#' @description
#' \code{AB_t2n} performs the power analysis for AB testing. It uses the Welch's t-test,
#' which allows for the standard deviation to vary across groups.
#'
#' @param N Total number of observations (sum of observations for groups A and B)
#' @param percent_B Percentage of total observations allocated to group B
#'     (between 0 and 1 - e.g. input .5 for 50\%)
#' @param mean_diff Difference in means of the two groups, with mean_B - mean_A
#' @param sd_A Standard deviation of group A
#' @param sd_B Standard deviation of group B
#' @param sig_level Significance level (Type I error probability)
#' @param power Power of test (1 minus Type II error probability)
#' @param alternative Character string specifying the alternative hypothesis,
#' must be one of "two_sided" (default), "greater" or "less"
#' @param max_sample Maximum sample size that is searched for
#'
#' @details
#' Exactly one of the parameters 'N', 'percent_B', 'mean_diff', 'sig_level', and 'power'
#' must be passed as NULL, and the omitted parameter is determined from the others.
#' sd_A and sd_B must be specified. When 'percent_B' is the parameter omitted, two solutions
#' may exist, in which case the smaller value will be returned
#'
#' @return Object of class "power.htest", a list of the arguments
#' (including the computed one).
#'
#' @examples
#' # Search for power given other parameters
#' AB_t2n(N = 3000, percent_B = .3, mean_diff = .15, sd_A = 1,
#' sd_B = 2, sig_level = .05, alternative = 'two_sided')
#'
#' # Search for sample size required to satisfy other parameters
#' AB_t2n(percent_B = .3, mean_diff = .15, sd_A = 1,
#' sd_B = 2, sig_level = .05, power = .8, alternative = 'two_sided')
#'
#' @importFrom stats uniroot
#' @export
AB_t2n <-
  function (N = NULL, percent_B = NULL,
            mean_diff = NULL, sd_A, sd_B,
            sig_level = NULL, power = NULL,
            alternative = c("two_sided", "less", "greater"),
            max_sample = 1e+07) {

    ### Input Error Checking
    if (sum(sapply(list(N, percent_B, mean_diff,
                        power, sig_level), is.null)) != 1)
      stop("Exactly one of N, percent_B, mean_diff, power, and sig_level must be NULL")
    if (sum(sapply(list(sd_A, sd_B), is.null)) != 0)
      stop("Both sd_A and sd_B must be specified")
    if (!is.null(sig_level) && !is.numeric(sig_level) ||
        any(sig_level < 0 | sig_level > 1))
      stop(sQuote("sig_level"), " must be numeric in [0, 1]")
    if (!is.null(percent_B) && !is.numeric(percent_B) ||
        any(percent_B < 0 | percent_B > 1))
      stop(sQuote("percent_B"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) ||
        any(power < 0 | power > 1))
      stop(sQuote("power"), " must be numeric in [0, 1]")
    if (!is.null(N) && N < 10)
      stop("Total number of observations must at least 10")
    if (!is.null(N) && N < 100)
      warning("This package was not designed for small sample experiment planning (N < 100),
              and the results may not be correct or optimal")

    alternative <- match.arg(alternative)
    test_type <- switch(alternative, less = 1, two_sided = 2, greater = 3)

    ### Code for power analysis (will be evaluated later)
    if (test_type == 2 && !is.null(mean_diff)){
      mean_diff <- abs(mean_diff)
    }
    if (test_type == 1){
      power_eval <- quote({
        N_B <- N * percent_B
        N_A <- N - N_B
        # Welch-Satterthwaite equation
        df_ws <- (sd_A ^ 2 / N_A + sd_B ^ 2 / N_B) ^ 2 / (
          (sd_A ^ 2 / N_A) ^ 2 / (N_A - 1) + (sd_B ^ 2 / N_B) ^ 2 / (N_B - 1)
        )
        t_stat <- mean_diff / sqrt((sd_A ^ 2) / N_A + (sd_B ^ 2) / N_B)

        pt(qt(sig_level, df = df_ws, lower = TRUE),
           df = df_ws, ncp = t_stat,
           lower = TRUE)
      })
    }
    if (test_type == 2){
      power_eval <- quote({
        mean_diff <- abs(mean_diff)
        N_B <- N * percent_B
        N_A <- N - N_B
        df_ws <- (sd_A ^ 2 / N_A + sd_B ^ 2 / N_B) ^ 2 / (
          (sd_A ^ 2 / N_A) ^ 2 / (N_A - 1) + (sd_B ^ 2 / N_B) ^ 2 / (N_B - 1)
        )
        t_stat <- mean_diff / sqrt((sd_A ^ 2) / N_A + (sd_B ^ 2) / N_B)

        qu <- qt(sig_level / 2, df= df_ws, lower = FALSE)
        pt(qu, df = df_ws, ncp = t_stat, lower = FALSE) +
          pt(-qu, df = df_ws, ncp = t_stat, lower = TRUE)
      })
    }
    if (test_type == 3){
      power_eval <- quote({
        N_B <- N * percent_B
        N_A <- N - N_B
        df_ws <- (sd_A ^ 2 / N_A + sd_B ^ 2 / N_B) ^ 2 / (
          (sd_A ^ 2 / N_A) ^ 2 / (N_A - 1) + (sd_B ^ 2 / N_B) ^ 2 / (N_B - 1)
        )
        t_stat <- mean_diff / sqrt((sd_A ^ 2) / N_A + (sd_B ^ 2) / N_B)

        pt(qt(sig_level, df = df_ws, lower = FALSE), df = df_ws,
           ncp = t_stat, lower = FALSE)
      })
    }

    ### Evaluation for omitted parameter
    if (is.null(power)) {
      power <- eval(power_eval)
    } else if (is.null(N)) {
      # Require each group to have at least 5 observations
      min_N <-  max((5 / percent_B), (5 / (1 - percent_B)))
      N <- min_N
      if (eval(power_eval) >= power){
        warning("Target power achieved at lowest feasible sample size given percent_B")
      } else {
        N <- uniroot(function(N) eval(power_eval) - power,
                     c(min_N, max_sample + 1))$root
      }
    } else if (is.null(percent_B)) {
      min_percent_B <- max(0.001, 10 / N)
      search_grid <- seq(min_percent_B, 1 - min_percent_B, 0.0001)
      length_grid <- length(search_grid)
      diff_power <- sapply(search_grid, function(percent_B) eval(power_eval) - power)
      percent_B <- search_grid[min(c(1:length_grid)[diff_power > 0])]
    } else if (is.null(mean_diff)) {
      if (test_type == 1){
        mean_diff <- uniroot(function(mean_diff) eval(power_eval) - power,
                             c(-10000, 0))$root
      }
      if (test_type == 2){
        mean_diff <- uniroot(function(mean_diff) eval(power_eval) - power,
                             c(0, 10000))$root
      }
      if (test_type == 3){
        mean_diff <- uniroot(function(mean_diff) eval(power_eval) - power,
                             c(0, 10000))$root
      }
    } else if (is.null(sig_level)) {
      sig_level <- uniroot(function(sig_level) eval(power_eval) - power,
                           c(1e-10, 1 - 1e-10))$root
    } else stop("solution not found for given parameters")

    ### Output
    structure(list(N = N,
                   percent_B = percent_B,
                   mean_diff = mean_diff,
                   sd_A = sd_A,
                   sd_B = sd_B,
                   sig_level = sig_level,
                   power = power,
                   alternative = alternative,
                   method = "t-test Power Calculation"),
              class = "power.htest")
  }
