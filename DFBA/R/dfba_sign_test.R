#' Bayesian Sign Test
#'
#' Given two paired continuous variates \code{Y1} and \code{Y2}, provides a
#' Bayesian sign test to assess the positivity rate for the difference
#' \code{Y1 - Y2}.
#'
#' @param Y1 Vector of the continuous measurements for one group
#' @param Y2 Vector of the continuous values paired with the \code{Y1} vector for the values in a second group
#' @param a0 The first shape parameter for the prior beta distribution for the positive-sign rate parameter (default is 1). Must be positive and finite.
#' @param b0 The second shape parameter for the prior beta distribution for the positive-sign rate parameter (default is 1). Must be positive and finite.
#' @param prob_interval	Desired probability within interval limits for interval estimates of the positivity rate parameter (default is .95)
#'
#' @return A list containing the following components:
#' @return \item{Y1}{Vector of continuous values for the first within-block group}
#' @return \item{Y2}{Vector of continuous values for the second within-block group}
#' @return \item{a0}{First shape parameter for the prior beta distribution for the population parameter for the positivity rate}
#' @return \item{b0}{Second shape parameter for the prior beta distribution for the population positivity rate}
#' @return \item{prob_interval}{The probability within the interval limits for the interval estimate of population positivity rate}
#' @return \item{n_pos}{Sample number of positive differences}
#' @return \item{n_neg}{Sample number of negative differences}
#' @return \item{a_post}{First shape parameter for the posterior beta distribution for the population  positivity rate}
#' @return \item{b_post}{Second shape parameter for the posterior beta distribution for the population positivity rate for differences}
#' @return \item{phimean}{Mean of the posterior distribution for the positivity rate parameter}
#' @return \item{phimedian}{Median of the posterior distribution for the positivity rate parameter}
#' @return \item{phimode}{Mode of the posterior distribution for the positivity rate parameter}
#' @return \item{eti_lower}{Lower limit of the equal-tail interval  estimate of the positivity rate parameter}
#' @return \item{eti_upper}{Upper limt of the equal-tail interval estimate of the positivity rate parameter}
#' @return \item{hdi_lower}{Lower limit for the highest-density interval estimate of the positivity rate parameter}
#' @return \item{hdi_upper}{Upper limit for the highest-density interval estimate of the positivity rate parameter}
#' @return \item{post_H1}{Posterior probability that the positivity rate is greater than .5}
#' @return \item{prior_H1}{Prior probability that the positivity rate is greater than .5}
#' @return \item{BF10}{Bayes factor in favor of the alternative hypothesis that the positivity rate is greater than .5}
#' @return \item{BF01}{Bayes factor in favor of the null hypothesis that the positivity rate is equal to or less than .5}
#'
#' @details
#'
#' Given two paired continuous variates \eqn{Y_1} and \eqn{Y_2} for two
#' repeated measures, statistical tests for differences examine the difference
#' measure \eqn{d = Y_1 - Y_2}. The \eqn{t}-test is a conventional frequentist
#' parametric procedure to assess values of \eqn{d}. There are also two common
#' frequentist nonparametric tests for assessing condition differences: the sign
#' test and the Wilcoxon signed-rank test. The sign test is less powerful than
#' the Wilcoxon signed-rank test (Siegel & Castellan, 1988). The appeal of the
#' sign test, for some researchers, is that it is simple and - in some cases -
#' sufficient for demonstrating strong differences.
#'
#' The \code{dfba_sign_test()} function provides a Bayesian version of the sign
#' test (the function \code{dfba_wilcoxon()} provides the Bayesian signed-rank
#' test). While the Wilcoxon procedure uses both rank and sign information, the
#' sign test uses only sign information. The \code{dfba_sign_test()} function
#' finds the number of positive and negative \eqn{d} values, which appear in the
#' output as \code{n_pos} and \code{n_neg}, respectively. Note that it is
#' standard both in the frequentist sign test and in the frequentist Wilcoxon
#' signed-rank procedure to remove the \eqn{d} values that are zero. Consequently,
#' the signs for the nonzero \eqn{d} values are binary, so the posterior is a
#' beta distribution with shape parameters \eqn{a} - denoted in the output as
#' \code{a_post} and \eqn{b} - denoted in the output as \code{b_post} - where
#' \code{a_post = a0 + n_pos} and \code{b_post = b0 + n_neg} and \code{a0} and \code{b0}
#' are the respective first and second beta shape parameters for the prior
#' distribution. The default prior is a uniform distribution \code{a0 = b0 = 1}.
#'
#' The function estimates the population rate for positive signs by calling
#' \code{dfba_beta_descriptive()} using the computed \code{a_post} and \code{b_post}
#' as arguments. Since interest in the sign test is focused on the null
#' hypothesis that the positivity rate is less than or equal to .5,
#' \code{dfba_sign_test()} calls \code{dfba_beta_bayes_factor()} to calculate the
#' prior and posterior probabilities for the alternative hypothesis that the
#' positivity rate is greater than .5. The output also includes the Bayes
#' factors \code{BF10} and \code{BF01}, where \code{BF01 = 1/BF10}. Large values
#' of \code{BF01} indicate support for the null hypothesis; large values of \code{BF10}
#' indicate support for the alternative hypothesis.
#'
#' @references
#' Chechile, R. A. (2020). Bayesian Statistics for Experimental Scientists: A
#' General Introduction Using Distribution_Free Methods. Cambridge, MIT Press.
#'
#' Siegel, S., & Castellan, N. J. (1988). Nonparametric Statistics for the
#' Behavioral Sciences. New York: McGraw Hill.
#'
#' @seealso
#' \code{\link{dfba_beta_descriptive}} for details on the descriptive statistics
#' in the output
#'
#' \code{\link{dfba_beta_bayes_factor}} for details on Bayes Factors calculated
#' on the basis of beta distributions
#'
#' \code{\link{dfba_wilcoxon}} for an alternative, more powerful Bayesian
#' nonparametric test for evaluting repeated-measures data.
#'
#' @examples
#'
#' measure_1 <- c(1.49, 0.64, 0.96, 2.34, 0.78, 1.29, 0.72, 1.52,
#'                0.62, 1.67, 1.19, 0.860)
#'
#' measure_2 <- c(0.53, 0.55, 0.58, 0.97, 0.60, 0.22, 0.05, 13.14,
#'                0.63, 0.33, 0.91, 0.37)
#'
## Example with defaults for prior and for interval width
#' dfba_sign_test(Y1 = measure_1,
#'                Y2 = measure_2)
#'
## Example with Jeffreys prior and 99% interval estimates
#' dfba_sign_test(measure_1,
#'                measure_2,
#'                a0 = .5,
#'                b0 = .5,
#'                prob_interval = .99)
#'
#' @export
dfba_sign_test<-function(Y1,
                         Y2,
                         a0 = 1,
                         b0 = 1,
                         prob_interval = .95){

  # Check if vectors have the same length

  if (length(Y1) != length(Y2)) {
    stop("Y1 and Y2 must have the same length. This function is for paired within-block data.")
  }

  # Check if beta shape parameters are > 0
  if (a0 <= 0 |
      a0 == Inf|
      is.na(a0)|
      b0 <= 0|
      b0 == Inf|
      is.na(b0)) {
    stop("Both a0 and b0 must be positive and finite.")
  }

  # Check if interval is between 0 and 1

  if (prob_interval < 0 |
      prob_interval > 1) {
    stop("The probability for the interval estimation must be a proper proportion.")
  }

  filtered_data <- data.frame(Y1 = Y1,
                              Y2 = Y2,
                              d = Y1 - Y2)[complete.cases(data.frame(Y1, Y2, Y1 - Y2)),]

  Y1 <- filtered_data$Y1
  Y2 <- filtered_data$Y2
  d <- filtered_data$d
  l1 <- nrow(filtered_data)

  if (l1 < 3) {
    stop("There are not enough values in the Y1 and Y2 vectors for meaningful results.")
  }

  sdd <- sd(d)

  n <- sum(abs(d) > sdd/30000)


  if (n == 0) {
    stop("Y1 and Y2 differences are all trivial")
  }

  n_pos <- sum(d > sdd/30000)
  n_neg <- n- n_pos
  a_post <- n_pos + a0
  b_post <- n_neg + b0

  des_out<-dfba_beta_descriptive(a_post,
                                 b_post,
                                 prob_interval = prob_interval)
  phimean <- des_out$x_mean
  phimedian <- des_out$x_median
  phimode <- des_out$x_mode
  eti_lower <- des_out$eti_lower
  eti_upper <- des_out$eti_upper
  hdi_lower <- des_out$hdi_lower
  hdi_upper <- des_out$hdi_upper
  prior_H1 <- 1-pbeta(.5,
                      a0,
                      b0)
  post_H1 <- 1-pbeta(.5,
                     a_post,
                     b_post)

  out_BF <- dfba_beta_bayes_factor(a_post,
                                   b_post,
                                   method="interval",
                                   H0 = c(0,
                                          .5))

    BF10 <- out_BF$BF10
    BF01 <- out_BF$BF01

      sign_list<-list(Y1 = Y1,
                      Y2 = Y2,
                      a0 = a0,
                      b0 = b0,
                      prob_interval = prob_interval,
                      n_pos = n_pos,
                      n_neg = n_neg,
                      a_post = a_post,
                      b_post = b_post,
                      phimean = phimean,
                      phimedian = phimedian,
                      phimode = phimode,
                      eti_lower = eti_lower,
                      eti_upper = eti_upper,
                      hdi_lower = hdi_lower,
                      hdi_upper = hdi_upper,
                      post_H1 = post_H1,
                      prior_H1 = prior_H1,
                      BF10 = BF10,
                      BF01 = BF01)

  new("dfba_sign_test_out", sign_list)
}



