#' Descriptive Statistics for a Beta Distribution
#'
#' Given the two shape parameters for a beta distribution, the function provides
#' central tendency statistics, interval limits, and density and cumulative
#' probabilities.
#'
#' @param a The first shape parameter for the beta distribution. Must be positive and finite.
#' @param b The second shape parameter for the beta distribution. Must be positive and finite.
#' @param prob_interval	Desired probability within interval limits (default is .95)
#'
#' @return A list containing the following components:
#' @return \item{a}{The first beta shape parameter}
#' @return \item{b}{The second beta shape parameter}
#' @return \item{prob_interval}{The probability for interval estimates}
#' @return \item{x_mean}{The mean of the distribution}
#' @return \item{x_median}{The median of the distribution}
#' @return \item{x_mode}{The mode for the distribution}
#' @return \item{eti_lower}{The equal-tail lower interval limit}
#' @return \item{eti_upper}{The equal-tail upper interval limit}
#' @return \item{hdi_lower}{The lower limit for the highest-density interval}
#' @return \item{hdi_upper}{The upper limit for the highest-density interval}
#' @return \item{outputdf}{A dataframe of \code{x}, density, and cumulative probability for \code{x} from 0 to 1 in steps of .005}
#'
#' @details
#'
#'
#' The density function for a beta variate is
#' \deqn{f(x) = \begin{cases} Kx^{a-1}(1-x)^{b-1} & \quad \textrm{if } 0 \le x \le 1, \\0 & \quad \textrm{otherwise} \end{cases}}
#' where \deqn{K = \frac{\Gamma(a + b)}{\Gamma(a)\Gamma(b)}.}
#' (Johnson, Kotz, & Balakrishnan, 1995). The two shape parameters \eqn{a} and \eqn{b} must
#' be positive values.
#'
#' The \code{dfba_beta_descriptive()} function provides features
#' to complement the beta distribution functions available in the \strong{stats}
#' package. The function provides the mean, median, and mode for a
#' beta variate in terms of its two shape parameters.
#'
#' While the mean and median are straightforward, there are several conditions
#' that result in an undefined mode. When either (1) \eqn{a = b = 1}, (2)  \eqn{a < 1},
#' or (3) \eqn{b < 1}, the mode is undefined. For example, when \eqn{a = b = 1},
#' the function is the uniform distribution, which does not have a modal value.
#' The other cases above result in the density function diverging at either
#' \eqn{x = 0} or \eqn{x = 1}. The function returns a value of \code{NA} for the
#' mode for all the cases where a unique mode does not exist.
#'
#' For interval estimation, the function finds an equal-tail interval limits in
#' all cases, and it also provides the highest-density limits when there is a
#' well-defined mode. When the mode does not exist, the function returns \code{NA}
#' for the limits for the highest-density interval (HDI). For interval
#' estimation, the probability between the lower and upper limit is the
#' probability specified in the \code{prob_interval} input. The
#' \code{dfba_beta_descriptive()} output object includes a dataframe that has
#' density and cumulative probability information that can be used for plotting.
#'
#' @references
#' Johnson, N. L., Kotz S., and Balakrishnan, N. (1995). \emph{Continuous Univariate}
#' \emph{Distributions}, Vol. 1, New York: Wiley.


#' @seealso
#' \code{\link[stats:Distributions]{Distributions}} for additional details on
#' functions for the beta distribution in the \strong{stats} package.


#' @examples
#'
#' dfba_beta_descriptive(a = 38,
#'                       b = 55)
#'
#' dfba_beta_descriptive(38,
#'                       55,
#'                       prob_interval=.99)
#'
#' @export
dfba_beta_descriptive <- function(a,
                                  b,
                                  prob_interval = .95){

  if (prob_interval > 1|
      prob_interval < 0){
    stop("The probability for the interval estimate must be between 0 and 1.")
    }

  if (a <= 0|
      a == Inf|
      b <= 0|
      b == Inf|
      is.na(a)|
      is.na(b)){
    stop("Both the a and b shape parameters for a beta must be positive and finite")
    }

  phimean = a/(a+b)
  phimedian = qbeta(.5,
                    a,
                    b)


  if ((a == 1 &
       b == 1)|
      a < 1|
      b < 1){
    phimode <- NA
    } else {
    phimode = (a-1)/(a+b-2)
    }

#  m1="Centrality Statistics"
#  cat(m1,"are :","\n")
#  cat("Mean","    ","Median","    ","Mode","\n")
#  cat(phimean," ",phimedian," ",phimode,"\n")
#  cat(" ","  ","\n")

#  hdiper = 100*prob_interval
#  m2 = as.character(hdiper)
#  cat(m2,"percent interval limits are :","\n")
#  cat("Interval with equal tails",":","\n")
  qlequal = qbeta((1-prob_interval)/2,
                  a,
                  b)
  qhequal = qbeta(prob_interval+((1-prob_interval)/2),
                  a,
                  b)
#  cat(qlequal," ",qhequal,"\n")
#
#  cat("Highest Density Interval",":","\n")

  if ((a == 1 & b == 1)|
      a < 1|
      b < 1){
    qLmin=NA
    qHmax=NA
    } else {
      alphaL = seq(0,
                   1 - prob_interval,
                   (1 - prob_interval)/1000)
      qL=qbeta(alphaL,
               a,
               b)
      qH=qbeta(prob_interval+alphaL,
               a,
               b)
      diff = qH - qL
      I=1
      mindiff = min(diff)
      while (diff[I] > mindiff){
        I=I+1}
      qLmin=qL[I]
      qHmax=qH[I]
    }

 # cat(qLmin," ",qHmax,"\n")

  x = seq(0, 1, .005)
  y = dbeta(x, a, b)
  ycumulative = pbeta(x, a, b)

  outputdf<-data.frame(x = x,
                       density = y,
                       cumulative_prob = ycumulative)

  out_descriptive<-list(a = a,
                        b = b,
                        prob_interval = prob_interval,
                        x_mean = phimean,
                        x_median = phimedian,
                        x_mode = phimode,
                        eti_lower = qlequal,
                        eti_upper = qhequal,
                        hdi_lower = qLmin,
                        hdi_upper = qHmax,
                        outputdf = outputdf)

  new("dfba_beta_descriptive_out", out_descriptive)
}


