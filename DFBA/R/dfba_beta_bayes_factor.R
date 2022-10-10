#' Bayes Factor for Posterior Beta Distribution
#'
#' Given a beta posterior distribution and given a prior for the variate,
#' computes the Bayes factor for either point
#' or interval null hypotheses.
#'
#' @importFrom stats pbeta
#'
#' @param a The first shape parameter for the posterior beta distribution
#' @param b The second shape parameter for the posterior beta distribution
#' @param method Input is either "interval" if the null hypothesis is a range on the [0,1] interval or it is "point" if the null hypothesis is a single number in the [0,1] interval
#' @param H0 If method="interval", then the H0 input is vector of two values, which are lower and upper limits for the null hypothesis; if method="point", then the H0 input is single number, which is the null hypothesis value
#' @param a0 The first shape parameter for the prior beta distribution (default is 1)
#' @param b0 The second shape parameter for the prior beta distribution(default is 1)
#'
#' @return A list containing the following components:
#' @return \item{method}{The string of either "interval" or "point" corresponding to the type of null hypothesis tested}
#' @return \item{a}{The input value for the posterior beta first shape parameter}
#' @return \item{b}{The input value for the posterior beta second shape parameter}
#' @return \item{a0}{The first shape parameter for the prior beta distribution}
#' @return \item{b0}{The second shape parameter for the prior beta distribution}
#' @return \item{BF10}{The Bayes factor for the alternative over the null hypothesis}
#' @return \item{BF01}{The Bayes factor for the null over the alternative hypothesis}
#' @return \item{null_hypothesis}{The value for the null hypothesis when method = "point"}
#' @return \item{H0lower}{The lower limit of the null hypothesis when method = "interval"}
#' @return \item{H0upper}{The upper limit of the null hypothesis when method = "interval"}
#' @return \item{dpriorH0}{The prior probability density for the null point when method = "point"}
#' @return \item{dpostH0}{The posterior probability density for the null point when method = "point"}
#' @return \item{pH0}{The prior probability for the null hypothesis when method = "interval"}
#' @return \item{pH1}{The prior probability for the alternative hypothesis when method = "interval"}
#' @return \item{postH0}{The posterior probability for the null hypothesis when method = "interval"}
#' @return \item{postH1}{The posterior probability for the alternative hypothesis when method = "interval"}
#'
#' @details
#'
#' For a binomial variate with \code{n1} successes and \code{n2} failures, the
#' Bayesian analysis for the population success rate parameter phi is
#' distributed as a beta density function with shape parameters \code{a}
#' and \code{b} for \code{a = n1 + a0} and \code{b = n2 + b0} where \code{a0} and \code{b0} are the shape
#' parameters for the prior beta distribution. It is common for users
#' to be interested in testing hypotheses about the population phi
#' parameter. The Bayes factor is useful to assess if either the null
#' or the alternative hypothesis are credible.
#'
#' There are two types of null hypotheses -- an interval null hypothesis
#' and a point null hypothesis. For example, an interval null hypothesis
#' might be phi <= .5 with the alternative hypothesis being phi > .5,
#' whereas a point null hypothesis might be phi=.5 with the alternative
#' being phi != .5. It is conventional to call the null hypothesis H_0
#' and to call the alternative hypothesis H_1. For frequentist null
#' hypothesis testing, H_0 is assumed to be true, to see if this
#' assumption is likely or not. With the frequentist approach the null
#' hypothesis cannot be proved since it was assumed in the first place.
#' With frequentist statistics, H_0 is thus either retained
#' as assumed or it is rejected. Unlike the frequentist approach,
#' Bayesian hypothesis testing does not assume either H_0 or H_1; it
#' instead assumes a prior distribution for the population parameter
#' phi, and based on this assumption arrives at a posterior
#' distribution for the parameter given the data of n1 and n2 for the
#' binomial outcomes.
#'
#' There are two related Bayes factors - BF10 and BF01 where BF01=1/BF10.
#' When BF10>1 there is more support of the alternative hypothesis,
#' whereas when BF01>1 there is more support for the null hypothesis.
#' Thus in Bayesian hypothesis testing it is possible to build support
#' for either H_0 or H_1. In essence the Bayes factor is a measure of the
#' relative strength of evidence. There is no standard guideline
#' for recommending a decision about the prevailing hypothesis, but
#' several statisticians have made some suggested criteria. Jeffreys
#' (1961) suggested that a Bayes factor > 10 was "strong" and BF>100
#' was "decisive"; however, Kass and Raffrey (1995) suggested that
#' BF>20 was "strong" and BF>150 was "decisive". Chechile (2020) argued
#' from a decision-theory framework for a third option for the user to
#' decide "not to decide" if the prevailing Bayes factor is not
#' sufficiently large. From this decision-making perspective, Chechile
#' (2020) suggested that BF>19 was a "good bet-too good to disregard",
#' whereas BF>99 was "a strong bet-irresponsible to avoid" and
#' BF>20,001 was "virtually certain". Yet Chechile pointed out that
#' despite the Bayes factor value there is often some probability,
#' however small, for either hypothesis. Ultimately each academic
#' discipline has to set the standard for their field for the
#' strength of evidence. Yet even when the Bayes factor is below
#' the user's threshold for making claims about the hypotheses, the
#' value of the Bayes factor from one study can be nonetheless
#' valuable to other researchers and might be combined via a product
#' rule in a meta-analysis. So, the value of the Bayes factor has
#' a descriptive utility.
#'
#' The Bayes factor BF10 for an interval null is the ratio of the
#' posterior odds of H_1 to H_0 divided by the prior odds of H_1 to H_0.
#' Also the converse Bayes factor BF01 is the ratio of posterior odds of
#' H_0 to H_1 divided by the prior odds of H_0 to H_1; hence BF01=1/BF10.
#' If there is no change in the odds ratio as a function of new data
#' being collected, then BF10=BF01=1. But if evidence is more likely for
#' one of the hypotheses, then either BF10 or BF01 will be greater than
#' 1.
#'
#' The population parameter phi is distributed on the continuous interval
#' [0,1]. The prior and posterior beta distribution are probability
#' density displays. Importantly this means that no point has a nonzero
#' probability. Alternatively it means that the probability for any
#' mathematical point is zero. For this reason all point null hypotheses
#' have a probability measure of zero, but they can have a probability
#' density that can be different for prior and posterior distributions.
#' Interestingly there still is a meaningful Bayes factor for a point
#' hypothesis. As described in Chechile (2020),
#' \emph{BF10 = [P(H1|D)/P(H1)][P(H0)/P(H0|D)]} where D denotes the data. The
#' first term in this equation is 1/1=1. But the second term is of the
#' form of 0/0, which appears to undefined. However, by using
#' L'Hospital's rule, it can be proved the term \emph{P(H0)/P(H0|D)} is the
#' ratio of prior probability density at the null point divided by the
#' posterior probability density. This method for finding the Bayes
#' factor for a point is called the Savage-Dickey method because of the
#' separate contributions from both of those statisticians (Dickey &
#' Lientz, 1970).
#'
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press.
#'
#' Dickey, J. M., & Lientz, B. P. (1970). The weighted likelihood ratio, sharp hypotheses about chance, the order of a Markov chain. The Annals of Mathematical Statistics, 41, 214-226.
#'
#' Jeffreys, H. (1961). Theory of Probability (3rd ed.). Oxford: Oxford University Press.
#'
#' Kass, R. E., & Rafftery, A. E. (1995). Bayes factors. Journal of the American Statistical Association, 90, 773-795.
#'
#' @examples
#' ## Examples with the default uniform prior
#' dfba_beta_bayes_factor(a = 17,
#'                        b = 5,
#'                        method = "interval",
#'                        H0 = c(0, .5)
#'                        )
#' dfba_beta_bayes_factor(a = 377,
#'                        b = 123,
#'                        method = "point",
#'                        H0 = .75)
#'
#' # An example with the Jeffreys prior
#' dfba_beta_bayes_factor(a = 377,
#'                        b = 123,
#'                        method = "point",
#'                        H0 = .75,
#'                        a0 = .5,
#'                        b0 = .5
#'                        )
#'
#'
## An example where the null is a narrow interval (.5 plus or minus .0025)
#' dfba_beta_bayes_factor(a = 273,
#'                        b = 278,
#'                        method = "interval",
#'                        H0 = c(.4975,
#'                               .5025)
#'                        )
#'
#' @export
dfba_beta_bayes_factor<-function(a,
                                 b,
                                 method,
                                 H0,
                                 a0 = 1,
                                 b0 = 1
                                 ){
  if(method != "point" & method != "interval"){
    stop("method must be either 'point' or 'interval'")
  }

  if(a0 < 0|b0 < 0|is.na(a0)|is.na(b0)){
    stop("Both a0 and b0 must be greater than or equal to 0")
  }


  if(method == "point"){ #point method

    # Checking input validity
    # H0 between 0 and 1 (inclusive)
    if((H0 > 1)|(H0 < 0)){
    stop("H0 must be greater than or equal to 0 and less than or equal to 1")
  }

    # H0 is a point (for point method)
    if(length(H0) != 1){
      stop("'H0' must be a single numeric value when method = 'point'")
    }

    dpriorH0 = dbeta(H0,
                     a0,
                     b0)
    dpostH0 = dbeta(H0,
                    a,
                    b)
    BF10 <- ifelse(dpostH0 == 0,
                   Inf,
                   dpriorH0/dpostH0
                   )
    BF01 <- ifelse(dpriorH0 == 0,
                   Inf,
                   dpostH0/dpriorH0
                   )

  dfba_point_BF_list<-list(method = method,
                           a = a,
                           b = b,
                           a0 = a0,
                           b0 = b0,
                           BF10 = BF10,
                           BF01 = BF01,
                           null_hypothesis = H0,
                           dpriorH0 = dpriorH0,
                           dpostH0 = dpostH0
                           )

  } else{ #interval method

    # Checking input validity
    # Interval must be defined by 2 numbers
    if(length(H0) != 2){
      stop("'H0' must be a vector of two values (lower and upper) when method = 'interval'")
    }

    # Both parts of interval have to be between 0 and 1 (inclusive)
    if((H0[1] > 1)|(H0[1] < 0)|(H0[2] > 1)|(H0[2] < 0)){
      stop("H0 interval limits must be greater than or equal to 0 and less than or equal to 1")
    }

    # Upper limit has to be greater than lower limit
    if(H0[1] >= H0[2]){
      stop("When method = 'interval', H0 upper limit must be greater than H0 lower limit")
    }

    lowerH0value <- H0[1]
    upperH0value <- H0[2]

    pH0up = pbeta(upperH0value,
                  a0,
                  b0)

    pH0lo = pbeta(lowerH0value,
                  a0,
                  b0)

    pH0 = pH0up-pH0lo
    pH1 = 1-pH0

    postH0up = pbeta(upperH0value,
                     a,
                     b)
    postH0lo = pbeta(lowerH0value,
                     a,
                     b)

    postH0 = postH0up-postH0lo
    postH1 = 1-postH0

    BF10 <- ifelse(postH0 == 0,
                   Inf,
                   ((1/postH0)-1)/((1/pH0)-1)
                   )
    BF01 <- ifelse(postH1 == 0,
                   Inf,
                   ((1/pH0)-1)/((1/postH0)-1))

    dfba_interval_BF_list<-list(method = method,
                               a = a,
                               b = b,
                               a0 = a0,
                               b0 = b0,
                               BF10 = BF10,
                               BF01 = BF01,
                               H0lower = H0[1],
                               H0upper = H0[2],
                               pH0 = pH0,
                               pH1 = pH1,
                               postH0 = postH0,
                               postH1 = postH1

                               )
  }

  if(method == "point"){
    new("dfba_point_BF_out", dfba_point_BF_list)
    } else {
      new("dfba_interval_BF_out", dfba_interval_BF_list)
      }
}
