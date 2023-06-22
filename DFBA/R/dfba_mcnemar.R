#' Bayesian Repeated-Measures McNemar Test for Change
#'
#' Given a randomized-block or repeated-measures design where the response is
#' coded as either 0 or 1, examines the subset of cases where there
#' is a change in the response between the two measurements and provides a
#' Bayesian analysis of the population change rate phi_rb \eqn{(\phi_{rb})} between
#' the two measurements.
#'
#' @importFrom graphics boxplot lines par
#' @import methods
#' @importFrom stats median
#'
#' @param n_01 The number of cases where the first response is 0 and the second response is 1.
#' @param n_10 The number of cases where the first response is 1 and the second response is 0.
#' @param a0 The first shape parameter for the prior beta distribution for the \code{phi_rb} parameter. Must be positive and finite.
#' @param b0 The second shape parameter for the prior beta distribution for the \code{phi_rb} parameter. Must be positive and finite.
#' @param prob_interval	Desired probability for interval estimates for \code{phi_rb} (default is .95).
#'
#' @return A list containing the following components:
#' @return \item{n_01}{The number of cases where the first response is 0 and the second response is 1}
#' @return \item{n_10}{The number of cases where the first response is 1 and the second response is 0}
#' @return \item{prob_interval}{Desired posterior probability within the equal-tail interval limits for \code{phi_rb}}
#' @return \item{a0}{The first shape parameter for the prior beta distribution for the \code{phi_rb} parameter}
#' @return \item{b0}{The second shape parameter for the prior beta distribution for the \code{phi_rb} parameter}
#' @return \item{a.post}{First shape parameter for the posterior beta distribution for the \code{phi_rb} parameter}
#' @return \item{b.post}{Second shape parameter for the posterior beta distribution for the \code{phi_rb} parameter}
#' @return \item{post.mean}{Posterior mean for \code{phi_rb}}
#' @return \item{post.median}{Posterior median for \code{phi_rb}}
#' @return \item{post_eti_lower}{Lower limit for the posterior equal-tail interval estimate for \code{phi_rb} that contains the probability defined in \code{prob_interval}}
#' @return \item{post_eti_upper}{Upper limit for the posterior equal-tail interval estimate for phi_rb that contains the probability defined in \code{prob_interval}}
#' @return \item{BF10point}{The Bayes factor against the point null hypothesis that \code{phi_rb = .5}}
#' @return \item{BF10interval}{The Bayes factor against the interval null hypothesis that \code{phi_rb} is less than or equal to \code{.5}}
#' @return \item{postH1}{The posterior probability that \code{phi_rb > .5}}
#'
#' @details
#'
#' Sometimes, researchers are interested in the detection of a change in the
#' response rate pre- and post-treatment. The frequentist McNemar test is a
#' nonparametric test that examines the subset of binary categorical responses
#' where the response changes between the two tests (Siegel & Castellan, 1988).
#' The frequentist test assumes the null hypothesis that the change rate is
#' 0.5. Chechile (2020) pointed out that the subset of change cases are binomial
#' data, so a Bayesian analysis can be done for the population
#' response-switching rate \eqn{\phi_{rb}} (styled \code{phi_rb} elsewhere in
#' the documentation for this function). Both the prior and posterior
#' distribution for \eqn{\phi_{rb}} are beta distributions.
#'
#' The user should be aware that the McNemar test is a change-detection
#' assessment of a binary response. To illustrate this fact, consider the
#' hypothetical case of a sample of 50 people who evaluate two political
#' candidates before and after a debate. Suppose 26 people prefer Candidate
#' A both before and after the debate and 14 people prefer Candidate B both
#' before and after the debate, but 9 people switch their preference from
#' Candidate A to Candidate B and 1 person switches their preference from
#' Candidate B to Candidate A. Despite the fact that this sample has 50
#' participants, it is only the 10 people who switch their preference that are
#' being analyzed with the McNemar test. Among this subset, there is evidence
#' that Candidate B did better on the debate. Overall, support for Candidate A
#' in the whole sample fell from 35 out of 50 (70\%) to 27 out of 50 (54\%):
#' still a majority, but a smaller one than Candidate A enjoyed prior to the
#' debate.
#'
#' The \code{dfba_mcnemar()} function requires two inputs, \code{n_01} and
#' \code{n_10}, which are, respectively, the number of \eqn{0 \to 1} changes
#' and the number of \eqn{1 \to 0} switches in the binary responses between the
#' two tests. Since the cases where there is a switch are binomial trials,
#' the prior and posterior distributions for \eqn{\phi_{rb}} are beta distributions.
#' The prior distribution shape parameters are \code{a0} and \code{b0}. The
#' default prior is a uniform distribution (\emph{i.e.}, \code{a0 = b0 = 1}).
#' The \code{prob_interval} argument stipulates the probability within the
#' equal-tail interval limits for \eqn{\phi_{rb}}. The default value for that
#' argument is \code{prob_interval =.95}.
#'
#' Besides computing the posterior mean, posterior median, equal-tail interval
#' limits, and the posterior probability that \eqn{\phi_{rb} > .5}, the function
#' also computes two Bayes factor values. One is the \emph{point} Bayes factor \code{BF10}
#' against the null hypothesis that \code{phi_rb = 0.5}. The second Bayes
#' factor \code{BF10} is the \emph{interval} Bayes factor against the null hypothesis
#' that \eqn{\phi_{rb} \le 0.5}. If the interval Bayes factor BF10 is very low,
#' then there is support to some degree for the null hypothesis that
#' \eqn{\phi_{rb} < 0.5}. In this case the Bayes factor \code{BF01} in support of
#' the interval null hypothesis is given by \code{BF01 = 1/BF10}.
#'
#' @references
#' Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists: A
#' General Introduction Using Distribution-Free Methods. Cambridge: MIT Press.
#'
#' Siegel, S., & Castellan, N. J. (1988) Nonparametric Statistics for the
#' Behavioral Sciences. New York: McGraw Hill.
#'
#' @seealso
#' \code{\link{dfba_beta_bayes_factor}} for further documentation about the
#' Bayes factor and its interpretation.
#'
#' @examples
#' ## Examples with default value for a0, b0 and prob_interval
#'
#' dfba_mcnemar(n_01 = 17,
#'              n_10 = 2)
#'
#' ## Using the Jeffreys prior and .99 equal-tail interval
#'
#' dfba_mcnemar(n_01 = 17,
#'              n_10 = 2,
#'              a0 = .5,
#'              b0 = .5,
#'              prob_interval = .99)
#'
#' @export
dfba_mcnemar <- function(n_01,
                         n_10,
                         a0 = 1,
                         b0 = 1,
                         prob_interval = .95) {

  if (a0<=0|
      a0 == Inf|
      b0<=0|
      b0 == Inf|
      is.na(a0)|
      is.na(b0)){
    stop("Both a0 and b0 must be positive and finite.")
  }

  if (prob_interval >= 1|
      prob_interval <= 0){
    stop("prob_interval must between 0 and 1")
    }

  if (n_01 < 0|
      n_10 < 0|
      is.na(n_01)|
      is.na(n_10)){
    stop("Neither n_01 nor n_10 can be negative")
    }

    if (n_01 != round(n_01)|
        n_10 != round(n_10)){
    stop("n_01 and n_10 must be integers")
      }

  a.post <- a0 + n_01
  b.post <- b0 + n_10

  mean_phi_rb <- a.post/(a.post + b.post)
  median_phi_rb <- qbeta(.5,
                        a.post,
                        b.post)


  eti_lower <- qbeta((1-prob_interval)/2,
                     a.post,
                     b.post)
  eti_upper <- qbeta(1-((1-prob_interval)/2),
                     a.post,
                     b.post)

    outBFpoint <- dfba_beta_bayes_factor(a = a.post,
                                         b = b.post,
                                         method = "point",
                                         H0 = .5,
                                         a0 = a0,
                                         b0 = b0)

    outBFinterval <- dfba_beta_bayes_factor(a = a.post,
                                            b = b.post,
                                            method = "interval",
                                            H0 = c(0, .5),
                                            a0 = a0,
                                            b0 = b0)


   mcnemar_out<-list(n_10 = n_10,
                     n_01 = n_01,
                     prob_interval = prob_interval,
                     a0 = a0,
                     b0 = b0,
                     a.post = a.post,
                     b.post = b.post,
                     post_mean = mean_phi_rb,
                     post_median = median_phi_rb,
                     eti_lower = eti_lower,
                     eti_upper = eti_upper,
                     BF10point = outBFpoint$BF10,
                     BF10interval = outBFinterval$BF10,
                     postH1 = outBFinterval$postH1)

   new("dfba_mcnemar_out", mcnemar_out)
}


