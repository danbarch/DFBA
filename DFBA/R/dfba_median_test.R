#' Bayesian Median Test
#'
#' Description
#'
#' Given two independent groups of continuous variables, performs a Bayesian
#' analysis of the likelihood of observing an above-median value from one of
#' the groups relative to expectation.
#'
#' @param E Numeric vector of values for the continuous measurements for group 1 (generically denoted \code{E} for \emph{Experimental} group).
#' @param C	Numeric vector of values for the continuous measurements for group 2 (generically denoted \code{C} for \emph{Control} group).
#' @param a0 The first shape parameter for the prior beta distribution for the binomial parameter \code{phi} (default is 1). Must be positive and finite.
#' @param b0 The second shape parameter for the prior beta distribution for the binomial parameter \code{phi} (default is 1). Must be positive and finite.
#'
#' @return A list containing the following components:
#' @return \item{median}{The sample combined median for the \code{E} and \code{C} values}
#' @return \item{nE}{The number of scores from group \code{E}}
#' @return \item{nC}{The number of scores from group \code{C}}
#' @return \item{Ebaserate}{The proportion nE/(nE+nC)}
#' @return \item{Cbaserate}{The proportion nC/(nE+nC)}
#' @return \item{nEabove}{Number of \code{E} responses above the median}
#' @return \item{nCabove}{Number of \code{C} responses above the median}
#' @return \item{nEbelow}{Number of \code{E} responses at or below median}
#' @return \item{nCbelow}{Number of \code{C} response at or below median}
#' @return \item{a0}{The first shape parameter for the prior beta distribution for the population binomial parameter}
#' @return \item{b0}{The second shape parameter for the prior beta distribution for the population binomial parameter}
#' @return \item{a.post}{Posterior first shape parameter for the beta distribution for the probability that an above-median response is from the \code{E} group}
#' @return \item{b.post}{Posterior second shape parameter for the beta distribution for the probability that an above-median response is from the \code{E} group}
#' @return \item{postEhi}{Posterior probability that an above-median response exceeds the \code{E} group base rate}
#' @return \item{postChi}{Posterior probabilty that an above-median response exceeds the \code{C} group base rate}
#' @return \item{priorEhi}{The probability that a beta prior distribution would exceed the \code{E} group base rate}
#' @return \item{priorChi}{The probability that a beta prior distribution would exceed the \code{E} group base rate}
#' @return \item{BF10E}{The Bayes factor in favor of the hypothesis that an above-median response from the \code{E} group is more probable than the \code{E} expected base rate}
#' @return \item{BF10C}{The Bayes factor in favor of the hypothesis that an above-median response from the \code{C} group is more probable than the \code{C} group base rate}
#'
#' @details
#'
#' Given continuous measurements \eqn{E} and \eqn{C} from two separate and
#' independent groups, a combined sample median value can be computed. For the
#' frequentist median test, a 2x2 table is created. Row 1 consists of the
#' frequencies of the above-median responses in terms of the two groups (\emph{i.e.},
#' \code{nEabove} and \code{nCabove}). Row 2 has the respective frequencies for the
#' values that are at or below the combined median (\emph{i.e.}, \code{nEbelow} and
#' \code{nCbelow}). See Siegel & Castellan (1988) for the details concerning the
#' frequentist median test.
#'
#' Chechile (2020) provided an alternative Bayesian analysis for the median-test
#' procedure of examining continuous data in terms of the categorization of the
#' values as being either above the combined median or not. The frequencies in
#' row 1 (above median response) are binomial frequencies in terms of the group
#' origin (\emph{i.e.}, \eqn{E} versus \eqn{C}). From a Bayesian perspective, a
#' population-level \eqn{\phi} parameter can be defined for the population
#' proportion of \eqn{E} values that are above the combined sample median.
#' Similarly, the frequencies for the scores at or below the combined sample
#' median can also be examined; in that case, the corresponding population
#' proportion in the E condition must be \eqn{1-\phi}. Thus, it is sufficient only
#' to examine the above-median frequencies to make an inference about the \eqn{\phi}
#' parameter. Since this is a binomial problem, the prior and posterior
#' distributions for the population \eqn{\phi} parameter belong to the beta family
#' of distributions. The default prior for this function is the uniform
#' distribution, \emph{i.e}, \code{a0 = b0 = 1}. The posterior shape parameters
#' for \eqn{\phi} are \code{a.post = a0 + nEabove} and
#' \code{b.post = b0 + nCabove}.
#'
#' Because the number of scores in groups \eqn{E} and \eqn{C} might be very
#' different, it is important to examine the \eqn{\phi} parameter relative to an
#' expected base-rate value from the sample. For example, suppose that there are
#' \code{nE = 90} values from the \eqn{E} group and \code{nC = 10} values from
#' the \eqn{C} group. In this example, there are 50 scores that are above the
#' combined median (and no ties that would result in fewer than half of the
#' scores being greater than the median) that should be examined to see if \eqn{\phi}
#' is greater than 0.9. If there were no difference between the \eqn{E} and \eqn{C}
#' conditions whatsoever in this hypothetical example, then about 90 percent of
#' the above-median values would be from the \eqn{E} group. If the posterior
#' \eqn{\phi} parameter were substantially above the group \eqn{E} base rate,
#' then that would support the hypothesis that group \eqn{E} has larger values
#' than group \eqn{C} in the population.
#'
#' The \code{dfba_median_test()} provides the descriptive sample information for
#' the combined median as well as the entries for a table for the frequencies
#' for the \eqn{E} and \eqn{C} scores that are above the median, as well as the
#' frequencies for the \eqn{E} and \eqn{C} scores at or below the median. The
#' function also provides the prior and posterior probabilities that the \eqn{E}
#' and \eqn{C} groups exceeding their respective base rates for a value being
#' above the median. The function also evaluates the hypotheses that the \eqn{E}
#' and \eqn{C} response rates for the above-median responses exceeding their
#' base rate. Bayes factors are provided for these hypothesis.
#'
#' Because the Bayesian median test ignores the available rank-order
#' information, this procedure has less power than the Bayesian Mann-Whitney
#' analysis that can be computed for the same data. Nonetheless, sometimes
#' researchers are interested if condition differences are so strong that even a
#' lower power median test can detect the difference.
#'
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
#' \code{\link{dfba_mann_whitney}} for a more powerful alternative Bayesian
#' analysis of the \eqn{E} and \eqn{C} values that use rank order information.
#'
#'
#' @examples
#'
#' ## Example with the default uniform prior
#' group1 <- c(12.90, 10.84, 22.67, 10.64, 10.67, 10.79, 13.55, 10.95, 12.19,
#'             12.76, 10.89, 11.02, 14.27, 13.98, 11.52, 13.49, 11.22, 15.07,
#'             15.74, 19.00)
#'
#' group2 <- c(4.63, 58.64, 5.07, 4.66, 4.13, 3.92, 3.39, 3.57, 3.56, 3.39)
#'
#' dfba_median_test(E = group1,
#'                  C = group2)
#'
#' ## Example with the Jeffreys prior
#' dfba_median_test(group1,
#'                  group2,
#'                  a0 = .5,
#'                  b0 = .5)
#'
#'@export
dfba_median_test <- function(E,
                             C,
                             a0 = 1,
                             b0 = 1){
# Check if a0 and b0 are positive
  if (a0 <= 0|
      a0 == Inf|
      b0 <= 0|
      b0 == Inf|
      is.na(a0)|
      is.na(b0)){
    stop("Both a0 and b0 must be positive and finite.")
    }

  # Remove NA values from E and C

  E <- E[!is.na(E)]
  C <- C[!is.na(C)]

#  Etemp=E
#  Ctemp=C
#  jc=0
#  for (j in 1:length(E)){
#    if (is.na(Etemp[j])){
#    } else {
#      jc=jc+1
#      E[jc]=Etemp[j]}
#  }
#  E=E[1:jc]
#
#  kc=0
#  for (k in 1:length(C)){
#    if (is.na(Ctemp[k])){} else {
#      kc=kc+1
#      C[kc]=Ctemp[k]}
#  }
#  C=C[1:kc]

#  Y<-c(E, C)

  # Find overall median
    med=median(c(E, C))

#  cat("Overall median is",":","\n")
#  cat(med," ","\n")
#  cat(" ","  ","\n")

#  l1=length(E)
#  l2=length(C)

    # Count E and C values above and below the median
#  nEabove=0
#  nEbelow=0

    nEabove = sum(E > med)
    nEbelow = sum(E <= med)

    nCabove = sum(C > med)
    nCbelow = sum(C <= med)

#  for (I in 1:length(E)){
#    if (E[I]>med){nEabove=nEabove+1} else {nEbelow=nEbelow+1}}
#  nCabove=0
#  nCbelow=0
#  for (I in 1:length(C)){
#    if (C[I]>med){nCabove=nCabove+1} else {nCbelow=nCbelow+1}}

#  cat("Frequencies above the median are",":","\n")
#  cat("E","   ","C","\n")
#  cat(nEabove,"   ",nCabove,"\n")
#  cat("Frequencies at or below the median are",":","\n")
#  cat("E","   ","C","\n")
#  cat(nEbelow,"   ",nCbelow,"\n")

# posterior beta parameters
  a.post = nEabove+a0
  b.post = nCabove+b0

  Ebaserate = length(E)/(length(E)+length(C))
  Cbaserate = 1 - Ebaserate

  postEhi= 1 - pbeta(Ebaserate,
                     a.post,
                     b.post)
  postChi= 1 - postEhi
  priorEhi= 1 - pbeta(Ebaserate,
                      a0,
                      b0)
  priorChi= 1 - priorEhi



#  cat("Respective baserates for E and C responses are",":","\n")
#  cat(Ebaserate," ",Cbaserate,"\n")
#  cat(" ","  ","\n")

#  cat("Following is an analysis of the responses above the median"," ","\n")
#  cat("to see if the rates of these above-median responses are higher for E or C"," ","\n")

#  cat("Posterior beta shape parameter for the phi parameter"," ","\n")
#  cat("a.post"," ","b.post","\n")
#  cat(a.post,"     ",b.post,"\n")
#  cat(" ","  ","\n")

#  cat("Prior prob. that E and C exceed their respective baserates are",":","\n")
#  cat(priorEhi," ",priorChi,"\n")
#  cat(" "," ","\n")
#  cat("Posterior prob. that E and C exceed their respective baserates are",":","\n")
#  cat(postEhi," ",postChi,"\n")
#  cat(" "," ","\n")

  # Calculate Bayes Factors
  BF10E <- ifelse(postChi == 0,
                  Inf,
                  ((1/postChi) - 1)/((1/priorChi) - 1))
  BF01E <- ifelse(postEhi == 0,
                  Inf,
                  ((1/priorChi) - 1)/((1/postChi) - 1))


#  cat("Bayes factor BF10 E>E_baserate"," ","Bayes factor BF10 C>C_baserate","\n")
#  cat(BF10E,"                     ",BF01E,"\n")

  # list of output objects
    outmedian <- list(median=med,
                    nE = length(E),
                    nC = length(C),
                    Ebaserate = Ebaserate,
                    Cbaserate = Cbaserate,
                    nEabove = nEabove,
                    nCabove = nCabove,
                    nEbelow = nEbelow,
                    nCbelow = nCbelow,
                    a0 = a0,
                    b0 = b0,
                    a.post = a.post,
                    b.post = b.post,
                    postEhi = postEhi,
                    postChi = postChi,
                    priorEhi = priorEhi,
                    priorChi = priorChi,
                    BF10E = BF10E,
                    BF10C = BF01E)

    # Define new class for median test output

    new("dfba_median_test_out", outmedian)

}

