#' Bayesian Contrasts
#'
#' This function implements a Bayesian analysis of a linear contrast of
#' conditions when there are 2 or more independent conditions and where
#' the variate for each condition is a binomial.
#'
#'
#' @importFrom stats rbeta
#' @importFrom stats var
#' @importFrom stats quantile
#'
#' @param n1_vec A vector of length K that consists of the observed number of successes for the categorical variable in each of the K separate conditions
#' @param n2_vec A vector of length K that consists of the observed number of failures for the  categorical variable in each of the K separate conditions
#' @param contrast_vec A vector of coefficients of a linear comparison among the conditions where the sum of all the coefficient must be 0 and the sum of the positive coefficients must be 1 and the sum of the negative coefficients must be -1
#' @param a0_vec A vector of length K that consists of the prior a0 shape parameters for the separate betas (the default values are 1)
#' @param b0_vec		A vector of length K that consists of the prior b0 shape parameters for the separate betas (the default values are 1)
#' @param prob_interval Desired probability for equal-tail interval estimate on the contrast (default is 0.95)
#' @param samples The desired number of Monte Carlo samples taken from each posterior beta variate (default is 10000)
#'
#'
#' @return A list containing the following components:
#' @return \item{mean}{Exact posterior mean estimate for the contrast}
#' @return \item{lower_limit}{The lower equal-tail limit for the contrast for the probability interval value specified by \code{prob_interval}}
#' @return \item{upper_limit}{The upper equal-tail limit for the contrast for the probability interval value specified by \code{prob_interval}}
#' @return \item{prob_positive_delta}{Posterior probability that the contrast is positive}
#' @return \item{prior_positive_delta}{Prior probability that the contrast is positive}
#' @return \item{bayes_factor}{The Bayes factor for the posterior-to-prior odds for a positive contrast to a non-positive contrast}
#' @return \item{delta_quantiles}{Quantile values (\code{probs = seq(0, 1, 0.01)}) for the posterior contrast from the Monte Carlo sampling}
#' @return \item{a_vec}{A vector of length K that consists of the posterior \code{a} shape parameters for the separate posterior beta distributions}
#' @return \item{b_vec}{A vector of length K that consists of the posterior \code{b} shape parameters for the separate posterior beta distributions}
#' @return \item{a0_vec}{A vector of length K that consists of the prior a0 shape parameters for the separate prior beta distributions}
#' @return \item{b0_vec}{A vector of length K that consists of the prior b0 shape parameters for the separate prior beta distributions}
#' @return \item{contrast_vec}{A vector for the contrast coefficients for a linear comparison of posterior beta variates}
#' @return \item{prob_interval}{The probability for the equal-tail estimate for the contrast (default is 0.95)}
#' @return \item{samples}{The number of Monte Carlo samples from the K separate posterior beta distributions}
#'
#' @details
#' Since the Bayesian analysis for each separate condition has a posterior beta
#' distribution with known shape parameters, the program approximates, \emph{via}
#' Monte Carlo sampling, a linear contrast among the set of independent beta
#' distributions because the contrast of beta distributions is not a known
#' probability model.
#'
#' Given a binomial categorical variate for each of K independent
#' conditions with K >= 2, the standard frequentist nonparametric
#' analysis is to do a chi-squared test with K-1 degrees of freedom
#' (Siegel & Castellan, 1988). Hypothesis testing for the
#' frequentist chi-squared test assesses the sharp-null hypothesis
#' that the binomial success rate is exactly equal in all the
#' conditions. But this point-null hypothesis is not an interesting
#' question about the population success rate from a Bayesian
#' viewpoint because the probability of any single point hypothesis
#' has a probability measure value of zero. Although it is possible
#' that the frequentist null hypothesis can be retained for small
#' n studies, the hypothesis itself is about the population in the
#' case of unlimited sample size, and surely for this limiting case
#' it is almost certain that the hypothesis is not exactly true.
#' So from the Bayesian framework, the point-null hypothesis is not
#' a good use of scientific effort and resources. From a Bayesian
#' framework it is more scientifically meaningful to assess a linear
#' comparison of the conditions, such as to assess if the population
#' success rate in condition 2 is greater than the success rate in
#' condition 1. An interval hypothesis such as this has a meaningful
#' probability value as does the complimentary hypothesis. If phi_1
#' and phi_2 are respectively the population success rates for the
#' binomials in conditions 1 and 2, then a meaningful comparison
#' might be to assess the probability distribution for
#' delta=phi_2-phi_1. This example is a simple linear contrast with
#' contrast coefficient weights of -1 and 1, which are tbe
#' multipliers for the two population success rates. If the
#' posterior interval estimate for the contrast contains 0, then the
#' hypothesis of a delta=0 has some credibility in light of the
#' current sample size. Thus by estimating the distribution of
#' delta the user learns important information about condition
#' differences. As another example of a contrast, suppose there are
#' three conditions where the first condition is a standard control,
#' and the other two conditions are different alternative conditions.
#' In this case a user might want to compare the control versus the
#' average of the two experimental conditions, i.e., the contrast of
#' delta = -1*phi_1 +.5*phi_2 + .5*phi_3. In this second example the
#' coefficients of the contrast are: -1,+.5,+.5. As a third example,
#' the user might also be interested in a comparison where the two
#' experimental conditions are compared, i.e., the contrast of
#' delta = 0*phi_1 + 1*phi_2 -1*phi_3. For the dfba_beta_contrast()
#' function, the user is required to stipulate the coefficients of
#' a contrast such that the sum of all the coefficients is 0, the
#' sum of the positive coefficients is 1, and the sum of the negative
#' coefficients is -1. This constraint on the coefficients forces
#' delta to be on the [-1,+1] interval.
#'
#'
#' There is a standard Bayesian posterior for each condition, which
#' is a beta distribution; see Chechile (2020) for a detailed
#' discussion of this literature. In short, it is well known that
#' the beta distribution is a natural Bayesian conjugate function
#' for Bernoulli random processes. Thus a prior beta distribution
#' with shape parameters a0 and b0 result (via Bayes's theorem) in a
#' posterior beta with shape parameters \emph{a} and \emph{b} where a=a0+n1
#' and b=b0+n2, where n1 and n2 are the respective successes and
#' failures of the categorical variable. While the Bayesian analysis
#' of each beta distribution for the separate conditions are known,
#' a comparison among 2 or more separate beta distributions is not
#' distributed as a beta. The posterior mean of a linear contrast
#' of separate beta variates has a known mean regardless of the
#' correlations among the variates, but the distributional form of
#' the contrast of independent betas is not known in closed form.
#' The distributional form is important for ascertaining issues
#' such as determining the probability that the contrast is positive
#' or specifying a probability interval for the contrast. But with
#' the \code{dfba_beta_contrast()} function, these important aspects of
#' the Bayesian analysis are approximated via Monte Carlo simulation
#' of the contrast.
#'
#' The input `samples' stipulates the number of random values that
#' will be drawn from each of the K posterior conditions. The default
#' value for `samples' is 10000, but the user can adjust this quantity
#' upperward. The Monte Carlo sampling is quite rapid, so the program
#' requires at least the default value of 10000, but the user might
#' desire to increase the value for `samples' if increased precision is
#' desired. Posterior interval estimation and the Bayes factor for
#' the contrast are provided on the basis of the Monte Carlo
#' sampling. If `samples' is equal to N and if phi_1, ..., phi_K
#' are the parameters for the population success rates, then there
#' are N random values drawn from each of phi_i parameters for
#' i=1,...,K. Given the contrast coeficients stipulated in the input,
#' there are N delta random posterior values where
#' delta[i]=contrast_vec[1]*phi_1[i]+...+contrast_vec[K]*phi_K[i].
#' The Monte Carlo sampling from each posterior beta with known
#' shape parameters uses the base R rbeta command. Thus unlike
#' Bayesian procedures that employ Markov chain Monte Carlo algorithm,
#' the Monte Carlo sampling in the dfba_beta_contrast () program
#' does not depend on a burn-in process or a starting estimate because
#' it is not a MCMC procedure. Thus all the N sampled values are valid
#' random samples. Of course repeated use of the dfba_beta_contrast ()
#' function for the same input will show some random variation in the
#' interval estimate and in the Bayes factor for a contrast greater
#' than 0. However, the point estimate for the contrast does not depend
#' on the Monte Carlo samplng, and it is constant given the vectors for
#' n1_vec and n2_vec and given the same prior.
#'
#' @references
#' Chechile, R. A. (2020). Bayesian Statistics for Experimental Scientists: A
#' General Introduction Using Distribution-Free Methods. Cambridge: MIT Press.
#'
#' Siegel, S. & Castellan, N. J. (1988). Nonparametric Statistics for the
#' Behavioral Sciences. New York: McGraw Hill.
#'
#' @examples
#' ## Suppose there are four conditions from a factorial design
#' # where the conditions labels are A1B1, A2B1, A1B2, and A2B2
#' # where the frequencies for success for the binomial variate are:
#' n1_vec<-c(22,15,13,21)
#' # and the frequencies for failures per condition are:
#' n2_vec<-c(18,25,27,19)
#' # Let us test the following three orthogonal contrasts
#' contrast.B1vsB2<-c(.5,.5,-.5,-.5)
#' contrast.A1vsA2<-c(.5,-.5,.5,-.5)
#' contrast.ABinter<-c(.5,-.5,-.5,.5)
#' testB1vsB2<-dfba_beta_contrast(n1_vec=n1_vec,n2_vec=n2_vec,contrast_vec=contrast.B1vsB2)
#' testA1vsA2<-dfba_beta_contrast(n1_vec,n2_vec,contrast_vec=contrast.A1vsA2)
#' testABinteraction<-dfba_beta_contrast(n1_vec,n2_vec,contrast_vec=contrast.ABinter)
#'
#' # Suppose the user wants to plot the cumulative distribution for
#' # AB interaction contrast from above then
#' xv=seq(0,1,.01)
#' plot(testABinteraction$delta_quantiles,xv,type="l",ylab="cum. prob.",xlab="contrast value")


#' @export
dfba_beta_contrast<-function(n1_vec,
                             n2_vec,
                             contrast_vec,
                             a0_vec=rep(1,length(n1_vec)),
                             b0_vec=rep(1,length(n1_vec)),
                             prob_interval=.95,
                             samples=10000){

  if (samples<10000){
    stop("For reliable results please use at least 10000 Monte Carlo samples")}

  if (missing(n1_vec)) stop ("n1_vec for the frequencies of successes in each condition is required.")
  if (missing(n2_vec)) stop ("n2_vec for the frequencies of failures in each condition is required.")
  if (missing(contrast_vec)) stop ("contrast_vec of contrast coefficients is required.")

  l1=length(n1_vec)
  l2=length(n2_vec)
  l3=length(contrast_vec)
  if ((l2!=l1)|(l3!=l1)){
    stop("The vectors n1_vec, n2_vec, and contrast_vec must have the same length.")}

  n1t_vec=round(n1_vec)
  n2t_vec=round(n2_vec)

  if(any(n1_vec!=round(n1_vec))|any(n1_vec<0)|any(is.na(n1_vec))){
    stop("n1_vec values must be non-negative integers")
  }

  if(any(n2_vec!=round(n2_vec))|any(n2_vec<0)|any(is.na(n2_vec))){
    stop("n2_vec values must be non-negative integers")
  }

#  for (i in 1:l1){
#    if (n1_vec[i]!=n1t_vec[i]){
#      stop("n1_vec values must be non-negative integers")}
#    if (n2_vec[i]!=n2t_vec[i]){
#      stop("n2_vec values must be non-negative integers")}
#  }
#
#  for (i in 1:l1){
#    if ((n1_vec[i]<0)|(n2_vec[i]<0)|(is.na(n1_vec[i]))|(is.na(n2_vec[i]))) {
#      stop("Both the n1_vec and n2_vec frequencies must be non-negative integer values.")}
#  }

  if(any(a0_vec <= 0)|any(is.na(a0_vec))|any(b0_vec <= 0)|any(is.na(b0_vec))){
    stop("Both the a0_vec and b0_vec shape parameters for the prior beta must be >0.")
  }

#  for (i in 1:l1){
#    if ((a0_vec[i]<=0)|(b0_vec[i]<=0)|(is.na(a0_vec[i]))|(is.na(b0_vec[i]))){
#      stop("Both the a0_vec and b0_vec shape parameters for the prior beta must be >0.")}
#    else {}}

  a_vec=n1_vec+a0_vec
  b_vec=n2_vec+b0_vec


  if (round(sum(contrast_vec))!=0){
    stop("The sum of the coefficients in the contrast_vec must be 0.")}

  if(sum(contrast_vec[contrast_vec > 0]) != 1){
    stop("The sum of the positive contast coefficients must be 1")
  }

  if(sum(contrast_vec[contrast_vec < 0]) != -1){
    stop("The sum of the negative contrast coefficients must be -1")
  }

  #totpos=0
  #
  #for (i in 1:l1){
  #  if (contrast_vec[i]>0){
  #    totpos=totpos+contrast_vec[i]} else {} }
  #if (totpos!=1){
  #  stop("The sum of the positive contrast coefficients must be 1.")}
  #
  #totneg=0
  #for (i in 1:l1){
  #  if (contrast_vec[i]<0){
  #    totneg=totneg+contrast_vec[i]} else {} }
  #if (totneg!=-1){
  #  stop("The sum of the negative contrast coefficients must be -1.")}

  if ((prob_interval<0)|(prob_interval>1)){
    stop("The probability for the interval estimate must be between 0 and 1.")}

 # cat("Contrast coefficients are:"," ","\n")
 # cat(contrast_vec,"\n")
 # cat(" ","  ","\n")

#  phimeans=(seq(1,l1,1))*0
#
#  for (i in 1:l1){
#    phimeans[i]=a_vec[i]/(a_vec[i]+b_vec[i])}

  phimeans <- a_vec/(a_vec + b_vec)

  mean_delta=sum(contrast_vec*phimeans)

#  cat("Exact posterior contrast mean is:"," ","\n")
#  cat(mean_delta," ","\n")
#  cat(" ","  ","\n")
#  cat("Following results are based on Monte Carlo sampling"," ","\n")
#  cat("The number of samples are:"," ","\n")
#  cat(samples," ","\n")
#  cat(" ","  ","\n")
#  cat("Probability in equal-tail interval: ",prob_interval,"\n")
#  cat(" "," ","\n")


#  delta=(seq(1,samples,1))*0
  delta <- rep(NA, length(samples))

  for (i in 1:samples){
    delta[i]=0
    for (j in 1:l1){
      delta[i]=delta[i]+contrast_vec[j]*rbeta(1,a_vec[j],b_vec[j]) }
  }

  xv = seq(0, 1, .01)
  deltaquan=seq(0, 1, .01)

  for (i in 1:101){
    deltaquan[i]=quantile(delta,
                          prob = deltaquan[i])
    }

  # plot(deltaquan,xv,type="l",xlab="contrast value",ylab="posterior cumulative probability",main="Based on Monte Carlo Sampling")

#  cat("The equal-tail limits for the contrast are:"," ","\n")
  lowlimit = quantile(delta,
                      prob=(1-prob_interval)/2)
  uplimit = quantile(delta,
                     prob=1-((1-prob_interval)/2))

#  cat(lowlimit," ",uplimit,"\n")
#  cat(" "," ","\n")

#  cat("Posterior probability that the contrast is positive is:"," ","\n")

    prH1 = (sum(delta>0))/samples

#      cat(prH1," ","\n")

#  delta0=(seq(1,samples,1))*0

  delta0 <- rep(NA, samples)

  for (i in 1:samples){
    delta0[i] = 0
    for (j in 1:l1){
      delta0[i] = delta0[i]+contrast_vec[j]*rbeta(1,a0_vec[j],b0_vec[j])
      }
  }

#  cat("Prior probability for a positive contrast is:"," ","\n")

  priorprH1=(sum(delta0>0))/samples

#    cat(priorprH1," ","\n")

  if (prH1==1|priorprH1==0){
    BF10 = samples
#    cat("Bayes factor BF10 for a positive contrast is estimated to be large than:"," ","\n")
#    cat(BF10," ","\n")
    } else {
      BF10=(prH1*(1-priorprH1))/(priorprH1*(1-prH1))
#      cat("Bayes factor BF10 for a positive contrast is:"," ","\n")
#      cat(BF10," ","\n")
      }
  dfba_beta_contrast_list<-list(mean = mean_delta,
                                lower_limit = lowlimit,
                                upper_limit = uplimit,
                                prob_positive_delta = prH1,
                                prior_positive_delta = priorprH1,
                                bayes_factor = BF10,
                                delta_quantiles = deltaquan,
                                a_vec = a_vec,
                                b_vec = b_vec,
                                contrast_vec = contrast_vec,
                                prob_interval = prob_interval,
                                samples = samples,
                                a0_vec = a0_vec,
                                b0_vec = b0_vec)
  new("dfba_beta_contrast_out", dfba_beta_contrast_list)
}

