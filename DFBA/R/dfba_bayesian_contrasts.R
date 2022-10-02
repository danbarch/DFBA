#' Bayesian Contrasts
#'
#' Compute contrasts for binomial data from multiple conditions
#'
#'
#' @importFrom stats rbeta
#' @importFrom stats var
#' @importFrom stats quantile
#'
#' @param wts A set of weights for contrast. Contrast weights must sum to 1.
#' @param n1v Number of successes in each condition
#' @param n2v Number of failures in each condition
#' @param priora0 (Optional) Set of prior values for the beta shape parameter alpha for each condition (default is a = 1 for each condition)
#' @param priorb0 (Optional) Set of prior values for the beta shape parameter beta for each condition (default is a = 1 for each condition)
#' @param prob_interval Desired probability for equal-tail interval estimate on the contrast (default is 0.95)
#' @param samples The desired number of Monte Carlo samples (default is 10000)



#' @return A list containing the following components:
#' @return \item{contrast_Weights}{Contrast weights from user input}
#' @return \item{contrast_mean}{Mean of the contrast}
#' @return \item{contrast_variance}{Variance of the contrast}
#' @return \item{samples}{The number of Monte Carlo samples (default is 10000)}
#' @return \item{sampling_contrast_mean}{Mean of the contrast samples}
#' @return \item{sampling_contrast_variance}{Variance of the contrast samples}
#' @return \item{equal_tail_interval}{Limits of the posterior equal-tail interval estimate on the contrast with probability defined by \code{prob_interval}}
#' @return \item{posterior_of_positive}{Posterior probability that the contrast is positive}
#' @return \item{prior_of_positive}{Prior probability that the contrast is positive}
#' @return \item{BF_positive_contrast}{Bayes Factor estimate in favor of the contrast being positive}
#' @return \item{BF_negative_contrast}{Bayes Factor estimate in favor of the contrast being negative}
#'
#' @details
#' These are the details.
#'
#' @examples
#' wts <- c(1/3,1/3,1/3,-.5,-.5)
#' n1v <- c(8,7,10,3,6) # Successes per condition
#' n2v <- c(4,5,2,9,6)  # Failures per condition
#' priora0 <- c(1,1,1,1,1)
#' priorb0 <- c(1,1,1,1,1)
#' dfba_bayesian_contrasts(wts, n1v, n2v, priora0, priorb0)

#' @export
dfba_bayesian_contrasts <- function(wts,
                                    n1v,
                                    n2v,
                                    priora0,
                                    priorb0,
                                    prob_interval = 0.95,
                                    samples = 10000){
  if ((samples != round(samples))|(samples < 0)){
    stop("'samples' must be a positive integer")
  }
  #else {N=N}

  l=length(wts)
  ln1=length(n1v)
  ln2=length(n2v)
  la0=length(priora0)
  lb0=length(priorb0)

  if ((ln1!=l)|(ln2!=l)|(la0!=l)|(lb0!=l)){
    stop("All five vectors must have same length")
  }

  totalwt=sum(wts)

  if (abs(totalwt) <= .001){
    totalwt = 0
  } else {
  stop("The sum of the weight coefficients must be zero")
  }

  n1count=0

  if(any(n1v < 0)){
    stop("All n1v values must be non-negative")
  }

  if(any(n2v < 0)){
    stop("All n2v values must be non-negative")
  }

#  for (I in 1:ln1){
#    if (n1v[I] < 0){
#      n1count = n1count + 1
#    } else {
#        n1count = n1count
#    }
#  }
#
#  if (n1count>0){
#
#  stop("No component in n1v can be negative")
#  }
#
#  n2count=0
#  for (I in 1:ln2){
#    if (n2v[I]<0){
#      n2count=n2count+1
#    } else {n2count=n2count}
#    }
#  if (n2count>0){
#    stop("No component in n2v can be negative")
#  }

  if(any(priora0 < 0)){
    stop("All npriora0 values must be non-negative")
  }

  if(any(priorb0 < 0)){
    stop("All priorb0 values must be non-negative")
  }

#  a0count=0
#  for (I in 1:la0){
#    if (priora0[I]<=0){
#      a0count=a0count+1
#    } else {a0count=a0count}
#    }
#  if (a0count>0){
#    stop("Error. All components in priora0 must be positive values")
#  }
#  b0count=0
#  for (I in 1:lb0){
#    if (priorb0[I]<=0){
#      b0count=b0count+1
#    } else {b0count=b0count}
#    }
#  if (b0count>0){
#    stop("Error. All components in priorb0 must be positve values")
#  }

  a=n1v+priora0
  b=n2v+priorb0

  #phimeans=(seq(1,l,1))*0
  #phivar=(seq(1,l,1))*0

  phimeans <- a/(a + b)
  phivar <- (phimeans*(1-phimeans))/(a + b+ 1)


#  for (I in 1:l){
#    phimeans[I] = a[I]/(a[I]+b[I])
#    phivar[I] = (phimeans[I]*(1-phimeans[I]))/(a[I]+b[I]+1)
#    }

#  contrastmean=0
#  contrastvar=0

  contrastmean <- sum(wts*phimeans)
  contrastvar <- sum((wts^2)*phivar)

#  for (I in 1:l){
#      contrastmean=contrastmean+(wts[I]*phimeans[I])
#      contrastvar=contrastvar+(wts[I]*wts[I]*phivar[I])
#  }

  #delta=(seq(1,N,1))*0

  delta <- rep(NA, samples)

    for (I in 1:length(delta)){
      delta[I]=0
      for (J in 1:l){
        delta[I]=delta[I]+wts[J]*rbeta(1,a[J],b[J])
      }
    }

    xtest=mean(delta)
    ytest=var(delta)

    qhigh=quantile(delta,
                   prob=1-(1-prob_interval)/2)
    qlow=quantile(delta,
                  prob=(1-prob_interval)/2)

    intervallimit<-c(qlow,
                     qhigh)
    postpositive<-(sum(delta > 0))/samples

#    deltaprior<-(seq(1,N,1))*0
    deltaprior <- rep(NA, samples)

    for (I in 1:samples){
      deltaprior[I] = 0

      for (J in 1:l){
        deltaprior[I] = deltaprior[I]+wts[J]*rbeta(1,priora0[J],priorb0[J])
      }
    }

    priorpos=(sum(deltaprior>0))/samples

    if (priorpos == 0){
      priorpos=.5/samples
    }
    #else {priorpos=priorpos}
    if (postpositive==1){
      postpositive=samples/(samples+1)
    }
#    else {postpositive=postpositive}

        BF10=(postpositive*(1-priorpos))/(priorpos*(1-postpositive))

        dfba_contrasts_list<-list(samples = samples,
                                  contrast_Weights = wts,
                                  contrast_mean = contrastmean,
                                  contrast_variance = contrastvar,
                                  sampling_contrast_mean = xtest,
                                  sampling_contrast_variance = ytest,
                                  prob_interval = prob_interval,
                                  equal_tail = intervallimit,
                                  posterior_of_positive = postpositive,
                                  prior_of_positive = priorpos,
                                  BF_positive_contrast = BF10,
                                  BF_negative_contrast = 1/BF10
       )

        new("dfba_contrasts_out", dfba_contrasts_list)
}

