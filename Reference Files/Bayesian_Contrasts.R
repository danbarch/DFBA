#' Bayesian Contrasts
#
#' Compute contrasts for binomial data from multiple conditions
#' @param wts A set of weights for contrast. Contrast weights must sum to 1.
#' @param n1v Number of successes in each condition
#' @param n1v Number of failures in each condition
#' @param priora0 (Optional) Set of prior values for the beta shape parameter alpha for each condition (default is a = 1 for each condition)
#' @param priorb0 (Optional) Set of prior values for the beta shape parameter beta for each condition (default is a = 1 for each condition)

#' @return contrast_Weights Contrast weights from user input
#' @return contrast_mean Mean of the contrast
#' @return contrast_variance Variance of the contrast
#' @return sampling_contrast_mean Mean of the contrast samples
#' @return sampling_contrast_variance Variance of the contrast samples
#' @return equal_tail_95 Limits of the posterior 95\% equal-tail interval estimate on the contrast
#' @return posterior_of_positive Posterior probability that the contrast is positive
#' @return prior_of_positive Prior probability that the contrast is positive
#' @return BF_positive_contrast Bayes Factor estimate in favor of the contrast being positive
#' @return BF_negative_contrast Bayes Factor estimate in favor of the contrast being negative
#'
#' @examples
#' wts<-c(1/3,1/3,1/3,-.5,-.5)
#' n1v<-c(8,7,10,3,6) # Successes per condition
#' n2v<-c(4,5,2,9,6)  # Failures per condition
#' priora0<-c(1,1,1,1,1)
#' priorb0<-c(1,1,1,1,1)

#' Beta_betweengroups(wts, n1v, n2v, priora0, priorb0)
#' $contrast_Weights
#' [1]  0.3333333  0.3333333  0.3333333 -0.5000000 -0.5000000

#' $contrast_mean
#' [1] 0.2738095

#' $contrast_variance
#' [1] 0.01232993

#' $sampling_contrast_mean
#' [1] 0.273381

#' $sampling_contrast_variance
#' [1] 0.01218238

#' $equal_tail_95
#' 2.5%      97.5%
#' 0.05174536 0.48247585

#' $posterior_of_positive
#' [1] 0.9917

#' $prior_of_positive
#' [1] 0.4964

#' $BF_positive_contrast
#' [1] 121.2149

#' $BF_negative_contrast
#' [1] 0.008249808


Beta_betweengroups<-function(wts, n1v, n2v, priora0, priorb0, N=10000){
  if ((N!=round(N))|(N<0)){
    stop("N must be a positive integer")
  } else {N=N}
  l=length(wts)
  ln1=length(n1v)
  ln2=length(n2v)
  la0=length(priora0)
  lb0=length(priorb0)
  if ((ln1!=l)|(ln2!=l)|(la0!=l)|(lb0!=l)){
    stop("Error - the five vectors do not have the same length; must redo")
  }
  totalwt=sum(wts)
  if (abs(totalwt)<=.001){
    totalwt=0
  } else {
  stop("Error: the sum of the weight coeficients is not zero; must redo")
    }
  n1count=0
  for (I in 1:ln1){
    if (n1v[I]<0){n1count=n1count+1} else {n1count=n1count}}
  if (n1count>0){
  stop("Error. No component in n1v can be negative")
  }
  n2count=0
  for (I in 1:ln2){
    if (n2v[I]<0){
      n2count=n2count+1
    } else {n2count=n2count}
    }
  if (n2count>0){
    stop("Error. No component in n2v can be negative")
  }
  a0count=0
  for (I in 1:la0){
    if (priora0[I]<=0){
      a0count=a0count+1
    } else {a0count=a0count}
    }
  if (a0count>0){
    stop("Error. All components in priora0 must be positive values")
  }
  b0count=0
  for (I in 1:lb0){
    if (priorb0[I]<=0){
      b0count=b0count+1
    } else {b0count=b0count}
    }
  if (b0count>0){
    stop("Error. All components in priorb0 must be positve values")
  }
  a=n1v+priora0
  b=n2v+priorb0
    phimeans=(seq(1,l,1))*0
    phivar=(seq(1,l,1))*0
    for (I in 1:l){
      phimeans[I]=a[I]/(a[I]+b[I])
      phivar[I]=(phimeans[I]*(1-phimeans[I]))/(a[I]+b[I]+1)
      }
    contrastmean=0
    contrastvar=0
    for (I in 1:l){
      contrastmean=contrastmean+(wts[I]*phimeans[I])
      contrastvar=contrastvar+(wts[I]*wts[I]*phivar[I])
      }
    delta=(seq(1,N,1))*0
    for (I in 1:N){
      delta[I]=0
      for (J in 1:l){
        delta[I]=delta[I]+wts[J]*rbeta(1,a[J],b[J])
      }
      }
    xtest=mean(delta)
    ytest=var(delta)
    qhigh=quantile(delta,prob=.975)
    qlow=quantile(delta,prob=.025)
    intervallimit<-c(qlow,qhigh)
    postpositive<-(sum(delta>0))/N
    deltaprior<-(seq(1,N,1))*0
    for (I in 1:N){
      deltaprior[I]=0
      for (J in 1:l){
        deltaprior[I]=deltaprior[I]+wts[J]*rbeta(1,priora0[J],priorb0[J])}}
    priorpos=(sum(deltaprior>0))/N
    if (priorpos==0){
      priorpos=.5/N
      } else {priorpos=priorpos}
    if (postpositive==1){
      postpositive=N/(N+1)
      } else {postpositive=postpositive}
    BF10=(postpositive*(1-priorpos))/(priorpos*(1-postpositive))

  list(contrast_Weights=wts,
       contrast_mean=contrastmean,
       contrast_variance=contrastvar,
       sampling_contrast_mean=xtest,
       sampling_contrast_variance=ytest,
       equal_tail_95=intervallimit,
       posterior_of_positive=postpositive,
       prior_of_positive=priorpos,
       BF_positive_contrast=BF10,
       BF_negative_contrast=1/BF10
       )
}

