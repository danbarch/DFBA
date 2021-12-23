# Bayesian Contrasts
#
#
#
#
#
#
#
#

#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


wts<-c(1/3,1/3,1/3,-.5,-.5)
n1v<-c(8,7,10,3,6)
n2v<-c(4,5,2,9,6)
priora0<-c(1,1,1,1,1)
priorb0<-c(1,1,1,1,1)

#The above are an example of inputs for the function


Beta_betweengroups<-function(wts, n1v, n2v, priora0, priorb0, N=10000){
  if ((N!=round(N))|(N<0)){
    stop("N must be a positive integer")
  } else {N=N}
#  testfailed=0
  l=length(wts)
  ln1=length(n1v)
  ln2=length(n2v)
  la0=length(priora0)
  lb0=length(priorb0)
  if ((ln1!=l)|(ln2!=l)|(la0!=l)|(lb0!=l)){
#    testfailed=testfailed+1
    stop("Error - the five vectors do not have the same length; must redo")
  }
  #else {testfailed=testfailed}
  totalwt=sum(wts)
  if (abs(totalwt)<=.001){
    totalwt=0
  } else {
  #totalwt=totalwt} else {totalwt=0}
#  if (totalwt!=0) {testfailed=testfailed+1
  stop("Error: the sum of the weight coeficients is not zero; must redo")
#  else {testfailed=testfailed}
    }
  n1count=0
  for (I in 1:ln1){
    if (n1v[I]<0){n1count=n1count+1} else {n1count=n1count}}
  if (n1count>0){
    #testfailed=testfailed+1
  stop("Error. No component in n1v can be negative")
  } else {
      #testfailed=testfailed
    }
  n2count=0
  for (I in 1:ln2){
    if (n2v[I]<0){
      n2count=n2count+1
    } else {n2count=n2count}
    }
  if (n2count>0){
    #testfailed=testfailed+1
    stop("Error. No component in n2v can be negative")
  } else {
      #testfailed=testfailed
    }
  a0count=0
  for (I in 1:la0){
    if (priora0[I]<=0){
      a0count=a0count+1
    } else {a0count=a0count}
    }
  if (a0count>0){
    #testfailed=testfailed+1
    stop("Error. All components in priora0 must be positive values")
  } else {
      #testfailed=testfailed
    }
  b0count=0
  for (I in 1:lb0){
    if (priorb0[I]<=0){
      b0count=b0count+1
    } else {b0count=b0count}
    }
  if (b0count>0){
    #testfailed=testfailed+1
    stop("Error. All components in priorb0 must be positve values")
  } else {
      #testfailed=testfailed
    }
#  print("The contrast weights are:")
#  print(wts)
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
#    print("contrast mean is:")
#    print(contrastmean)
#    print("contrast variance is:")
#    print(contrastvar)
    delta=(seq(1,N,1))*0
    for (I in 1:N){
      delta[I]=0
      for (J in 1:l){
        delta[I]=delta[I]+wts[J]*rbeta(1,a[J],b[J])
      }
      }
    xtest=mean(delta)
    ytest=var(delta)
#    print("mean of the contrast from sampling:")
#    print(xtest)
#    print("variance of the contrast from sampling:")
#    print(ytest)
    qhigh=quantile(delta,prob=.975)
    qlow=quantile(delta,prob=.025)
    intervallimit<-c(qlow,qhigh)
#    print("approximate equal-tail 95% interval for the contrast is:")
#    print(intervallimit)
    postpostive=(sum(delta>0))/N
#    print("posterior probability for a positive contrast from sampling is:")
#    print(postpostive)
    deltaprior=(seq(1,N,1))*0
    for (I in 1:N){
      deltaprior[I]=0
      for (J in 1:l){
        deltaprior[I]=deltaprior[I]+wts[J]*rbeta(1,priora0[J],priorb0[J])}}
    priorpos=(sum(deltaprior>0))/N
#    print("prior probability for a positive contrast from sampling is:")
#    print(priorpos)
    if (priorpos==0){
      priorpos=.5/N
      } else {priorpos=priorpos}
    if (postpostive==1){
      postpostive=N/(N+1)
      } else {postpostive=postpostive}
    BF10=(postpostive*(1-priorpos))/(priorpos*(1-postpostive))
#    print("Bayes factor for a postive contrast from simuation is:")
#    print(BF10)
#    print("Bayes factor for a negative contrast from sampling is:")
#    print(1/BF10)
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

