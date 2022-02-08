dfba_mann_whitney<-function(E,C,prior_vec=c(1,1),prob_interval=.95,samples=30000,method=""){
  if (length(prior_vec)!=2){
    stop("an explicit stipulation of prior_vec must only have the two shape parameters for the prior beta distribution")} else {}

  a0<-prior_vec[1]
  b0<-prior_vec[2]

  if ((a0<=0)|(b0<=0)){
    stop("Both of the beta shape parameters for in the prior_vec must be >0.")} else {}

  if ((prob_interval<0)|(prob_interval>1)){
    stop("The probability for the interval estimate of phi_w must be a proper proportion.")}
  else {}

  if (samples<10000){
    stop("stipulating Monte Carlo samples < 10000 is too small")} else {}

  Etemp=E
  Ctemp=C
  jc=0
  for (j in 1:length(E)){
    if (is.na(Etemp[j])){} else {
      jc=jc+1
      E[jc]=Etemp[j]}
  }
  E=E[1:jc]

  kc=0
  for (k in 1:length(C)){
    if (is.na(Ctemp[k])){} else {
      kc=kc+1
      C[kc]=Ctemp[k]}
  }
  C=C[1:kc]

  nE=length(E)
  nC=length(C)
  if ((nE==0)|(nC==0)){
    stop("The E and C vectors must have a length greater than 0.")} else {}
  Emean=mean(E)
  Cmean=mean(C)
  cat("E mean","    ","C mean","\n")
  cat(Emean," ",Cmean,"\n")
  cat("n_E","  ","n_C","\n")
  cat(nE,"   ",nC,"\n")
  cat(" ","   ","\n")

  #following code finds U_E and U_C
  UE_vector<-rep(NA, length(E)) # UE counter
  UC_vector<-rep(NA, length(C)) # UC counter
  for (i in 1:length(E)){
    UE_vector[i]<-sum(E[i]>C)
  }
  for (j in 1:length(C)){
    UC_vector[j]<-sum(C[j]>E)
  }
  UE=sum(UE_vector)
  UC=sum(UC_vector)
  cat("U_E and U_C Mann-Whitney statistics are:"," ","\n")
  cat("U_E","   ","U_C","\n")
  cat(UE,"   ",UC,"\n")
  cat(" ","   ","\n")
  cat(" ","  ","\n")

  nH=(2*nE*nC)/(nE+nC)

  if (method==""){
    if (nH>=20){method="large"} else {
      method="small"}
  } else {}

  if (method=="small"){
    m1lable<-"Following is based on Monte Carlo samples"
    m2lable<-"with discrete prob. values."
    cat(m1lable,m2lable,"\n")
    cat("The number of Monte Carlo samples is:"," ","\n")
    cat(samples," ","\n")
    cat(" ","   ","\n")

    #Code for the discrete prior
    phiv=seq(1/400,.9975,.005)
    x=phiv+.0025
    priorvector=rep(0,200)
    priorvector[1]=pbeta(x[1],a0,b0)
    for (i in 2:200){
      priorvector[i]=pbeta(x[i],a0,b0)-pbeta(x[i-1],a0,b0)}

    XE=seq(1, length(E), 1)
    XC=seq(1, length(C), 1)
    fomega<-rep(0.0,200)


    for (j in 1:200){
      omega=(1/400)+(j-1)*(1/200)
      komega=(1-omega)/omega

      for (k in 1:samples){
        Uz<-rep(NA, length(E))
        XE<-rexp(length(E), rate=komega)
        XC<-rexp(length(C), rate = 1)

        for (i in 1:length(E)){
          Uz[i]<-sum(XE[i]>XC)
        }

        if(sum(Uz) == UE) {fomega[j] = fomega[j]+1} else {}
      }
    }

    tot=sum(priorvector*fomega)
    omegapost=(priorvector*fomega)/tot
    phiv=rep(0.0,200)
    for (j in 1:200){
      phiv[j]=(1/400)+(j-1)*(1/200)}
    postdis<-data.frame(phiv,omegapost)

    #Folowing finds the mean of the posterior omega distribution
    #and provides a plot of the distribution.
    omegabar=sum(phiv*omegapost)

    plot(phiv,omegapost,type="l",xlab="omega_E",ylab="posterior discrete probabilities",main="posterior-solid; prior-dashed")
    lines(phiv,priorvector,type="l",lty=2)

    cat("mean for omega_E is:"," ","\n")
    cat(omegabar," ","\n")
    cat(" ","  ","\n")

    postdis<-data.frame(phiv,omegapost)

    cum_omega=cumsum(omegapost)

    I=1
    while (cum_omega[I]<(1-prob_interval)/2){
      I=I+1}
    qLbelow=phiv[I]-.0025

    if (I!=1){
      extrap=(1-prob_interval)/2-cum_omega[I-1]
      probI=cum_omega[I]-cum_omega[I-1]} else {
        extrap=(1-prob_interval)/2
        probI=cum_omega[1]}
    qLv=qLbelow+(.005)*(extrap/probI)

    I=1
    while (cum_omega[I]<1-(1-prob_interval)/2){
      I=I+1}
    qHbelow=phiv[I]-.0025
    extrapup=1-((1-prob_interval)/2)-cum_omega[I-1]
    probIu=cum_omega[I]-cum_omega[I-1]
    qHv=qHbelow+(.005)*(extrapup/probIu)
    cat(" ","  ","\n")
    cat("equal-tail area interval"," ","\n")
    probpercent=100*prob_interval
    mi=as.character(probpercent)
    cat(mi,"percent interval limits are:","\n")
    cat(qLv," ",qHv,"\n")
    cat(" ","  ","\n")

    omega_E=phiv
    cumdis<-data.frame(omega_E,cum_omega)
    #The prH1 is the probability that omega_E is greater than .5.

    prH1=1-cum_omega[round(100)]
    cum_prior=cumsum(priorvector)
    priorprH1=1-cum_prior[round(100)]
    cat("probability that omega_E exceeds .5 is:"," ","\n")
    cat("prior","  ","posterior","\n")
    cat(priorprH1,"  ",prH1,"\n")
    cat(" ","  ","\n")

    #Following finds the Bayes factor for omega_E being greater than .5.

    if ((prH1==1)|(priorprH1==0)){
      BF10=samples
      cat("Bayes factor BF10 for omega_E >.5 is estimated to be greater than:"," ","\n")
      cat(BF10," ","\n")} else {
        BF10=(prH1*(1-priorprH1))/(priorprH1*(1-prH1))
        cat("Bayes factor BF10 for omega_E>.5 is:"," ","\n")
        cat(BF10," ","\n")}
    #list(posterior_discrete_values=phipost,posterior_cum_distribution=cumdis)
    return(cat(" ","  ","\n"))} else {}

  if (method=="large"){
    m1L<-"Following is a beta approximation model for omega_E"
    m2L<-"when 2*nE*nC/(nE+nC)>19"
    cat(m1L,m2L,"\n")
    cat(" ","   ","\n")

    #Following is a Lagrange interpolation method to find posterior a and b
    #shape parameters for the beta approximation to the omega_E distribution
    xs=UE/(UE+UC)
    if (xs>=.5) {x=xs} else {x=1-xs}
    nH=(2*nE*nC)/(nE+nC)
    y5=(nH^1.1489)/(.4972+(nH^1.1489))
    w4=.8-(1/(1+(1.833*nH)))
    w3=.6-(1/(1+(2.111*nH)))
    w2=.4-(1/(1+(2.520*nH)))
    w1=.2-(1/(1+(4.813*nH)))
    y4=(y5*w4)+(1-w4)*.5
    y3=(y5*w3)+(1-w3)*.5
    y2=(y5*w2)+(1-w2)*.5
    y1=(y5*w1)+(1-w1)*.5
    Y=c(.5,y1,y2,y3,y4,y5)
    La0=252-(1627*x)+((12500*x^2)-(15875*x^3)+(10000*x^4)-(2500*x^5))/3
    La1=-1050+((42775*x)/6)-(38075*.5*x^2)+((75125*x^3)-(48750*x^4)+(12500*x^5))/3
    La2=1800-(12650*x)+((104800*x^2)-(142250*x^3)+(95000*x^4)-(25000*x^5))/3
    La3=-1575+(11350*x)+((-96575*x^2)+(134750*x^3)-(92500*x^4)+(25000*x^5))/3
    La4=700+(14900*x^2)+(15000*x^4)-((15425*x)+(63875*x^3)+(12500*x^5))/3
    La5=-126+(1879*.5*x)+((-16625*.5*x^2)+(12125*x^3)-(8750*x^4)+(2500*x^5))/3
    LA=c(La0,La1,La2,La3,La4,La5)
    ombar=sum(Y*LA)
    absum=nH*(1.028+(.75*x))+2
    a=ombar*absum
    b=(1-ombar)*absum
    omegabar=ombar
    if (xs<.5){
      a=(1-ombar)*absum
      b=ombar*absum
      omegabar=1-ombar} else {}
    na=a-1
    nb=b-1
    apost=a0+na
    bpost=b0+nb
    cat("The posterior beta shape parameters are:"," ","\n")
    cat("posterior a"," ","posterior b","\n")
    cat(apost,"   ",bpost,"\n")
    cat(" ","  ","\n")

    a=apost
    b=bpost
    x=seq(0,1,.005)
    y=dbeta(x,a,b)
    y0=dbeta(x,a0,b0)
    plot(x,y,type="l",xlab="omega_E",ylab="probability density",main="posterior- solid; prior-dashed")
    lines(x,y0,type="l",lty=2)

    postmean=a/(a+b)
    postmedian=qbeta(.5,a,b)
    mc1="posterior mean"
    mc2="posterior median"
    cat(mc1,"  ",mc2,"\n")
    cat(postmean,"    ",postmedian,"\n")
    cat(" ","  ","\n")

    probpercent=100*prob_interval
    mi=as.character(probpercent)
    cat("probability within interval is:"," ","\n")
    cat(mi,"percent","\n")
    qlequal=qbeta((1-prob_interval)*.5,a,b)
    qhequal=qbeta(1-(1-prob_interval)*.5,a,b)

    met1="equal-tail limit values are:"
    met2=" "
    cat(met1,met2,"\n")
    cat(qlequal," ",qhequal,"\n")
    cat(" ","  ","\n")

    alphaL=seq(0,(1-prob_interval),(1-prob_interval)/1000)
    qL=qbeta(alphaL,a,b)
    qH=qbeta(prob_interval+alphaL,a,b)
    diff=qH-qL
    I=1
    mindiff=min(diff)
    while (diff[I]>mindiff){
      I=I+1}
    qLmin=qL[I]
    qHmax=qH[I]
    probpercent=100*prob_interval
    mi=as.character(probpercent)
    cat(mi,"percent highest-density limits are:","\n")
    cat(qLmin," ",qHmax,"\n")
    cat(" ","  ","\n")

    prH1=1-pbeta(.5,a,b)
    priorprH1=1-pbeta(.5,a0,b0)
    mH11="probability that omega_E > .5"
    mH12=" "
    cat(mH11,mH12,"\n")
    mH1prior="prior"
    mH1post="posterior"
    cat(mH1prior,"  ",mH1post,"\n")
    cat(priorprH1,"  ",prH1,"\n")
    cat(" ","  ","\n")
    if ((prH1==1)|(priorprH1==0)){
      minf1="Bayes factor BF10 for omega_E >.5 is approaching"
      minf2="infinity"
      cat(minf1,minf2,"\n")} else {
        BF10=(prH1*(1-priorprH1))/(priorprH1*(1-prH1))
        mBF1="Bayes factor BF10 for omega_E > .5"
        mBF2="is:"
        cat(mBF1,mBF2,"\n")
        cat(BF10," ","\n")}


    cat(" ","   ","\n")
    m1X=" "
    m2X=" "
    return(cat(m1X,m2X,"\n"))} else {}

  if ((method!="large")&(method!="small")) {
    stop("An explicit method stipulation must be either the word large or small.")} else {}

}
