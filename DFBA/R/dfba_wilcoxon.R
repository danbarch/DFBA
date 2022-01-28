
dfba_wilcoxon<-
  function(Y1,Y2,method,priorvalues=rep(1/200,200)){
    Y1mean=mean(Y1,na.rm=TRUE)
    Y2mean=mean(Y2,na.rm=TRUE)
    cat("Y1 mean","   ","Y2 mean","\n")
    cat(Y1mean,"  ",Y2mean,"\n")
    l1=length(Y1)
    l2=length(Y2)
    if (l1!=l2) {
      stop("Y1 and Y2 must have the same length. This function is within-block data with no missing values.")} else {}

    dt=Y1-Y2
    d=Y1
    jc=0
    for (j in 1:l1){
      if (is.na(dt[j])){} else {
        jc=jc+1
        d[jc]=dt[j]}
    }
    d=d[1:jc]
    l1=jc

    if (l1<3){
      stop("There are not enough values in the Y1 and Y2 vector for meaningful results.")} else {}

    #The following code computes the within-block difference scores, and finds the
    #number of blocks where the difference scores are nonzero (within a trivial rounding error).
    sdd=sd(d)
    IC=0
    for (I in 1:l1){
      if (abs(d[I])<=sdd/30000){IC=IC} else {IC=IC+1}}
    n=IC
    #The following code deals with the case where all differnces are trivially close to 0.
    if (n==0){stop("Y1 and Y2 differences are all trivial")} else {}

    #The following finds the Tplus and Tminus stats and the number of nonzero blocks
    dt=(seq(1,n,1))*0
    IC=0
    for (I in 1:l1){
      if (abs(d[I])<=sdd/30000){IC=IC} else {
        IC=IC+1
        dt[IC]=d[I]}}
    dta=(seq(1,n,1))*0
    for (I in 1:n){
      dta[I]=abs(dt[I])}
    #The vector dtar is for the ranks of the absolute-value d scores; whereas
    #the dta vector is for the absolute-value of the d scores, and dtars is
    #the vector of signed rank scores.
    dtar=rank(dta)
    dtars=dtar*dt/dta
    #The following computes the Tplus and Tminus statistics
    tplus=0
    for (I in 1:n){
      if (dtars[I]>0){tplus=tplus+dtar[I]} else {tplus=tplus} }
    tneg=(n*(n+1)*.5)-tplus
    cat("n","  ","T_plus","  ","T_negative","\n")
    #desstat<-c(n,tplus,tneg)
    #print(desstat)
    cat(n,"  ",tplus,"      ",tneg,"\n")

    if (method=="small"){
      m1lable<-"Following is based on 30000 Monte Carlo samples"
      m2lable<-" with discrete prob. values."
      cat(m1lable,m2lable,"\n")
      if (length(priorvalues)!=200) {
        stop("priorvalues must have a length of 200 or use the default by omitting priorvalues")}
      else {}
      totprior=sum(priorvalues)
      if ((totprior>1.01)|(totprior<.99)){
        priorvalues=rep(1/200,200)
        message("User's prior did not sum to 1, so a flat prior was used instead.")} else {}


      samples=30000
      priorvector=priorvalues
      fphi<-rep(0.0,200)
      for (j in 1:200){
        phi=1/(400)+(j-1)*(1/200)
        for (k in 1:samples){
          tz=sum((1:n)*rbinom(n, 1, phi))

          if(tz==tplus) {
            fphi[j]=fphi[j]+1.0
          } else{
            fphi[j]=fphi[j]
          }
        }
      }
      #The tot value below is the denominator of the discrete analysis,
      #phipost is the vector for the posterior distribution.
      tot=sum(priorvector*fphi)
      phipost=(priorvector*fphi)/tot
      phiv=rep(0.0,200)
      phibar=0.0
      for (j in 1:200){
        phiv[j]=(1/400)+(j-1)*(1/200)
        phibar=phibar+(phiv[j]*phipost[j])}
      print("mean for phi_w is:")
      print(phibar)
      postdis<-data.frame(phiv,phipost)

      plot(phiv,phipost,type="l",xlab="phi_w",ylab="posterior discrete probabilities",main="Based on Monte Carlo Samples")
      #The following finds the poserior cumulative distribution and outputs these values.
      cum_phi=cumsum(phipost)
      phi_w=phiv
      cumdis<-data.frame(phi_w,cum_phi)
      #The prH1 is the probability that phi_w is greater than .5.
      prH1=1-cum_phi[round(100)]
      print("probability that phi_w exceeds .5 is:")
      print(prH1)
      #Following finds the Bayes factor for phi_w being greater than .5.
      cum_prior=cumsum(priorvector)
      priorprH1=1-cum_prior[round(100)]
      if ((prH1==1)|(priorprH1==0)){
        BF10=2*samples
        print("Bayes factor BF10 for phi_w >.5 is estimated to be greater than:")
        print(BF10)} else {
          BF10=(prH1*(1-priorprH1))/(priorprH1*(1-prH1))
          print("Bayes factor BF10 for phi_w>.5 is:")
          print(BF10)}
      #Following provides output of key statistics.
      #list(posterior_cum_distribution=cumdis)
      #m1X=" "
      #m2X=" "
      return(list(posterior_discrete_values=phipost,posterior_cum_distribution=cumdis))}

    if (method=="large"){
      m1L<-"Following is based on beta approximation for phi_w"
      m2L<-"which is highly accurate for n>24"
      cat(m1L,m2L,"\n")
      cat(" ","   ","\n")
      if (length(priorvalues)!=2) {
        stop("priorvalues must have two value which are a0 and b0")}
      else {}
      if ((priorvalues[1]<=0)|(priorvalues[2]<=0)){
        stop("both of the shape parameters for priorvalues must be >0. Use 1 for each for a flat prior.")} else {}

      #The following code finds the shape parameters of a beta
      #distribution that approximates the posterior distribution
      #for the phi_w parameter
      na0=priorvalues[1]-1
      nb0=priorvalues[2]-1
      term=(3*tplus)/((2*n)+2)
      na=term+.25
      nb=(((3*n)-1)/4)-term
      a=na+na0+1
      b=nb+nb0+1
      x=seq(0,1,.005)
      y=dbeta(x,a,b)
      y0=dbeta(x,priorvalues[1],priorvalues[2])
      plot(x,y,type="l",xlab="phi_w",ylab="probability density",main="posterior solid; prior dashed")
      lines(x,y0,type="l",lty=2)

      postmean=a/(a+b)
      postmedian=qbeta(.5,a,b)
      ms1="posterior shape parameters are"
      ms2=":"
      cat(ms1,ms2,"\n")
      cat(a,"  ",b,"\n")
      cat("  ","   ","\n")
      mc1="posterior mean"
      mc2="posterior median"
      cat(mc1,"  ",mc2,"\n")
      cat(postmean," ",postmedian,"\n")

      qlequal=qbeta(.025,a,b)
      qhequal=qbeta(.975,a,b)

      met1="equal-tail 95-percent limit values are"
      met2=":"
      cat(met1,met2,"\n")
      cat(qlequal," ",qhequal,"\n")

      alphaL=seq(0,.05,.05/1000)
      qL=qbeta(alphaL,a,b)
      qH=qbeta(.95+alphaL,a,b)
      diff=qH-qL
      I=1
      mindiff=min(diff)
      while (diff[I]>mindiff){
        I=I+1}
      qLmin=qL[I]
      qHmax=qH[I]
      mhdi1="95-percent highest density limits are"
      mhdi2=":"
      cat(mhdi1,mhdi2,"\n")
      cat(qLmin," ",qHmax,"\n")

      prH1=1-pbeta(.5,a,b)
      #print("Posterior probabilty that phi_w > .5 is:")
      #print(prH1)
      priorprH1=1-pbeta(.5,na0+1,nb0+1)
      #print("Prior probabilty that phi_w > .5 is:")
      #print(priorprH1)
      mH11="assessing the probability that phi_w > .5"
      mH12=" "
      cat(mH11,mH12,"\n")
      mH1prior="prior"
      mH1post="posterior"
      cat(mH1prior,"  ",mH1post,"\n")
      cat(priorprH1,"  ",prH1,"\n")
      if ((prH1==1)|(priorprH1==0)){
        minf1="Bayes factor BF10 for phi_w >.5 is approaching"
        minf2="infinity"
        cat(minf1,minf2,"\n")} else {
          BF10=(prH1*(1-priorprH1))/(priorprH1*(1-prH1))
          mBF1="Bayes factor BF10 for phi_w > .5"
          mBF2="is"
          cat(mBF1,mBF2,"\n")
          print(BF10)}
      m1X=" "
      m2X=" "
      return(cat(m1X,m2X,"\n"))}

    if ((method!="large")&(method!="small")) {
      stop("method must be included, and it must equal either the word small or large")} else {}
  }

