dfba_mann_whitney_u<-
  function(E,C,method,priorvalues=rep(1/200,200)){
    #testing if E and C vectors are valid after removing
    #NA values from the two vectors
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
    cat("E mean","  ","C mean","\n")
    cat(Emean,"  ",Cmean,"\n")
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
    cat("UE","  ","UC","\n")
    cat(UE,"   ",UC,"\n")
    cat(" ","   ","\n")

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


      XE=seq(1, length(E), 1)
      XC=seq(1, length(C), 1)
      fomega<-rep(0.0,200)
      n_samples=30000


      for (j in 1:200){
        omega=(1/400)+(j-1)*(1/200)
        komega=(1-omega)/omega

        for (k in 1:n_samples){
          Uz<-rep(NA, length(E))
          XE<-rexp(length(E), rate=komega)
          XC<-rexp(length(C), rate = 1)

          for (i in 1:length(E)){
            Uz[i]<-sum(XE[i]>XC)
          }

          if(sum(Uz) == UE) {fomega[j] = fomega[j]+1} else {}
        }
      }

      tot=sum(priorvalues*fomega)
      omegapost=(priorvalues*fomega)/tot
      phiv=rep(0.0,200)
      for (j in 1:200){
        phiv[j]=(1/400)+(j-1)*(1/200)}
      postdis<-data.frame(phiv,omegapost)

      #Folowing finds the mean of the posterior omega distribution
      #and provides a plot of the distribution.
      omegabar=sum(phiv*omegapost)
      print("mean for omega based on Monte Carlo sampling is:")
      print(omegabar)
      plot(phiv,omegapost,type="l",xlab="omega",ylab="posterior discrete probabilities",main="Based on Monte Carlo Samples")
      #The following finds the poserior cumulative distribution and outputs these values.
      cum_omega=cumsum(omegapost)
      omega_E=phiv
      cumdis<-data.frame(omega_E,cum_omega)
      #The prH1 is the probability that phi_w is greater than .5.
      prH1=1-cum_omega[round(100)]
      print("probability that omega exceeds .5 is:")
      print(prH1)
      #Following finds the Bayes factor for omega being greater than .5.
      cum_prior=cumsum(priorvalues)
      priorprH1=1-cum_prior[round(100)]
      if ((prH1==1)|(priorprH1==0)){
        BF10=2*n_samples
        print("Bayes factor BF10 for omega >.5 is estimated to be greater than:")
        print(BF10)} else {
          BF10=(prH1*(1-priorprH1))/(priorprH1*(1-prH1))
          print("Bayes factor BF10 for omega>.5 is:")
          print(BF10)}
      #Following provides output of key statistics.
      #list(posterior_cum_distribution=cumdis)
      #m1X=" "
      #m2X=" "
      return(list(posterior_discrete_values=omegapost,posterior_cum_distribution=cumdis))}
    else {}

    if (method=="large"){
      m1L<-"Following is a beta approximation for omega"
      m2L<-"which is accurate for n>14 in both groups"
      cat(m1L,m2L,"\n")
      cat(" ","   ","\n")
      if (length(priorvalues)!=2) {
        stop("priorvalues must have two value which are a0 and b0")}
      else {}
      if ((priorvalues[1]<=0)|(priorvalues[2]<=0)){
        stop("both of the shape parameters for priorvalues must be >0. Use 1 for each for a flat prior.")}
      else {}

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
      apost=priorvalues[1]+na
      bpost=priorvalues[2]+nb
      cat("posterior a"," ","posterior b","\n")
      cat(apost,"   ",bpost,"\n")
      cat(" ","  ","\n")

      a=apost
      b=bpost
      x=seq(0,1,.005)
      y=dbeta(x,a,b)
      y0=dbeta(x,priorvalues[1],priorvalues[2])
      plot(x,y,type="l",xlab="omega_E",ylab="probability density",main="posterior solid; prior dashed")
      lines(x,y0,type="l",lty=2)

      postmean=a/(a+b)
      postmedian=qbeta(.5,a,b)
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
      #print("Posterior probabilty that omega_E > .5 is:")
      #print(prH1)
      priorprH1=1-pbeta(.5,priorvalues[1],priorvalues[2])
      #print("Prior probabilty that phi_w > .5 is:")
      #print(priorprH1)
      mH11="assessing the probability that omega_E > .5"
      mH12=" "
      cat(mH11,mH12,"\n")
      mH1prior="prior"
      mH1post="posterior"
      cat(mH1prior,"  ",mH1post,"\n")
      cat(priorprH1,"  ",prH1,"\n")
      if ((prH1==1)|(priorprH1==0)){
        minf1="Bayes factor BF10 for omega_E >.5 is approaching"
        minf2="infinity"
        cat(minf1,minf2,"\n")} else {
          BF10=(prH1*(1-priorprH1))/(priorprH1*(1-prH1))
          mBF1="Bayes factor BF10 for omega_E > .5"
          mBF2="is"
          cat(mBF1,mBF2,"\n")
          print(BF10)}
      m1X=" "
      m2X=" "
      return(cat(m1X,m2X,"\n"))
    } else {}

    if ((method!="large")&(method!="small")) {
      stop("method must be included, and it must equal either the word small or large")} else {}

  }
