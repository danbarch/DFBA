dfba_kendall_tau<-function(x, y,a0=1,b0=1){
  #computes Kendall's tau_a and tau_b rank based coefficients,
  #along with the phi_c the concordance parameter where
  #phi_c is (1+tau_a)/2.
  #The function also finds the number of concordant changes n_c
  #and the number of discordance change.
  #The tau_b coefficient is (n_c-n_d)/sqrt([(.5*(n*(n-1)-T_x]*(.5(n*(n-1)-T_y])
  #where T_x is the number of lost comparisons due to ties in the x variate and
  #T_y is the corresponding lost comparisons due to ties in the y variate.
  #The tau_b coefficient does not adjust for excessively correcting for multiply
  #tied pairs. However,tau_a coefficient is simply equal to (n_c-n_d)/(n_c+n_d) and
  #this coefficent properly adjusts for all tied values. If the are no tied value
  #tau_a and tau_b are equal.

  if ((a0<=0) | (b0<=0)){
    a0=1
    b0=1
    message("a0 and b0 shape parameters cannot be negative, so the default values were used.")}
  else {}


  l1=length(x)
  l2=length(y)
  if (l1!=l2) {
    stop("The two variate have unequal length. The x and y variates must be paired.")}
  else {}

  Xtest=x
  Ytest=y
  Xtemp=Xtest
  Ytemp=Ytest
  jc=0
  for (j in 1:length(Xtest)){
    if ((is.na(Xtemp[j]))|(is.na(Ytemp[j]))) {} else {
      jc=jc+1
      Xtest[jc]=Xtemp[j]
      Ytest[jc]=Ytemp[j]
    } }
  x=Xtest[1:jc]
  y=Ytest[1:jc]


  xy<-data.frame(x,y)
  x_ties<-table(x)[table(x)>1]
  y_ties<-table(y)[table(y)>1]
  xy_ties<-table(xy)[table(xy)>1]
  t_xi<-unname(table(x)[table(x)>1])
  t_yi<-unname(table(y)[table(y)>1])
  t_xyi<-unname(table(xy)[table(xy)>1])
  Tx<-sum((t_xi*(t_xi-1))/2)
  Ty<-sum((t_yi*(t_yi-1))/2)
  Txy<-sum(t_xyi*(t_xyi-1)/2)
  n<-length(x)
  n_max<-n*(n-1)/2-Tx-Ty+Txy
  xy_ranks<-data.frame(xrank=rank(xy$x, ties.method="average"),
                       yrank=rank(xy$y, ties.method="average"))
  xy_c<-xy_ranks[order(x, -y),] # for n_c, sort on ascending x then descending y
  xy$concordant<-rep(NA, nrow(xy))
  for (i in 1:nrow(xy-1)){
    xy$concordant[i]<-sum(xy_c$yrank[(i+1):length(xy_c$yrank)]>xy_c$yrank[i])
  }
  nc<-sum(xy$concordant, na.rm=TRUE)
  nd<-n_max-nc

  Tau_a<-(nc-nd)/n_max
  Tau_b<-(nc-nd)/sqrt((n*(n-1)/2-Tx)*(n*(n-1)/2-Ty))
  phi_c<-nc/(nc+nd)

  m1="Frequentist point estimates for tau_a and tau_b are"
  m2=":"
  cat(m1,m2,"\n")
  cat("tau_a","      ","tau_b","\n")
  cat(Tau_a," ",Tau_b,"\n")
  cat(" ","   ","\n")
  m21="Sample concordant n_c and discordant differences are"
  cat(m21,m2,"\n")
  cat("n_c","  ","n_d","\n")
  cat(nc,"  ",nd,"\n")
  cat(" ","   ","\n")
  m31="Bayesian analysis deal with the concordance proportion"
  m32="phi_c"
  cat(m31,m32,"\n")
  m41="posterior mean and median phi_c are"
  cat(m41,m2,"\n")
  cat("mean","      ","median","\n")
  apost=nc+a0
  bpost=nd+b0
  postphimean=apost/(apost+bpost)
  postphimedian=qbeta(.5,apost,bpost)
  cat(postphimean,"  ",postphimedian,"\n")
  cat("  "," ","\n")
  m51="Corresponding posterior mean and median for tau_a are"
  cat(m51,m2,"\n")
  cat("mean","      ","median","\n")
  tau_a_mean=(2*postphimean)-1
  tau_a_median=(2*postphimedian)-1
  cat(tau_a_mean,"  ",tau_a_median,"\n")
  cat("  "," ","\n")
  m61="Beta shape parameters for the phi_c distribution are"
  cat(m61,m2,"\n")
  cat("a","   ","b","\n")
  cat(apost," ",bpost,"\n")
  cat("  "," ","\n")

  a=apost
  b=bpost
  qlequal=qbeta(.025,a,b)
  qhequal=qbeta(.975,a,b)

  met1="phi_c equal-tail 95-percent limit values are"
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
  mhdi1="phi_c 95-percent highest density limits are"
  mhdi2=":"
  cat(mhdi1,mhdi2,"\n")
  cat(qLmin," ",qHmax,"\n")
  cat("  "," ","\n")
  cat("The corresponding tau_a interval"," ","\n")
  met1="tau_a equal-tail 95-percent limit values are"
  met2=":"
  cat(met1,met2,"\n")
  lowval=(qlequal*2)-1
  hival=(qhequal*2)-1
  cat(lowval," ",hival,"\n")
  met1a="tau_a 95-percent highest density limit values are"
  met2a=":"
  cat(met1a,met2a,"\n")
  lowvalhdi=(qLmin*2)-1
  hivalhdi=(qHmax*2)-1
  cat(lowvalhdi," ",hivalhdi)
  cat(" ","  ","\n")

  prH1=1-pbeta(.5,a,b)
  priorprH1=1-pbeta(.5,a0,b0)
  mH11="Probabilities that either phi_c > .5 or tau_a>0"
  cat(mH11,"are","\n")


  mH1prior="Prior"
  mH1post="Posterior"
  cat(mH1prior,"  ",mH1post,"\n")
  cat(priorprH1,"  ",prH1,"\n")
  if ((prH1==1)|(priorprH1==0)){
    minf1="Bayes factor BF10 for phi_c >.5 is approaching"
    minf2="infinity"
    cat(minf1,minf2,"\n")} else {
      BF10=(prH1*(1-priorprH1))/(priorprH1*(1-prH1))
      list("Bayes Factor tau_a > 0"=BF10)}
}

