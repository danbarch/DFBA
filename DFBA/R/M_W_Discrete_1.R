
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

### Mann-Whitney Discrete Function 1

# This version takes as input $U_E$, $n_E$, $n_C$,
# the desired number of samples (default = 10000)
# and the desired number of intervals (default = 200).

mann_whitney_discrete_approx_1<-function(UE, nE, nC, n_samples = 10000, n_intervals = 200){
  XE=seq(1, nE, 1)
  XC=seq(1, nC, 1)
  fomega<-rep(0.0, n_intervals)

  for (j in 1:n_intervals){
    omega=0.5/n_intervals+(j-1)*(1/n_intervals)
    komega=(1-omega)/omega

    for (k in 1:n_samples){
      Uz<-rep(NA, nE)
      XE<-rexp(nE, rate=komega)
      XC<-rexp(nC, rate = 1)

      for (i in 1:nE){
        Uz[i]<-sum(XE[i]>XC)
      }

      if(sum(Uz) == UE) {fomega[j] = fomega[j]+1} else {}
    }
  }


  tot=sum(fomega)
  omegapost=fomega/tot
  omegav=seq(0.5/n_intervals, 1-(0.5/n_intervals), 1/n_intervals)
  omegabar=sum(omegapost*omegav)
  comega=cumsum(omegapost)
  prH1=1-comega[n_intervals/2]

  list(omegapost=omegapost,
       omegabar=omegabar,
       comega=comega,
       prH1=prH1)

}


