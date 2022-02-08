
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Mann-Whitney Discrete Function 1

#' This version takes as input $U_E$, $n_E$, $n_C$
#' the desired number of samples (default = 10000)
#' and the desired number of intervals (default = 200).
#'
#' @param U_A U statistic for the A variable
#' @param n_A Number of values for the A variable
#' @param n_B Number of values for the B variable
#' @param n_samples Number of desired samples
#' @param n_intervals Number of desired intervals for the discrete approximation
#'
#' @return omegapost
#' @return omegabar
#' @return comega
#' @return prH1 Posterior probability of the hypothesis that A > B
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press


mann_whitney_discrete_approx_1<-function(U_A, n_A, n_B, n_samples = 10000, n_intervals = 200){
  XA=seq(1, n_A, 1)
  XB=seq(1, n_B, 1)
  fomega<-rep(0.0, n_intervals)

  for (j in 1:n_intervals){
    omega=0.5/n_intervals+(j-1)*(1/n_intervals)
    komega=(1-omega)/omega

    for (k in 1:n_samples){
      Uz<-rep(NA, n_A)
      XA<-rexp(n_A, rate=komega)
      XB<-rexp(n_B, rate = 1)

      for (i in 1:n_A){
        Uz[i]<-sum(XA[i]>XB)
      }

      if(sum(Uz) == U_A) {fomega[j] = fomega[j]+1} else {}
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


