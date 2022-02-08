# Beta Stats
#
# This function takes the shape parameters (a and b)
# for a Beta Distribution and returns:
#   the mean
#   the mode
#   the median
#   a equal-tail credible interval (default is 95%)
#   a highest-density interval (default is 95%)

#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

Beta_descriptive<-function(a, b, hdiprob =.95){
  if(hdiprob > 1 | hdiprob < 0){hdiprob=.95} else{}
  if(a <= 0 | b <= 0){
    print(" Error. Both a and b must be greater than 0 ; please redo")}
  else{
    phimean=a/(a+b)
    phimedian=qbeta (.5, a, b)
    if ((a>1)&(b>1)){
      phimode=(a-1)/(a+b-2)
      }
      else {}
    qlequal=qbeta((1-hdiprob)/2, a, b)
    qhequal=qbeta(hdiprob+((1-hdiprob)/2), a, b)
    alphaL=seq(0, 1-hdiprob, (1-hdiprob)/1000)
    qL=qbeta(alphaL, a ,b)
    qH=qbeta(hdiprob+alphaL, a, b)
    diff=qH-qL
    I=1
    mindiff=min(diff)
    while(diff[I]>mindiff){
      I=I+1
    }
    qLmin=qL[I]
    qHmax=qH[I]
  }
  list(Mean = phimean,
       Median = phimedian,
       mode = phimode,
       "Equal-tail limit values" = c(qlequal, qhequal),
       "Probability within interval limits"=hdiprob,
       "Highest density limit values" = c(qLmin, qHmax))
}

