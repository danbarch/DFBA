#' Beta Point Bayes Factor
#
#' @param n1 Number of values for variable 1
#' @param n2 Number of values for variable 2
#' @param ptH0 (Optional) Point estimate for the null hypothesis (default is 0.5)
#' @param a0 (Optional) Shape parameter alpha for the prior beta distribution (default is 1)
#' @param b0 (Optional) Shape parameter beta for the prior beta distribution (default is 1)
#' @return \item{H0_point}{Value of the point null hypothesis -- either the default of 0.5 or the value specified by user}
#' @return \item{Direction}{Statement describing the numerator and denominator of Bayes Factor estimate}
#' @return \item{BF}{Bayes Factor estimate}

Beta_pointBayesfactor<-function(n1,n2,ptH0=.5,a0=1,b0=1){
  if ((a0<=0)|(b0<=0)){
    a0=1
    b0=1} else {}
  if ((ptH0>1)|(ptH0<0)){
    ptH0=.5}
  if ((n1<0)|(n2<0)){
    stop("Error. Both n1 and n2 must be nonnegative; please redo")} else
    {
      a=n1+a0
      b=n2+b0
      dpriorH0=dbeta(ptH0,a0,b0)
      dpostH0=dbeta(ptH0,a,b)
      if (dpriorH0>=dpostH0){
        Direction<-"Bayes factor is for alternative over null"
        if(dpostH0==0){print("BF10 approaches infinity")} else
        {BF<-dpriorH0/dpostH0
        }} else {
          Direction<-"Bayes factor is for null over alternative"
          BF<-dpostH0/dpriorH0
          }
    }
  list(H0_point=H0,
       Direction=Direction,
       BF=BF)
}
