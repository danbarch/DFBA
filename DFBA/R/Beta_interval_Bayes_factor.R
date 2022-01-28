#' Beta Interval Bayes Factor
#'
#'
#' @param n1 Number of values for variable 1
#' @param n2 Number of values for variable 2
#' @param lowerH0value (Optional) Lower limit of interval estimate for the null hypothesis (default is 0)
#' @param upperH0value (Optional) Upper limit of interval estimate for the null hypothesis (default is 0.5)
#' @param a0 (Optional) Shape parameter alpha for the prior beta distribution (default is 1)
#' @param b0 (Optional) Shape parameter beta for the prior beta distribution (default is 1)
#' @return \item{H0_range}{Interval estimate of the null hypothesis -- either the default of [0, 0.5] or the value specified by user}
#' @return \item{H0_prior}{Prior probability of the null interval}
#' @return \item{H1_prior}{Prior probability of the alternative interval}
#' @return \item{H0_posterior}{Posterior probability of the null interval}
#' @return \item{H1_posterior}{Posterior probability of the alternative interval}
#' @return \item{Direction}{Statement describing the numerator and denominator of Bayes Factor estimate}
#' @return \item{BF}{Bayes Factor estimate}
#'

Beta_intervalBayesfactor<-function(n1,n2,lowerH0value=0,upperH0value=.5,a0=1,b0=1){
  if ((lowerH0value<0)|(lowerH0value>1)|(lowerH0value>=upperH0value)|(upperH0value>1)) {lowerH0value=0
  upperH0value=.5} else {}
  rangeH0<-c(lowerH0value,upperH0value)
  if ((a0<=0)|(b0<=0)) {
    a0=1
    b0=1} else {}
  if ((n1<0)|(n2<0)){
    stop("Error. Both n1 and n2 must be nonnegative; please redo")} else {
      a=n1+a0
      b=n2+b0
      pH0up=pbeta(upperH0value,a0,b0)
      pH0lo=pbeta(lowerH0value,a0,b0)
      pH0=pH0up-pH0lo
      pH1=1-pH0
      postH0up=pbeta(upperH0value,a,b)
      postH0lo=pbeta(lowerH0value,a,b)
      postH0=postH0up-postH0lo
      postH1=1-postH0
      if (pH0>=postH0){
        Direction<-"Bayes factor is for alternative over null"
        if(postH0==0) {
          BF10<-Inf
          message("BF10 approaches infinity")
          } else {
            BF10=((1/postH0)-1)/((1/pH0)-1)
            BF<-BF10
        }
        } else {
          Direction<-"Bayes factor is for null over alternative"
          if (postH0==1){
            BF01<-Inf
            message("BF01 approaches infinity")
            } else {
              BF01=((1/pH0)-1)/((1/postH0)-1)

          BF<-BF01}}
    }
  list(H0_Range=rangeH0,
       H0_prior=pH0,
       H1_prior=pH1,
       H0_posterior=postH0,
       H1_posterior=postH1,
       Direction=Direction,
       BF=BF
       )
}
