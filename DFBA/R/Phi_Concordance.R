#' Concordance Parameter Phi
#'
#' This function takes two vectors and shape parameters (a and b) for the prior
#' beta distribution (defaults are [1,1])
#' @param x vector of x variable values
#' @param y vector of y variable values
#' @param a.prior shape parameter a of the prior beta distribution
#' @param b.prior shape parameter b of the prior beta distribution
#' @param hdi.width Desired width of the highest density interval (HDI) of the posterior distribution (default is 95\%)
#'
#' @return A list containing the following components:
#' @return \item{Tau}{Nonparametric Tau-a correlation}
#' @return \item{sample_p}{Sample concordance proportion}
#' @return \item{nc}{Number of concordant (x, y) pairs}
#' @return \item{nd}{Number of discordant (x, y) pairs}
#' @return \item{post.median}{Median of posterior distribution on phi}
#' @return \item{post.hdi.lower}{lower limit of the HDI with width specified by hdi.width}
#' @return \item{post.hdi.upper}{upper limit of the HDI with width specified by hdi.width}
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press.
#' @references Chechile, R.A., & Barch, D.H. (2021). Distribution-free, Bayesian goodness-of-fit method for assessing similar scientific prediction equations. Journal of Mathematical Psychology.

#' @importFrom stats qbeta
#'
#' @export
dfba_phi<-function(x, y, a.prior=1, b.prior=1, hdi.width=0.95){
  xy<-data.frame(x,y)                               #append x and y vectors
  t_xi<-unname(table(x)[table(x)>1])                #Counting T_x sizes of ties
  t_yi<-unname(table(y)[table(y)>1])                #Counting T_y sizes of ties
  Tx<-sum((t_xi*(t_xi-1))/2)                        #Calculating Tx
  Ty<-sum((t_yi*(t_yi-1))/2)                        #Calculating Ty
  t_xyi<-unname(table(xy)[table(xy)>1])             #Calculating txyi
  Txy<-sum(t_xyi*(t_xyi-1)/2)                       #Calculating Txy
  n<-length(x)
  n_max<-n*(n-1)/2-Tx-Ty+Txy
  xy_ranks<-data.frame(xrank=rank(xy$x, ties.method="average"),
                       yrank=rank(xy$y, ties.method="average"))
  xy_c<-xy_ranks[order(x, -y),]       # for n_c, sort on ascending x then descending y
  xy$concordant<-rep(NA, nrow(xy))
  for (i in 1:nrow(xy-1)){
    xy$concordant[i]<-sum(xy_c$yrank[(i+1):length(xy_c$yrank)]>xy_c$yrank[i])
  }
  nc<-sum(xy$concordant, na.rm=TRUE)
  nd<-n_max-nc
  Tau<-(nc-nd)/n_max
  p_c<-(Tau+1)/2
  a.post<-a.prior+nc
  b.post<-b.prior+nd
  post.median<-qbeta(0.5, a.post, b.post)
  post.hdi.lower<-qbeta((1-hdi.width)/2, a.post, b.post)
  post.hdi.upper<-qbeta(1-(1-hdi.width)/2, a.post, b.post)

  dfba_phi_list<-list(tau=Tau,
                      nc=nc,
                      nd=nd,
                      sample.p=p_c,
                      alpha=a.post,
                      beta=b.post,
                      post.median=post.median,
                      hdi.width=hdi.width,
                      post.hdi.lower=post.hdi.lower,
                      post.hdi.upper=post.hdi.upper)

  new("dfba_phi_out", dfba_phi_list)
}


