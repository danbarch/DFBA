#' Parameterization-adjusted Concordance Parameter Phi*
#'
#' This function takes two vectors,
#' shape parameters (a and b) for the prior
#' beta distribution (defaults are [1,1]),
#' and the number of fitting parameters used
#' to fit the model to the observed data.
#' @param x Vector of values for the x variable
#' @param y Vector of values for the y variable
#' @param a.prior (Optional) Value for the shape parameter alpha (default is 1) for the prior beta distribution
#' @param b.prior (Optional) Value for the shape parameter beta (default is 1) for the prior beta distribution
#' @param hdi.width (Optional) Desired range of the highest density interval (HDI) on the posterior estimate of the phi parameter (default is 95\%)
#' @param fitting.parameters Number of parameters used in the predictive model
#'
#' @return \item{Tau}{sample statistic tau-a}
#' @return \item{Tau_star}{Tau-a value adjusted for the number of fitting parameters used in the predictive model}
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press.
#' @references Chechile, R.A., & Barch, D.H. (2021). Distribution-free, Bayesian goodness-of-fit method for assessing similar scientific prediction equations. Journal of Mathematical Psychology
#'


Phi_star<-function(x, y, a.prior=1, b.prior=1, hdi.width=0.95, fitting.parameters){
  m<-fitting.parameters
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
  Tau_star<-(nc_star-nd)/(((n*(n-1))/2)-Tx-Ty+Txy)  #adjusted Tau
  p_c_star<-(Tau_star+1)/2                          #adjusted p_c (maybe unnecessary?)
  a.post_star<-a.prior+nc_star                      #adjusted a'
  b.post_star<-b.prior+nd                           #b' is unchanged
  post.median_star<-qbeta(0.5, a.post_star, b.post_star)
  post.hdi_star.lower<-qbeta((1-hdi.width)/2, a.post_star, b.post_star)
  post.hdi_star.upper<-qbeta(1-(1-hdi.width)/2, a.post_star, b.post_star)

  list(Tau=Tau,
       Tau_star=Tau_star,
       sample.p=p_c_star,
       post.median=post.median_star,
       post.hdi_star.lower=post.hdi.lower,
       post.hdi_star.upper=post.hdi.upper)
}

