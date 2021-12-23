# Concordance Parameter Phi*
#
# This function takes two vectors,
# shape parameters (a and b) for the prior
# beta distribution (defaults are [1,1]),
# and the number of fitting parameters used
# to fit the model to the observed data.
#
# It returns the same values as Phi_Concordance:
#   Tau-a
#   Sample concordance p_c
#   shape parameters (a and b) for posterior beta
## NOTE: update below with beta_descriptive?
#   posterior median
#   lower and upper limits on posterior distribution
# adjusted for the number of fitting parameters
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

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

  list(tau=Tau,
       Tau_star=Tau_star,
       sample.p=p_c_star,
       post.median=post.median_star,
       post.hdi_star.lower=post.hdi.lower,
       post.hdi_star.upper=post.hdi.upper)
}

