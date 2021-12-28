# Goodman-Kruskal Gamma
#
# This function takes either:
# two vectors of equal length OR
# a cross-tabulation of values
# and returns:
#   the Goodman-Kruskal Gamma Statistic
#   the observed concordance statistic p
#   the concordance parameter Phi
#   shape parameters a and b (alpha and beta) on the
#     beta distribution that describes Phi
#   Highest Density Interval (HDI) limits on Phi
#


#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

## Function to format two (raw) vectors as a gamma table

Vec_to_table<-function(x, y, quantiles_x, quantiles_y){
  x_cut<-cut(x, quantiles_x)
  y_cut<-cut(y, quantiles_y)
  return(table(x_cut, y_cut))
}

## Function to format a gamma table to two (grouped) vectors (for use in the $\phi_c$ function)


Table_to_vec<-function(table){
  x<-rep(1:nrow(table), unname(rowSums(table)))
  y<-rep(as.vector(t(col(table))), as.vector(t(table)))
  list(x=x,
       y=y)
}

## Goodman-Kruskal Gamma Analysis Using Concordance Parameter Phi

Gamma_Concordance<-function(x, y=NULL, quantiles_x=NULL, quantiles_y=NULL, a.prior=1, b.prior=1, hdi.width=0.95){
  if(is.matrix(x)==TRUE){
    table<-x
  } else {
    if(length(x)!=length(y)){
      stop("x and y must have equal length")
    }
    table<-Vec_to_table(x, y, quantiles_x, quantiles_y)
    x<-Table_to_vec(table)$x
    y<-Table_to_vec(table)$y
  }
  list(gamma=Phi(x, y, a.prior, b.prior, hdi.width)$tau,
       sample.p=Phi(x, y, a.prior, b.prior, hdi.width)$sample.p,
       alpha=Phi(x, y, a.prior, b.prior, hdi.width)$alpha,
       beta=Phi(x, y, a.prior, b.prior, hdi.width)$beta,
       post.median=Phi(x, y, a.prior, b.prior, hdi.width)$post.median,
       post.hdi.lower=Phi(x, y, a.prior, b.prior, hdi.width)$post.hdi.lower,
       post.hdi.upper=Phi(x, y, a.prior, b.prior, hdi.width)$post.hdi.upper)
}
