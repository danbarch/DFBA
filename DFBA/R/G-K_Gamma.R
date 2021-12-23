# Goodman-Kruskal Gamma
#
# This function takes the shape parameters (a and b)
# for a Beta Distribution and returns:
#   the mean
#   the mode
#   the median
#   a equal-tail credible interval (default is 95%)
#   a highest-density interval (default is 95%)
#

# SAMPLE DATA:
# x<-c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5)
# y<-c(1, 1, 4, 4, 5, 1, 2, 3, 5, 1, 2, 3, 3, 3, 3, 5, 2, 4, 4, 4, 5, 5, 3, 4, 4, 5, 5)
# Some useful keyboard shortcuts for package authoring:
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

Gamma_Concordance<-function(x, y=NULL, a.prior=1, b.prior=1, hdi.width=0.95){
  if(is.matrix(x)==TRUE){
    table<-x
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
