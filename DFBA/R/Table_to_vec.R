#' Table-to-vector
#
#' Helper function for dfba_gamma
#'
#' @param table A cross-tabulation of two variables (x and y)
#'
#' @return \item{x}{Vector of x variates grouped according to cross-tabulation in input table}
#' @return \item{y}{Vector of y variates grouped according to cross-tabulation in input table}
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press.
#' @references Chechile, R.A., & Barch, D.H. (2021). Distribution-free, Bayesian goodness-of-fit method for assessing similar scientific prediction equations. Journal of Mathematical Psychology.

#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


## Function to format a gamma table to two (grouped) vectors (for use in the $\phi_c$ function)

#' @export
Table_to_vec<-function(table){
  x<-rep(1:nrow(table), unname(rowSums(table)))
  y<-rep(as.vector(t(col(table))), as.vector(t(table)))
  list(x=x,
       y=y)
}
