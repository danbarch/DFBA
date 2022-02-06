#' Vector-to-table
#
#' Helper function for dfba_gamma
#'
#' @param x vector of x variable values
#' @param y vector of y variable values
#' @param quantiles_x Desired number of bins for the x vector
#' @param quantiles_y Desired number of bins for the y vector
#'
#' @return Cross-tabulation of x and y vectors
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press.
#' @references Chechile, R.A., & Barch, D.H. (2021). Distribution-free, Bayesian goodness-of-fit method for assessing similar scientific prediction equations. Journal of Mathematical Psychology.

#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

## Function to format two (raw) vectors as a gamma table

#' @export
Vec_to_table<-function(x, y, quantiles_x, quantiles_y){
  x_cut<-cut(x, quantiles_x)
  y_cut<-cut(y, quantiles_y)
  return(table(x_cut, y_cut))
}
