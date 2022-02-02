#' Goodman-Kruskal Gamma
#
#' This function takes either:
#' two vectors of equal length OR
#' a cross-tabulation of values
#' and returns:
#'   the Goodman-Kruskal Gamma Statistic
#'   the observed concordance statistic p
#'   the concordance parameter Phi
#'   shape parameters a and b (alpha and beta) on the
#'     beta distribution that describes Phi
#'   Highest Density Interval (HDI) limits on Phi
#'
#' @param x vector of x variable values
#' @param y vector of y variable values
#' @param quantiles_x Desired number of quantiles for the x variable
#' @param quantiles_y Desired number of quantiles for the y variable
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



#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


## Goodman-Kruskal Gamma Analysis Using Concordance Parameter Phi

#' @export
dfba_gamma<-function(x,
                     y=NULL,
                     quantiles_x=NULL,
                     quantiles_y=NULL,
                     a.prior=1,
                     b.prior=1,
                     hdi.width=0.95){
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
  dfba_gamma_list<-list(gamma=dfba_phi(x, y, a.prior, b.prior, hdi.width)$tau,
                        a.prior=a.prior,
                        b.prior=b.prior,
                sample.p=dfba_phi(x, y, a.prior, b.prior, hdi.width)$sample.p,
                alpha=dfba_phi(x, y, a.prior, b.prior, hdi.width)$alpha,
                beta=dfba_phi(x, y, a.prior, b.prior, hdi.width)$beta,
                hdi.width=hdi.width,
                post.median=dfba_phi(x, y, a.prior, b.prior, hdi.width)$post.median,
                post.hdi.lower=dfba_phi(x, y, a.prior, b.prior, hdi.width)$post.hdi.lower,
                post.hdi.upper=dfba_phi(x, y, a.prior, b.prior, hdi.width)$post.hdi.upper)
  new("dfba_gamma_out", dfba_gamma_list)
}

