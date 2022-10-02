#' Goodman-Kruskal Gamma
#'
#' This function takes either: two vectors of equal length OR a
#' cross-tabulation of values and returns:
#'   the Goodman-Kruskal Gamma Statistic
#'   the observed concordance statistic p
#'   the concordance parameter Phi
#'   shape parameters a and b (alpha and beta) on the
#'     beta distribution that describes Phi
#'   Highest Density Interval (HDI) limits on Phi
#'
#'
#' @param x cross-tabulated data in matrix (or table) format
#' @param a.prior shape parameter a of the prior beta distribution
#' @param b.prior shape parameter b of the prior beta distribution
#' @param interval.width Desired width of the highest density interval (HDI) of the posterior distribution (default is 95\%)
#' @param ... Additional arguments passed to the cut() function (for example: `include.lowest`)
#'
#' @return A list containing the following components:
#' @return \item{Tau}{Nonparametric Tau-a correlation}
#' @return \item{sample_p}{Sample concordance proportion}
#' @return \item{nc}{Number of concordant (x, y) pairs}
#' @return \item{nd}{Number of discordant (x, y) pairs}
#' @return \item{post.median}{Median of posterior distribution on phi}
#' @return \item{post.hdi.lower}{lower limit of the HDI with width specified by interval.width}
#' @return \item{post.hdi.upper}{upper limit of the HDI with width specified by interval.width}
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
#                     y = NULL,
#                     breaks_x = NULL,
#                     breaks_y = NULL,
                     a.prior = 1,
                     b.prior = 1,
                     interval.width = 0.95,
                     ...){
#  if(is.matrix(x)==TRUE){
  if(is.matrix(x)==FALSE){
    stop("input must be in matrix or table format")
  }
    table<-x
    x_vec<-rep(1:nrow(table), unname(rowSums(table)))
    y_vec<-rep(as.vector(t(col(table))), as.vector(t(table)))
#  } else {
#    if(length(x)!=length(y)){
#      stop("x and y must have equal length")
#    }
#    if(is.numeric(x)){
#      if(is.null(breaks_x)){
#        stop("When x is numeric, either a numeric vector of two or more unique cut points or a single number of intervals into which x variable is to be cut must be specified")
#      }
#      x_cut<-cut(x, breaks_x, ...)
#    } else{
#      x_cut<-x
#    }
#    if(is.numeric(y)){
#      if(is.null(breaks_y)){
#        stop("When y is numeric, either a numeric vector of two or more unique cut points or a single number of intervals into which y variable is to be cut must be specified")
#      }
#      y_cut<-cut(y, breaks_y, ...)
#    } else{
#      y_cut<-y
#  }
    table<-(table(x_vec, y_vec))

#  }

  x<-rep(1:nrow(table), unname(rowSums(table)))
  y<-rep(as.vector(t(col(table))), as.vector(t(table)))

    dfba_gamma_list<-list(gamma=dfba_phi(x, y, a.prior, b.prior, interval.width)$tau,
                        a.prior=a.prior,
                        b.prior=b.prior,
                sample.p=dfba_phi(x, y, a.prior, b.prior, interval.width)$sample.p,
                nc=dfba_phi(x, y, a.prior, b.prior, interval.width)$nc,
                nd=dfba_phi(x, y, a.prior, b.prior, interval.width)$nd,
                alpha=dfba_phi(x, y, a.prior, b.prior, interval.width)$alpha,
                beta=dfba_phi(x, y, a.prior, b.prior, interval.width)$beta,
                interval.width=interval.width,
                post.median=dfba_phi(x, y, a.prior, b.prior, interval.width)$post.median,
                post.eti.lower=dfba_phi(x, y, a.prior, b.prior, interval.width)$post.eti.lower,
                post.eti.upper=dfba_phi(x, y, a.prior, b.prior, interval.width)$post.eti.upper
#                table.row=x_vec,
#                table.column=y_vec
                )
  new("dfba_gamma_out", dfba_gamma_list)
}

