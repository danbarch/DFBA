#' dfba_plot_beta
#
#' Plots data from dfba functions
#'
#' @param a.post Shape parameter a for posterior distribution
#' @param b.post Shape parameter b for posterior distribution
#' @param a.prior Shape parameter a for prior distribution
#' @param b.prior Shape parameter b for prior distribution
#' @param plot.prior (optional) If TRUE, plots the prior distribution
#'
#' @return Plot
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press.
#' @references Chechile, R.A., & Barch, D.H. (2021). Distribution-free, Bayesian goodness-of-fit method for assessing similar scientific prediction equations. Journal of Mathematical Psychology.
#' @importFrom stats dbeta
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics par
#'
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

## Function to format two (raw) vectors as a gamma table

#' @export
dfba_plot_beta<-function(a.post,
                         b.post,
                         a.prior=NULL,
                         b.prior=NULL,
                         plot.prior=FALSE){
  x.phi<-seq(0, 1, 1/1000)
  y.phi<-dbeta(x.phi, a.post, b.post)
  if (plot.prior==FALSE){
    plot(x.phi,
         y.phi,
         type="l",
         xlab="Phi",
         ylab="Probability Density")
  } else {
    opar<-par(no.readonly=TRUE)
    par(mar=c(4.1, 4.1, 4.1, 4.1), xpd=TRUE)
    plot(x.phi,
         y.phi,
         type="l",
         xlab="Phi",
         ylab="Probability Density")
    lines(x.phi,
          dbeta(x.phi, a.prior, b.prior),
          lty=2)
    legend("top",
           inset = c(0, -0.25),
           legend=c("Posterior",
                    "Prior"),
           lty=c(1, 2),
           xpd=TRUE,
           horiz=TRUE)
    on.exit(par(opar))
  }
}
