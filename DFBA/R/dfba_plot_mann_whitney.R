#' dfba_plot_mann_whitney
#
#' Plots data from dfba mann whitney
#'
#' @param x A dfba_mann_whitney object
#' @param plot.prior (optional) If TRUE, plots the prior distribution
#'
#' @return Plot
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press.
#' @references Chechile, R.A., & Barch, D.H. (2021). Distribution-free, Bayesian goodness-of-fit method for assessing similar scientific prediction equations. Journal of Mathematical Psychology.
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics par
#'
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' @export
dfba_plot_mann_whitney<-function(x,
                                 plot.prior=TRUE){
  if (x$method=="small"){
    x.data<-x$phiv
    y.predata<-x$priorvector
    y.postdata<-x$omegapost
    xlab="omega_E"
    ylab="Discrete Probability"
  } else {
    x.data<-seq(0, 1, 1/1000)
    y.predata<-dbeta(x.data, x$a0, x$b0)
    y.postdata<-dbeta(x.data, x$apost, x$bpost)
    xlab="omega_E"
    ylab="Probability Density"
  }
  if (plot.prior==FALSE){
    plot(x.data,
         y.postdata,
         type="l",
         xlab=xlab,
         ylab=ylab)
  } else {
    opar<-par(no.readonly=TRUE)
#    par(mar=c(4.1, 4.1, 4.1, 4.1), xpd=TRUE)
    plot(x.data,
         y.postdata,
         type="l",
         xlab=xlab,
         ylab=ylab,
         main=expression("--"~"Prior"~ - "Posterior"))
    lines(x.data,
          y.predata,
          lty=2)
#    legend("top",
#           inset = c(0, -0.1),
#           legend=c("Posterior",
#                    "Prior"),
#           lty=c(1, 2),
#           xpd=TRUE,
#           horiz=TRUE)
#    on.exit(par(opar))
  }
}


