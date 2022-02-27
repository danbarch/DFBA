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
                                 plot.prior=FALSE){
  if (x$method=="small"){
    x.data<-x$phiv
    y.predata<-x$priorvector
    y.postdata<-y$omegapost
  } else {
    x.data<-seq(0, 1, 1/1000)
    y.predata<-dbeta(x.data, x$a0, y$b0)
    y.postdata<-dbeta(x.data, x$apost, y$bpost)
  }
  if (plot.prior==FALSE){
    plot(x.data,
         y.postdata,
         type="l",
         xlab="omega_E",
         ylab="Posterior Discrete Probabilities")
  } else {
    opar<-par(no.readonly=TRUE)
    par(mar=c(4.1, 4.1, 4.1, 4.1), xpd=TRUE)
    plot(x.data,
         y.postdata,
         type="l",
         xlab="omega_E",
         ylab="Posterior Discrete Probabilities")
    lines(x.data,
          y.postdata,
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


