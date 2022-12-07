#' Formats for large-n Wilcoxon

## Show


#' @export
#' @rdname dfba_wilcoxon_method_large
#' @param object An object of class \code{\linkS4class{dfba_wilcoxon_large_out}}
setMethod("show", "dfba_wilcoxon_large_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Wilcoxon Signed-Rank Statistics", "\n")
  cat(" ", "n", "\t", "T_plus", "\t", "T_minus", "\n")
  cat(" ", object$n, "\t", object$T_plus, "\t", object$T_negative, "\n")
  cat("\n  Beta Approximation Model for Phi_W\n")
  cat(" for n > 24\n")
  cat("========================\n")
  cat(" ", "The posterior beta shape parameters are:\n")
  cat(" ", "posterior a", "\t\t\t", "posterior b\n")
  cat(" ", object$apost, "\t\t\t", object$bpost, "\n")
  cat(" ", "posterior mean", "\t\t\t", "posterior median\n")
  cat(" ", object$postmean, "\t\t\t", object$postmedian, "\n")
  cat(" ", "probability within interval:\n")
  cat(" ", round(object$prob_interval*100), " percent\n")
  cat(" ", "equal-tail limit values are:\n")
  cat(" ", object$qlequal, "\t\t\t", object$qhequal, "\n")
  cat(" ", "highest-density limits are:\n")
  cat(" ", object$qLmin, "\t\t\t", object$qHmax, "\n")
  cat(" ", "probability that phi_W > 0.5:\n")
  cat(" ", "prior", "\t\t\t", "posterior\n")
  cat(" ", object$priorprH1, "\t\t\t", object$prH1, "\n")
  cat(" ", "Bayes factor BF10 for phi_W > 0.5:\n")
  cat(" ", object$BF10, "\n")
})

# Plot

#' @export
#' @rdname dfba_wilcoxon_method_large
#' @param x An object of class \code{\linkS4class{dfba_wilcoxon_large_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
setMethod("plot",
          signature("dfba_wilcoxon_large_out"),
          function(x,
                   plot.prior=TRUE){
            x.data<-seq(0, 1, 1/1000)
            y.predata<-dbeta(x.data, x$a0, x$b0)
            y.postdata<-dbeta(x.data, x$apost, x$bpost)
            xlab="phi_W"
            ylab="Probability Density"

            if (plot.prior==FALSE){
              plot(x.data,
                   y.postdata,
                   type="l",
                   xlab=xlab,
                   ylab=ylab)
            } else {
              plot(x.data,
                   y.postdata,
                   type="l",
                   xlab=xlab,
                   ylab=ylab,
                   main=expression("--"~"Prior"~ - "Posterior"))
              lines(x.data,
                    y.predata,
                    lty=2)
            }
          })
