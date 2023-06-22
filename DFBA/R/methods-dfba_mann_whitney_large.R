#' Formats for large-n Mann Whitney

## Show

#' @export
#' @rdname dfba_mann_whitney_large_method
#' @param object An object of class \code{\linkS4class{dfba_mann_whitney_large_out}}
setMethod("show", "dfba_mann_whitney_large_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "n_E", "\t", "n_C", "\n")
  cat(" ", object$n_E, "\t\t\t", object$n_C, "\n")
  cat(" ", "E mean", "\t", "C mean", "\n")
  cat(" ", object$Emean, "\t\t\t", object$Cmean, "\n")
  cat(" ", "U_E and U_C Mann-Whitney Statistics", "\n")
  cat(" ", object$U_E, "\t\t\t", object$U_C, "\n")
  cat("\n  Beta Approximation Model for Omega_E\n")
  cat(" for 2*nE*nC/(nE+nC) > 19\n")
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
  cat(" ", "probability that omega_E > 0.5:\n")
  cat(" ", "prior", "\t\t\t", "posterior\n")
  cat(" ", object$priorprH1, "\t\t\t", object$prH1, "\n")
  cat(" ", "Bayes factor BF10 for omega_E > 0.5:\n")
  cat(" ", ifelse(object$BF10 == Inf, "approaching infinity", object$BF10), "\n")
})


## plot
#' @export
#' @rdname dfba_mann_whitney_large_method
#' @param x An object of class \code{\linkS4class{dfba_mann_whitney_small_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
setMethod("plot",
          signature("dfba_mann_whitney_large_out"),
          function(x,
                   plot.prior=TRUE){
            x.data<-seq(0, 1, 1/1000)
            y.predata<-dbeta(x.data, x$a0, x$b0)
            y.postdata<-dbeta(x.data, x$apost, x$bpost)
            xlab <- "omega_E"
            ylab <- "Probability Density"
            if (plot.prior==FALSE){
              plot(x.data,
                   y.postdata,
                   type="l",
                   xlab=xlab,
                   ylab=ylab)
            } else {
              #    opar<-par(no.readonly=TRUE)
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
)
