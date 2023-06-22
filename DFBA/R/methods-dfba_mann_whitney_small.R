#' Formats for small-n Mann Whitney

## Small n

#' @export
#' @rdname dfba_mann_whitney_small_method
#' @param object An object of class \code{\linkS4class{dfba_mann_whitney_small_out}}
setMethod("show", "dfba_mann_whitney_small_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "n_E", "\t", "n_C", "\n")
  cat(" ", object$n_E, "\t\t\t", object$n_C, "\n")
  cat(" ", "E mean", "\t", "C mean", "\n")
  cat(" ", object$Emean, "\t\t\t", object$Cmean, "\n")
  cat(" ", "U_E and U_C Mann-Whitney Statistics", "\n")
  cat(" ", object$U_E, "\t\t\t", object$U_C, "\n")
  cat("\n  Monte Carlo Sampling with Discrete Probability Values\n")
  cat("========================\n")
  cat(" ", "Number of MC Samples\n")
  cat(" ", object$samples, "\n")
  cat(" ", "\n  Mean of omega_E:\n")
  cat(" ", object$omegabar, "\n")
  cat("equal-tail area interval")
  cat(" ", object$prob_interval*100, "% interval limits:", "\n", sep="")
  cat(" ", object$qLv, "\t\t\t", object$qHv, "\n")
  cat(" ", "probability that omega_E exceeds 0.5:\n")
  cat(" ", "prior", "\t\t\t", "posterior\n")
  cat(" ", object$priorprH1, "\t\t\t", object$prH1, "\n")
  cat("  Bayes factor BF10 for omega_E > 0.5:\n")
  cat(" ", ifelse((object$prH1==1)|(object$priorprH1==0),
                  paste0("Bayes factor BF10 for omega_E >.5 is estimated to be greater than: ", object$samples),
                  object$BF10), "\n")
})



# Plot

#' @export
#' @rdname dfba_mann_whitney_small_method
#' @param x An object of class \code{\linkS4class{dfba_mann_whitney_small_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
setMethod("plot",
          signature("dfba_mann_whitney_small_out"),
          function(x,
                   plot.prior=TRUE){
            x.data<-x$omega_E
            y.predata<-x$priorvector
            y.postdata<-x$omegapost
            xlab <- "omega_E"
            ylab <- "Discrete Probability"
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
          }
)

