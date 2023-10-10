#' Formats for small-n Mann Whitney

## Small n
#' @keywords internal
#' @export
#' @rdname dfba_mann_whitney_small_method
#' @param object An object of class \code{\linkS4class{dfba_mann_whitney_small_out}}
setMethod("show", "dfba_mann_whitney_small_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", sprintf("%-10s", "n_E"), "\t", "n_C", "\n")
  cat(" ", sprintf("%-10g", object$n_E), "\t", object$n_C, "\n")
  cat(" ", sprintf("%-10s", "E mean"), "\t", "C mean", "\n")
  cat(" ", sprintf("%-10g", object$Emean), "\t", object$Cmean, "\n")
  cat(" ", "Mann-Whitney Statistics", "\n")
  cat(" ", sprintf("%-10s", "U_E"), "\t", "U_C", "\n")
  cat(" ", sprintf("%-10g", object$U_E), "\t", object$U_C, "\n")
  cat("\n  Monte Carlo Sampling with Discrete Probability Values\n")
  cat("========================\n")
  cat(" ", "Number of MC Samples\n")
  cat(" ", object$samples, "\n")
  cat(" ", "\n  Mean of omega_E:\n")
  cat(" ", object$omegabar, "\n")
  cat(" ", paste0(round(object$prob_interval*100), "% Equal-tail interval limits:"), "\n")
  cat(" ",
      sprintf("%-12s", "Lower Limit"),
      "\t",
      "Upper Limit",
      "\n")
  cat(" ",
      sprintf("%-12g",
              object$eti_lower),
      "\t",
      object$eti_upper,
      "\n")
  cat(" ", "probability that omega_E exceeds 0.5:\n")
  cat(" ", sprintf("%-10s", "prior"), "\t", "posterior\n")
  cat(" ", sprintf("%-10g", object$priorprH1), "\t", object$prH1, "\n")
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

