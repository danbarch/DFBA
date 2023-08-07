#' Formats for small-n Wilcoxon

## Show

#' @export
#' @rdname dfba_wilcoxon_method_small
#' @param object An object of class \code{\linkS4class{dfba_wilcoxon_small_out}}
setMethod("show", "dfba_wilcoxon_small_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Wilcoxon Signed-Rank Statistics", "\n")
  cat(" ", "n", "\n")
  cat(" ", object$n, "\n")
  cat(" ", "T_pos", "\n")
  cat(" ", object$T_pos, "\n")
  cat(" ", "T_neg", "\n")
  cat(" ", object$T_neg, "\n")
  cat("\n  Monte Carlo Sampling with Discrete Probability Values\n")
  cat("========================\n")
  cat(" ", "Number of MC Samples\n")
  cat(" ", object$samples, "\n")
  cat(" ", "\n  Posterior mean of phi_w:\n")
  cat(" ", object$phibar, "\n")
#  cat("equal-tail area interval")
#  cat(" ", object$prob_interval*100, "% interval limits:", "\n", sep="")
#  cat(" ", object$qLv, "\t\t\t", object$qHv, "\n")
  cat(" ", paste0(round(object$prob_interval*100), "% Highest-density interval limits:"), "\n")
  cat(" ",
      sprintf("%-12s", "Lower Limit"),
      "\t",
      "Upper Limit",
      "\n")
  cat(" ",
      sprintf("%-12g",
              object$hdi_lower),
      "\t",
      object$hdi_upper,
      "\n")
  cat(" ", "probability that phi_W exceeds 0.5:\n")
  cat(" ", sprintf("%-10s", "prior"), "\t", "posterior\n")
  cat(" ", sprintf("%-10g", object$priorprH1), "\t", object$prH1, "\n")
  cat("  Bayes factor BF10 for phi_W > 0.5:\n")
  cat(" ", ifelse(object$prH1==1|object$priorprH1==0,
                  paste0("Bayes factor BF10 for omega_E >.5 is estimated to be greater than: ", object$samples),
                  object$BF10), "\n")
})


# Plot
#' @export
#' @rdname dfba_wilcoxon_method_small
#' @param x An object of class \code{\linkS4class{dfba_wilcoxon_small_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
setMethod("plot",
          signature("dfba_wilcoxon_small_out"),
          function(x,
                   plot.prior=TRUE){
            x.data<-x$phiv
            y.predata<-x$priorvector
            y.postdata<-x$phipost
            xlab <- "phi_W"
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
          })

