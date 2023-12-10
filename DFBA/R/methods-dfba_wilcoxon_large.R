#' Formats for large-n Wilcoxon

## Show

#' @keywords internal
#' @export
#' @rdname dfba_wilcoxon_method_large
#' @param object An object of class \code{\linkS4class{dfba_wilcoxon_large_out}}
#' @return No return value, called for side effect. Objects of class \code{\linkS4class{dfba_wilcoxon_large_out}} are printed.
setMethod("show", "dfba_wilcoxon_large_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Wilcoxon Signed-Rank Statistics", "\n")
  cat(" ", "n", "\n")
  cat(" ", object$n, "\n")
  cat(" ", "T_pos", "\n")
  cat(" ", object$T_pos, "\n")
  cat(" ", "T_neg", "\n")
  cat(" ", object$T_neg, "\n")
  cat("\n  Beta Approximation Model for Phi_W\n")
  cat(" for n > 24\n")
  cat("========================\n")
  cat(" ", "The posterior beta shape parameters are:\n")
  cat(" ", sprintf("%-14s", "posterior a"), "\t", "posterior b\n")
  cat(" ", sprintf("%-14g", object$a_post), "\t", object$b_post, "\n")
  cat(" ", sprintf("%-14s", "posterior mean"), "\t", "posterior median\n")
  cat(" ", sprintf("%-14g", object$post_mean), "\t", object$post_median, "\n")
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
  cat(" ", paste0(round(object$prob_interval*100), "% Highest-density interval limits:"), "\n")
  cat(" ",
      sprintf("%-12s",
              "Lower Limit"),
      "\t",
      "Upper Limit",
      "\n")
  cat(" ",
      ifelse(is.na(object$hdi_lower),
             sprintf("%-12s", "NA*"),
             sprintf("%-12g", object$hdi_lower)
      ),
      "\t",
      ifelse(is.na(object$hdi_upper),
             "NA*",
             object$hdi_upper),
      "\n\n",
      ifelse(is.na(object$hdi_lower), "Note: this beta distribution has no defined highest-density interval\n
             ", "\n"))
  cat(" ", "probability that phi_W > 0.5:\n")
  cat(" ", sprintf("%-10s", "prior"), "\t", "posterior\n")
  cat(" ", sprintf("%-10g", object$priorprH1), "\t", object$prH1, "\n")
  cat(" ", "Bayes factor BF10 for phi_W > 0.5:\n")
  cat(" ", ifelse(object$prH1==1|object$priorprH1==0,
                  "BF10 approaches Infinity",
                  object$BF10), "\n")
})

# Plot

#' @export
#' @rdname dfba_wilcoxon_method_large
#' @param x An object of class \code{\linkS4class{dfba_wilcoxon_large_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
#' @return No return value, called for side effect. Method produces a plot of class \code{\linkS4class{dfba_wilcoxon_large_out}}
setMethod("plot",
          signature("dfba_wilcoxon_large_out"),
          function(x,
                   plot.prior=TRUE){
            x.data<-seq(0, 1, 1/1000)
            y.predata<-dbeta(x.data, x$a0, x$b0)
            y.postdata<-dbeta(x.data, x$a_post, x$b_post)
            xlab <- "phi_W"
            ylab <- "Probability Density"

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
