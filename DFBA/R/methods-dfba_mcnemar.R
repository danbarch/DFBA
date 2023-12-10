#' Format for Bayesian McNemar Test
# Show
#' @keywords internal
#' @export
#' @rdname dfba_mcnemar_method
#' @param object An object of class \code{\linkS4class{dfba_mcnemar_out}}
#' @return No return value, called for side effect. Objects of class \code{\linkS4class{dfba_mcnemar_out}} are printed.
setMethod("show", "dfba_mcnemar_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Frequencies of a change in 0/1 response between the two tests\n")
  cat(" ", "0 to 1 shift", "\t\t", "1 to 0 shift", "\n")
  cat(" ", sprintf("%-12g", object$n_01), "\t\t", object$n_10, "\n")
  cat("\n  Bayesian Analysis\n")
  cat("========================\n")
  cat(" ", "Posterior Beta Shape Parameters for Phi_rb\n")
  cat(" ", sprintf("%-11s", "a_post"), "\t", "b_post", "\n")
  cat(" ", sprintf("%-11g", object$a_post), "\t", object$b_post, "\n")
  cat(" ", "Posterior Point Estimates for Phi_rb\n")
  cat(" ", sprintf("%-11s", "Mean"), "\t", "Median", "\n")
  cat(" ", sprintf("%-11g", object$post_mean), "\t", object$post_median, "\n")
  cat(" ", paste0("Equal-tail ", round(object$prob_interval*100), "% Probability Interval"), "\n")
  cat(" ", sprintf("%-11s", "Lower Limit"), "\t", "Upper Limit", "\n")
  cat(" ", sprintf("%-11g", object$eti_lower), "\t", object$eti_upper, "\n")
  cat(" ", "Point Bayes factor against null of phi_rb = .5:\n")
  cat(" ", object$BF10point, "\n")
  cat(" ", "Interval Bayes factor against the null that phi_rb less than or equal to .5:\n")
  cat(" ", object$BF10interval, "\n")
  cat(" ", "Posterior Probability that Phi_rb > .5:\n")
  cat(" ", object$postH1, "\n")
})

# Plot

#' @export
#' @rdname dfba_mcnemar_method
#' @param x An object of class \code{\linkS4class{dfba_mcnemar_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
#' @return No return value, called for side effect. Method produces a plot of class \code{\linkS4class{dfba_mcnemar_out}}
setMethod("plot",
          signature("dfba_mcnemar_out"),
          function(x,
                   plot.prior=TRUE){
            x.data<-seq(0, 1, 1/1000)
            y.predata<-dbeta(x.data, x$a0, x$b0)
            y.postdata<-dbeta(x.data, x$a_post, x$b_post)
            xlab <- "phi_rb"
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
