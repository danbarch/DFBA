#' Formats for Bayesian Sign Test

# Show
#' @keywords internal
#' @export
#' @rdname dfba_sign_test_method
#' @param object An object of class \code{\linkS4class{dfba_sign_test_out}}
#' @return No return value, called for side effect. Objects of class \code{\linkS4class{dfba_sign_test_out}} are printed.
setMethod("show", "dfba_sign_test_out", function(object) {
  cat("Analysis of the Signs of the Y1 - Y2 Differences", "\n")
  cat("========================\n")
  cat(" ", sprintf("%-20s", "Positive Differences"),"\t", "Negative Differences","\n")
  cat(" ", sprintf("%-20g", object$n_pos), "\t\t\t", object$n_neg,"\n")
  cat("Analysis of the Positive Sign Rate"," ","\n")
  cat("========================\n")
  cat(" ", "Posterior Mean", "\n")
  cat(" ", object$phimean, "\n")
  cat(" ", "Posterior Median", "\n")
  cat(" ", object$phimedian, "\n")
  cat(" ", "Posterior Mode", "\n")
  cat(" ", object$phimode, "\n\n")
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
  cat(" ", sprintf("%-17s", "Prior Probability"), "\t", "Posterior Probability"," ","\n")
  cat(" ", sprintf("%-17g", object$prior_H1), "\t", object$post_H1,"\n")
  cat(" ", "Bayes Factors for Pos. Rate > .5","\n")
  cat(" ", sprintf("%-10s", "BF10"),"\t", "BF01","\n")
  cat(" ", sprintf("%-10g", object$BF10), "\t", object$BF01,"\n")
})

# plot

#' @export
#' @rdname dfba_sign_test_method
#' @param x An object of class \code{\linkS4class{dfba_sign_test_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
#' @return No return value, called for side effect. Method produces a plot of class \code{\linkS4class{dfba_sign_test_out}}
setMethod("plot",
          signature("dfba_sign_test_out"),
          function(x,
                   plot.prior=TRUE){
            x.data<-seq(0, 1, 1/1000)
            y.predata<-dbeta(x.data, x$a0, x$b0)
            y.postdata<-dbeta(x.data, x$a_post, x$b_post)
            xlab <- "phi"
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
