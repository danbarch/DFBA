#' Formats for Bayesian Binomial Test

# Show

#' @keywords internal
#' @export
#' @rdname dfba_binomial_method
#' @param object An object of class \code{\linkS4class{dfba_binomial_out}}
#' @return No return value, called for side effect. Objects of class \code{\linkS4class{dfba_binomial_out}} are printed.
setMethod("show", "dfba_binomial_out", function(object) {
  cat("Prior and Posterior Beta Shape Parameters:","\n")
  cat("========================\n")
  cat(" ", "Prior Beta Shape Parameters","\n")
  cat(" ", sprintf("%-10s", "a0"), "\t", "b0", "\n")
  cat(" ", sprintf("%-10g", object$a0), "\t", object$b0, "\n")
  cat(" ", "Posterior Beta Shape Parameters:"," ","\n")
  cat(" ", sprintf("%-10s", "a_post"), "\t", "b_post", "\n")
  cat(" ", sprintf("%-10g", object$a_post), "\t", object$b_post, "\n")
  cat("Estimates of the Binomial Population Rate Parameter", "\n")
  cat("========================\n")
  cat(" ", "Posterior Mean", "\n")
  cat(" ", object$phimean, "\n")
  cat(" ", "Posterior Median", "\n")
  cat(" ", object$phimedian, "\n")
  cat(" ", "Posterior Mode", "\n")
  cat(" ", object$phimode, "\n")
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
})

# Plot

#' @export
#' @rdname dfba_binomial_method
#' @param x An object of class \code{\linkS4class{dfba_binomial_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
#' @return No return value, called for side effect. Method produces a plot of class \code{\linkS4class{dfba_binomial_out}}
setMethod("plot",
          signature("dfba_binomial_out"),
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
