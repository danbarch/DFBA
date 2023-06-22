#' Formats for Bayesian Sign Test

# Show

#' @export
#' @rdname dfba_sign_test_method
#' @param object An object of class \code{\linkS4class{dfba_sign_test_out}}
setMethod("show", "dfba_sign_test_out", function(object) {
  cat("Analysis of the Signs of the Y1 - Y2 Differences", "\n")
  cat("========================\n")
  cat(" ", "Positive Differences","\t", "Negative Differences","\n")
  cat(" ", object$n_pos, "\t\t\t", object$n_neg,"\n")
  cat("Analysis of the Positive Sign Rate"," ","\n")
  cat("========================\n")
  cat(" ", "Posterior Mean", "\n")
  cat(" ", object$phimean, "\n")
  cat(" ", "Posterior Median", "\n")
  cat(" ", object$phimedian, "\n")
  cat(" ", "Posterior Mode", "\n")
  cat(" ", object$phimode, "\n")
  cat("Probability within", round(object$prob_interval*100), "percent interval\n")
  cat("========================\n")
  cat(" ", "equal-tail limit values:\n")
  cat(" ", object$eti_lower, "\t\t\t", object$eti_upper, "\n")
  cat(" ", "highest-density limits:\n")
  cat(" ", object$hdi_lower, "\t\t\t", object$hdi_upper, "\n")
  cat(" ", "Prior Probability", "\t", "Posterior Probability"," ","\n")
  cat(" ", object$prior_H1,"\t\t\t", object$post_H1,"\n")
  cat(" ", "Bayes Factors for Pos. Rate > .5","\n")
  cat(" ", "BF10","\t\t\t", "BF01","\n")
  cat(" ", object$BF10, "\t\t", object$BF01,"\n")
})

# plot

#' @export
#' @rdname dfba_sign_test_method
#' @param x An object of class \code{\linkS4class{dfba_sign_test_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
setMethod("plot",
          signature("dfba_sign_test_out"),
          function(x,
                   plot.prior=TRUE){
            x.data<-seq(0, 1, 1/1000)
            y.predata<-dbeta(x.data, x$a0, x$b0)
            y.postdata<-dbeta(x.data, x$a.post, x$b.post)
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
