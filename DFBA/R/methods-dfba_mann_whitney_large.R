#' Formats for large-n Mann Whitney

## Show

#' @export
#' @rdname dfba_mann_whitney_large_method
#' @param object An object of class \code{\linkS4class{dfba_mann_whitney_large_out}}
setMethod("show", "dfba_mann_whitney_large_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", sprintf("%-10s", "n_E"), "\t", "n_C", "\n")
  cat(" ", sprintf("%-10g", object$n_E), "\t", object$n_C, "\n")
  cat(" ", sprintf("%-10s", "E mean"), "\t", "C mean", "\n")
  cat(" ", sprintf("%-10g", object$Emean), "\t", object$Cmean, "\n")
  cat(" ", "Mann-Whitney Statistics", "\n")
  cat(" ", sprintf("%-10s", "U_E"), "\t", "U_C", "\n")
  cat(" ", sprintf("%-10g", object$U_E), "\t", object$U_C, "\n")
  cat("\n Beta Approximation Model for Omega_E\n")
  cat(" for 2*nE*nC/(nE+nC) > 19\n")
  cat("========================\n")
  cat(" ", "Posterior beta shape parameters:\n")
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

 # cat(" ", "probability within interval:\n")
 #  cat(" ", round(object$prob_interval*100), " percent\n")
 #  cat(" ", "equal-tail limit values are:\n")
 #  cat(" ", object$qlequal, "\t\t\t", object$qhequal, "\n")
 #  cat(" ", "highest-density limits are:\n")
 #  cat(" ", object$qLmin, "\t\t\t", object$qHmax, "\n")
  cat(" ", "probability that omega_E > 0.5:\n")
  cat(" ", sprintf("%-10s", "prior"), "\t", "posterior\n")
  cat(" ", sprintf("%-10g", object$priorprH1), "\t", object$prH1, "\n")
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
            y.postdata<-dbeta(x.data, x$a_post, x$b_post)
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
