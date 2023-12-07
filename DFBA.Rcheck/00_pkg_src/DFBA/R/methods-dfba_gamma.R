#' Formatted output for dfba_gamma

#' @keywords internal
#' @export
#' @rdname dfba_gamma_method
#' @param object An object of class \code{\linkS4class{dfba_gamma_out}}
setMethod("show", "dfba_gamma_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", sprintf("%-16s", "Concordant Pairs"), "\t", "Discordant Pairs", "\n")
  cat(" ", sprintf("%-16g", object$nc), "\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample_p, "\n")
  cat(" ", "Goodman-Kruskal Gamma\n")
  cat(" ", object$gamma, "\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Posterior Beta Shape Parameters for the Concordance Phi\n")
  cat(" ", sprintf("%-10s", "a"), "\t", "b\n")
  cat(" ", sprintf("%-10g", object$a_post), "\t", object$b_post, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post_median, "\n")
  cat(" ", paste0(round(object$prob_interval*100), "% Equal-tail interval limits:"), "\n")
  cat(" ", sprintf("%-12s", "Lower Limit"), "\t", "Upper Limit", "\n")
  cat(" ", sprintf("%-12g", object$eti_lower), "\t", object$eti_upper, "\n")
})


# Plot method for gamma
#' @rdname dfba_gamma_method
#' @export
#' @param x An object of class \code{\linkS4class{dfba_gamma_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
setMethod("plot",
          signature("dfba_gamma_out"),
          function(x, plot.prior=TRUE){
            x.phi<-seq(0, 1, 1/1000)
            y.phi<-dbeta(x.phi,
                         x$a_post,
                         x$b_post)
            if (plot.prior==FALSE){
              plot(x.phi,
                   y.phi,
                   type="l",
                   xlab="Phi",
                   ylab="Probability Density")
            } else {
              plot(x.phi,
                   y.phi,
                   type="l",
                   xlab="Phi",
                   ylab="Probability Density",
                   main=expression("--"~"Prior"~ - "Posterior")
              )
              lines(x.phi,
                    dbeta(x.phi,
                          x$a0,
                          x$b0),
                    lty=2)  }
          })
