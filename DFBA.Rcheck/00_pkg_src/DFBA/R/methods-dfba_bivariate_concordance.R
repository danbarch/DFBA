#' Formatted output for dfba_bivariate_concordance
#'
#' @keywords internal
#' @export
#' @rdname dfba_bivariate_concordance_methods
#' @param object An object of class \code{\linkS4class{dfba_bivariate_concordance_out}}
setMethod("show", signature("dfba_bivariate_concordance_out"), function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", sprintf("%-16s", "Concordant Pairs"), "\t", "Discordant Pairs", "\n")
  cat(" ", sprintf("%-16g", object$nc), "\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample_p, "\n")
  cat("\nFrequentist Analyses\n")
  cat("========================\n")
  cat("  ", "Tau_A\n")
  cat("  ", object$tau, "\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Posterior Beta Shape Parameters for the Phi Concordance Measure\n")
  cat(" ", sprintf("%-10s", "a_post"), "\t", "b_post\n")
  cat(" ", sprintf("%-10g", object$a_post), "\t", object$b_post, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post_median, "\n")
  cat(" ", paste0(round(object$prob_interval*100), "% Equal-tail interval limits:"), "\n")
  cat(" ", sprintf("%-12s", "Lower Limit"), "\t", "Upper Limit", "\n")
  cat(" ", sprintf("%-12g", object$eti_lower), "\t", object$eti_upper, "\n")
})

# Formatted output for dfba_bivariate_concordance when fitting parameters are specified in options
#' @export
#' @rdname dfba_bivariate_concordance_methods
#' @param object An object of class \code{\linkS4class{dfba_bivariate_concordance_star_out}}
setMethod("show", "dfba_bivariate_concordance_star_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", sprintf("%-16s", "Concordant Pairs"), "\t", "Discordant Pairs", "\n")
  cat(" ", sprintf("%-16g", object$nc), "\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample_p, "\n")
  cat("\nFrequentist Analyses\n")
  cat("========================\n")
  cat("  ", "Tau_A point estimate\n")
  cat("  ", object$tau, "\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Posterior Beta Shape Parameters for the Phi Concordance Measure\n")
  cat(" ", sprintf("%-10s", "a_post"), "\t", "b_post\n")
  cat(" ", sprintf("%-10g", object$a_post), "\t", object$b_post, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post_median, "\n")
  cat(" ", paste0(round(object$prob_interval*100), "% Equal-tail interval limits:"), "\n")
  cat(" ", sprintf("%-12s", "Lower Limit"), "\t", "Upper Limit", "\n")
  cat(" ", sprintf("%-12g", object$eti_lower), "\t", object$eti_upper, "\n")
  cat("\nAdjusted for number of model-fitting parameters\n")
  cat("------------------------\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", sprintf("%-10s", "a_post"), "\t", "b_post\n")
  cat(" ", sprintf("%-10g", object$a_post_star), "\t", object$b_post_star, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post_median_star, "\n")
  cat(" ", paste0(round(object$prob_interval*100), "% Equal-tail interval limits:"), "\n")
  cat(" ", sprintf("%-12s", "Lower Limit"), "\t", "Upper Limit", "\n")
  cat(" ", sprintf("%-12g", object$eti_lower_star), "\t", object$eti_upper_star, "\n")
})


# Plot posterior and prior (optional) for dfba_bivariate_concordance
# To call plots, use plot(dfba_bivariate_concordance())
#' @export
#' @rdname dfba_bivariate_concordance_methods
#' @param x An object of class \code{\linkS4class{dfba_bivariate_concordance_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
setMethod("plot",
          signature("dfba_bivariate_concordance_out"),
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
                    lty=2)
            }

          })

# Plot posterior and prior (optional) for dfba_bivariate_concordance when fitting parameters are specified in options
# To call plots, use plot(dfba_bivariate_concordance())
#' @export
#' @rdname dfba_bivariate_concordance_methods
#' @param x An object of class \code{\linkS4class{dfba_bivariate_concordance_star_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
setMethod("plot",
          signature("dfba_bivariate_concordance_star_out"),
          function(x, plot.prior=TRUE){
            x.phi<-seq(0, 1, 1/1000)
            y.phi<-dbeta(x.phi,
                         x$a_post_star,
                         x$b_post_star)
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
