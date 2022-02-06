#' Methods for DFBA
#'
#' @param object list output from phi function
#' @importFrom stats dbeta


# Formatted output for dfba_phi

#' @export
setMethod("show", "dfba_phi_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Concordant Pairs", "\t", "Discordant Pairs", "\n")
  cat(" ", object$nc, "\t\t\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample.p, "\n")
  cat("\nFrequentist Analyses\n")
  cat("========================\n")
  cat("  ", "Tau point estimate\n")
  cat("  ", object$tau, "\n")
  cat(" ", object$interval.width*100, "% Confidence Interval", "\n", sep="")
  cat(" ", "CI to be added\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t\t", "Beta\n")
  cat(" ", object$alpha, "\t\t", object$beta, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median, "\n")
  cat(" ", object$interval.width*100, "% Equal-tail Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.eti.lower, "\t\t", object$post.eti.upper, "\n")
})

# Formatted output for dfba_phi when fitting parameters are specified in options
#' @export
setMethod("show", "dfba_phi_star_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Concordant Pairs", "\t", "Discordant Pairs", "\n")
  cat(" ", object$nc, "\t\t\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample.p, "\n")
  cat("\nFrequentist Analyses\n")
  cat("========================\n")
  cat("  ", "Tau point estimate\n")
  cat("  ", object$tau, "\n")
  cat(" ", object$interval.width*100, "% Confidence Interval", "\n", sep="")
  cat(" ", "CI to be added\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t\t", "Beta\n")
  cat(" ", object$alpha, "\t\t", object$beta, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median, "\n")
  cat(" ", object$interval.width*100, "% Equal-tail Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.eti.lower, "\t\t", object$post.eti.upper, "\n")
  cat("\nAdjusted for number of model-fitting parameters\n")
  cat("------------------------\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t\t", "Beta\n")
  cat(" ", object$alpha_star, "\t\t", object$beta_star, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median_star, "\n")
  cat(" ", object$interval.width*100, "% Equal-tail Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.eti.lower_star, "\t\t", object$post.eti.upper_star, "\n")
})

# Plot posterior and prior (optional) for dfba_phi
# To call plots, use plot(dfba_phi())
#' @export
setMethod("plot",
          signature("dfba_phi_out"),
          function(x, plot.prior=FALSE){
            dfba_plot_beta(x$alpha,
                           x$beta,
                           x$a.prior,
                           x$b.prior,
                           plot.prior)
          })

# Plot posterior and prior (optional) for dfba_phi when
# fitting parameters are specified in options
# To call plots, use plot(dfba_phi())
#' @export
setMethod("plot",
          signature("dfba_phi_star_out"),
          function(x, plot.prior=FALSE){
            dfba_plot_beta(x$alpha_star,
                           x$beta_star,
                           x$a.prior,
                           x$b.prior,
                           plot.prior)
          })


#' @export
setMethod("show", "dfba_gamma_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Concordant Pairs", "\t", "Discordant Pairs", "\n")
  cat(" ", object$nc, "\t\t\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample.p, "\n")
  cat(" ", "Goodman-Kruskal Gamma\n")
  cat(" ", object$gamma, "\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t", "Beta\n")
  cat(" ", object$alpha, "\t\t", object$beta, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median, "\n")
  cat(" ", object$interval.width*100, "% Equal-tail Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.eti.lower, "\t\t", object$post.eti.upper)
})



#' @export
setMethod("plot",
          signature("dfba_gamma_out"),
          function(x, plot.prior=FALSE){
            dfba_plot_beta(x$alpha,
                           x$beta,
                           x$a.prior,
                           x$b.prior,
                           plot.prior)
          })


