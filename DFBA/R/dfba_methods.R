#' Methods for DFBA
#'
#' @param object list output from phi function
#' @importFrom stats dbeta

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
  cat("  ", "Tau value", "\t\t", "p-value", "\n")
  cat("  ", object$tau, "\t\t\t", "to be added", "\n")
  cat(" ", object$hdi.width*100, "% Confidence Interval", "\n", sep="")
  cat(" ", "CI to be added\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t\t", "Beta\n")
  cat(" ", object$alpha, "\t\t", object$beta, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median, "\n")
  cat(" ", object$hdi.width*100, "% Highest Density Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.hdi.lower, "\t\t", object$post.hdi.upper)
})

#' @export
setMethod("plot",
          signature("dfba_phi_out"),
          function(x){
            x.vals=seq(0, 1, 1/1000)
            plot(x.vals, dbeta(x.vals, x$alpha, x$beta), type="l")
          })

#' @export
setMethod("show", "dfba_gamma_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Concordant Pairs", "\t", "Discordant Pairs", "\n")
  cat(" ", object$nc, "\t\t\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample.p, "\n")
  cat("\nFrequentist Analyses\n")
  cat("========================\n")
  cat("  ", "Gamma value", "\t\t", "p-value", "\n")
  cat("  ", object$gamma, "\t\t\t", "to be added", "\n")
  cat(" ", object$hdi.width*100, "% Confidence Interval", "\n", sep="")
  cat(" ", "CI to be added\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t", "Beta\n")
  cat(" ", object$alpha, "\t\t", object$beta, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median, "\n")
  cat(" ", object$hdi.width*100, "% Highest Density Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.hdi.lower, "\t\t", object$post.hdi.upper)
})



#' @export
setMethod("plot",
          signature("dfba_gamma_out"),
          function(x, plot.prior=FALSE){
            dfba_plot_beta(x$alpha, x$beta, x$a.prior, x$b.prior, plot.prior)
          })


