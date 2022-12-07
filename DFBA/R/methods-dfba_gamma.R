#' Formatted output for dfba_gamma
#' @export
#' @rdname dfba_gamma_method
#' @param object An object of class \code{\linkS4class{dfba_gamma_out}}
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
  cat(" ", "Posterior Beta Shape Parameters for the Concordance Phi\n")
  cat(" ", "a.post", "\t", "b.post\n")
  cat(" ", object$alpha, "\t\t", object$beta, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median, "\n")
  cat(" ", object$interval.width*100, "% Equal-tail Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.eti.lower, "\t\t", object$post.eti.upper)
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
                         x$a.post,
                         x$b.post)
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
                          x$a.prior,
                          x$b.prior),
                    lty=2)  }
          })
