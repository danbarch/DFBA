#' Formats for Bayesian Binomial Test

# Show

#' @export
#' @rdname dfba_binomial_method
#' @param object An object of class \code{\linkS4class{dfba_binomial_out}}
setMethod("show", "dfba_binomial_out", function(object) {
  cat("Estimate of the Binomial Population Rate Parameter", "\n")
  cat("========================\n")
  cat(" ", "Prior Beta Shape Parameters:","\n")
  cat(" ", "a0", "\t\t\t", "b0", "\n")
  cat(" ", object$a0,"\t\t\t", object$b0,"\n")
  cat("Posterior Beta Shape Parameters are :"," ","\n")
  cat("post.a","\t\t\t","post.b","\n")
  cat(object$post.a,"\t\t\t", object$post.b,"\n")
})

# Plot

#' @export
#' @rdname dfba_binomial_method
#' @param x An object of class \code{\linkS4class{dfba_binomial_out}}
#' @param plot.prior Show prior distribution (default = TRUE)
setMethod("plot",
          signature("dfba_binomial_out"),
          function(x,
                   plot.prior=TRUE){
            x.data<-seq(0, 1, 1/1000)
            y.predata<-dbeta(x.data, x$a0, x$b0)
            y.postdata<-dbeta(x.data, x$a.post, x$b.post)
            xlab="phi"
            ylab="Probability Density"

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
