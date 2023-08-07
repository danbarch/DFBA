#' Bayesian vs. t Power Methods
#'
# Show
#' @export
#' @rdname dfba_t_power_method
#' @param object An object of class \code{\linkS4class{dfba_t_power_out}}
setMethod("show", "dfba_t_power_out", function(object) {
  cat("Power results for the proportion of samples detecting effects"," ","\n")
  cat(" ", "where the variates are distributed as a",object$model,"random variable","\n")
  cat(" ", "and where the design is",object$design,"\n")
  cat(" ", "The number of Monte Carlo samples are:"," ","\n")
  cat(" ", object$nsims," ","\n")
  cat(" ", "Criterion for detecting an effect is"," ","\n")
  cat(" ", object$effect_crit," ","\n")
  cat(" ", "The delta offset parameter:"," ","\n")
  cat(" ", object$deltav," ","\n")
  cat("Output Results:", "\n")
  print(object$outputdf,
        row.names = FALSE)
})

# Plot

#' @export
#' @rdname dfba_t_power_method
#' @param x An object of class \code{\linkS4class{dfba_t_power_out}}
setMethod("plot",
          signature("dfba_t_power_out"),
          function(x){
            plot(x$outputdf$sample_size,
                 x$outputdf$Bayes_power,
                 type="b",
                 lty = 1,
                 ylim=c(0,1),
                 main=expression(cdots~"Frequentist"~ - "Bayesian"),
                 xlab="Sample Size",
                 ylab="Power Estimate")
            lines(x$outputdf$sample_size,
                  x$outputdf$t_power,
                  type="b",
                  lty=3)
          })
