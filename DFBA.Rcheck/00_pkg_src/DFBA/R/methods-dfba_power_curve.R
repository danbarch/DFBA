#' Formats for power curve
#'
#' @keywords internal
#' @export
#' @rdname dfba_power_curve_method
#' @param object An object of class \code{\linkS4class{dfba_power_curve_out}}
setMethod("show", "dfba_power_curve_out", function(object) {
  cat("Power results for the proportion of samples detecting effects"," ","\n")
  cat(" ", "where the variates are distributed as a",object$model,"random variable","\n")
  cat(" ", "and where the design is",object$design,"\n")
  if(object$design=="paired"){cat(" ", "with a blocking max of ",object$block_max,"\n")}
  cat(" ", "The number of Monte Carlo samples are:"," ","\n")
  cat(" ", object$nsims," ","\n")
  cat(" ", "Criterion for detecting an effect is"," ","\n")
  cat(" ", object$effect_crit," ","\n")
  cat("The n value per condition:"," ","\n")
  cat(object$n,"  ","\n")
  cat("Output Results:", "\n")
  print(object$outputdf)
})

#' @export
#' @rdname dfba_power_curve_method
#' @param x An object of class \code{\linkS4class{dfba_power_curve_out}}
setMethod("plot",
          signature("dfba_power_curve_out"),
          function(x){
            plot(x$outputdf$delta_value,
                 x$outputdf$Bayes_power,
                 type="b",
                 lty = 1,
                 ylim=c(0,1),
                 main=expression(cdots~"Frequentist"~ - "Bayesian"),
                 xlab="Delta",
                 ylab="Power Estimate")
            lines(x$outputdf$delta_value,
                  x$outputdf$t_power,
                  type="b",
                  lty=3)
          })
