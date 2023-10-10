#' Format for Simulated Data Function

#' @keywords internal
#' @export
#' @rdname dfba_sim_data_method
#' @param object An object of class \code{\linkS4class{dfba_sim_data_out}}
setMethod("show", "dfba_sim_data_out", function(object) {
  cat("Frequentist p-value \n")
  cat("", object$pvalue, "\n")
  cat("Bayesian posterior probability \n")
  cat("", object$prH1, "\n")
})

## Sim Data Plot

#' @export
#' @rdname dfba_sim_data_method
#' @param x  An object of class \code{\linkS4class{dfba_sim_data_out}}
setMethod("plot",
          signature("dfba_sim_data_out"),
          function(x){
            if(x$design == "independent"){
              sim_data <- c(x$E,
                            x$C)
              group_labs <- c(rep("E",
                                  length(x$E)),
                              rep("C",
                                  length(x$C)))
              boxplot(sim_data~group_labs,
                      main=expression("Distributions of Simulated Data"),
                      xlab="Simulated Data Values",
                      ylab="Group",
                      horizontal = TRUE)
            }else{
              sim_data<-x$E - x$C
              group_labs <- rep("diff", length(x$E))
              boxplot(sim_data~group_labs,
                      main=expression("Distribution of Differences"),
                      xlab="Simulated Data Values",
                      ylab="Difference (E - C)",
                      horizontal = TRUE)
            }
          })
