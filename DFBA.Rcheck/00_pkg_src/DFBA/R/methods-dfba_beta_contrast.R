#' Formats for Beta Contrasts

#' @export
#' @rdname dfba_beta_contrast_method
#' @param object An object of class \code{\linkS4class{dfba_beta_contrast_out}}
setMethod("show", "dfba_beta_contrast_out", function(object) {
  cat("Bayesian Contrasts \n")
  cat("========================\n")
  cat(" ", "Contrast Weights", "\n")
  cat(" ", object$contrast_vec, "\n")
  cat(" ", "Exact posterior contrast mean", "\n")
  cat(" ", object$mean, "\n")
  cat(" ", "Monte Carlo sampling results", "\n")
  cat(" ", "Number of Monte Carlo Samples", "\n")
  cat(" ", object$samples, "\n")
  cat(" ", paste0("Equal-tail ", round(object$prob_interval*100), "% Probability Interval"), "\n")
  cat(" ", "Lower Limit", "\t\t\t", "Upper Limit", "\n")
  cat(" ", object$lower_limit, "\t\t\t", object$upper_limit, "\n")
  cat(" ", "Posterior Probability that Contrast is Positive", "\n")
  cat(" ", object$prob_positive_delta, "\n")
  cat(" ", "Prior Probability that Contrast is Positive", "\n")
  cat(" ", object$prior_positive_delta, "\n")
  cat(" ", "Bayes Factor Estimate that Contrast is Positive", "\n")
  cat(ifelse(object$prob_positive_delta==1|object$prior_positive_delta==0,
             " Estimated to be greater than ",
             " "),
      object$bayes_factor, "\n")
})

### Beta Contrasts Plot

#' @export
#' @rdname dfba_beta_contrast_method
#' @param x object An object of class \code{\linkS4class{dfba_beta_contrast_out}}
setMethod("plot",
          signature("dfba_beta_contrast_out"),
          function(x){
            x.data<-x$delta_quantiles
            y.data<-seq(0, 1, 0.01)
            xlab="contrast value"
            ylab="posterior cumulative probability"

            plot(x.data,
                 y.data,
                 type = "l",
                 xlab = xlab,
                 ylab = ylab,
                 main = "Based on Monte Carlo Sampling")
          })
