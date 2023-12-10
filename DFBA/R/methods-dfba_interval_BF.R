#' Formats for Interval Bayes Factor

# Show
#' @keywords internal
#' @export
#' @rdname dfba_interval_BF_method
#' @param object An object of class \code{\linkS4class{dfba_interval_BF_out}}
#' @return No return value, called for side effect. Objects of class \code{\linkS4class{dfba_interval_BF_out}} are printed.
setMethod("show", "dfba_interval_BF_out", function(object){
  cat("Bayes Factor for Interval Estimates \n")
  cat("========================\n")
  cat(" ", "Interval Null Hypothesis", "\n")
  cat(" ", sprintf("%-11s", "Lower Limit"), "\t", "Upper Limit", "\n")
  cat(" ", sprintf("%-11g", object$H0lower),"\t", object$H0upper, "\n")
  cat(" ", "Shape Parameters for Prior Beta Distribution", "\n")
  cat(" ", sprintf("%-11s", "a0"), "\t", "b0", "\n")
  cat(" ", sprintf("%-11g", object$a0), "\t", object$b0, "\n")
  cat(" ", "Shape Parameters for Posterior Beta Distribution", "\n")
  cat(" ", sprintf("%-11s", "a_post"), "\t", "b_post", "\n")
  cat(" ", sprintf("%-11g", object$a_post), "\t", object$b_post, "\n")
  cat(" ", "Prior Probability for Null Hypothesis", "\n")
  cat(" ", object$pH0, "\n")
  cat(" ", "Posterior Probability for Null Hypothesis", "\n")
  cat(" ", object$postH0, "\n")
  cat(" ", "Prior Probability for Alternative Hypothesis", "\n")
  cat(" ", object$pH1, "\n")
  cat(" ", "Posterior Probability for Alternative Hypothesis", "\n")
  cat(" ", object$postH1, "\n")
  cat(" ", "Bayes Factor Estimate for the Alternative over the Null Hypothesis", "\n")
  cat(" ", object$BF10, "\n")
  cat(" ", "Bayes Factor Estimate for the Null over the Alternative Hypothesis", "\n")
  cat(" ", object$BF01, "\n")
})
