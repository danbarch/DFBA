#' Formats for Point Bayes Factor

# Show
#' @keywords internal
#' @export
#' @rdname dfba_point_BF_show_method
#' @param object An object of class \code{\linkS4class{dfba_point_BF_out}}
setMethod("show", "dfba_point_BF_out", function(object) {
  cat("Bayes Factor for Point Estimates \n")
  cat("========================\n")
  cat(" ", "Point Null Hypothesis", "\n")
  cat(" ", object$null_hypothesis, "\n")
  cat(" ", "Shape Parameters for Prior Beta Distribution", "\n")
  cat(" ", sprintf("%-10s", "a0"), "\t", "b0", "\n")
  cat(" ", sprintf("%-10g", object$a0), "\t", object$b0, "\n")
  cat(" ", "Shape Parameters for Posterior Beta Distribution", "\n")
  cat(" ", sprintf("%-10s", "a_post"), "\t", "b_post", "\n")
  cat(" ", sprintf("%-10g", object$a_post), "\t", object$b_post, "\n")
  cat(" ", "Prior Probability Density for Null Hypothesis", "\n")
  cat(" ", object$dpriorH0, "\n")
  cat(" ", "Posterior Probability Density for Null Hypothesis", "\n")
  cat(" ", object$dpostH0, "\n")
  cat(" ", "Bayes Factor Estimate for the Alternative over the Null Hypothesis", "\n")
  cat(" ", object$BF10, "\n")
  cat(" ", "Bayes Factor Estimate for the Null over the Alternative Hypothesis", "\n")
  cat(" ", object$BF01, "\n")
})
