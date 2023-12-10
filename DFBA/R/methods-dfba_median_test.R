#' Formats for Bayesian Median Test Output

# Show

#' @keywords internal
#' @export
#' @rdname dfba_median_test_method
#' @param object An object of class \code{\linkS4class{dfba_median_test_out}}
#' @return No return value, called for side effect. Objects of class \code{\linkS4class{dfba_median_test_out}} are printed.
setMethod("show", "dfba_median_test_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Observed frequencies:", "\n")
  cat(" ", sprintf("%-10s", "E"),"\t","C","\n")
  cat(" ", sprintf("%-10g", object$nE),"\t", object$nC,  "\n")
  cat(" ", "Overall median:", "\n")
  cat(" ", object$median, "\n")
  cat(" ", "Frequencies above the median:", "\n")
  cat(" ", sprintf("%-10s", "E"),"\t","C","\n")
  cat(" ", sprintf("%-10g", object$nEabove),"\t", object$nCabove,  "\n")
  cat(" ", "Frequencies at or below the median:", "\n")
  cat(" ", sprintf("%-10s", "E"),"\t","C","\n")
  cat(" ", sprintf("%-10g", object$nEbelow),"\t", object$nCbelow,  "\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Base rates for E and C responses:\n")
  cat(" ", sprintf("%-10s", "E"), "\t", "C\n")
  cat(" ", sprintf("%-10g", object$Ebaserate), "\t", object$Cbaserate, "\n")
  cat(" ", "Analysis of above-median response rates for E and C:\n")
  cat(" ", "Posterior beta shape parameter for the phi parameter","\n")
  cat(" ",  sprintf("%-10s", "a_post"), "\t", "b_post","\n")
  cat(" ", sprintf("%-10g", object$a_post), "\t", object$b_post,"\n")
  cat(" ",  "Prior probability of exceeding base rate:", "\n")
  cat(" ", sprintf("%-10s", "E"), "\t", "C", "\n")
  cat(" ", sprintf("%-10g", object$priorEhi),"\t", object$priorChi,"\n")
  cat(" ", "Posterior probability of exceeding base rate:", "\n")
  cat(" ", sprintf("%-10s", "E"), "\t", "C", "\n")
  cat(" ", sprintf("%-10g", object$postEhi),"\t", object$postChi,"\n")
  cat(" ", "Bayes factor BF10 E > E_baserate:","\n")
  cat(" ", object$BF10E, "\n")
  cat(" ",  "Bayes factor BF10 C > C_baserate", "\n")
  cat(" ", object$BF10C, "\n")
})
