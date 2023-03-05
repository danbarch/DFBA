#' Formats for Bayesian Median Test Output

# Show

#' @export
#' @rdname dfba_median_test_method
#' @param object An object of class \code{\linkS4class{dfba_median_test_out}}
setMethod("show", "dfba_median_test_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Observed frequencies:", "\n")
  cat(" ", "E","\t\t\t","C","\n")
  cat(" ", object$nE,"\t\t\t", object$nC,  "\n")
  cat(" ", "Overall median:", "\n")
  cat(" ", object$median, "\n")
  cat(" ", "Frequencies above the median:", "\n")
  cat(" ", "E","\t\t\t","C","\n")
  cat(" ", object$nEabove,"\t\t\t", object$nCabove,  "\n")
  cat(" ", "Frequencies at or below the median:", "\n")
  cat(" ", "E","\t\t\t","C","\n")
  cat(" ", object$nEbelow,"\t\t\t", object$nCbelow,  "\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Base rates for E and C responses:\n")
  cat(" ", "E", "\t\t\t", "C\n")
  cat(" ", object$Ebaserate, "\t\t", object$Cbaserate, "\n")
  cat(" ", "Analysis of above-median response rates for E and C:\n")
  cat(" ", "Posterior beta shape parameter for the phi parameter","\n")
  cat(" ",  "a.post", "\t\t", "b.post","\n")
  cat(" ", object$a.post, "\t\t\t", object$b.post,"\n")
  cat("Prior probability of exceeding base rate:", "\n")
  cat(" ", "E", "\t\t\t", "C", "\n")
  cat(" ", object$priorEhi,"\t\t\t", object$priorChi,"\n")
  cat("Posterior probability of exceeding base rate:", "\n")
  cat(" ", "E", "\t\t\t", "C", "\n")
  cat(" ", object$postEhi,"\t\t\t", object$postChi,"\n")
  cat(" ", "Bayes factor BF10 E > E_baserate:","\n")
  cat(" ", object$BF10E, "\n")
  cat(" ",  "Bayes factor BF10 C > C_baserate", "\n")
  cat(" ", object$BF10C, "\n")
})
