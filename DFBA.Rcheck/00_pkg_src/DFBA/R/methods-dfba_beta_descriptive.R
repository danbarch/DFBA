#' Formats for Beta Descriptive

# Show
#' @keywords internal
#' @export
#' @rdname dfba_beta_descriptive_method
#' @param object An object of class \code{\linkS4class{dfba_beta_descriptive_out}}
setMethod("show", "dfba_beta_descriptive_out", function(object) {
  cat("Centrality Estimates", "\n")
  cat("========================\n")
  cat(" ",
      sprintf("%-10s", "Mean"),
      "\t",
      sprintf("%-10s", "Median"),
      "\t",
      "Mode",
      "\n")
  cat(" ",
      sprintf("%-10g", object$x_mean),
      "\t",
      sprintf("%-10g", object$x_median),
      "\t",
      ifelse(is.na(object$x_mode),
             "NA*",
             object$x_mode),
      "\n",
      ifelse(is.na(object$x_mode),
             "Note: this beta distribution has no unique mode\n\n",
             "\n")
      )
  cat("Spread Estimate", "\n")
  cat("========================\n")
  cat(" ",
      sprintf("%-10s", "Variance"),
      "\n"
      )
  cat(" ",
      sprintf("%-10g", object$x_variance),
      "\n\n"
  )
#  cat(" ", "Mean","\t\t\t", "Median", "\t\t\t", "Mode", "\n")
#  cat(" ", object$x_mean, "\t\t", object$x_median, "\t\t", object$x_mode,
#      ifelse(is.na(object$x_mode), "Note: this beta distribution has no unique mode\n", "\n"))
  cat(" ", "Interval Estimates", "\n")
  cat("========================\n")
  cat(" ", paste0(round(object$prob_interval*100), "% Equal-tail interval limits:"), "\n")
  cat(" ",
      sprintf("%-12s", "Lower Limit"),
      "\t",
      "Upper Limit",
      "\n")
  cat(" ",
      sprintf("%-12g",
              object$eti_lower),
      "\t",
      object$eti_upper,
      "\n")
  cat(" ", paste0(round(object$prob_interval*100), "% Highest-density interval limits:"), "\n")
  cat(" ",
      sprintf("%-12s",
              "Lower Limit"),
      "\t",
      "Upper Limit",
      "\n")
  cat(" ",
      ifelse(is.na(object$hdi_lower),
             sprintf("%-12s", "NA*"),
             sprintf("%-12g", object$hdi_lower)
             ),
      "\t",
      ifelse(is.na(object$hdi_upper),
             "NA*",
             object$hdi_upper),
      "\n",
      ifelse(is.na(object$hdi_lower), "Note: this beta distribution has no defined highest-density interval\n", "\n"))
})

# Plot

#' @export
#' @rdname dfba_beta_descriptive_method
#' @param x An object of class \code{\linkS4class{dfba_beta_descriptive_out}}
setMethod("plot",
          signature("dfba_beta_descriptive_out"),
          function(x){
            par(mfrow = c(1, 2))
            plot(x = x$outputdf$x,
                 y = x$outputdf$density,
                 type="l",
                 xlab = "x",
                 ylab = "Probability Density")
            plot(x = x$outputdf$x,
                 y = x$outputdf$cumulative_prob,
                 type="l",
                 xlab = "x",
                 ylab = "Cumulative Probability")
            par(mfrow=c(1,1))
          })

