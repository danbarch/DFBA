#' Methods for DFBA
#'
#' @param object list output from phi function
#' @importFrom stats dbeta
#'


# Formatted output for dfba_phi

#' @export
setMethod("show", "dfba_phi_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Concordant Pairs", "\t", "Discordant Pairs", "\n")
  cat(" ", object$nc, "\t\t\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample.p, "\n")
  cat("\nFrequentist Analyses\n")
  cat("========================\n")
  cat("  ", "Tau point estimate\n")
  cat("  ", object$tau, "\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t\t", "Beta\n")
  cat(" ", object$alpha, "\t\t", object$beta, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median, "\n")
  cat(" ", object$interval.width*100, "% Equal-tail Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.eti.lower, "\t\t", object$post.eti.upper, "\n")
})

# Formatted output for dfba_phi when fitting parameters are specified in options
#' @export
setMethod("show", "dfba_phi_star_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Concordant Pairs", "\t", "Discordant Pairs", "\n")
  cat(" ", object$nc, "\t\t\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample.p, "\n")
  cat("\nFrequentist Analyses\n")
  cat("========================\n")
  cat("  ", "Tau point estimate\n")
  cat("  ", object$tau, "\n")
  cat(" ", object$interval.width*100, "% Confidence Interval", "\n", sep="")
  cat(" ", "CI to be added\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t\t", "Beta\n")
  cat(" ", object$alpha, "\t\t", object$beta, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median, "\n")
  cat(" ", object$interval.width*100, "% Equal-tail Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.eti.lower, "\t\t", object$post.eti.upper, "\n")
  cat("\nAdjusted for number of model-fitting parameters\n")
  cat("------------------------\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t\t", "Beta\n")
  cat(" ", object$alpha_star, "\t\t", object$beta_star, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median_star, "\n")
  cat(" ", object$interval.width*100, "% Equal-tail Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.eti.lower_star, "\t\t", object$post.eti.upper_star, "\n")
})

# Plot posterior and prior (optional) for dfba_phi
# To call plots, use plot(dfba_phi())
#' @export
setMethod("plot",
          signature("dfba_phi_out"),
          function(x, plot.prior=FALSE){
            dfba_plot_beta(x$alpha,
                           x$beta,
                           x$a.prior,
                           x$b.prior,
                           plot.prior)
          })

# Plot posterior and prior (optional) for dfba_phi when
# fitting parameters are specified in options
# To call plots, use plot(dfba_phi())
#' @export
setMethod("plot",
          signature("dfba_phi_star_out"),
          function(x, plot.prior=FALSE){
            dfba_plot_beta(x$alpha_star,
                           x$beta_star,
                           x$a.prior,
                           x$b.prior,
                           plot.prior)
          })


#' @export
setMethod("show", "dfba_gamma_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Concordant Pairs", "\t", "Discordant Pairs", "\n")
  cat(" ", object$nc, "\t\t\t", object$nd, "\n")
  cat(" ", "Proportion of Concordant Pairs", "\n")
  cat(" ", object$sample.p, "\n")
  cat(" ", "Goodman-Kruskal Gamma\n")
  cat(" ", object$gamma, "\n")
  cat("\nBayesian Analyses\n")
  cat("========================\n")
  cat(" ", "Beta Shape Parameters\n")
  cat(" ", "Alpha", "\t", "Beta\n")
  cat(" ", object$alpha, "\t\t", object$beta, "\n")
  cat(" ", "Posterior Median\n")
  cat(" ", object$post.median, "\n")
  cat(" ", object$interval.width*100, "% Equal-tail Interval\n", sep="")
  cat(" ", "Lower Limit", "\t\t", "Upper Limit\n")
  cat(" ", object$post.eti.lower, "\t\t", object$post.eti.upper)
})



#' @export
setMethod("plot",
          signature("dfba_gamma_out"),
          function(x, plot.prior=FALSE){
            dfba_plot_beta(x$alpha,
                           x$beta,
                           x$a.prior,
                           x$b.prior,
                           plot.prior)
          })


# Formats for small- and large-n Mann Whitney

## Small n

#' @export
setMethod("show", "dfba_mann_whitney_small_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "n_E", "\t", "n_C", "\n")
  cat(" ", object$n_E, "\t\t\t", object$n_C, "\n")
  cat(" ", "E mean", "\t", "C mean", "\n")
  cat(" ", object$Emean, "\t\t\t", object$Cmean, "\n")
  cat(" ", "U_E and U_C Mann-Whitney Statistics", "\n")
  cat(" ", object$U_E, "\t\t\t", object$U_C, "\n")
  cat("\n  Monte Carlo Sampling with Discrete Probability Values\n")
  cat("========================\n")
  cat(" ", "Number of MC Samples\n")
  cat(" ", object$samples, "\n")
  cat(" ", "\n  Mean of omega_E:\n")
  cat(" ", object$omegabar, "\n")
  cat("equal-tail area interval")
  cat(" ", object$prob_interval*100, "% interval limits:", "\n", sep="")
  cat(" ", object$qLv, "\t\t\t", object$qHv, "\n")
  cat(" ", "probability that omega_E exceeds 0.5 is:\n")
  cat(" ", "prior", "\t\t\t", "posterior\n")
  cat(" ", object$priorprH1, "\t\t\t", object$prH1, "\n")
  cat("  Bayes factor BF10 for omega_E > 0.5 is:\n")
  cat(" ", object$BF10, "\n")
})

#' @export
setMethod("show", "dfba_mann_whitney_large_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "n_E", "\t", "n_C", "\n")
  cat(" ", object$n_E, "\t\t\t", object$n_C, "\n")
  cat(" ", "E mean", "\t", "C mean", "\n")
  cat(" ", object$Emean, "\t\t\t", object$Cmean, "\n")
  cat(" ", "U_E and U_C Mann-Whitney Statistics", "\n")
  cat(" ", object$U_E, "\t\t\t", object$U_C, "\n")
  cat("\n  Beta Approximation Model for Omega_E\n")
  cat(" for 2*nE*nC/(nE+nC) > 19\n")
  cat("========================\n")
  cat(" ", "The posterior beta shape parameters are:\n")
  cat(" ", "posterior a", "\t\t\t", "posterior b\n")
  cat(" ", object$apost, "\t\t\t", object$bpost, "\n")
  cat(" ", "posterior mean", "\t\t\t", "posterior median\n")
  cat(" ", object$postmean, "\t\t\t", object$postmedian, "\n")
  cat(" ", "probability within interval is:\n")
  cat(" ", round(object$prob_interval*100), " percent\n")
  cat(" ", "equal-tail limit values are:\n")
  cat(" ", object$qlequal, "\t\t\t", object$qhequal, "\n")
  cat(" ", "highest-density limits are:\n")
  cat(" ", object$qLmin, "\t\t\t", object$qHmax, "\n")
  cat(" ", "probability that omega_E > 0.5:\n")
  cat(" ", "prior", "\t\t\t", "posterior\n")
  cat(" ", object$priorprH1, "\t\t\t", object$prH1, "\n")
  cat(" ", "Bayes factor BF10 for omega_E > 0.5 is:\n")
  cat(" ", ifelse(object$BF10 == Inf, "approaching infinity", object$BF10), "\n")
})


# Plots for Mann-Whitney
## small method

#' @export
setMethod("plot",
          signature("dfba_mann_whitney_small_out"),
          function(x,
                   plot.prior=TRUE){
              x.data<-x$omega_E
              y.predata<-x$priorvector
              y.postdata<-x$omegapost
              xlab="omega_E"
              ylab="Discrete Probability"
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
          }
)

## large method
#' @export
setMethod("plot",
          signature("dfba_mann_whitney_large_out"),
          function(x,
                   plot.prior=TRUE){
              x.data<-seq(0, 1, 1/1000)
              y.predata<-dbeta(x.data, x$a0, x$b0)
              y.postdata<-dbeta(x.data, x$apost, x$bpost)
              xlab="omega_E"
              ylab="Probability Density"
            if (plot.prior==FALSE){
              plot(x.data,
                   y.postdata,
                   type="l",
                   xlab=xlab,
                   ylab=ylab)
            } else {
              #    opar<-par(no.readonly=TRUE)
              #    par(mar=c(4.1, 4.1, 4.1, 4.1), xpd=TRUE)
              plot(x.data,
                   y.postdata,
                   type="l",
                   xlab=xlab,
                   ylab=ylab,
                   main=expression("--"~"Prior"~ - "Posterior"))
              lines(x.data,
                    y.predata,
                    lty=2)
              #    legend("top",
              #           inset = c(0, -0.1),
              #           legend=c("Posterior",
              #                    "Prior"),
              #           lty=c(1, 2),
              #           xpd=TRUE,
              #           horiz=TRUE)
              #    on.exit(par(opar))
            }
          }
)

# Formats for Wilcoxon small and large

## Small n

#' @export
setMethod("show", "dfba_wilcoxon_small_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Wilcoxon Signed-Rank Statistics", "\n")
  cat(" ", "n", "\t", "T_plus", "\t", "T_minus", "\n")
  cat(" ", object$n, "\t\t\t", object$T_plus, "\t\t\t", object$T_negative,"\n")
  cat("\n  Monte Carlo Sampling with Discrete Probability Values\n")
  cat("========================\n")
  cat(" ", "Number of MC Samples\n")
  cat(" ", object$samples, "\n")
  cat(" ", "\n  Posterior mean of phi_w:\n")
  cat(" ", object$phibar, "\n")
  cat("equal-tail area interval")
  cat(" ", object$prob_interval*100, "% interval limits:", "\n", sep="")
  cat(" ", object$qLv, "\t\t\t", object$qHv, "\n")
  cat(" ", "probability that phi_W exceeds 0.5 is:\n")
  cat(" ", "prior", "\t\t\t", "posterior\n")
  cat(" ", object$priorprH1, "\t\t\t", object$prH1, "\n")
  cat("  Bayes factor BF10 for phi_W > 0.5 is:\n")
  cat(" ", object$BF10, "\n")
})

#' @export
setMethod("show", "dfba_wilcoxon_large_out", function(object) {
  cat("Descriptive Statistics \n")
  cat("========================\n")
  cat(" ", "Wilcoxon Signed-Rank Statistics", "\n")
  cat(" ", "n", "\t", "T_plus", "\t", "T_minus", "\n")
  cat(" ", object$n, "\t", object$T_plus, "\t", object$T_negative, "\n")
  cat("\n  Beta Approximation Model for Phi_W\n")
  cat(" for n > 24\n")
  cat("========================\n")
  cat(" ", "The posterior beta shape parameters are:\n")
  cat(" ", "posterior a", "\t\t\t", "posterior b\n")
  cat(" ", object$apost, "\t\t\t", object$bpost, "\n")
  cat(" ", "posterior mean", "\t\t\t", "posterior median\n")
  cat(" ", object$postmean, "\t\t\t", object$postmedian, "\n")
  cat(" ", "probability within interval is:\n")
  cat(" ", round(object$prob_interval*100), " percent\n")
  cat(" ", "equal-tail limit values are:\n")
  cat(" ", object$qlequal, "\t\t\t", object$qhequal, "\n")
  cat(" ", "highest-density limits are:\n")
  cat(" ", object$qLmin, "\t\t\t", object$qHmax, "\n")
  cat(" ", "probability that phi_W > 0.5:\n")
  cat(" ", "prior", "\t\t\t", "posterior\n")
  cat(" ", object$priorprH1, "\t\t\t", object$prH1, "\n")
  cat(" ", "Bayes factor BF10 for phi_W > 0.5 is:\n")
  cat(" ", object$BF10, "\n")
})

#' @export
setMethod("show", "dfba_t_power_out", function(object) {
  cat("Power results for the proportion of samples detecting effects"," ","\n")
  cat(" ", "where the variates are distributed as a",object$model,"random variable","\n")
  cat(" ", "and where the design is",object$design,"\n")
  cat(" ", "The number of Monte Carlo samples are:"," ","\n")
  cat(" ", object$nsims," ","\n")
  cat(" ", "Criterion for detecting an effect is"," ","\n")
  cat(" ", object$effect_crit," ","\n")
  cat(" ", "The delta offset parameter is:"," ","\n")
  cat(" ", object$deltav," ","\n")
  cat("Output Results:", "\n")
  print(object$outputdf)
  })

#' @export
setMethod("show", "dfba_power_curve_out", function(object) {
  cat("Power results for the proportion of samples detecting effects"," ","\n")
  cat(" ", "where the variates are distributed as a",object$model,"random variable","\n")
  cat(" ", "and where the design is",object$design,"\n")
  if(object$design=="paired"){cat(" ", "with a blocking max of ",object$block.max,"\n")}
  cat(" ", "The number of Monte Carlo samples are:"," ","\n")
  cat(" ", object$nsims," ","\n")
  cat(" ", "Criterion for detecting an effect is"," ","\n")
  cat(" ", object$effect_crit," ","\n")
  cat("The n value per condition is:"," ","\n")
  cat(object$n,"  ","\n")
  cat("Output Results:", "\n")
  print(object$outputdf)
})

# Plots for Wilcoxon

#' @export
setMethod("plot",
          signature("dfba_wilcoxon_small_out"),
          function(x,
                   plot.prior=TRUE){
              x.data<-x$phiv
              y.predata<-x$priorvector
              y.postdata<-x$phipost
              xlab="phi_W"
              ylab="Discrete Probability"
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

#' @export
setMethod("plot",
          signature("dfba_wilcoxon_large_out"),
          function(x,
                   plot.prior=TRUE){
              x.data<-seq(0, 1, 1/1000)
              y.predata<-dbeta(x.data, x$a0, x$b0)
              y.postdata<-dbeta(x.data, x$apost, x$bpost)
              xlab="phi_W"
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
# Plots for power functions

## bayes_v_t

#' @export
setMethod("plot",
          signature("dfba_t_power_out"),
          function(x){
            plot(x$outputdf$sample_size,
                 x$outputdf$Bayes_power,
                 type="b",
                 ylim=c(0,1),
                 main=expression("--"~"Frequentist"~ - "Bayesian"),
                 xlab="Sample Size",
                 ylab="Power Estimate")
            lines(x$outputdf$sample_size,
                  x$outputdf$t_power,
                  type="b",
                  lty=2)
          })

## power_curve

#' @export
setMethod("plot",
          signature("dfba_power_curve_out"),
          function(x){
            plot(x$outputdf$delta_value,
                 x$outputdf$Bayes_power,
                 type="b",
                 ylim=c(0,1),
                 main=expression("--"~"Frequentist"~ - "Bayesian"),
                 xlab="Delta",
                 ylab="Power Estimate")
            lines(x$outputdf$delta_value,
                  x$outputdf$t_power,
                  type="b",
                  lty=2)
          })

# Formats for Bayes Factor Functions

## Point Bayes Factor

#' @export
setMethod("show", "dfba_point_BF_out", function(object) {
  cat("Bayes Factor for Point Estimates \n")
  cat("========================\n")
  cat(" ", "Point Null Hypothesis", "\n")
  cat(" ", object$null_hypothesis, "\n")
  cat(" ", "Shape Parameters for Prior Beta Distribution", "\n")
  cat(" ", "a0", "\t\t\t", "b0", "\n")
  cat(" ", object$a0, "\t\t\t", object$b0, "\n")
  cat(" ", "Shape Parameters for Posterior Beta Distribution", "\n")
  cat(" ", "a", "\t\t\t", "b", "\n")
  cat(" ", object$a, "\t\t\t", object$b, "\n")
  cat(" ", "Prior Probability Density for Null Hypothesis", "\n")
  cat(" ", object$dpriorH0, "\n")
  cat(" ", "Posterior Probability Density for Null Hypothesis", "\n")
  cat(" ", object$dpostH0, "\n")
  cat(" ", "Bayes Factor Estimate for the Alternative over the Null Hypothesis", "\n")
  cat(" ", object$BF10, "\n")
  cat(" ", "Bayes Factor Estimate for the Null over the Alternative Hypothesis", "\n")
  cat(" ", object$BF01, "\n")
})

## Interval Bayes Factor

#' @export
setMethod("show", "dfba_interval_BF_out", function(object){
  cat("Bayes Factor for Interval Estimates \n")
  cat("========================\n")
  cat(" ", "Interval Null Hypothesis", "\n")
  cat(" ", "Lower Limit", "\t\t\t", "Upper Limit", "\n")
  cat(" ", object$H0lower,"\t\t\t", object$H0upper, "\n")
  cat(" ", "Shape Parameters for Prior Beta Distribution", "\n")
  cat(" ", "a0", "\t\t\t", "b0", "\n")
  cat(" ", object$a0, "\t\t\t", object$b0, "\n")
  cat(" ", "Shape Parameters for Posterior Beta Distribution", "\n")
  cat(" ", "a", "\t\t\t", "b", "\n")
  cat(" ", object$a, "\t\t\t", object$b, "\n")
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

## Beta Contrasts

#' @export
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

## Simulated Data

#' @export
setMethod("show", "dfba_sim_data_out", function(object) {
  cat("Frequentist p-value \n")
  cat("", object$pvalue, "\n")
  cat("Bayesian posterior probability \n")
  cat("", object$prH1, "\n")
})

## Sim Data Plot

#' @export
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
