#'Power Curves
#'
#'Function runs a number of Monte Carlo data sets from one of the models
#'provided by the function dfba_sim_data. For each sample, there is a
#'frequentist t-test as well as a Bayesian test to compute the probability that
#'the population parameter is greater than .5. The proportion of all the
#'samples that detect an effect better than the the stipulated effect_crit
#'value is the power. For the Bayesian analysis, this would be the proportion
#'of samples where the posterior probability that the parameter is greater than
#'.5 is larger than the stipulated effect_crit value.
#'
#'A flat prior is used for all the Bayesian analyses. If the design is equal to
#'"independent", then the Bayesian probability is based on the omega parameter
#'for the Mann-Whitney U statistic. The corresponding t power is based on the
#'two-sample or independent t-test p value. The t power is the proportion of
#'samples where the p -alue is less than 1-effect_crit. The default value for
#'effect_crit is .95, but the user can vary that criterion. If design="paired",
#'then the Bayesian analysis is based on the Bayesian posterior probability that
#'the parameter phi_w for the Wilcoxon signed-rank statistic is greater than .5.
#'The corresponding t power is based on the independent (two-group) t-test.
#'
#'See documentation and remarks for the dfba_sim data function for the
#'probability models that can be examined. In each case the delta parameter is
#'an offset value between the E and C variates. If delta is zero, then the
#'Bayesian power should be approximately the value for 1-effect_crit. However,
#'for the t power case, the t-test is a misspecified procedure for the actual
#'data-generating model when model is not equal to "normal". Hence is some cases
#'such as with the, cauchy and pareto distributions, the t power is less that
#'1-effect_crit when delta equals zero. The function computes power for 11
#'different n values that vary from n_min in steps of 5. The power values can
#'be useful in planning an actual experiment. The user can explore any of the
#'nine models for the probability distribution for the two variates to see what
#'sample size is suitable for a stipulated delta value. The nine models are:
#'"normal","weibull", "cauchy", "lognormal", "chisquare", "logistic",
#'"exponential", "gumbel", "pareto". Details about the shape and offset
#'parameters for these distributions are in the remarks for the dfba_sim_data
#'function. The function will generally show that the sample size for normally
#'distributed data for the Bayesian distribution-free procedure is close to the
#'sample size used for the conventional t test. However,for alternative
#'distributions, the Bayesian nonparametric procedure generally requires a
#'smaller sample size than a parametric t-test.
#'
#' @param n smallest value of sample size for power calculations
#' @param a0 shape parameter value 1 for prior distribution (default is `1`)
#' @param b0 shape parameter value 2 for prior distribution (default is `1`)
#' @param delta.step desired step size of range of offset variable between variates
#' @param model hypothesized probability density function of data distributions
#' @param design one of "independent" or "paired" to indicate data structure
#' @param effect_crit stipulated critical value for reliable/significant differences
#' @param shape1 First shape parameter for the distribution indicated by `model` input (default is `1`)
#' @param shape2 First shape parameter for the distribution indicated by `model` input (default is `1`)
#' @param block.max Maximum value of blocking parameter (default is `1`)
#' @param samples desired number of Monte Carlo samples
#'
#' @return A list containing the following components:
#' @return \item{outputdf}{A dataframe of possible sample sizes and corresponding Bayesian and Frequentist power values}
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press.
#' @references Chechile, R.A., & Barch, D.H. (2021). Distribution-free, Bayesian goodness-of-fit method for assessing similar scientific prediction equations. Journal of Mathematical Psychology.


dfba_power_curve<-function(n = 20,
                           a0 = 1,
                           b0 = 1,
                           delta.step = .05,
                           model,
                           design,
                           effect_crit=.95,
                           shape1 = 1,
                           shape2 = 1,
                           block.max = 0,
                           samples=1000){

    if (delta.step<0){
      stop("The function requires a nonnegative value for delta.")
      }

    n<-round(n)
    if (n < 20){
      stop("The function requires n to be an integer that is 20 or larger")
      }


    mlist<-c("normal",
             "weibull",
             "cauchy",
             "lognormal",
             "chisquare",
             "logistic",
             "exponential",
             "gumbel",
             "pareto"
             )

    if (!model %in% mlist){
      cat("The set of distributions for model are:"," ","\n")
      print(mlist)
      stop("The stipulated model is not on the list")
      }

    designlist<-c("paired","independent")
    if (!design %in% designlist){
      cat("The options for experimental design are:"," ","\n")
      print(designlist)
      stop("The stipulated design is not on the list")
      }

    if (effect_crit<0|
        effect_crit>1){
      stop("The effect_crit value must be a number nonzero number less than 1.")
      }


    shape_values<-c(shape1,
                    shape2)

    a0=a0
    b0=b0
    block.max=block.max

    if (a0<=0|
        b0<=0|
        is.na(a0)|
        is.na(b0)){
      stop("Both a0 and b0 must be positive")
      }
    if (block.max<0|
        is.na(block.max)){
        stop("block.max must be nonnegative")
        }
    nsims=round(samples)


#    detect_bayes=seq(1,21,1)*0.0
    detect_bayes<-rep(NA, 21)
#    detect_t=seq(1,21,1)*0.0
    detect_t<-rep(NA, 21)
#    outputsim=seq(1,2,1)*0.0
    outputsim<-rep(NA, 2)
#    tpvalue=seq(1,nsims,1)*0.0
    tpvalue<-rep(NA, nsims)
#    bayesprH1=seq(1,nsims,1)*0.0
    bayesprH1<-rep(NA, nsims)
    delta_vec<-seq(0,20*delta.step,delta.step)

    for (i in 1:21){
      deltav=delta_vec[i]
      for (j in 1:nsims){
        outputsim=dfba_sim_data(n,
                                a0 = a0,
                                b0 = b0,
                                model,
                                design,
                                delta=deltav,
                                #shape_vec=shape_values,
                                shape1 = shape1,
                                shape2 = shape2,
                                block.max = block.max)
        bayesprH1[j]=outputsim$prH1
        tpvalue[j]=outputsim$pvalue
        cat(round((j/nsims+(i-1))/21, 2)*100, '% complete', sep="", '\r')
        }
      detect_bayes[i]=(sum(bayesprH1>effect_crit))/nsims
      detect_t[i]=(sum(tpvalue<1-effect_crit))/nsims
    }
#    cat("Power results for the proportion of samples detecting effects"," ","\n")
#    cat("where the variates are distributed as a",model,"random variable","\n")
#    cat("and where prior shape values are ",a0," ",b0,"\n")
#    cat("and where the design is",design,"\n")
#    cat("with a blocking max of ",block.max,"\n")
#    cat("The number of Monte Carlo samples are:"," ","\n")
#    cat(nsims," ","\n")
#    cat("Criterion for detecting an effect is"," ","\n")
#    cat(effect_crit," ","\n")
#    cat("The n value per condition is:"," ","\n")
#    cat(n,"  ","\n")
#    cat(" "," ","\n")
#    delta_value=delta_vec
#    Bayes_power=detect_bayes
#    t_power=detect_t
#    outputresults=data.frame(delta_value,Bayes_power,t_power)
#    print(outputresults)

    dfba_power_curve_list<- list(n = n,
                            nsims = nsims,
                            model = model,
                            a0 = a0,
                            b0 = b0,
                            design = design,
                            effect_crit = effect_crit,
                            block.max = block.max,
                            outputdf=data.frame(delta_value = delta_vec,
                                                Bayes_power = detect_bayes,
                                                t_power = detect_t)
    )
    new("dfba_power_curve_out", dfba_power_curve_list)

  }
