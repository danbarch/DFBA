#' Simulated Data Generator and Inferential Comparison
#'
#' This function is designed to be called by other DFBA programs that compare
#' frequentist and Bayesian power. The function generates simulated data for two
#' conditions that can be from nine different probability models.
#' The program also computes the frequentist \emph{p}-value from a \emph{t}-test
#' on the generated data, and it computes the Bayesian posterior probability
#' from a distribution-free analysis of the difference between the two
#' conditions.
#'
#'
#' @importFrom stats rnorm
#' @importFrom stats rweibull
#' @importFrom stats rcauchy
#' @importFrom stats rlnorm
#' @importFrom stats rchisq
#' @importFrom stats rlogis
#' @importFrom stats rexp
#' @importFrom stats runif
#' @importFrom stats var
#' @importFrom stats pt
#' @importFrom stats sd
#'
#' @param n Number of values per condition
#' @param a0 The first shape parameter for the prior beta distribution (default is 1). Must be positive and finite.
#' @param b0 The second shape parameter for the prior beta distribution (default is 1). Must be positive and finite.
#' @param model Theoretical probability model for the data. One of \code{"normal"}, \code{"weibull"}, \code{"cauchy"}, \code{"lognormal"}, \code{"chisquare"}, \code{"logistic"}, \code{"exponential"}, \code{"gumbel"}, or \code{"pareto"}
#' @param design Indicates the data structure. One of \code{"independent"} or \code{"paired"}.
#' @param delta Theoretical mean difference between conditions; the second condition minus the first condition
#' @param shape1 The shape parameter for condition 1 for the distribution indicated by \code{model} input (default is 1)
#' @param shape2 The shape parameter for condition 2 for the distribution indicated by \code{model} input (default is 1)
#' @param block.max The maximum size for a block effect (default is 0)
#'
#' @return A list containing the following components:
#' @return \item{pvalue}{The upper tail of the sample t value for the test that delta <= 0}
#' @return \item{prH1}{Bayesian posterior probability either for the hypothesis that phi_w > .5 from the nonparametric Wilcoxon test when \code{design = "paired"} or for the hypothesis that omega_E > .5 from the Mann-Whitney test when \code{design = "independent"}}
#' @return \item{E}{Vector of length n of simulated values for condition 1}
#' @return \item{C}{Vector of length n of simulated values for condition 2}
#'
#' @details
#'
#' Researchers need to make experimental-design decisions such as
#' the choice about the sample size per condition and the decision
#' to use a within-block design or an independent-group design. These
#' planning issues arise regardless if one uses either a frequentist
#' or Bayesian approach to statistical inference. In the DFBA package,
#' there are a number of functions to help users with these decisions.
#'
#' The \code{dfba_sim_data()} program is used along with other functions to
#' assess the relative power for detecting a condition difference of an
#' amount delta between two conditions. Delta is an input for the
#' \code{dfba_sim_data()} program, and it must be a nonnegative value.
#' Specifically, the \code{dfba_sim_data()} program generates two sets of data
#' that are randomly drawn from one of nine different theoretical
#' models. The input `model' stipulates the data generating probability
#' function. The input `model' is one of the following strings:
#'
#' \itemize{
#'    \item \code{"normal"}
#'    \item \code{"weibull"}
#'    \item \code{"cauchy"}
#'    \item \code{"lognormal"}
#'    \item \code{"chisquare"}
#'    \item \code{"logistic"}
#'    \item \code{"exponential"}
#'    \item \code{"gumbel"}
#'    \item \code{"pareto"}
#'  }
#'
#' For each model there are \code{n} continuous scores randomly sampled for each
#' condition, where \code{n} is a  user-specified input value. The \code{design}
#' argument is either \code{"independent"} or \code{"paired"}, and stipulates
#' whether the two sets of scores are either independent or from a common blocks
#' such as for the case of two scores for the same person (\emph{i.e.}, one in
#' each condition).
#'
#' The \code{shape1} and \code{shape2} arguments are values for the shape parameter
#' for the respective first and second condition, and their meaning
#' depends on the probability model. For \code{model="normal"}, these
#' parameters are the standard deviations of the two distributions. For
#' \code{model = "weibull"}, the parameters are the Weibull shape parameters.
#' For \code{model = "cauchy"}, the parameters are the scale factors for the
#' Cauchy distributions. For \code{model = "lognormal"}, the shape
#' parameters are the standard deviations for log(X). For \code{model = "chisquare"},
#' the parameters are the degrees of freedom (\emph{df}) for the two
#' distributions. For \code{model = "logistic"}, the parameters are the scale
#' factors for the distributions. For \code{model = "exponential"}, the parameters
#' are the rate parameters for the distributions.
#'
#' For the Gumbel distribution, the \code{E} variate is equal to
#' \code{delta - shape2*log(log(1/U))} where \code{U} is a random value sampled
#' from the uniform distribution on the interval \code{[.00001, .99999]}, and
#' the \code{C} variate is equal to \code{-shape1*log(log(1/U))} where \code{U}
#' is another score sampled from the uniform distribution. The \code{shape1} and
#' \code{shape2} arguments for \code{model = "gumbel"} are the scale parameters
#' for the distributions. The Pareto model is a distribution designed to account
#' for income distributions as studied by economists (Pareto, 1897). For the
#' Pareto distribution, the cumulative function is equal to \code{1-(x_m/x)^alpha}
#' where \code{x} is greater than \code{x_m} (Arnold, 1983). In the \code{E}
#' condition, \code{x_m = 1 + delta} and in the \code{C} condition \code{x_m = 1}.
#' The alpha parameter is 1.16 times the shape parameters \code{shape1} and
#' \code{shape2}. Since the default value for each shape parameter is 1, the
#' resulting alpha value of 1.16 is the default value. When alpha = 1.16, the
#' Pareto distribution approximates an income distribution that represents the
#' 80-20 law where 20\% of the population receives 80\% of the income
#' (Hardy, 2010).
#'
#' The \code{block.max} argument provides for incorporating block effects in the
#' random sampling. The block effect for each score is a separate effect for the
#' block. The block effect B for a score is a random number drawn from a uniform
#' distribution on the interval \code{[0, block.max]}. When \code{design = "paired"},
#' the same random block effect is added to the score in the first condition,
#' which is the random \code{C} value, and it is also added to the corresponding
#' paired value for the \code{E} variate. Thus, the pairing research design
#' eliminates the effect of block variation for the assessment of condition
#' differences. When \code{design = "independent"}, there are different block-effect
#' contributions to the \code{E} and \code{C} variates, which reduces the
#' discrimination of condition differences because it increases the variability
#' of the difference in the two variates. The user can study the effect of the
#' relative discriminability of detecting an effect of delta by adjusting the
#' value of the \code{block.max} argument. The default for \code{block.max} is 0,
#' but it can be altered to any non-negative real number.
#'
#' The output from calling the \code{dfba_sim_data()} function are two
#' statistics that are based on the \emph{n} scores generated in the two
#' conditions. One statistic is the frequentist \emph{p}-value for rejecting the
#' null hypothesis that delta <= 0 from a parametric \emph{t}-test. The
#' \emph{p}-value is the upper tail probability of the sample \emph{t}-statistic
#' for either the paired \emph{t}-test when \code{design = "paired"} or it is
#' the upper tail probability of the sample \emph{t}-statistic for the two-group
#' \emph{t}-test when \code{design = "independent"}. The second output statistic
#' is the Bayesian posterior probability for one of two possible nonparametric
#' tests. If \code{design = "paired"}, the \code{dfba_sim_sim()} function
#' calls the \code{dfba_wilcoxon()} function to ascertain the posterior
#' probability that \code{phi_w > .5}. If \code{design = "independent"}, the
#' \code{dfba_sim_data()} function calls the \code{dfba_mann_whitney()} function
#' to estimate the posterior probability that \code{omega_E > .5}. The arguments
#' \code{a0} and \code{b0} for the \code{dfba_sim_data()} function are passed
#' along to either the \code{dfba_wilcoxon()} function or the
#' \code{dfba_mann_whitney()} function. The default values are \code{a0 = b0 = 1}.
#'
#' @note
#' Random sampling for both the Gumbel and the Pareto distributions are
#' generated by the \code{dfba_sim_data()} function using the inverse transform
#' method (Fishman, 1996).
#'
#' @seealso
#' \code{\link[stats:Distributions]{Distributions}} for details on the
#' parameters of the normal, Weibull, Cauchy, lognormal, chi-squared, logistic,
#' and exponential distributions.
#'
#' \code{\link{dfba_wilcoxon}}
#'
#' \code{\link{dfba_mann_whitney}}
#'
#' @references
#'
#' Arnold, B. C. (1983). Pareto Distribution. Fairland, MD:
#' International Cooperative Publishing House.
#'
#' Chechile, R. A. (2017). A Bayesian analysis for the Wilcoxon signed-rank
#' statistic. Communications in Statistics - Theory and Methods,
#' https://doi.org/10.1080/03610926.2017.1388402
#'
#' Chechile, R. A. (2020). A Bayesian analysis for the Mann- Whitney statistic.
#' Communications in Statistics - Theory and Methods,
#' https://10.1080/03610926.2018.1549247
#'
#' Fishman, G. S. (1996) Monte Carlo: Concepts, Algorithms and Applications.
#' New York: Springer.
#'
#' Hardy, M. (2010). Pareto's Law. Mathematical Intelligencer,
#' 32, 38-43.
#'
#' Johnson, N. L., Kotz S., and Balakrishnan, N. (1995). Continuous Univariate
#' Distributions, Vol. 1, New York: Wiley.
#'
#' Pareto, V. (1897). Cours d'Economie Politique. Vol. 2,
#' Lausanne: F. Rouge.
#'
#' @examples
#'
#' # Example of two paired normal distributions where the s.d. of the two
#' # conditions are 1 and 4.
#'
#'dfba_sim_data(n = 50,
#'              model = "normal",
#'              design = "paired",
#'              delta = .4,
#'              shape1 = 1,
#'              shape2 = 4)
#'
#' # Example of two independent Weibull variates with their shape parameters =.8
#' # and with a .25 offset
#'
#' dfba_sim_data(n = 80,
#'               model = "weibull",
#'               design = "independent",
#'               delta = .25,
#'               shape1 = .8,
#'               shape2 = .8)
#'
#' # Example of two independent Weibull variates with their shape
#' # parameters = .8 and with a .25 offset along with some block differences
#' # with the max block effect being 1.5
#'
#'dfba_sim_data(n = 80,
#'              model = "weibull",
#'              design = "independent",
#'              delta = .25,
#'              shape1 = .8,
#'              shape2 = .8,
#'              block.max = 1.5)
#'
#' # Example of two paired Cauchy variates with a .4 offset
#'
#'dfba_sim_data(n = 50,
#'              model = "cauchy",
#'              design = "paired",
#'              delta = .4)
#' # Example of two paired Cauchy variates with a .4 offset where the Bayesian
#' # analysis uses the Jeffreys prior
#'
#'dfba_sim_data(n = 50,
#'              a0 = .5,
#'              b0 = .5,
#'              model = "cauchy",
#'              design = "paired",
#'              delta=.4)
#'
#' @export
dfba_sim_data<-function(n = 20,
                        a0 = 1,
                        b0 = 1,
                        model,
                        design,
                        delta,
                        shape1 = 1,
                        shape2 = 1,
                        block.max=0){

  if (delta<0){
    stop("The function requires a positive difference in the location of the two conditions.")
    }

  if (n<20){
    stop("The function requires an integer that is 20 or larger for sample size")
    }


  mlist<-c("normal",
           "weibull",
           "cauchy",
           "lognormal",
           "chisquare",
           "logistic",
           "exponential",
           "gumbel",
           "pareto")

  if (!model %in% mlist){
    modelstop <- paste0("The set of distributions for model are:"," ","\n",
                        "\t\t", paste0(mlist, collapse = "\n\t\t"), "\n",
                        "The stipulated model is not on the list")
    stop(modelstop)
  }

  designlist<-c("paired",
                "independent")

  if (!design %in% designlist){
    designstop <- paste0("The set of distributions for design are:"," ","\n",
                         "\t\t", paste0(designlist, collapse = "\n\t\t"), "\n",
                         "The stipulated design is not on the list")
    stop(designstop)
  }

  if (block.max<0|(is.na(block.max))){
    stop("block.max must be nonnegative")
  }

   if (a0 <= 0|
       a0 == Inf|
       b0 <= 0|
       b0 == Inf|
       is.na(a0)|
       is.na(b0)){
     stop("Both a0 and b0 must be positive and finite.")
   }

  if (model=="normal"){
    #The delta parameter is a nonnegative offset for the normal distribution for the experimental
    #condition, and the shape_vec components are the standard deviations of the respective control and
    #experiment conditions.
    if((shape1<=0)|(shape2<=0)){
      stop("shape1 and shape2 are standard deviations and must be positive values.")
      }
    C<- rnorm(n,
              0,
             shape1)
    E <- rnorm(n,
               delta,
               shape2)
  }

  if (model=="weibull"){
    #The delta parameter is a nonnegative offset for the Weibull distribution for the experimental
    #condition, and the shape_vec components are the shape parameters of the respective control and
    #experiment conditions. The scale factor for the Weibull distribution is 1 for both conditions
    if((shape1<=0)|(shape2<=0)){
      stop("shape1 and shape2 are the Weibull shape parameters for the respective control and experiment conditions and must be positive values.")
      }
    C <- rweibull(n,
                  shape1,
                  1)
    E <- delta + rweibull(n,
                          shape2,
                          1)
  }

  if (model=="cauchy"){
    #The delta parameter is a nonnegative offset for the Cauchy distribution for the experimental
    #condition, and the shape_vec components are the shape parameters of the respective control and
    #experiment conditions.

    if((shape1<=0)|(shape2<=0)){
      stop("shape1 and shape2 are the Cauchy distribution scale factors for the respective control and experiment conditions and must be positive values.")
      }
    C <- rcauchy(n,
                 0,
                 shape1)
    E <- rcauchy(n,
                 delta,
                 shape2)
    }

  if (model=="lognormal"){
    #The delta parameter is a nonnegative offset for a lognormal distribution for the experimental
    #condition, and the shape_vec components are standard deviations of the respective control and
    #experiment conditions.
    if((shape1<=0)|(shape2<=0)){
      stop("shape1 and shape2 are the sdlog values for the respective control and experiment conditions and must be positive values.")
      }
    C <- rlnorm(n,
                0,
                shape1)
    E <- rlnorm(n,
                delta,
                shape2)
    }

  if (model=="chisquare"){
    #The delta parameter is a nonnegative offset for a chi square random variable
    #for the experimental condition, and the shape_vec components are the df values
    #for the respective control and experimental conditions.
    if((shape1<=0)|(shape2<=0)){
      stop("shape1 and shape2 are df for respectively the control and experiment conditions and must be positive values.")
    }
    C <- rchisq(n,
                shape1)
    E <- delta +(rchisq(n,
                        shape2))
  }

  if (model=="logistic"){
    #The delta parameter is a nonnegative offset for the logistic distribution for the experimental
    #condition, and the shape_vec components are the scale factors of the respective control and
    #experiment conditions.
    if((shape1<=0)|(shape2<=0)){
      stop("shape1 and shape2 are scale factors and must be positive values.")
    }
    #else {}
    C <- rlogis(n,
                0,
                shape1)
    E <- rlogis(n,
                delta,
                shape2)
  }

  if (model=="exponential"){
    #The delta parameter is a nonnegative offset for the exponential distribution for the experimental
    #condition, and the shape_vec components are the rate parameters of the respective control and
    #experiment conditions.
    if((shape1<=0)|(shape2<=0)){
      stop("shape1 and shape2 are scale factors and must be positive values.")
    }
    C <- rexp(n,
              shape1)
    E <- delta+rexp(n,
                    shape2)
  }

  if (model=="gumbel"){
    #The delta parameter is the nonnegative offset for a Gumbel distribution for the experimental
    #condition, and the shape_vec components are the scale factors for the respective control
    #experiment conditions. The random scores are obtained via the inverse transform method.
    if((shape1<=0)|(shape2<=0)){
      stop("shape1 and shape2 are scale factors and must be positive values.")
    }
    g <- runif(n,.00001,.99999)
    C <- -shape1*log(log(1/g))
    g2 <- runif(n,.00001,.99999)
    E <- delta-shape2*log(log(1/g2))
  }

  if (model=="pareto"){
    #The x_m parameter is 1 for the control condition and it is 1+delta for the experimental
    #condition. The alpha shape parameter for the respective control and experiment condition
    #are 1.16 times the shape_vec[1] and shape_vec[2] values. This function requires the alpha
    #to be at least 1.16 so shape_vec components should not be less than 1.
    #The random scores are obtained via the inverse transform method.

    if((shape1<1)|(shape2<1)){
      stop("shape1 and shape2 must be greater than or equal to 1.")
    }
    g <- runif(n,.00001,.99999)
    alpha1 <- 1.16*shape1
    C <- 1/(1-g)^(1/alpha1)
    alpha2 <- 1.16*shape2
    g2 <- runif(n,.00001,.99999)
    E<- (1+delta)/(1-g2)^(1/alpha2)
  }

  if (design=="independent"){
    Bex <- runif(n,0,block.max)
    Bcn <- runif(n,0,block.max)
    E <- E+Bex
    C <- C+Bcn

    tnum <- mean(E)-mean(C)
    tdem <- sqrt((var(E)+var(C))/n)
    t_sample <- tnum/tdem
    pvalue <- pt(t_sample,2*n-2, lower.tail=FALSE)

    independent_out<-list(pvalue = pvalue,
                          prH1 = dfba_mann_whitney(E,
                                            C,
                                            a0,
                                            b0,
                                            method="large")$prH1,
                          E = E,
                          C = C,
                          design = design)
  }

  if (design=="paired"){
    B <- runif(n,0,block.max)
    E <- E+B
    C <- C+B
    d <- E-C

    t_sample <- (sqrt(n)*mean(d))/sd(d)

    pvalue <- pt(t_sample,n-1, lower.tail=FALSE)


    dependent_out<-list(pvalue=pvalue,
                        prH1 = dfba_wilcoxon(E,
                                             C,
                                             a0,
                                             b0,
                                             method="large")$prH1,
                        E = E,
                        C = C,
                        design = design)
  }
    #else {}
  if(design=="independent"){
    new("dfba_sim_data_out", independent_out)
  }else{
    new("dfba_sim_data_out", dependent_out)
  }

}

#
#
#  gumbel<-function(n,delta,shape_vec=c(1,1)){
#    g=runif(n,.00001,.99999)
#    C=-shape_vec[1]*log(log(1/g))
#    g2=runif(n,.00001,.99999)
#    E=delta-shape_vec[2]*log(log(1/g2))
#    return(E)
#  }
#
#  pareto<-function(n,delta,shape_vec=c(1.16,1.16)){
#    g=runif(n,.00001,.99999)
#    C=1/(1-g)^(1/shape_vec[1])
#    g2=runif(n,.00001,.99999)
#    E=(1+delta)/(1-g2)^(1/shape_vec[2])
#    return(E)
#  }
