#' Independent Samples Test (Mann Whitney U)
#'
#' Given two independent vectors \code{E} and \code{C}, the function computes
#' the sample Mann-Whitney \eqn{U} statistics \code{U_E} and \code{U_C} and
#' provides a Bayesian analysis for the population parameter \code{omega_E},
#' which is the population ratio of \eqn{U_E/(U_E+U_C)}.
#'
#' @importFrom stats pbeta
#' @importFrom stats rexp
#'
#' @param E Data for independent sample 1 ("Experimental")
#' @param C Data for independent sample 2 ("Control")
#' @param a0 The first shape parameter for the prior beta distribution for \code{omega_E} (default is 1). Must be positive and finite.
#' @param b0 The second shape parameter for the prior beta distribution for \code{omega_E} (default is 1). Must be positive and finite.
#' @param prob_interval Desired probability value for the interval estimate for \code{omega_E} (default is 95\%)
#' @param samples The number of Monte Carlo samples for \code{omega_E} when \code{method = "small"} (default is 30000)
#' @param method (Optional) The method option is either "small" or "large". The "small" algorithm is based on a discrete Monte Carlo solution for cases where n is typically less than 20. The "large" algorithm is based on beta approximation model for the posterior distribution for the omega_E parameter. This approximation is reasonable when n > 19. Regardless of \eqn{n}, the user can stipulate \code{method}. When the \code{method} argument is omitted, the program selects the appropriate procedure
#' @param hide_progress (Optional) If \code{TRUE}, hide percent progress while Monte Carlo sampling is running when \code{method = SMALL}. (default is \code{FALSE}).
#'
#' @return A list containing the following components:
#' @return \item{Emean}{Mean of the independent sample 1 ("Experimental") data}
#' @return \item{Cmean}{Mean of the independent sample 1 ("Control") data}
#' @return \item{n_E}{Number of observations of the independent sample 1 ("Experimental") data}
#' @return \item{n_C}{Mean of observations of the independent sample 2 ("Control") data}
#' @return \item{U_E}{Total number of comparisons for which observations from independent sample 1 ("Experimental") data exceed observations from independent sample 2 ("Control") data)}
#' @return \item{U_C}{Total number of comparisons for which observations from independent sample 2 ("Control") data exceed observations from independent sample 1 ("Experimental") data)}
#' @return \item{prob_interval}{User-defined width of \code{omega_E} interval estimate (default is 0.95)}
#' @return \item{a0}{First shape parameter for the prior beta distribution}
#' @return \item{b0}{Second shape parameter for the prior beta distribution}
#' @return \item{a_post}{First shape parameter for the posterior beta distribution}
#' @return \item{b_post}{Second shape parameter for the posterior beta distribution}
#' @return \item{samples}{The number of desired Monte Carlo samples (default is 30000)}
#' @return \item{method}{A character string indicating the calculation method used}
#' @return \item{omega_E}{A vector of values representing candidate values for \code{omega_E} when \code{method = "small"}}
#' @return \item{omegapost}{A vector of values representing discrete probabilities for candidate values of \code{omega_E}}
#' @return \item{priorvector}{A vector of values representing prior discrete probabilities of candidate values of \code{omega_E} when \code{method = "small"}}
#' @return \item{priorprH1}{Prior probability of the alternative model that omega_E exceeds 0.5}
#' @return \item{prH1}{Posterior probability of the alternative model that omega_E exceeds 0.5}
#' @return \item{BF10}{Bayes Factor describing the relative increase in the posterior odds for the alternative model that \code{omega_E} exceeds 0.5 over the null model of \code{omega_E} less than or equal to 0.5}
#' @return \item{omegabar}{Posterior mean estimate for \code{omega_E}}
#' @return \item{eti_lower}{Lower limit of the equal-tail probability interval for \code{omega_E} with probability width indicated by \code{prob_interval}}
#' @return \item{eti_upper}{Upper limit of the equal-tail probability interval for \code{omega_E} with probability width indicated by \code{prob_interval}}
#' @return \item{hdi_lower}{Lower limit of the highest-density probability interval for \code{omega_E} with probability width indicated by \code{prob_interval} when \code{method = "small"}}
#' @return \item{hdi_upper}{Upper limit of the highest-density probability interval for \code{omega_E} with probability width indicated by \code{prob_interval} when \code{method = "small"}}
#'
#' @details
#'
#' The Mann-Whitney \emph{U} test is the frequentist nonparametric counterpart
#' to the independent-groups \eqn{t}-test. The sample \code{U_E} statistic is
#' the number of times that the \emph{E} variate is larger than the
#' \emph{C} variate, whereas \code{U_C} is the converse number.
#'
#' This test uses only rank information, so it is robust with respect to
#' outliers, and it does not depend on the assumption of a normal model for the
#' variates. The Bayesian version for the Mann-Whitney is focused on the
#' population parameter \code{omega_E}, which is the population ratio
#' \code{U_E/(U_E+U_C)}.
#'
#' While the frequentist test effectively assumes the sharp null hypothesis that
#' \code{omega_E} is .5, the Bayesian analysis has a prior and posterior
#' distribution for \code{omega_E} on the [0, 1] interval. The prior is a beta
#' distribution with shape parameters \code{a0} and \code{b0}. The default is
#' the flat prior (\eqn{a0 = b0 =} 1), but this prior can be altered by the
#' user.
#'
#' The \code{prob_interval} input is the value for probability interval estimates for
#' omega_E. There are two cases depending on the sample size for the \emph{E}
#' and \emph{C} variates. When the samples sizes are small, there is a discrete
#' approximation method used. In this case, the Bayesian analysis considers 200
#' discrete values for \code{omega_E} from .0025 to .9975 in steps of .005. For
#' each discrete value, a prior and a posterior probability are obtained. The
#' posterior probabilities are based on Monte Carlo sampling to approximate the
#' likelihood of obtaining the observed \code{U_E} and \code{U_C} values for each candidate
#' value for omega_E. For each candidate value for omega_E, the likelihood for
#' the observed sample U statistics does not depend on the true distributions of
#' the \emph{E} and \emph{C} variates in the population. For each candidate
#' \code{omega_E}, the software constructs two exponential variates that have
#' the same omega_E value. The argument \code{samples} specifies the number of
#' Monte Carlo samples used for each candidate value of \code{omega_E}.
#'
#' For large sample sizes of the \emph{E} and \emph{C} variates,
#' the Bayesian posterior distribution is closely approximated by a beta
#' distribution where the shape parameters are a function of the sample
#' \code{U_E} and \code{U_C} statistics. The large-sample beta approximation was
#' developed from extensive previous empirical studies designed to approximate
#' the quantiles of the discrete approach with the corresponding quantiles for a
#' particular beta distribution. The large-\emph{n} solution also uses Lagrange
#' polynomials for interpolation. The large-\emph{n} approximation is reasonably
#' accurate when \eqn{n > 19} for each condition. When the \code{method} input
#' is omitted, the function selects the appropriate procedure (\emph{i.e.},
#' either the discrete case for a small sample size or the large-\emph{n}
#' approach). Nonetheless, the user can stipulate which method they desire
#' regardless of sample size by inputting either \code{method="small"} or
#' \code{method="large"}. The large-\emph{n} solution is rapid compared
#' to the small-sample solution, so care should be executed when choosing the
#' \code{method="small"}, even for large sample sizes.
#'
#' Technical details of the analysis are explained in the Chechile (2020)
#' Communications in Statistics paper cited below.
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental
#' Scientists: A General Introduction Using Distribution-Free Methods.
#' Cambridge: MIT Press.
#'
#' @references Chechile, R.A. (2020). A Bayesian analysis for the Mann-Whitney
#' statistic. Communications in Statistics -- Theory and Methods 49(3): 670-696.
#' https://doi.org/10.1080/03610926.2018.1549247.

#' @importFrom stats qbeta
#'
#' @examples
#'
#' # Note: examples with method = "small" have long runtimes due to Monte Carlo
#' # sampling; please feel free to run them in the console.
#'
#' # Examples with large n per group
#' # The data for each condition are presorted only for the user convenience if
#' # checking the U stats by hand
#'
#' groupA <- c(43, 45, 47, 50, 54, 58, 60, 63, 69, 84, 85, 91, 99, 127, 130,
#'             147, 165, 175, 193, 228, 252, 276)
#' groupB <- c(0, 01, 02, 03, 05, 14, 15, 23, 23, 25, 27, 32, 57, 105, 115, 158,
#'             161, 181, 203, 290)
#'
#' dfba_mann_whitney(E = groupA,
#'                   C = groupB)
#'
#' # The following uses a Jeffreys prior instead of a default flat prior:
#' dfba_mann_whitney(E = groupA,
#'                   C = groupB,
#'                   a0 = .5,
#'                   b0 =.5)
#'
#' # The following also uses a Jeffreys prior but the analysis reverses the
#' # variates:
#' dfba_mann_whitney(E = groupB,
#'                   C = groupA,
#'                   a0 = .5,
#'                   b0 = .5)
#'
#' # Note that BF10 from the above analysis is 1/BF10 from the original order
#' # of the variates.
#'
#' # The next analysis constructs 99% interval estimates with the Jeffreys
#' # prior.
#'
#' dfba_mann_whitney(E = groupA,
#'                   C = groupB,
#'                   a0 = .5,
#'                   b0 = .5,
#'                   prob_interval=.99)
#'
#' # The following forces a discrete approach with a flat prior for a case with
#' # large n:
#'
#'
#' \dontrun{
#' dfba_mann_whitney(E = groupA,
#'                   C = groupB,
#'                   method = "small",
#'                   hide_progress = FALSE)
#' }
#'
#' #Examples with small n per group
#'
#' groupC <- c(96.49, 96.78, 97.26, 98.85, 99.75, 100.14, 101.15, 101.39,
#'             102.58, 107.22, 107.70, 113.26)
#' groupD <- c(101.16, 102.09, 103.14, 104.70, 105.27, 108.22, 108.32, 108.51,
#'             109.88, 110.32, 110.55, 113.42)
#'
#' \dontrun{
#' CDex1<-dfba_mann_whitney(E = groupC,
#'                          C = groupD,
#'                          hide_progress = FALSE)
#'
#' CDex1
#'
#' CDex2<-dfba_mann_whitney(E = groupC,
#'                          C = groupD,
#'                          samples = 50000,
#'                          hide_progress = FALSE)
#' CDex2
#'
#'
#' CDex3<-dfba_mann_whitney(E = groupC,
#'                          C = groupD,
#'                          hide_progress = FALSE)
#'
#' CDex3
#'
#' }
#' # Note that CDex1 and CDex2 are replication analyses for the discrete approach.
#' # The variability is due to the different outcomes from the Monte Carlo
#' # sampling.
#'
#' # Plot output
#' # with prior and posterior curves
#'
#' \dontrun{
#'
#' plot(CDex1)
#'
#' # with only posterior curve
#' plot(CDex2,
#'      plot.prior = FALSE)
#'
#' }
#' @export
dfba_mann_whitney<-function(E,
                            C,
                            a0 = 1,
                            b0 = 1,
                            prob_interval=.95,
                            samples=30000,
                            method=NULL,
                            hide_progress = FALSE){

  if (is.na(a0)|
      a0 <= 0|
      a0 == Inf|
      is.na(b0)|
      b0 <= 0|
      b0 == Inf){
    stop("Both a0 and b0 must be positive and finite.")}


  if ((prob_interval < 0)|(prob_interval > 1)){
    stop("The probability for the interval estimate of phi_w must be a proper proportion.")}

  if (samples < 10000){
    message("For reliable results, the recommended minimum number of Monte Carlo samples is 10000")}

  Etemp <- E
  Ctemp <- C

  E <- Etemp[!is.na(Etemp)]
  E_missing <- length(Etemp)-length(E)
  C <- Ctemp[!is.na(Ctemp)]
  C_missing <- length(Ctemp)-length(C)

  if(E_missing > 0){
    message(paste0("Note: ", E_missing, " cells with missing values in E vector omitted"))
  }

  if(C_missing > 0){
    message(paste0("Note: ", C_missing, " cells with missing values in C vector omitted"))
  }

  nE <- length(E)
  nC <- length(C)
  if ((nE==0)|(nC==0)){
    stop("The E and C vectors must have a length greater than 0.")
    }

  #following code finds U_E and U_C
  UE_vector <- rep(NA, length(E)) # UE counter
  UC_vector <- rep(NA, length(C)) # UC counter
  for (i in seq_along(E)){
    UE_vector[i]<-sum(E[i]>C)
  }
  for (j in seq_along(C)){
    UC_vector[j]<-sum(C[j]>E)
  }
  UE <- sum(UE_vector)
  UC <- sum(UC_vector)

  nH <- (2*nE*nC)/(nE+nC)

  if (is.null(method)){
    if (nH >= 20){
      method <- "large"
    } else {
        method <- "small"}
  }

  if ((method!="large")&(method!="small")) {
    stop("An explicit method stipulation must be either the word large or small.")
  }

  if (method=="small"){

    #Code for the discrete prior
    phiv <- seq(1/400,.9975,.005)
    x <- phiv+.0025
    priorvector <- rep(0,200)
    priorvector[1] <- pbeta(x[1],a0,b0)
    for (i in 2:200){
      priorvector[i] <- pbeta(x[i],a0,b0)-pbeta(x[i-1],a0,b0)}

    XE <- seq(1, length(E), 1)
    XC <- seq(1, length(C), 1)
    fomega<-rep(0.0,200)
    Ntot <- UE+UC


    for (j in 1:200){
      if(hide_progress == FALSE){
        message('\r', round(j/200, 2)*100, '% complete', appendLF = FALSE)
      }
      omega <- (1/400)+(j-1)*(1/200)
      komega <- (1-omega)/omega

      if(length(E)*length(C)>Ntot){
        for (k in 1:samples) {
          Uz <- 0
          Jc <- 0

          XE <- rexp(length(E), rate = komega)
          XC <- rexp(length(C), rate = 1)

          for (i in seq_along(E)) {
            for(q in seq_along(C)){
              Jc <- Jc+1
              if ((XE[i]>XC[q])&(Jc<=Ntot)){
                Uz <- Uz+1
              }

            }
          }
          if (Uz==UE){
            fomega[j] <- fomega[j] + 1
          }
          else {
          }
        }
      }else{
        for (k in 1:samples){
          Uz<-rep(NA, length(E))
          XE<-rexp(length(E), rate=komega)
          XC<-rexp(length(C), rate = 1)

          for (i in seq_along(E)){
            Uz[i]<-sum(XE[i]>XC)
          }

          if(sum(Uz) == UE) {
            fomega[j] <- fomega[j]+1
          } else {

            }
        }
      }

    }
    if (hide_progress == FALSE){
      message('\n', appendLF = FALSE)
    }

    tot <- sum(priorvector*fomega)
    omegapost <- (priorvector*fomega)/tot
    phiv <- rep(0.0,200)
    for (j in 1:200){
      phiv[j] <- (1/400)+(j-1)*(1/200)}
    postdis<-data.frame(phiv,
                        omegapost)

    #Following finds the mean of the posterior omega distribution
    #and provides a plot of the distribution.
    omegabar <- sum(phiv*omegapost)

    postdis<-data.frame(phiv,
                        omegapost)

    cumulative_omega <- cumsum(omegapost)

    I <- 1
    while (cumulative_omega[I]<(1-prob_interval)/2){
      I <- I+1}
    qLbelow <- phiv[I]-.0025

    if (I!=1){
      extrap <- (1-prob_interval)/2-cumulative_omega[I-1]
      probI <- cumulative_omega[I]-cumulative_omega[I-1]} else {
        extrap <- (1-prob_interval)/2
        probI <- cumulative_omega[1]}
    eti_lower <- qLbelow+(.005)*(extrap/probI)

    I <- 1
    while (cumulative_omega[I]<1-(1-prob_interval)/2){
      I <- I+1}
    qHbelow <- phiv[I]-.0025
    extrapup <- 1-((1-prob_interval)/2)-cumulative_omega[I-1]
    probIu <- cumulative_omega[I]-cumulative_omega[I-1]
    eti_upper <- qHbelow+(.005)*(extrapup/probIu)

    probpercent <- 100*prob_interval

    omega_E <- phiv

    prH1 <- 1-cumulative_omega[round(100)]
    cumulative_prior <- cumsum(priorvector)
    priorprH1 <- 1-cumulative_prior[round(100)]

    #Following finds the Bayes factor for omega_E being greater than .5.

    if ((prH1==1)|(priorprH1==0)){
      BF10 <- samples
      } else {
        BF10 <- (prH1*(1-priorprH1))/(priorprH1*(1-prH1))
        }

    dfba_mann_whitney_small_list<-list(Emean=mean(E),
                                       Cmean=mean(C),
                                       n_E = nE,
                                       n_C = nC,
                                       U_E = UE,
                                       U_C = UC,
                                       prob_interval = prob_interval,
                                       samples = samples,
                                       method = method,
                                       omega_E = omega_E,
                                       omegapost = omegapost,
                                       cumulative_omega = cumulative_omega,
                                       priorvector = priorvector,
                                       priorprH1 = priorprH1,
                                       prH1 = prH1,
                                       BF10 = BF10,
                                       omegabar = omegabar,
                                       eti_lower = eti_lower,
                                       eti_upper = eti_upper)

    }

  if (method=="large"){

    #Following is a Lagrange interpolation method to find posterior a and b
    #shape parameters for the beta approximation to the omega_E distribution
    xs <- UE/(UE+UC)
    if (xs>=.5){
      x <- xs
    }
    else {
      x <- 1-xs
      }
    nH <- (2*nE*nC)/(nE+nC)
    y5 <- (nH^1.1489)/(.4972+(nH^1.1489))
    w4 <- .8-(1/(1+(1.833*nH)))
    w3 <- .6-(1/(1+(2.111*nH)))
    w2 <- .4-(1/(1+(2.520*nH)))
    w1 <- .2-(1/(1+(4.813*nH)))
    y4 <- (y5*w4)+(1-w4)*.5
    y3 <- (y5*w3)+(1-w3)*.5
    y2 <- (y5*w2)+(1-w2)*.5
    y1 <- (y5*w1)+(1-w1)*.5
    Y <- c(.5,
           y1,
           y2,
           y3,
           y4,
           y5)

    La0 <- 252-(1627*x)+((12500*x^2)-(15875*x^3)+(10000*x^4)-(2500*x^5))/3
    La1 <- -1050+((42775*x)/6)-(38075*.5*x^2)+((75125*x^3)-(48750*x^4)+(12500*x^5))/3
    La2 <- 1800-(12650*x)+((104800*x^2)-(142250*x^3)+(95000*x^4)-(25000*x^5))/3
    La3 <- -1575+(11350*x)+((-96575*x^2)+(134750*x^3)-(92500*x^4)+(25000*x^5))/3
    La4 <- 700+(14900*x^2)+(15000*x^4)-((15425*x)+(63875*x^3)+(12500*x^5))/3
    La5 <- -126+(1879*.5*x)+((-16625*.5*x^2)+(12125*x^3)-(8750*x^4)+(2500*x^5))/3

    LA <- c(La0,
            La1,
            La2,
            La3,
            La4,
            La5)
    ombar <- sum(Y*LA)
    absum <- nH*(1.028+(.75*x))+2
    a <- ombar*absum
    b <- (1-ombar)*absum
    omegabar <- ombar
    if (xs<.5){
      a <- (1-ombar)*absum
      b <- ombar*absum
      omegabar <- 1-ombar
      }

    na <- a-1
    nb <- b-1
    a_post <- a0 + na
    b_post <- b0 + nb
    a <- a_post
    b <- b_post

    post_mean <- a/(a+b)
    post_median <- qbeta(.5,a,b)

    eti_lower <- qbeta((1-prob_interval)*.5,
                       a,
                       b)
    eti_upper <- qbeta(1-(1-prob_interval)*.5,
                       a,
                       b)


    alphaL <- seq(0,(1-prob_interval),(1-prob_interval)/1000)
    qL <- qbeta(alphaL,a,b)
    qH <- qbeta(prob_interval+alphaL,a,b)
    diff <- qH-qL
    I <- 1
    mindiff <- min(diff)
    while (diff[I]>mindiff){
      I <- I+1
      }
    hdi_lower <- qL[I]
    hdi_upper <- qH[I]


    prH1 <- 1-pbeta(.5,a,b)
    priorprH1 <- 1-pbeta(.5,a0,b0)

    if ((prH1==1)|(priorprH1==0)){

      BF10 <- Inf
      } else {
        BF10 <- (prH1*(1-priorprH1))/(priorprH1*(1-prH1))
      }

        dfba_mann_whitney_large_list<-list(Emean=mean(E),
                                           Cmean=mean(C),
                                           n_E = nE,
                                           n_C = nC,
                                           U_E = UE,
                                           U_C = UC,
                                           prob_interval = prob_interval,
                                           a0 = a0,
                                           b0 = b0,
                                           a_post = a_post,
                                           b_post = b_post,
                                           post_mean = post_mean,
                                           post_median = post_median,
                                           method = method,
                                           omegabar = omegabar,
                                           eti_lower = eti_lower,
                                           eti_upper = eti_upper,
                                           hdi_lower = hdi_lower,
                                           hdi_upper = hdi_upper,
                                           priorprH1 = priorprH1,
                                           prH1 = prH1,
                                           BF10 = BF10)
               }


if(method == "small"){
  new("dfba_mann_whitney_small_out", dfba_mann_whitney_small_list)
} else {
    new("dfba_mann_whitney_large_out", dfba_mann_whitney_large_list)
  }
}
