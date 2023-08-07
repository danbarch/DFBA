#' Repeated-Measures Test (Wilcoxon Signed-Ranks Test)
#'
#' Given two continuous, paired variates \code{Y1} and \code{Y2},
#' computes the sample \code{T_pos} and \code{T_neg} statistics for the Wilcoxon
#' signed-rank test and provides a Bayesian analysis for the population
#' sign-bias parameter \code{phi_w}, which is the population proportion of
#' positive differences.
#'
#' @importFrom stats sd
#' @importFrom stats pbeta
#' @importFrom stats rbinom
#'
#' @param Y1 Numeric vector for one continuous variate
#' @param Y2 Numeric vector for values paired with Y1 variate
#' @param a0 The first shape parameter for the prior beta distribution for \code{phi_w}. Must be positive and finite.
#' @param b0 The second shape parameter for the prior beta distribution for \code{phi_w}. Must be positive and finite.
#' @param prob_interval Desired probability for interval estimates of the sign bias parameter \code{phi_w} (default is 0.95)
#' @param samples When \code{method = "small"}, the number of desired Monte Carlo samples per candidate value for \code{phi_w} (default is 30000 per candidate phi)
#' @param method (Optional) The method option is either \code{"small"} or \code{"large"}. The "small" algorithm is based on a discrete Monte Carlo solution for cases where \emph{n} is typically less than 20. The \code{"large"} algorithm is based on beta approximation model for the posterior distribution for the \code{phi_w} parameter. This approximation is reasonable when \emph{n} > 19. Regardless of \emph{n} the user can stipulate either method. When the \code{method} argument is omitted, the program selects the appropriate procedure.
#' @param hide_progress (Optional) If \code{TRUE}, hide percent progress while Monte Carlo sampling is running when \code{method = SMALL}. (default is \code{FALSE}).
#'
#' @return A list containing the following components:
#' @return \item{T_pos}{Sum of the positive ranks in the pairwise comparisons}
#' @return \item{T_neg}{Sum of the negative ranks in the pairwise comparisons}
#' @return \item{n}{Number of nonzero differences for differences \code{d = Y1-Y2}}
#' @return \item{prob_interval}{User-defined probability for interval estimates for phi_w}
#' @return \item{samples}{The number of Monte Carlo samples per candidate phi_w for \code{method = "small"} (default is 30000)}
#' @return \item{method}{A character string that is either \code{"small"} or \code{"large"} for the algorithm used (default is NULL)}
#' @return \item{a0}{The first shape parameter for the beta prior distribution (default is 1)}
#' @return \item{b0}{The second shape parameter for the beta distribution prior (default is 1)}
#' @return \item{a_post}{First shape parameter for the posterior beta distribution}
#' @return \item{b_post}{Second shape parameter for the posterior beta distribution}
#' @return \item{phiv}{The 200 candidate values for phi_w for \code{method = "small"}}
#' @return \item{phipost}{The discrete posterior distribution for phi_w when \code{method = "small"}}
#' @return \item{priorprH1}{The prior probability that phi_w > .5}
#' @return \item{prH1}{The posterior probability for phi_w > .5}
#' @return \item{BF10}{Bayes factor for the relative increase in the posterior odds for the alternative hypothesis that phi_w > .5 over the null model for phi_w <= .5}
#' @return \item{post_mean}{The posterior mean for phi_w}
#' @return \item{cumulative_phi}{The posterior cumulative distribution for phi_w when \code{method = "small"}}
#' @return \item{hdi_lower}{The lower limit for the posterior highest-density interval estimate for phi_w}
#' @return \item{hdi_upper}{The upper limit for the posterior highest-density interval estimate for phi_w}
#' @return \item{a_post}{The first shape parameter for a beta distribution model for phi_w when \code{method = "large"}}
#' @return \item{b_post}{The second shape parameter for a beta distribution model for phi_w when \code{method = "large"}}
#' @return \item{post_median}{The posterior median for phi_w when \code{method = "large"}}
#' @return \item{eti_lower}{The equal-tail lower limit for phi_w}
#' @return \item{eti_upper}{The equal-tail upper limit for phi_w}
#'
#' @details
#'
#' The Wilcoxon signed-rank test is the frequentist nonparametric counterpart to
#' the paired \emph{t}-test. The procedure is based on the rank of the difference
#' scores \emph{d} = \code{Y1} - \code{Y2}.
#' The ranking is initially done on the absolute value of the nonzero \code{d} values,
#' and each rank is then multiplied by the sign of the difference. Differences
#' equal to zero are dropped. Since the procedure is based on only ranks of the
#' differences, it is robust with respect to outliers in either the \code{Y1} or
#' \code{Y2} measures. The procedure does not depend on the assumption of a
#' normal distribution for the two continuous variates.
#'
#' The sample \code{T_pos} statistic is the sum of the ranks that have a positive
#' sign, whereas \code{T_neg} is the positive sum of the ranks that have a
#' negative value. Given \emph{n} nonzero \emph{d} scores, \code{T_pos} +
#' \code{T_neg} = \emph{n}(\emph{n} + 1)/2. Tied ranks are possible, especially
#' when there are \code{Y1} and \code{Y2} values that have low precision. In
#' such cases, the Wilcoxon statistics are rounded to the nearest integer.
#'
#' The Bayesian analysis is based on a parameter \code{phi_w}, which is the
#' population proportion for positive \emph{d} scores. The default prior for \code{phi_w}
#' is a flat beta distribution with shape parameters \code{a0} = \code{b0} =1,
#' but the user can stipulate their preferred beta prior by assigning values for
#' \code{a0} and \code{b0}. The \code{prob_interval} input, which has a default
#' value of .95, is the value for interval estimates for the \code{phi_w}
#' parameter, but the user can alter this value if they prefer.
#'
#' There are two cases for the Bayesian analysis - one for a small number of
#' pairs and another for when there is a large number of pairs. The \code{method = small}
#' sample algorithm uses a discrete approximation where there are 200 candidate
#' values for phi_w, which are .0025 to .9975 in steps of .005. For each
#' candidate value for \code{phi_w}, there is a prior and posterior probability.
#' The posterior probability is based on Monte Carlo sampling to approximate the
#' likelihood for obtaining the observed  Wilcoxon statistics. That is, for each
#' candidate value for \code{phi_w}, thousands of Monte Carlo samples are
#' generated for the signs on the numbers (1,2, ..., n) where each number is
#' multiplied by the sign. The proportion of the samples that result in the
#' observed Wilcoxon statistics is an estimate for the likelihood value for that
#' candidate \code{phi_w}. The likelihood values along with the prior result in
#' a discrete posterior distribution for \code{phi_w}. The default for the
#' number of Monte Carlo samples per candidate \code{phi_w} is the input
#' quantity called \code{samples}. The default value for samples is 30000,
#' but this quantity can be altered by the user.
#'
#' Chechile (2018) empirically found that for large \emph{n} there was a beta
#' distribution that approximated the quantiles of the discrete, small sample
#' approach. This approximation is reasonably accurate for \emph{n} > 24, and is
#' used when \code{method = "large"}.
#'
#' If the \code{method} argument is omitted, the function employs the method
#' that is appropriate given the sample size. Note: the \code{method = "small"}
#' algorithm is slower than the algorithm for \code{method = "large"}; for cases
#' where \emph{n} > 24, \code{method = "small"} and \code{method = "large"} will
#' produce similar estimates but the former method requires increased
#' processing time.
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental
#' Scientists: A General Introduction to Distribution-Free Methods.
#' Cambridge: MIT Press.
#'
#' Chechile, R. A. (2018) A Bayesian analysis for the Wilcoxon signed-rank
#' statistic. Communications in Statistics - Theory and Methods,
#' https://doi.org/10.1080/03610926.2017.1388402
#'
#'
#' @examples
#' ## Examples with a small number of pairs
#' conditionA <- c(1.49, 0.64, 0.96, 2.34, 0.78, 1.29, 0.72, 1.52, 0.62, 1.67,
#'                 1.19, 0.86)
#' conditionB <- c(0.53, 0.55, 0.58, 0.97, 0.60, 0.22, 0.05, 13.14, 0.63, 0.33,
#'                 0.91, 0.37)
#'
#' dfba_wilcoxon(Y1 = conditionA,
#'               Y2 = conditionB,
#'               hide_progress = TRUE)
#'
#' # Note the results for this method="small" analysis differs from
#' # the previously run. These differences are the differences from
#' # different Monte Carlo sampling
#'
#' # Using the Jeffreys prior for the same two conditions.
#'
#' dfba_wilcoxon(conditionA,
#'               conditionB,
#'               a0 = .5,
#'               b0 = .5,
#'               hide_progress = TRUE)
#'
#' # Using 99% interval estimates and with 50000 Monte Carlo samples per
#' # candidate phi_w
#'
#' dfba_wilcoxon(conditionA,
#'               conditionB,
#'               prob_interval=.99,
#'               samples=50000,
#'               hide_progress = TRUE)
#'
#' # Examples with large sample size
#'
#' E <- c(6.45, 5.65, 4.34, 5.92, 2.84, 13.06, 6.61, 5.47, 4.49, 6.39, 6.63,
#'        3.55, 3.76, 5.61, 7.45, 6.41, 10.16, 6.26, 8.46, 2.29, 3.16, 5.68,
#'        4.13, 2.94, 4.87, 4.44, 3.13, 8.87)
#'
#' C <- c(2.89, 4.19, 3.22, 6.50, 3.10, 4.19, 5.13, 3.77, 2.71, 2.58, 7.59,
#'        2.68, 4.98, 2.35, 5.15, 8.46, 3.77, 8.83, 4.06, 2.50, 5.48, 2.80,
#'        8.89, 3.19, 9.36, 4.58, 2.94, 4.75)
#'
#' BW<-dfba_wilcoxon(Y1 = E,
#'                   Y2 = C)
#' BW
#' plot(BW)
#'
#'# Forcing the method="small" despite a sufficiently large n value
#'
#'CW<-dfba_wilcoxon(Y1 = E,
#'                  Y2 = C,
#'                  method = "small",
#'                  hide_progress = TRUE)
#'CW
#'plot(CW)
#'plot(CW, plot.prior = FALSE)

#' @export
dfba_wilcoxon<-function(Y1,
                        Y2,
                        a0 = 1,
                        b0 = 1,
                        prob_interval = .95,
                        samples = 30000,
                        method = NULL,
                        hide_progress = FALSE){
  l1 <- length(Y1)
  l2 <- length(Y2)
  if (l1!=l2) {
    stop("Y1 and Y2 must have the same length. This function is for paired within-block data.")
  }

  if (a0<=0|
      a0 == Inf|
      is.na(a0)|
      b0<=0|
      b0 == Inf|
      is.na(b0)){
    stop("Both a0 and b0 must be positive and finite")
    }
  if ((prob_interval<0)|(prob_interval>1)){
    stop("The probability for the interval estimate of phi_w must be a proper proportion.")
    }
  if (samples<10000){
    stop("stipulating Monte Carlo samples < 10000 is too few")
  }
  #Following code checks for NA values and cleans the difference scores
  Etemp <- Y1
  Ctemp <- Y2
  d <- Y1 - Y2
  dtemp <- d
  jc <- 0
  for (j in seq_along(Y1)){
    if (is.na(dtemp[j])){} else {
      jc <- jc+1
      Y1[jc] <- Etemp[j]
      Y2[jc] <- Ctemp[j]
      d[jc]<- dtemp[j]}
  }
  Y1 <- Y1[1:jc]
  Y2 <- Y2[1:jc]
  d <- d[1:jc]
  l1 <- jc

  if (l1<3){
    stop("There are not enough values in the Y1 and Y2 vectors for meaningful results.")
    }

# The following code computes the within-block difference scores, and finds the
# number of blocks where the difference scores are nonzero (within a trivial rounding error).
  sdd <- sd(d)
  IC <- 0
  for (I in 1:l1){
    if (abs(d[I])<=sdd/30000){
      IC <- IC
    } else {
        IC <- IC+1
    }
    }
  n <- IC
# The following code deals with the case where all differences are trivially close to 0.
  if (n==0){stop("Y1 and Y2 differences are all trivial")
    }

# The following finds the Tpos and Tneg stats and the number of nonzero blocks
  dt <- rep(0, n)
  IC <- 0
  for (I in 1:l1){
    if (abs(d[I])<=sdd/30000){
      IC <- IC
      } else {
      IC <- IC+1
      dt[IC] <- d[I]}}
  dta <- rep(0, n)
  for (I in 1:n){
    dta[I] <- abs(dt[I])}
# The vector dtar is for the ranks of the absolute-value d scores; whereas
# the dta vector is for the absolute-value of the d scores, and dtars is
# the vector of signed rank scores.
  dtar <- rank(dta)
  dtars <- dtar*dt/dta
# The following computes the Tpos and Tneg statistics
  tpos <- 0
  for (I in 1:n){
    if (dtars[I]>0){
      tpos <- tpos+dtar[I]
    } else {
      tpos <- tpos
      }
  }
  tpos <- round(tpos)
  tneg <- (n*(n+1)*.5)-tpos

  if (is.null(method)){
    if (n > 24){
      method <- "large"
      } else {
      method <- "small"}
  }

  if (method=="small"){

# Code for the discrete prior
    phiv <- seq(1/400,.9975,.005)
    x <- phiv+.0025
    priorvector <- rep(0,200)
    priorvector[1] <- pbeta(x[1],a0,b0)
    for (i in 2:200){
      priorvector[i] <- pbeta(x[i],a0,b0)-pbeta(x[i-1],a0,b0)
      }

    fphi<-rep(0.0,200)
    for (j in 1:200){
      if (hide_progress == FALSE) {
        cat(round(j/200, 2)*100, '% complete', '\r')
      }
      phi <- 1/(400)+(j-1)*(1/200)
      for (k in 1:samples){
        tz <- sum((1:n)*rbinom(n, 1, phi))

        if(tz==tpos) {
          fphi[j] <- fphi[j]+1.0
        } else{
          fphi[j] <- fphi[j]
        }
      }
    }
    if (hide_progress == FALSE) {
      cat("\n")
    }

# The tot value below is the denominator of the discrete analysis,
# phipost is the vector for the posterior distribution.
    tot <- sum(priorvector*fphi)
    phipost <- (priorvector*fphi)/tot
    phiv <- rep(0.0,200)
    phibar <- 0.0
    for (j in 1:200){
      phiv[j] <- (1/400)+(j-1)*(1/200)
      phibar <- phibar+(phiv[j]*phipost[j])
      }

    postdis<-data.frame(phiv,phipost)


# The following finds the posterior cumulative distribution
# and outputs those values.
    cumulative_phi <- cumsum(phipost)

    I <- 1
    while (cumulative_phi[I]<(1-prob_interval)/2){
      I <- I+1}
    qLbelow <- phiv[I]-.0025

    if (I!=1){
      extrap <- (1-prob_interval)/2-cumulative_phi[I-1]
      probI <- cumulative_phi[I]-cumulative_phi[I-1]
      } else {
        extrap <- (1-prob_interval)/2
        probI <- cumulative_phi[1]
        }
    hdi_lower <- qLbelow+(.005)*(extrap/probI)

    I <- 1
    while (cumulative_phi[I]<1-(1-prob_interval)/2){
      I <- I+1}
    qHbelow <- phiv[I]-.0025
    extrapup <- 1-((1-prob_interval)/2)-cumulative_phi[I-1]
    probIu <- cumulative_phi[I]-cumulative_phi[I-1]
    hdi_upper <- qHbelow+(.005)*(extrapup/probIu)

# The prH1 is the probability that phi_w is greater than .5.
    prH1 <- 1-cumulative_phi[round(100)]
    cumulative_prior <- cumsum(priorvector)
    priorprH1 <- 1-cumulative_prior[round(100)]

# Following finds the Bayes factor for phi_w being greater than .5.

    if ((prH1==1)|(priorprH1==0)){
      BF10 <- samples
      }
      else {
        BF10 <- (prH1*(1-priorprH1))/(priorprH1*(1-prH1))
        }

  dfba_wilcoxon_small_list<-list(T_pos=tpos,
                                 T_neg=tneg,
                                 n = n,
                                 prob_interval = prob_interval,
                                 samples = samples,
                                 method = method,
                                 a0 = a0,
                                 b0 = b0,
                                 priorvector = priorvector,
                                 priorprH1 = priorprH1,
                                 phiv = phiv,
                                 phipost = phipost,
                                 prH1 = prH1,
                                 BF10 = BF10,
                                 phibar = phibar,
                                 hdi_lower = hdi_lower,
                                 hdi_upper = hdi_upper,
                                 cumulative_phi = cumulative_phi)
  } else {
    # method="large"

    #The following code finds the shape parameters of a beta
    #distribution that approximates the posterior distribution
    #for the phi_w parameter
    na0 <- a0-1
    nb0 <- b0-1
    term <- (3*tpos)/((2*n)+2)
    na <- term-.25
    nb <- (((3*n)-1)/4)-term
    a_post <- na + na0 + 1
    b_post <- nb + nb0 + 1

    post_mean <- a_post/(a_post + b_post)
    post_median <- qbeta(.5,
                        a_post,
                        b_post)

    eti_lower <- qbeta((1-prob_interval)*.5,
                     a_post,
                     b_post)

        eti_upper <- qbeta(1-(1-prob_interval)*.5,
                         a_post,
                         b_post)

    alphaL <- seq(0,(1-prob_interval),(1-prob_interval)/1000)
    qL <- qbeta(alphaL,
                a_post,
                b_post)
    qH <- qbeta(prob_interval+alphaL,
                a_post,
                b_post)
    diff <- qH-qL
    I <- 1
    mindiff <- min(diff)
    while (diff[I]>mindiff){
      I <- I+1
      }
    hdi_lower <- qL[I]
    hdi_upper <- qH[I]
    probpercent <- 100*prob_interval

    prH1 <- 1-pbeta(.5,
                    a_post,
                    b_post)
    priorprH1 <- 1-pbeta(.5,
                         na0 + 1,
                         nb0 + 1)

    if ((prH1==1)|(priorprH1==0)){
      BF10 <- Inf
      } else {
        BF10 <- (prH1*(1-priorprH1))/(priorprH1*(1-prH1))
        }

  dfba_wilcoxon_large_list<-list(T_pos=tpos,
                                 T_neg=tneg,
                                 n = n,
                                 prob_interval = prob_interval,
                                 samples = samples,
                                 method = method,
                                 a0 = a0,
                                 b0 = b0,
                                 a_post = a_post,
                                 b_post = b_post,
                                 post_mean = post_mean,
                                 post_median = post_median,
                                 priorprH1=priorprH1,
                                 prH1 = prH1,
                                 BF10 = BF10,
                                 eti_lower = eti_lower,
                                 eti_upper = eti_upper,
                                 hdi_lower = hdi_lower,
                                 hdi_upper = hdi_upper)
    }

  if ((method!="large")&(method!="small")) {
    stop("An explicit method stipulation must be either the word large or the word small.")
  }

  if(method == "small"){
    new("dfba_wilcoxon_small_out", dfba_wilcoxon_small_list)
  } else {
    new("dfba_wilcoxon_large_out", dfba_wilcoxon_large_list)
  }
}

