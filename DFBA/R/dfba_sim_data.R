#' Simulated Data Generator and Inferential Comparison
#'
#' Computes the Bayesian probability that the population parameter is
#' greater than .5 as well as the frequentist p value via a t test. The
#' function is designed to be used for sampling from nine different continuous
#' univariate probability models. The models are the distribution on mlist
#' below When the design="independent", then the Bayesian analysis is in terms
#' of the Mann-Whitney statistic, and the t is the independent t-test.
#' When design="paired", then the Bayesian analysis is in terms of the Wilcoxon
#' signed-rank statistic, and t is paired t. The parameter delta is a
#' difference between the E and C conditions. The shape_vec components are
#' parameters for the E and C conditions. These shape_vec components are
#' described for each distributional model. For all models, there is a possible
#' blocking factor, which is a correlated values added to both the experimental
#' and control condition values. These add values are distributed as a uniform
#' on the zero to value of the blocking input. This added blocking factor has a
#' default of zero but positive values induces a correlation between the E and
#' C variates.
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
#' @param n number of values per group or paired sample
#' @param a0 shape parameter a of the prior beta distribution
#' @param b0 shape parameter b of the prior beta distribution
#' @param model hypothesized probability density function of data distribution
#' @param design one of "independent" or "paired" to indicate data structure
#' @param delta hypothesized mean difference between groups
#' @param shape1 First shape parameter for the distribution indicated by `model` input (default is `1`)
#' @param shape2 First shape parameter for the distribution indicated by `model` input (default is `1`)
#' @param block.max unclear.
#'
#' @return A list containing the following components:
#' @return \item{Tau}{Nonparametric Tau-a correlation}
#' @return \item{sample_p}{Sample concordance proportion}
#' @return \item{nc}{Number of concordant (x, y) pairs}
#' @return \item{nd}{Number of discordant (x, y) pairs}
#' @return \item{post.median}{Median of posterior distribution on phi}
#' @return \item{post.hdi.lower}{lower limit of the HDI with width specified by interval.width}
#' @return \item{post.hdi.upper}{upper limit of the HDI with width specified by interval.width}
#'
#' @references Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press.
#' @references Chechile, R.A., & Barch, D.H. (2021). Distribution-free, Bayesian goodness-of-fit method for assessing similar scientific prediction equations. Journal of Mathematical Psychology.



#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' @export
dfba_sim_data<-function(n=20,
                        a0=1,
                        b0=1,
                        model,
                        design,
                        delta,
                        shape1 = 1,
                        shape2 = 1,
#                        shape_vec=c(1,1),
                        block.max=0){


  if (delta<0){
    stop("The function requires a positive difference in the location of the two conditions.")}
  #else {}

  if (n<20){
    stop("The function requires an integer that is 20 or larger for sample size")}
  #else {}

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
    cat("The set of distributions for model are:"," ","\n")
    print(mlist)
    stop("The stipulated model is not on the list")
    }

#  designlist<-
  if (!design %in% c("paired","independent")){
    cat("The options for experimental design are:"," ","\n")
    cat("paired or independent"," ","\n")
    stop("The stipulated design is not on the list")
    }

  a0=a0
  b0=b0
  block.max=block.max

  if (block.max<0|(is.na(block.max))){
    stop("block.max must be nonnegative")
  }
  #else {}

   if ((a0<=0)|(b0<=0)|(is.na(a0))|(is.na(b0))){
     stop("Both a0 and b0 must be positive")
   }
  #else {}

  if (model=="normal"){
    #The delta parameter is a nonnegative offset for the normal distribution for the experimental
    #condition, and the shape_vec components are the standard deviations of the respective control and
    #experiment conditions.
  #  if((shape_vec[1]<=0)|(shape_vec[2]<=0)){
    if((shape1<=0)|(shape2<=0)){
#      stop("Both components of shape_vec are standard deviations and must be positive values.")
      stop("shape1 and shape2 are standard deviations and must be positive values.")
      }
  #  else {}
    C=rnorm(n,
            0,
            #shape_vec[1]
            shape1)
    E=rnorm(n,
            delta,
            #shape_vec[2]
            shape2)
  }
  #else {}

  if (model=="weibull"){
    #The delta parameter is a nonnegative offset for the Weibull distribution for the experimental
    #condition, and the shape_vec components are the shape parameters of the respective control and
    #experiment conditions. The scale factor for the Weibull distribution is 1 for both conditions
    #if((shape_vec[1]<=0)|(shape_vec[2]<=0)){
    if((shape1<=0)|(shape2<=0)){
#      stop("The components of shape_vec are the Weibull shape parameters for the respective control and experiment conditions and must be positive values.")
      stop("shape1 and shape2 are the Weibull shape parameters for the respective control and experiment conditions and must be positive values.")
      }
#    else {}
    C=rweibull(n,
               #shape_vec[1],
               shape1,
               1)
    E=delta + rweibull(n,
                       #shape_vec[2],
                       shape2,
                       1)
  }
  #else {}

  if (model=="cauchy"){
    #The delta parameter is a nonnegative offset for the Cauchy distribution for the experimental
    #condition, and the shape_vec components are the shape parameters of the respective control and
    #experiment conditions.
#    if((shape_vec[1]<=0)|(shape_vec[2]<=0)){
    if((shape1<=0)|(shape2<=0)){
#      stop("The components of shape_vec are the Cauchy distribution scale factor for the respective control and experiment conditions and must be positive values.")
      stop("shape1 and shape2 are the Cauchy distribution scale factor for the respective control and experiment conditions and must be positive values.")
      }
#    else {}
    C=rcauchy(n,
              0,
              #shape_vec[1]
              shape1)
    E=rcauchy(n,
              delta,
              #shape_vec[2]
              shape2)
    }
#  else {}

  if (model=="lognormal"){
    #The delta parameter is a nonnegative offset for a lognormal distribution for the experimental
    #condition, and the shape_vec components are standard deviations of the respective control and
    #experiment conditions.
    #if((shape_vec[1]<=0)|(shape_vec[2]<=0)){
    if((shape1<=0)|(shape2<=0)){
    #  stop("The components of shape_vec are the sdlog values for the respective control and experiment conditions and must be positive values.")
      stop("shape1 and shape2 are the sdlog values for the respective control and experiment conditions and must be positive values.")
      }
#    else {}
    C=rlnorm(n,
             0,
             shape1)
    E=rlnorm(n,
             delta,
             shape2)
    }
  #else {}

  if (model=="chisquare"){
    #The delta parameter is a nonnegative offset for a chi square random variable
    #for the experimental condition, and the shape_vec components are the df values
    #for the respective control and experimental conditions.
    #if((shape_vec[1]<=0)|(shape_vec[2]<=0)){
    if((shape1<=0)|(shape2<=0)){
    #  stop("Both components of shape_vec are df for respectively the control and experiment conditions and must be positive values.")
      stop("shape1 and shape2 are df for respectively the control and experiment conditions and must be positive values.")
    }
  #  else {}
    C=rchisq(n,
             #shape_vec[1]
             shape1)
    E=delta +(rchisq(n,
                     #shape_vec[2]
                     shape2))
  }
  #else {}

  if (model=="logistic"){
    #The delta parameter is a nonnegative offset for the logistic distribution for the experimental
    #condition, and the shape_vec components are the scale factors of the respective control and
    #experiment conditions.
    #if((shape_vec[1]<=0)|(shape_vec[2]<=0)){
    if((shape1<=0)|(shape2<=0)){
    #  stop("Both components of shape_vec are scale factors and must be positive values.")
      stop("shape1 and shape2 are scale factors and must be positive values.")
    }
    #else {}
    C=rlogis(n,
             0,
             #shape_vec[1]
             shape1)
    E=rlogis(n,
             delta,
             #shape_vec[2]
             shape2)
  }
  #else {}

  if (model=="exponential"){
    #The delta parameter is a nonnegative offset for the exponential distribution for the experimental
    #condition, and the shape_vec components are the rate parameters of the respective control and
    #experiment conditions.
#    if((shape_vec[1]<=0)|(shape_vec[2]<=0)){
    if((shape1<=0)|(shape2<=0)){
    #  stop("Both components of shape_vec are scale factors and must be positive values.")
      stop("shape1 and shape2 are scale factors and must be positive values.")
    }
#    else {}
    C=rexp(n,
           #shape_vec[1]
           shape1)
    E=delta+rexp(n,
                 #shape_vec[2]
                 shape2)
  }
  #else {}

  if (model=="gumbel"){
    #The delta parameter is the nonnegative offset for a Gumbel distribution for the experimental
    #condition, and the shape_vec components are the scale factors for the respective control
    #experiment conditions. The random scores are obtained via the inverse transform method.
    #if((shape_vec[1]<=0)|(shape_vec[2]<=0)){
    if((shape1<=0)|(shape2<=0)){
    #  stop("Both components of shape_vec are scale factors and must be positive values.")
      stop("shape1 and shape2 are scale factors and must be positive values.")
    }
#    else {}
    g=runif(n,.00001,.99999)
#    C=-shape_vec[1]*log(log(1/g))
    C=-shape1*log(log(1/g))
    g2=runif(n,.00001,.99999)
#    E=delta-shape_vec[2]*log(log(1/g2))
    E=delta-shape2*log(log(1/g2))
  }
  #else {}

  if (model=="pareto"){
    #The x_m parameter is 1 for the control condition and it is 1+delta for the experimental
    #condition. The alpha shape parameter for the respective control and experiment condition
    #are 1.16 times the shape_vec[1] and shape_vec[2] values. This function requires the alpha
    #to be at least 1.16 so shape_vec components should not be less than 1.
    #The random scores are obtained via the inverse transform method.
    #if((shape_vec[1]<1)|(shape_vec[2]<1)){
    if((shape1<1)|(shape2<1)){
    #  stop("Both components of shape_vec must be 1 or greater.")}
      stop("shape1 and shape2 must be greater than or equal to 1.")
    }
#  else {}
    g=runif(n,.00001,.99999)
#    alpha1=1.16*shape_vec[1]
    alpha1=1.16*shape1
    C=1/(1-g)^(1/alpha1)
#    alpha2=1.16*shape_vec[2]
    alpha2=1.16*shape2
    g2=runif(n,.00001,.99999)
    E=(1+delta)/(1-g2)^(1/alpha2)
  }
  #else {}



  if (design=="independent"){
    Bex=runif(n,0,block.max)
    Bcn=runif(n,0,block.max)
    E=E+Bex
    C=C+Bcn

    tnum=mean(E)-mean(C)
    tdem=sqrt((var(E)+var(C))/n)
    t_sample=tnum/tdem
    #pvalue=1-pt(t_sample,2*n-2)
    pvalue=pt(abs(t_sample),2*n-2, lower.tail=FALSE)

# Mann-Whitney (large)

#    UE_vector<-rep(NA, length(E)) # UE counter
#    UC_vector<-rep(NA, length(C)) # UC counter
#    for (i in 1:length(E)){
#      UE_vector[i]<-sum(E[i]>C)
#    }
#    for (j in 1:length(C)){
#      UC_vector[j]<-sum(C[j]>E)
#    }
#    UE=sum(UE_vector)
#    UC=sum(UC_vector)
#    nE=n
#    nC=n
#
#
#    xs=UE/(UE+UC)
#    if (xs>=.5) {x=xs} else {x=1-xs}
#    nH=(2*nE*nC)/(nE+nC)
#    y5=(nH^1.1489)/(.4972+(nH^1.1489))
#    w4=.8-(1/(1+(1.833*nH)))
#    w3=.6-(1/(1+(2.111*nH)))
#    w2=.4-(1/(1+(2.520*nH)))
#    w1=.2-(1/(1+(4.813*nH)))
#    y4=(y5*w4)+(1-w4)*.5
#    y3=(y5*w3)+(1-w3)*.5
#    y2=(y5*w2)+(1-w2)*.5
#    y1=(y5*w1)+(1-w1)*.5
#    Y=c(.5,y1,y2,y3,y4,y5)
#    La0=252-(1627*x)+((12500*x^2)-(15875*x^3)+(10000*x^4)-(2500*x^5))/3
#    La1=-1050+((42775*x)/6)-(38075*.5*x^2)+((75125*x^3)-(48750*x^4)+(12500*x^5))/3
#    La2=1800-(12650*x)+((104800*x^2)-(142250*x^3)+(95000*x^4)-(25000*x^5))/3
#    La3=-1575+(11350*x)+((-96575*x^2)+(134750*x^3)-(92500*x^4)+(25000*x^5))/3
#    La4=700+(14900*x^2)+(15000*x^4)-((15425*x)+(63875*x^3)+(12500*x^5))/3
#    La5=-126+(1879*.5*x)+((-16625*.5*x^2)+(12125*x^3)-(8750*x^4)+(2500*x^5))/3
#    LA=c(La0,La1,La2,La3,La4,La5)
#    ombar=sum(Y*LA)
#    absum=nH*(1.028+(.75*x))+2
#    a=ombar*absum
#    b=(1-ombar)*absum
#    omegabar=ombar
#    if (xs<.5){
#      a=(1-ombar)*absum
#      b=ombar*absum
#      omegabar=1-ombar} else {}
#    na=a-1
#    nb=b-1
#    apost=a0+na
#    bpost=b0+nb
#    a=apost
#    b=bpost
#    prH1=1-pbeta(.5,a,b)
#    outstat=c(prH1,pvalue)
#    return(outstat)} else {}
    independent_out<-list(pvalue = pvalue,
                          prH1 = dfba_mann_whitney(E,
                                            C,
                                            a0,
                                            b0,
                                            method="large")$prH1)
  }

  if (design=="paired"){
    B=runif(n,0,block.max)
    E=E+B
    C=C+B
    d=E-C

    t_sample=(sqrt(n)*mean(d))/sd(d)
    #pvalue=1-pt(t_sample,n-1)
    pvalue=pt(abs(t_sample),n-1, lower.tail=FALSE)

#    sdd=sd(d)
##    dt=(seq(1,n,1))*0
#    dt<-rep(NA, n)
#    IC=0
#    for (I in 1:n){
#      if (abs(d[I])<=sdd/30000){IC=IC} else {
#        IC=IC+1
#        dt[IC]=d[I]}}
#
#
#    dt<-ifelse(abs(d)<=sdd/30000, 0, d)
#    IC<-sum(d>sdd/30000)
#    n=IC
#
#    ##    dt=dt[1:n]
#    dta=abs(dt)
#    #The vector dtar is for the ranks of the absolute-value d scores; whereas
#    #the dta vector is for the absolute-value of the d scores, and dtars is
#    #the vector of signed rank scores.
#    dtar=rank(dta)
#    dtars=(seq(1,n,1))*0
#    for (I in 1:n){
#      dtars[I]=(dtar[I]*dt[I])/dta[I] }
#    dtars<-(dtar*dt)/dta
#
#
#    #The following computes the Tplus and Tminus statistics
#    tplus=0
#    for (I in 1:n){
#      if (dtars[I]>0){tplus=tplus+dtar[I]} else {tplus=tplus} }
#    tneg=(n*(n+1)*.5)-tplus
#
#
#    na0=a0-1
#    nb0=b0-1
#    term=(3*tplus)/((2*n)+2)
#    na=term-.25
#    nb=(((3*n)-1)/4)-term
#    a=na+na0+1
#    b=nb+nb0+1
#    prH1=1-pbeta(.5,a,b)
#    outstat=c(prH1,pvalue)
#    return(outstat)

    dependent_out<-list(pvalue=pvalue,
                        prH1 = dfba_wilcoxon(E,
                                             C,
                                             a0,
                                             b0,
                                             method="large")$prH1)
  }
    #else {}
  if(design=="independent"){
    independent_out
  }else{
    dependent_out
  }

}

#------------
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
