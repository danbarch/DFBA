\name{Gamma_Concordance}
\alias{Gamma_Concordance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Goodman-Kruskal Gamma Statistic with Bayesian Analysis
}
\description{
This function takes either two vectors of equal length OR a cross-tabulation of values and returns:
   the Goodman-Kruskal Gamma Statistic
   the observed concordance statistic p
   the concordance parameter Phi
   shape parameters a and b (alpha and beta) on the
     beta distribution that describes Phi
   Highest Density Interval (HDI) limits on Phi
}
\usage{
Gamma_Concordance(x, y=NULL, quantiles_x=NULL, quantiles_y=NULL, a.prior=1, b.prior=1, hdi.width=0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A vector or matrix of values.
}
  \item{y}{
  (optional) A vector of values with the same length as x. If x is a matrix, y is unnecessary and ignored.
}
  \item{quantiles_x}{
  (optional) If x and y are vectors, the desired number of quantiles of the x variable
}
  \item{quantiles_y}{
  (optional) If x and y are vectors, the desired number of quantiles of the x variable
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{gamma}{The Goodman-Kruskal Gamma Statistic}
  \item{sample.p}{Sample concordance proportion}
  \item{alpha}{Shape parameter alpha of the posterior (beta) distribution of the population concordance (phi)}
  \item{beta}{Shape parameter beta of the posterior (beta) distribution of the population concordance (phi)}
  \item{post.median}{Median of the posterior (beta) distribution of the population concordance (phi)}
  \item{post.hdi.lower}{Lower limit of the specified highest density interval (default is 95\%) of the population concordance (phi)}
  \item{post.hdi.upper}{Upper limit of the specified highest density interval (default is 95\%) of the population concordance (phi)}
}

\references{
Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists. Cambridge: MIT Press
}
\author{
Barch, D.H. & Chechile, R.A.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x<-c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5)
y<-c(1, 1, 4, 4, 5, 1, 2, 3, 5, 1, 2, 3, 3, 3, 3, 5, 2, 4, 4, 4, 5, 5, 3, 4, 4, 5, 5)

Gamma_Concordance(x = x, y = y, quantiles_x = 5, quantiles_y = 5)


$gamma
[1] 0.3670886

$sample.p
[1] 0.6835443

$alpha
[1] 163

$beta
[1] 76

$post.median
[1] 0.6825168

$post.hdi.lower
[1] 0.6217135

$post.hdi.upper
[1] 0.7394179

{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
