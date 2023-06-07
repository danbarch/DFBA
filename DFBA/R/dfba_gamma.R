#' Goodman-Kruskal Gamma
#'
#' Given bivariate data in the form of either a rank-ordered table or a matrix,
#' returns the number of concordant and discordant changes between the variates,
#' the Goodman-Kruskal gamma statistic, and a Bayesian analysis of the
#' population concordance proportion parameter \emph{phi}.
#'
#'
#' @param x Cross-tabulated matrix or table where cell [I, J] represents the frequency of observations where the rank of measure 1 is I and the rank of measure 2 is J.
#' @param a0 First shape parameter for the prior beta distribution (default is 1)
#' @param b0 Second shape parameter for the prior beta distribution (default is 1)
#' @param prob_interval Desired width for interval estimates (default is 0.95)
#'
#' @return A list containing the following components:
#' @return \item{gamma}{Sample Goodman-Kruskal gamma statistic; equivalent to the sample rank correlation coefficient tau_A}
#' @return \item{a0}{First shape parameter for prior beta}
#' @return \item{b0}{Second shape parameter for prior beta}
#' @return \item{sample.p}{Sample estimate for proportion concordance \code{nc/(nc+nd)}}
#' @return \item{nc}{Number of concordant comparisons between the paired measures}
#' @return \item{nd}{Number of discordant comparisons between the paired measures}
#' @return \item{a.post}{First shape parameter for the posterior beta distribution for the phi parameter}
#' @return \item{b.post}{Second shape parameter for the posterior beta distribution for the phi parameter}
#' @return \item{post.median}{Median of the posterior distribution for the phi concordance parameter}
#' @return \item{prob_interval}{The probability of the interval estimate for the phi parameter}
#' @return \item{post.eti.lower}{Lower limit of the posterior equal-tail interval for the phi parameter where the width of the interval is specified by the \code{prob_interval} input}
#' @return \item{post.eti.upper}{Upper limit of the posterior equal-tail interval for the phi parameter where the width of the interval is specified by the \code{prob_interval} input}
#'
#' @details
#' For bivariate data where two measures are restricted on an ordinal scale,
#' such as when the two variates are ranked data over a limited set of integers,
#' then an ordered contingency table is often a convenient data representation.
#' For such a case the element in the \eqn{[I, J]} cell of the matrix is the
#' frequency of occasions where one variate has a rank value of \eqn{I} and the
#' corresponding rank for the other variate is \eqn{J}. This situation is a
#' special case of the more general case where there are two continuous
#' bivariate measures. For the special case of a rank-order matrix with
#' frequencies, there is a distribution-free concordance correlation that is in
#' common usage: Goodman and Kruskal's gamma \eqn{G} (Siegel & Castellan, 1988).
#'
#' Chechile (2020) showed that Goodman and Kruskal's gamma is equivalent to the
#' more general \eqn{\tau_A} nonparametric correlation coefficient.
#' Historically, gamma was considered a different metric from \eqn{\tau} because
#' typically the version of \eqn{\tau} in standard use was \eqn{\tau_B}, which
#' is a flawed metric because it does not properly correct for ties. Note:
#' \code{cor(... ,method = "kendall")} returns the \eqn{\tau_B} correlation, which
#' is incorrect when there are ties. The correct \eqn{\tau_A} is computed by the
#' \code{dfba_bivariate_concordance()} function.
#'
#' The gamma statistic is equal to \eqn{(n_c-n_d)/(n_c+n_d)}, where \eqn{n_c} is
#' the number of occasions when the variates change in a concordant way and \eqn{n_d}
#' is the number of occasions when the variates change in a discordant fashion.
#' The value of \eqn{n_c} for an order matrix is the sum of terms for each \eqn{[I, J]}
#' that are equal to \eqn{n_{ij}N^{+}_{ij}}, where \eqn{n_{ij}} is the frequency
#' for cell \eqn{[I, J]} and \eqn{N^{+}_{ij}} is the sum of a frequencies in the
#' matrix where the row value is greater than \eqn{I} and where the column value is
#' greater than \eqn{J}. The value \eqn{n_d} is the sum of terms for each \eqn{[I, J]} that
#' are \eqn{n_{ij}N^{-}_{ij}}, where \eqn{N^{-}_{ij}} is the sum of the frequencies
#' in the matrix where row value is greater than \eqn{I} and the column value is
#' less than \eqn{J}. The \eqn{n_c} and \eqn{n_d} values computed in this fashion
#' are, respectively, equal to \eqn{n_c} and \eqn{n_d} values found when the bivariate
#' measures are entered as paired vectors into the \code{dfba_bivariate_concordance()} function.
#'
#' As with the \code{dfba_bivariate_concordance()} function, the Bayesian analysis focuses on the
#' population concordance proportion phi \eqn{(\phi)}; and \eqn{G=2\phi-1}. The
#' likelihood function is proportional to \eqn{\phi^{n_c}(1-\phi)^{n_d}}. The
#' prior distribution is a beta function, and the posterior distribution is the
#' conjugate beta where \code{a = a0 + nc} and
#' \code{b = b0 + nd}.

#' @references
#' Chechile, R.A. (2020). Bayesian Statistics for Experimental Scientists: A
#' General Introduction Using Distribution-Free Methods. Cambridge: MIT Press.
#'
#' Siegel, S., & Castellan, N. J. (1988) Nonparametric Statistics for the
#' Behavioral Sciences. New York: McGraw Hill.
#'
#' @seealso
#' \code{\link{dfba_bivariate_concordance}} for a more extensive discussion about the \eqn{\tau_A}
#' statistic and the flawed \eqn{\tau_B} correlation
#'
#' @examples

#' # Example with matrix input
#' N <- matrix(c(38, 4, 5, 0, 6, 40, 1, 2, 4, 8, 20, 30),
#'             ncol = 4,
#'             byrow = TRUE)
#' colnames(N) <- c('C1', 'C2', 'C3', 'C4')
#' rownames(N) <- c('R1', 'R2', 'R3')
#' dfba_gamma(N)
#'
#' # Sample problem with table input
#' T <- as.table(N)
#' dfba_gamma(T)

#' @export
dfba_gamma<-function(x,
#                     y = NULL,
#                     breaks_x = NULL,
#                     breaks_y = NULL,
                     a0 = 1,
                     b0 = 1,
                     prob_interval = 0.95
                     ){
#  if(is.matrix(x)==TRUE){
  if(is.matrix(x)==FALSE){stop("input must be in matrix or table format")
  }

  if(a0 <= 0|
     a0 == Inf|
     is.na(a0)|
     b0 <= 0|
     b0 == Inf|
     is.na(b0)){
    stop("Both the a0 and b0 shape parameters must be positive and finite.")
    }

  table<-x
    x_vec<-rep(1:nrow(table), unname(rowSums(table)))
    y_vec<-rep(as.vector(t(col(table))), as.vector(t(table)))
#  } else {
#    if(length(x)!=length(y)){
#      stop("x and y must have equal length")
#    }
#    if(is.numeric(x)){
#      if(is.null(breaks_x)){
#        stop("When x is numeric, either a numeric vector of two or more unique cut points or a single number of intervals into which x variable is to be cut must be specified")
#      }
#      x_cut<-cut(x, breaks_x, ...)
#    } else{
#      x_cut<-x
#    }
#    if(is.numeric(y)){
#      if(is.null(breaks_y)){
#        stop("When y is numeric, either a numeric vector of two or more unique cut points or a single number of intervals into which y variable is to be cut must be specified")
#      }
#      y_cut<-cut(y, breaks_y, ...)
#    } else{
#      y_cut<-y
#  }
    table<-(table(x_vec, y_vec))

#  }

  x<-rep(1:nrow(table), unname(rowSums(table)))
  y<-rep(as.vector(t(col(table))), as.vector(t(table)))

    dfba_gamma_list<-list(gamma=dfba_bivariate_concordance(x, y, a0, b0, prob_interval)$tau,
                        a0=a0,
                        b0=b0,
                sample.p=dfba_bivariate_concordance(x, y, a0, b0, prob_interval)$sample.p,
                nc=dfba_bivariate_concordance(x, y, a0, b0, prob_interval)$nc,
                nd=dfba_bivariate_concordance(x, y, a0, b0, prob_interval)$nd,
                a.post=dfba_bivariate_concordance(x, y, a0, b0, prob_interval)$a.post,
                b.post=dfba_bivariate_concordance(x, y, a0, b0, prob_interval)$b.post,
                prob_interval=prob_interval,
                post.median=dfba_bivariate_concordance(x, y, a0, b0, prob_interval)$post.median,
                post.eti.lower=dfba_bivariate_concordance(x, y, a0, b0, prob_interval)$post.eti.lower,
                post.eti.upper=dfba_bivariate_concordance(x, y, a0, b0, prob_interval)$post.eti.upper
#                table.row=x_vec,
#                table.column=y_vec
                )
  new("dfba_gamma_out", dfba_gamma_list)
}

