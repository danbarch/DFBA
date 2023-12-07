pkgname <- "DFBA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "DFBA-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('DFBA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("dfba_bayes_vs_t_power")
### * dfba_bayes_vs_t_power

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_bayes_vs_t_power
### Title: Simulated Distribution-Free Bayesian Power and _t_ Power
### Aliases: dfba_bayes_vs_t_power

### ** Examples


# Note: these examples have long runtimes due to Monte Carlo sampling;
# please feel free to run them in the console.

# Examples for two data sets sampled from standard normal distributions with
# no blocking effect

## Not run: 
##D dfba_bayes_vs_t_power(n_min = 40,
##D                       delta = .45,
##D                       model = "normal",
##D                       design = "paired",
##D                       hide_progress = FALSE)
##D 
##D dfba_bayes_vs_t_power(n_min = 40,
##D                       delta = .45,
##D                       model = "normal",
##D                       design = "independent",
##D                       hide_progress = FALSE)
##D 
##D # Examples with Weibull-distributed variates with no blocking effect
##D 
##D dfba_bayes_vs_t_power(n_min = 50,
##D                       delta = .45,
##D                       model = "weibull",
##D                       design ="paired",
##D                       hide_progress = FALSE)
##D 
##D dfba_bayes_vs_t_power(n_min = 50,
##D                       delta = .45,
##D                       model = "weibull",
##D                       design = "independent",
##D                       hide_progress = FALSE)
##D 
##D # Examples with Weibull-distributed variates with a blocking effect
##D 
##D dfba_bayes_vs_t_power(n_min = 50,
##D                       delta = .45,
##D                       model = "weibull",
##D                       design = "independent",
##D                       shape1 = .8,
##D                       shape2 = .8,
##D                       block_max = 2.3,
##D                       hide_progress = FALSE)
##D 
##D dfba_bayes_vs_t_power(n_min = 50,
##D                       delta = .45,
##D                       model = "weibull",
##D                       design = "paired",
##D                       shape1 = .8,
##D                       shape2 = .8,
##D                       block_max = 2.3,
##D                       hide_progress = FALSE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_bayes_vs_t_power", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_beta_bayes_factor")
### * dfba_beta_bayes_factor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_beta_bayes_factor
### Title: Bayes Factor for Posterior Beta Distribution
### Aliases: dfba_beta_bayes_factor

### ** Examples

## Examples with the default uniform prior
dfba_beta_bayes_factor(a_post = 17,
                       b_post = 5,
                       method = "interval",
                       H0 = c(0, .5)
                       )
dfba_beta_bayes_factor(a_post = 377,
                       b_post = 123,
                       method = "point",
                       H0 = .75)

# An example with the Jeffreys prior
dfba_beta_bayes_factor(a_post = 377.5,
                       b_post = 123.5,
                       method = "point",
                       H0 = .75,
                       a0 = .5,
                       b0 = .5
                       )


dfba_beta_bayes_factor(a_post = 273,
                       b_post = 278,
                       method = "interval",
                       H0 = c(.4975,
                              .5025)
                       )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_beta_bayes_factor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_beta_contrast")
### * dfba_beta_contrast

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_beta_contrast
### Title: Bayesian Contrasts
### Aliases: dfba_beta_contrast

### ** Examples

## Suppose there are four conditions from a factorial design
# where the conditions labels are A1B1, A2B1, A1B2, and A2B2
# where the frequencies for success for the binomial variate are:
n1_vec <- c(22, 15, 13, 21)
# and the frequencies for failures per condition are:
n2_vec <- c(18, 25, 27, 19)
# Let us test the following three orthogonal contrasts
contrast.B1vsB2 <- c(.5, .5, -.5, -.5)
contrast.A1vsA2 <- c(.5, -.5, .5, -.5)
contrast.ABinter <- c(.5, -.5, -.5, .5)

dfba_beta_contrast(n1_vec = n1_vec,
                   n2_vec = n2_vec,
                   contrast_vec = contrast.B1vsB2)

dfba_beta_contrast(n1_vec,
                   n2_vec,
                   contrast_vec = contrast.A1vsA2)

dfba_beta_contrast(n1_vec,
                   n2_vec,
                   contrast_vec = contrast.ABinter)

# Plot the cumulative distribution for AB interaction
testABinteraction<-dfba_beta_contrast(n1_vec,
                                      n2_vec,
                                      contrast_vec = contrast.ABinter)
plot(testABinteraction)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_beta_contrast", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_beta_descriptive")
### * dfba_beta_descriptive

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_beta_descriptive
### Title: Descriptive Statistics for a Beta Distribution
### Aliases: dfba_beta_descriptive

### ** Examples


dfba_beta_descriptive(a = 38,
                      b = 55)

dfba_beta_descriptive(38,
                      55,
                      prob_interval=.99)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_beta_descriptive", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_binomial")
### * dfba_binomial

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_binomial
### Title: Bayesian Binomial Rate Parameter Inference
### Aliases: dfba_binomial

### ** Examples

# Example using defaults of a uniform prior and 95% interval estimates
dfba_binomial(n1 = 16,
              n2 = 2)

 # Example with the Jeffreys prior and 99% interval estimates
dfba_binomial(n1 = 16,
              n2 = 2,
              a0 = .5,
              b0 = .5,
              prob_interval = .99)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_binomial", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_bivariate_concordance")
### * dfba_bivariate_concordance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_bivariate_concordance
### Title: Bayesian Distribution-Free Correlation and Concordance
### Aliases: dfba_bivariate_concordance

### ** Examples



x <- c(47, 39, 47, 42, 44, 46, 39, 37, 29, 42, 54, 33, 44, 31, 28, 49, 32, 37, 46, 55, 31)
y <- c(36, 40, 49, 45, 30, 38, 39, 44, 27, 48, 49, 51, 27, 36, 30, 44, 42, 41, 35, 49, 33)

dfba_bivariate_concordance(x, y)

## A goodness-of-fit example for a hypothetical case of fitting data in a
## yobs vector with prediction model

p = seq(.05,.95,.05)
ypred= 17.332 - (50.261*p) + (48.308*p^2)

# Note the coefficients in the ypred equation were found first via a
# polynomial regression

yobs<-c(19.805, 10.105, 9.396, 8.219, 6.110, 4.543, 5.864, 4.861, 6.136,
         5.789,  5.443, 5.548, 4.746, 6.484, 6.185, 6.202, 9.804, 9.332,
         14.408)

dfba_bivariate_concordance(x = yobs,
         y = ypred,
         fitting.parameters = 3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_bivariate_concordance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_gamma")
### * dfba_gamma

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_gamma
### Title: Goodman-Kruskal Gamma
### Aliases: dfba_gamma

### ** Examples

# Example with matrix input
N <- matrix(c(38, 4, 5, 0, 6, 40, 1, 2, 4, 8, 20, 30),
            ncol = 4,
            byrow = TRUE)
colnames(N) <- c('C1', 'C2', 'C3', 'C4')
rownames(N) <- c('R1', 'R2', 'R3')
dfba_gamma(N)

# Sample problem with table input
NTable <- as.table(N)
dfba_gamma(NTable)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_gamma", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_mann_whitney")
### * dfba_mann_whitney

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_mann_whitney
### Title: Independent Samples Test (Mann Whitney U)
### Aliases: dfba_mann_whitney

### ** Examples


# Note: examples with method = "small" have long runtimes due to Monte Carlo
# sampling; please feel free to run them in the console.

# Examples with large n per group
# The data for each condition are presorted only for the user convenience if
# checking the U stats by hand

groupA <- c(43, 45, 47, 50, 54, 58, 60, 63, 69, 84, 85, 91, 99, 127, 130,
            147, 165, 175, 193, 228, 252, 276)
groupB <- c(0, 01, 02, 03, 05, 14, 15, 23, 23, 25, 27, 32, 57, 105, 115, 158,
            161, 181, 203, 290)

dfba_mann_whitney(E = groupA,
                  C = groupB)

# The following uses a Jeffreys prior instead of a default flat prior:
dfba_mann_whitney(E = groupA,
                  C = groupB,
                  a0 = .5,
                  b0 =.5)

# The following also uses a Jeffreys prior but the analysis reverses the
# variates:
dfba_mann_whitney(E = groupB,
                  C = groupA,
                  a0 = .5,
                  b0 = .5)

# Note that BF10 from the above analysis is 1/BF10 from the original order
# of the variates.

# The next analysis constructs 99% interval estimates with the Jeffreys
# prior.

dfba_mann_whitney(E = groupA,
                  C = groupB,
                  a0 = .5,
                  b0 = .5,
                  prob_interval=.99)

# The following forces a discrete approach with a flat prior for a case with
# large n:


## Not run: 
##D dfba_mann_whitney(E = groupA,
##D                   C = groupB,
##D                   method = "small",
##D                   hide_progress = FALSE)
## End(Not run)

#Examples with small n per group

groupC <- c(96.49, 96.78, 97.26, 98.85, 99.75, 100.14, 101.15, 101.39,
            102.58, 107.22, 107.70, 113.26)
groupD <- c(101.16, 102.09, 103.14, 104.70, 105.27, 108.22, 108.32, 108.51,
            109.88, 110.32, 110.55, 113.42)

## Not run: 
##D CDex1<-dfba_mann_whitney(E = groupC,
##D                          C = groupD,
##D                          hide_progress = FALSE)
##D 
##D CDex1
##D 
##D CDex2<-dfba_mann_whitney(E = groupC,
##D                          C = groupD,
##D                          samples = 50000,
##D                          hide_progress = FALSE)
##D CDex2
##D 
##D 
##D CDex3<-dfba_mann_whitney(E = groupC,
##D                          C = groupD,
##D                          hide_progress = FALSE)
##D 
##D CDex3
##D 
## End(Not run)
# Note that CDex1 and CDex2 are replication analyses for the discrete approach.
# The variability is due to the different outcomes from the Monte Carlo
# sampling.

# Plot output
# with prior and posterior curves

## Not run: 
##D 
##D plot(CDex1)
##D 
##D # with only posterior curve
##D plot(CDex2,
##D      plot.prior = FALSE)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_mann_whitney", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_mcnemar")
### * dfba_mcnemar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_mcnemar
### Title: Bayesian Repeated-Measures McNemar Test for Change
### Aliases: dfba_mcnemar

### ** Examples

## Examples with default value for a0, b0 and prob_interval

dfba_mcnemar(n_01 = 17,
             n_10 = 2)

## Using the Jeffreys prior and .99 equal-tail interval

dfba_mcnemar(n_01 = 17,
             n_10 = 2,
             a0 = .5,
             b0 = .5,
             prob_interval = .99)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_mcnemar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_median_test")
### * dfba_median_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_median_test
### Title: Bayesian Median Test
### Aliases: dfba_median_test

### ** Examples


## Example with the default uniform prior
group1 <- c(12.90, 10.84, 22.67, 10.64, 10.67, 10.79, 13.55, 10.95, 12.19,
            12.76, 10.89, 11.02, 14.27, 13.98, 11.52, 13.49, 11.22, 15.07,
            15.74, 19.00)

group2 <- c(4.63, 58.64, 5.07, 4.66, 4.13, 3.92, 3.39, 3.57, 3.56, 3.39)

dfba_median_test(E = group1,
                 C = group2)

## Example with the Jeffreys prior
dfba_median_test(group1,
                 group2,
                 a0 = .5,
                 b0 = .5)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_median_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_power_curve")
### * dfba_power_curve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_power_curve
### Title: Power Curves
### Aliases: dfba_power_curve

### ** Examples


# Note: these examples have long runtimes due to Monte Carlo sampling;
# please feel free to run them in the console.

## Not run: 
##D dfba_power_curve(n = 85,
##D                  model = "normal",
##D                  design = "independent",
##D                  hide_progress = FALSE)
##D 
##D dfba_power_curve(n = 85,
##D                  model = "normal",
##D                  design = "paired",
##D                  hide_progress = FALSE)
##D 
##D dfba_power_curve(n = 85,
##D                  model = "normal",
##D                  design = "paired",
##D                  delta_step = .03,
##D                  hide_progress = FALSE)
##D 
##D dfba_power_curve(n = 30,
##D                  model = "lognormal",
##D                  design = "independent",
##D                  delta_step = .06,
##D                  block_max = 3,
##D                  samples = 2500,
##D                  hide_progress = FALSE)
##D 
##D dfba_power_curve(n = 30,
##D                  model = "lognormal",
##D                  design = "paired",
##D                  delta_step =.06,
##D                  block_max = 3,
##D                  samples = 2500,
##D                  hide_progress = FALSE)
##D 
##D # Using the Jeffreys prior rather than default flat prior
##D 
##D dfba_power_curve(n = 30,
##D                  model = "lognormal",
##D                  design = "independent",
##D                  a0 = .5,
##D                  b0 = .5,
##D                  delta_step = .06,
##D                  block_max = 3,
##D                  samples = 2500,
##D                  hide_progress = FALSE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_power_curve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_sign_test")
### * dfba_sign_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_sign_test
### Title: Bayesian Sign Test
### Aliases: dfba_sign_test

### ** Examples


measure_1 <- c(1.49, 0.64, 0.96, 2.34, 0.78, 1.29, 0.72, 1.52,
               0.62, 1.67, 1.19, 0.860)

measure_2 <- c(0.53, 0.55, 0.58, 0.97, 0.60, 0.22, 0.05, 13.14,
               0.63, 0.33, 0.91, 0.37)

dfba_sign_test(Y1 = measure_1,
               Y2 = measure_2)

dfba_sign_test(measure_1,
               measure_2,
               a0 = .5,
               b0 = .5,
               prob_interval = .99)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_sign_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_sim_data")
### * dfba_sim_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_sim_data
### Title: Simulated Data Generator and Inferential Comparison
### Aliases: dfba_sim_data

### ** Examples


# Example of two paired normal distributions where the s.d. of the two
# conditions are 1 and 4.

dfba_sim_data(n = 50,
             model = "normal",
             design = "paired",
             delta = .4,
             shape1 = 1,
             shape2 = 4)

# Example of two independent Weibull variates with their shape parameters =.8
# and with a .25 offset

dfba_sim_data(n = 80,
              model = "weibull",
              design = "independent",
              delta = .25,
              shape1 = .8,
              shape2 = .8)

# Example of two independent Weibull variates with their shape
# parameters = .8 and with a .25 offset along with some block differences
# with the max block effect being 1.5

dfba_sim_data(n = 80,
             model = "weibull",
             design = "independent",
             delta = .25,
             shape1 = .8,
             shape2 = .8,
             block_max = 1.5)

# Example of two paired Cauchy variates with a .4 offset

dfba_sim_data(n = 50,
             model = "cauchy",
             design = "paired",
             delta = .4)
# Example of two paired Cauchy variates with a .4 offset where the Bayesian
# analysis uses the Jeffreys prior

dfba_sim_data(n = 50,
             a0 = .5,
             b0 = .5,
             model = "cauchy",
             design = "paired",
             delta=.4)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_sim_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_wilcoxon")
### * dfba_wilcoxon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_wilcoxon
### Title: Repeated-Measures Test (Wilcoxon Signed-Ranks Test)
### Aliases: dfba_wilcoxon

### ** Examples


# Note: examples with method = "small" have long runtimes due to Monte Carlo
# sampling; please feel free to run them in the console.

## Examples with a small number of pairs



conditionA <- c(1.49, 0.64, 0.96, 2.34, 0.78, 1.29, 0.72, 1.52, 0.62, 1.67,
                1.19, 0.86)
conditionB <- c(0.53, 0.55, 0.58, 0.97, 0.60, 0.22, 0.05, 13.14, 0.63, 0.33,
                0.91, 0.37)

## Not run: 
##D dfba_wilcoxon(Y1 = conditionA,
##D               Y2 = conditionB,
##D               hide_progress = FALSE)
## End(Not run)

# Note the results for this method="small" analysis differs from
# the previously run. These differences are the differences from
# different Monte Carlo sampling

# Using the Jeffreys prior for the same two conditions.

## Not run: 
##D dfba_wilcoxon(conditionA,
##D               conditionB,
##D               a0 = .5,
##D               b0 = .5,
##D               hide_progress = FALSE)
## End(Not run)

# Using 99% interval estimates and with 50000 Monte Carlo samples per
# candidate phi_w

## Not run: 
##D 
##D dfba_wilcoxon(conditionA,
##D               conditionB,
##D               prob_interval=.99,
##D               samples=50000,
##D               hide_progress = FALSE)
## End(Not run)

# Examples with large sample size

E <- c(6.45, 5.65, 4.34, 5.92, 2.84, 13.06, 6.61, 5.47, 4.49, 6.39, 6.63,
       3.55, 3.76, 5.61, 7.45, 6.41, 10.16, 6.26, 8.46, 2.29, 3.16, 5.68,
       4.13, 2.94, 4.87, 4.44, 3.13, 8.87)

C <- c(2.89, 4.19, 3.22, 6.50, 3.10, 4.19, 5.13, 3.77, 2.71, 2.58, 7.59,
       2.68, 4.98, 2.35, 5.15, 8.46, 3.77, 8.83, 4.06, 2.50, 5.48, 2.80,
       8.89, 3.19, 9.36, 4.58, 2.94, 4.75)

BW<-dfba_wilcoxon(Y1 = E,
                  Y2 = C)
BW
plot(BW)

# Forcing the method="small" despite a sufficiently large n value


## Not run: 
##D CW<-dfba_wilcoxon(Y1 = E,
##D                  Y2 = C,
##D                  method = "small",
##D                  hide_progress = FALSE)
##D CW
##D plot(CW)
##D plot(CW, plot.prior = FALSE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_wilcoxon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
