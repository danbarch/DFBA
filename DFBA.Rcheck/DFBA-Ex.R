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
nameEx("Beta_betweengroups")
### * Beta_betweengroups

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Beta_betweengroups
### Title: Bayesian Contrasts Compute contrasts for binomial data from
###   multiple conditions
### Aliases: Beta_betweengroups

### ** Examples

wts<-c(1/3,1/3,1/3,-.5,-.5)
n1v<-c(8,7,10,3,6) # Successes per condition
n2v<-c(4,5,2,9,6)  # Failures per condition
priora0<-c(1,1,1,1,1)
priorb0<-c(1,1,1,1,1)
Beta_betweengroups(wts, n1v, n2v, priora0, priorb0)
$contrast_Weights
[1]  0.3333333  0.3333333  0.3333333 -0.5000000 -0.5000000
$contrast_mean
[1] 0.2738095
$contrast_variance
[1] 0.01232993
$sampling_contrast_mean
[1] 0.273381
$sampling_contrast_variance
[1] 0.01218238
$equal_tail_95
2.5%      97.5%
0.05174536 0.48247585
$posterior_of_positive
[1] 0.9917
$prior_of_positive
[1] 0.4964
$BF_positive_contrast
[1] 121.2149
$BF_negative_contrast
[1] 0.008249808



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Beta_betweengroups", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfba_mann_whitney")
### * dfba_mann_whitney

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfba_mann_whitney
### Title: Independent Samples Test (Mann Whitney U)
### Aliases: dfba_mann_whitney

### ** Examples


Examples with large n per group
The data for each condition are presorted only for the user convenience if checking the U stats by hand

groupA <- c(43, 45, 47, 50, 54, 58, 60, 63, 69, 84, 85, 91, 99, 127, 130, 147, 165, 175, 193, 228, 252, 276)
groupB<-c(0, 01, 02, 03, 05, 14, 15, 23, 23, 25, 27, 32, 57, 105, 115, 158, 161, 181, 203, 290)

dfba_mann_whitney(E = groupA,C = groupB)

The following uses a Jeffreys prior instead of a default flat prior:
dfba_mann_whitney(E = groupA,C = groupB, a0 = .5,b0 =.5)

The following also uses a Jeffreys prior but the analysis reverses the variates:
dfba_mann_whitney(E=groupB,C=groupA,a0=.5,b0=.5)
Notice that BF10 from the above analysis is 1/BF10 from the original order of the variates.

The next analysis constructs 99% interval estimates with the Jeffreys prior.
dfba_mann_whitney(E=groupA,C=groupB,a0=.5,b0=.5,prob_interval=.99)

The following forces a discrete approach with a flat prior for a case with large n:
dfba_mann_whitney(E=groupA,C=groupB,method="small")

Examples with small n per group

groupC <- c(96.49, 96.78, 97.26, 98.85, 99.75, 100.14, 101.15, 101.39, 102.58, 107.22, 107.70, 113.26)
groupD <- c(101.16, 102.09, 103.14, 104.70, 105.27, 108.22, 108.32, 108.51, 109.88, 110.32, 110.55, 113.42)
S1ex<-dfba_mann_whitney(E=groupC,C=groupD)
S2ex<-dfba_mann_whitney(E=groupC,C=groupD,samples=50000)
S3ex<-dfba_mann_whitney(E=groupC,C=groupD)

To see output from the above three runs enter the folowing:
S1ex
S2ex
S3ex
Note that S1ex and S3ex are replication analyses for the discrete approach. The variabilty is due to
the different outcomes from the Monte Carlo sampling.

Output from above examples are without a plot of the omega_E distribution. The dfba_plot_mann_whitney()
function can plot the omega_E distribution for R objects. For example
A <- dfba_mann_whitney(E = groupA, C = groupB)
dfba_plot_mann_whitney(A)
Note this solution for the large-n case is a probabibility density display rather than a distribution
for the discrete probabilities for omega_E.
To see displays for the above two discrete small-sample cases then do the following where S1ex and
S2ex R objects are defined above.

dfba_plot_mann_whitney(x=S1ex)
dfba_plot_mann_whitney(x=S2ex)

To have the S2ex R object displayed for only the posterior then do the following:
dfba_plot_mann_whitney(S2ex, plot.prior=FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfba_mann_whitney", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
