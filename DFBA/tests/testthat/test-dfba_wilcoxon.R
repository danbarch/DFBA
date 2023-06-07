# Wilcoxon tests

## Test Data

### data for small method tests

Y1<-c(2.286,0.621,0.967,3.782,18.960,5.473,4.274,0.605)
Y2<-c(1.114,0.002,0.382,1.251,0.003,8.413,7.947,0.050)

### data for large method tests

Y1L<-rep(Y1, 4)
Y2L<-rep(Y2, 4)

# Error tests

test_that("Vector lengths must match",{
  expect_error(dfba_wilcoxon(Y1[-1],
                             Y2,
                             samples=10000),
               "Y1 and Y2 must have the same length. This function is for paired within-block data.")
})

test_that("Missing a0 parameter produces stop error",{
  expect_error(dfba_wilcoxon(a0 = NA,
                             Y1,
                             Y2,
                             samples=10000),
               "Both a0 and b0 must be positive and finite")
})

test_that("Missing b0 parameter produces stop error",{
  expect_error(dfba_wilcoxon(b0 = NA,
                             Y1,
                             Y2,
                             samples=10000),
               "Both a0 and b0 must be positive and finite")
})

test_that("Unreasonable probability intervals must be stopped",{
  expect_error(dfba_wilcoxon(Y1,
                             Y2,
                             samples=10000,
                             prob_interval = 1000000),
               "The probability for the interval estimate of phi_w must be a proper proportion.")
})

test_that("Too few samples throws error",{
  expect_error(dfba_wilcoxon(Y1,
                             Y2,
                             samples=10),
               "stipulating Monte Carlo samples < 10000 is too few")
})

test_that("Too small vectors throws error",{
  expect_error(dfba_wilcoxon(Y1[1:2],
                             Y2[1:2],
                             samples=10000),
               "There are not enough values in the Y1 and Y2 vectors for meaningful results.")
})

test_that("Stop if differences are trivial",{
  expect_error(dfba_wilcoxon(Y1,
                             Y1,
                             samples=10000),
               "Y1 and Y2 differences are all trivial")
})

test_that("Method must be large or small",{
  expect_error(dfba_wilcoxon(Y1,
                             Y2,
                             samples=10000,
                             method = "medium"),
               "An explicit method stipulation must be either the word large or the word small.")
})
# Small method tests


Awil<-dfba_wilcoxon(Y1,
                    Y2,
                    samples=10000)


test_that("T_positive for small method is correct",{
  expect_equal(floor(Awil$T_pos + 0.1), 23)
})

test_that("T_negative for small method is correct",{
  expect_equal(floor(Awil$T_neg + 0.1), 13)
})

test_that("Phibar for small method is correct",{
  expect_lte(abs(Awil$phibar - 0.612888), 0.00568)
})

test_that("Prior H1 for small method is correct",{
  expect_lte(abs(Awil$prH1 - 0.75174), 0.020536)
})

test_that("Equal tail lower limit for small method is correct",{
  expect_lte(abs(Awil$qLv - 0.282248), 0.01515)
})

test_that("Equal tail upper limit for small method is correct",{
  expect_lte(abs(Awil$qHv - 0.893838), 0.00708)
})

test_that("Bayes Factor 10 for small method is correct",{
  expect_lte(abs(Awil$BF10 - 3.028392), 0.5)
})

test_that("Posterior distribution for Phi for small method is correct",{
  expect_equal(floor(sum(Awil$phipost)+.0001), 1)
})

test_that("Total cumulative probability for Phi for small method is correct",{
  expect_equal(floor(Awil$cumulative_phi[200]+.0001), 1)
})

test_that("Function works with a tie",{
  expect_lte(abs(dfba_wilcoxon(Y1,
                             c(Y1[1], Y2[-1]),
                             samples=10000)$phibar-0.613),
               0.05)
})

test_that("Equal-tail interval works for tiny LL",{
  expect_lte(dfba_wilcoxon(Y1,
                               Y1+3,
                               samples=10000)$qLv,
             0.05)
})

test_that("Giant BF = Samples",{
  expect_equal(dfba_wilcoxon(Y1L,
                             Y1L-30,
                             method = "small",
                           samples=10000)$BF10,
             10000)
})
# Large method tests


  Bwil<-dfba_wilcoxon(Y1L,Y2L)

  test_that("T_positive for large method is correct",{
    expect_equal(floor(Bwil$T_pos + 0.1), 332)
  })

  test_that("T_negative for large method is correct",{
    expect_equal(floor(Bwil$T_neg + 0.1), 196)
  })

  test_that("n value for large method is correct",{
    expect_equal(floor(Bwil$n + 0.1), 32)
  })

  test_that("posterior a parameter for large method is correct",{
    expect_lte(abs(Bwil$apost - 15.84091), 0.0001)
  })

  test_that("posterior b parameter for large method is correct",{
    expect_lte(abs(Bwil$bpost - 9.659091), 0.0001)
  })

  test_that("posterior mean for large method is correct",{
    expect_lte(abs(Bwil$postmean - 0.6212121), 0.0001)
  })

  test_that("posterior median for large method is correct",{
    expect_lte(abs(Bwil$postmedian - 0.6244281), 0.0001)
  })

  test_that("Posterior probability of H1 for large method is correct",{
    expect_lte(abs(Bwil$prH1 - 0.8945555), 0.0001)
  })

  test_that("Bayes Factor 1/0 for large method is correct",{
    expect_lte(abs(Bwil$BF10 - 8.483664), 0.02)
  })

  test_that("Lower Limit for Equal-Tail Interval for large method is correct",{
    expect_lte(abs(Bwil$qlequal - 0.4293161), 0.0001)
  })

  test_that("Upper Limit for Equal-Tail Interval for large method is correct",{
    expect_lte(abs(Bwil$qhequal - 0.7950966), 0.0001)
  })

  test_that("Lower Limit for Highest-Density Interval for large method is correct",{
    expect_lte(abs(Bwil$qLmin - 0.4361713), 0.0001)
  })

  test_that("Upper Limit for Highest-Density Interval for large method is correct",{
    expect_lte(abs(Bwil$qHmax - 0.8010602), 0.0001)
  })

  test_that("Giant BF = Inf",{
    expect_equal(dfba_wilcoxon(rep(Y1L, 10),
                               rep(Y1L-20, 10))$BF10,
                 Inf)
  })
