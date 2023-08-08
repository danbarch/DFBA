# Tests of mann-whitney

# Small method test data
E <- c(1.61, 2.02, 2.34, 2.89, 4.51, 4.72, 4.86, 6.21, 9.50, 33.39)
C <- c(1.11, 1.13, 1.32, 1.82, 1.99, 4.12, 6.28, 6.21, 8.24, 44.55)

# Large method test data

Elarge<-rep(E, 3)
Clarge<-rep(C, 3)

# Error tests

test_that("Missing a0 parameter produces stop error",{
  expect_error(dfba_mann_whitney(a0 = NA,
                                 E,
                                 C,
                                 samples=10000),
               "Both a0 and b0 must be positive and finite")
})

test_that("Missing b0 parameter produces stop error",{
  expect_error(dfba_mann_whitney(b0 = NA,
                                 E,
                                 C,
                                 samples=10000),
               "Both a0 and b0 must be positive and finite")
})

test_that("Unreasonable probability intervals must be stopped",{
  expect_error(dfba_mann_whitney(E,
                                 C,
                                 samples=10000,
                                 prob_interval = 77),
               "The probability for the interval estimate of phi_w must be a proper proportion.")
})
test_that("Not enough samples",{
  expect_error(dfba_mann_whitney(E,
                                 C,
                                 samples=8),
               "For reliable results please use at least 10000 Monte Carlo samples")
})

test_that("empty E vector stops function",{
  expect_error(dfba_mann_whitney(E = NA,
                                 C,
                                 samples=10000),
               "The E and C vectors must have a length greater than 0."
  )
})

test_that("empty C vector stops function",{
  expect_error(dfba_mann_whitney(E,
                                 C = NA,
                                 samples=10000),
               "The E and C vectors must have a length greater than 0."
  )
})
test_that("Missing data in E vector throws message",{
  expect_message(dfba_mann_whitney(c(NA, E[-1]),
                                   C,
                                   samples=10000)
  )
})

test_that("Missing data in C vector throws message",{
  expect_message(dfba_mann_whitney(E,
                                   c(NA, C[-1]),
                                   samples=10000)
  )
})

test_that("Warning if method is neither large nor small",{
  expect_error(dfba_mann_whitney(E,
                                   C,
                                   samples=10000,
                                   method = "medium"),
                 "An explicit method stipulation must be either the word large or small."
  )
})

# Small method

  AMann<-dfba_mann_whitney(E,
                           C,
                           samples=10000)

  test_that("[small] E mean is correct",{
    expect_lte(abs(AMann$Emean - 7.205), 0.001)
  })

  test_that("[small] C mean is correct",{
    expect_lte(abs(AMann$Cmean - 7.677), 0.001)
  })

  test_that("[small] n of E vector is correct",{
    expect_equal(floor(AMann$n_E + 0.1), 10)
  })

  test_that("[small] n of C vector is correct",{
    expect_equal(floor(AMann$n_C + 0.1), 10)
  })

  test_that("[small] U_E is correct",{
    expect_equal(floor(AMann$U_E + 0.1), 60)
  })

  test_that("[small] U_C is correct",{
    expect_equal(floor(AMann$U_C + 0.1), 39)
  })

  test_that("[small] Total of discrete posterior probabilities is 1",{
    expect_equal(floor(sum(AMann$omegapost) + 0.0001), 1)
  })

  test_that("[small] Total cumulative posterior probability is 1",{
    expect_equal(floor(AMann$cumulative_omega[200] + 0.001), 1)
  })

  test_that("[small] Prior probability of H1 is correct",{
    expect_lte(abs(AMann$prH1 - 0.7778633), 0.02116715)
  })

  test_that("[small] Bayes Factor 1/0 is correct",{
    expect_lte(abs(AMann$BF10 - 3.502526), 0.6)
  })

  test_that("[small] Posterior mean is correct",{
    expect_lte(abs(AMann$omegabar - 0.5911166), 0.005174341)
  })

  test_that("[small] Posterior mean is correct with ties",{
    expect_lte(abs(dfba_mann_whitney(E = c(3, 5, 6),
                                 C = c(1, 2, 3, 4),
                                 samples = 10000)$omegabar)-0.756,
               0.1)
  })

  test_that("[small] Equal-tail interval lower limit is correct",{
    expect_lte(abs(AMann$eti_lower - 0.3524778), 0.013732592)
  })

  test_that("[small] Equal-tail interval upper limit is correct",{
    expect_lte(abs(AMann$eti_upper-.8073713), 0.013191075)
  })

  test_that("[small] Giant BF = samples",{
    expect_equal(dfba_mann_whitney(E = rep(E, 2),
                                   C = rep(E, 2)-40,
                                   samples = 10000,
                                   method = "small")$BF10, 10000)
  })

  test_that("Equal-tail interval works for tiny LL",{
    expect_lte(dfba_mann_whitney(E,
                                 C = E + 40,
                             samples=10000)$eti_lower,
               0.05)
  })

  ## The next tests are for the large-sample case.
  # Tests 14 and 15 check the sample mean for E and C,
  # Tests 16 and 17 check the values for n_E and n_C
  # Tests 18 and 19 check the values for U_E and U_C,
  # Tests 20 and 21 check the values for the
  # posterior values for a and b shape parameters,
  # Tests 22 and 23 ckecks value for posterior mean
  # and median, Test 24 checks omegabar value,
  # Tests 25 and 26 check the equal-tail 95 percent
  # interval limits, Tests 27 and 28 check the 95
  # percent HDI limits, Test 29 checks the value
  # for the posterior prH1, Test 30 checks the
  # value for BF10.
  ## The output of function is a vector of 30 values
  # which are 0 if the test passes and 1 if it fails.
  # The output vector is called dfba_mann.

# Large method

  BMann <- dfba_mann_whitney(Elarge,
                             Clarge,
                             method = "large")

  test_that("[large] E mean is correct",{
    expect_lte(abs(BMann$Emean - 7.205), 0.0005)
  })

  test_that("[large] C mean is correct",{
    expect_lte(abs(BMann$Cmean - 7.677), 0.0005)
  })

  test_that("[large] n of E vector is correct",{
    expect_equal(floor(BMann$n_E + 0.1), 30)
  })

  test_that("[large] n of C vector is correct",{
    expect_equal(floor(BMann$n_C + 0.1), 30)
  })

  test_that("[large] U_E is correct",{
    expect_equal(floor(BMann$U_E + 0.1), 540)
  })

  test_that("[large] U_C is correct",{
    expect_equal(floor(BMann$U_C + 0.1), 351)
  })

  test_that("[large] Posterior shape parameter a is correct",{
    expect_lte(abs(BMann$a_post - 27.90098), 0.0005)
  })

  test_that("[large] Posterior shape parameter b is correct",{
    expect_lte(abs(BMann$b_post - 18.57538), 0.0005)
  })

  test_that("[large] Posterior mean is correct",{
    expect_lte(abs(BMann$post_mean - 0.6003262), 0.0005)
  })

  test_that("[large] Posterior median is correct",{
    expect_lte(abs(BMann$post_median - 0.6017773), 0.0005)
  })

  test_that("[large] Omega average is correct",{
    expect_lte(abs(BMann$omegabar - 0.6003262), 0.0005)
  })

  test_that("[large] Equal-tail interval lower limit is correct",{
    expect_lte(abs(BMann$eti_lower - 0.4576042), 0.0005)
  })

  test_that("[large] Equal-tail interval upper limit is correct",{
    expect_lte(abs(BMann$eti_upper - 0.7348651), 0.0005)
  })

  test_that("[large] Highest-density interval lower limit is correct",{
    expect_lte(abs(BMann$hdi_lower - 0.4606209), 0.0005)
  })

  test_that("[large] Highest-density interval upper limit is correct",{
    expect_lte(abs(BMann$hdi_upper - 0.7376452), 0.0005)
  })

  test_that("[large] Prior probability of H1 is correct",{
    expect_lte(abs(BMann$prH1 - 0.9169977), 0.0005)
  })

  test_that("[large] Bayes Factor 1/0 is correct",{
    expect_lte(abs(BMann$BF10 - 11.04786), 0.08)
  })

  test_that("[large] Posterior mean is correct when UC > UE",{
    expect_lte(abs(dfba_mann_whitney(Clarge,
                                     Elarge,
                                     method = "large")$post_mean - 0.3996738), 0.0005)
  })

  test_that("[large] Giant BF = Inf",{
    expect_equal(dfba_mann_whitney(rep(Elarge, 2),
                                   C = rep(Elarge, 2) - 40)$BF10, Inf)
  })
