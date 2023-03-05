# Small method
  E <- c(1.61, 2.02, 2.34, 2.89, 4.51, 4.72, 4.86, 6.21, 9.50, 33.39)
  C <- c(1.11, 1.13, 1.32, 1.82, 1.99, 4.12, 6.28, 6.21, 8.24, 44.55)

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

  test_that("[small] Equal-tail interval lower limit is correct",{
    expect_lte(abs(AMann$qLv - 0.3524778), 0.013732592)
  })

  test_that("[small] Equal-tail interval upper limit is correct",{
    expect_lte(abs(AMann$qHv-.8073713), 0.013191075)
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

  Elarge<-rep(E, 3)
  Clarge<-rep(C, 3)

  BMann <- dfba_mann_whitney(Elarge,
                             Clarge)

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
    expect_lte(abs(BMann$apost - 27.90098), 0.0005)
  })

  test_that("[large] Posterior shape parameter b is correct",{
    expect_lte(abs(BMann$bpost - 18.57538), 0.0005)
  })

  test_that("[large] Posterior mean is correct",{
    expect_lte(abs(BMann$postmean - 0.6003262), 0.0005)
  })

  test_that("[large] Posterior median is correct",{
    expect_lte(abs(BMann$postmedian - 0.6017773), 0.0005)
  })

  test_that("[large] Omega average is correct",{
    expect_lte(abs(BMann$omegabar - 0.6003262), 0.0005)
  })

  test_that("[large] Equal-tail interval lower limit is correct",{
    expect_lte(abs(BMann$qlequal - 0.4576042), 0.0005)
  })

  test_that("[large] Equal-tail interval upper limit is correct",{
    expect_lte(abs(BMann$qhequal - 0.7348651), 0.0005)
  })

  test_that("[large] Highest-density interval lower limit is correct",{
    expect_lte(abs(BMann$qLmin - 0.4606209), 0.0005)
  })

  test_that("[large] Highest-density interval upper limit is correct",{
    expect_lte(abs(BMann$qHmax - 0.7376452), 0.0005)
  })

  test_that("[large] Prior probability of H1 is correct",{
    expect_lte(abs(BMann$prH1 - 0.9169977), 0.0005)
  })

  test_that("[large] Bayes Factor 1/0 is correct",{
    expect_lte(abs(BMann$BF10 - 11.04786), 0.08)
  })

