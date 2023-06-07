# McNemar Tests

# Error Tests

test_that("Missing a0 parameter produces stop error",{
  expect_error(dfba_mcnemar(a0 = NA,
                            n_01 = 17,
                            n_10 = 2),
               "Both a0 and b0 must be positive and finite")
})

test_that("Missing b0 parameter produces stop error",{
  expect_error(dfba_mcnemar(b0 = NA,
                            n_01 = 17,
                            n_10 = 2),
               "Both a0 and b0 must be positive and finite")
})

test_that("Negative frequency throws error",{
  expect_error(dfba_mcnemar(n_01 = -17,
                            n_10 = 2),
               "Neither n_01 nor n_10 can be negative")
})

test_that("Noninteger frequency throws error",{
  expect_error(dfba_mcnemar(n_01 = 17.777,
                            n_10 = 2),
               "n_01 and n_10 must be integers")
})

test_that("Unreasonable probability intervals must be stopped",{
  expect_error(dfba_mcnemar(n_01 = 17,
                            n_10 = 2,
                            prob_interval = 77),
               "prob_interval must between 0 and 1")
})

# Function Tests

AMc<-dfba_mcnemar(n_01 = 17,
                    n_10 = 2)

  test_that("Posterior shape parameter a is correct",{
    expect_equal(floor(AMc$a.post + 0.1), 18)
  })

  test_that("Posterior shape parameter b is correct",{
    expect_equal(floor(AMc$b.post + 0.1), 3)
  })

  test_that("Posterior mean is correct",{
    expect_lte(abs(AMc$post_mean - 0.8571429), 3e-05)
  })

  test_that("Posterior median is correct",{
    expect_lte(abs(AMc$post_median - 0.8685263), 3e-05)
  })

  test_that("Equal-tail interval lower limit is correct",{
    expect_lte(abs(AMc$eti_lower - 0.6830173), 3e-05)
  })

  test_that("Equal-tail interval upper limit is correct",{
    expect_lte(abs(AMc$eti_upper - 0.9679291), 3e-05)
  })

  test_that("Point Bayes Factor 1/0 is correct",{
    expect_lte(abs(AMc$BF10point - 153.3006), 0.4)
  })

  test_that("Interval Bayes Factor 1/0 is correct",{
    expect_lte(abs(AMc$BF10interval - 4968.555), 31.0)
  })

  test_that("Posterior probability of H1 is correct",{
    expect_lte(abs(AMc$postH1 - 0.9997988), 3e-05)
  })
