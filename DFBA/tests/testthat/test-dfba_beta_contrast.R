# DFBA beta contrast tests

## Test data
n1_vec<-c(22, 15, 13, 21)
n2_vec<-c(18, 25, 27, 19)
ABcontrast<-c(.5, -.5, -.5, .5)

# Error tests

test_that("Not enough samples",{
  expect_error(dfba_beta_contrast(n1_vec = n1_vec,
                                  n2_vec = n2_vec,
                                  contrast_vec = ABcontrast,
                                  samples = 8),
               "For reliable results please use at least 10000 Monte Carlo samples")
})

test_that("Stop if vector lengths are mismatched",{
  expect_error(dfba_beta_contrast(n1_vec = n1_vec[-1],
                                  n2_vec = n2_vec,
                                  contrast_vec = ABcontrast),
               "The vectors n1_vec, n2_vec, and contrast_vec must have the same length.")
})

test_that("Stop if any of n1 is negative",{
  expect_error(dfba_beta_contrast(n1_vec = c(n1_vec[-1], -1),
                                  n2_vec = n2_vec,
                                  contrast_vec = ABcontrast),
               "n1_vec values must be non-negative integers")
})

test_that("Stop if any of n2 is negative",{
  expect_error(dfba_beta_contrast(n1_vec = n1_vec,
                                  n2_vec = c(n2_vec[-1], -1),
                                  contrast_vec = ABcontrast),
               "n2_vec values must be non-negative integers")
})

test_that("Stop if any of a0 is negative",{
  expect_error(dfba_beta_contrast(n1_vec = n1_vec,
                                  n2_vec = n2_vec,
                                  contrast_vec = ABcontrast,
                                  a0_vec = c(-1, rep(1, length(n1_vec)-1))),
               "All values in both the a0_vec and b0_vec shape parameter vectors for the prior beta must be positive and finite.")
})

test_that("Stop if any of b0 is negative",{
  expect_error(dfba_beta_contrast(n1_vec = n1_vec,
                                  n2_vec = n2_vec,
                                  contrast_vec = ABcontrast,
                                  b0_vec = c(-1, rep(1, length(n1_vec)-1))),
               "All values in both the a0_vec and b0_vec shape parameter vectors for the prior beta must be positive and finite.")
})


test_that("Stop if positive contrast coefficient sum != 1",{
  expect_error(dfba_beta_contrast(n1_vec = n1_vec,
                                  n2_vec = n2_vec,
                                  contrast_vec = c(.5, -.5, -.5, 500)),
               "The sum of the positive contrast coefficients must be 1")
})

test_that("Stop if negative contrast coefficient sum != -1",{
  expect_error(dfba_beta_contrast(n1_vec = n1_vec,
                                  n2_vec = n2_vec,
                                  contrast_vec = c(.5, -500, -.5, .5)),
               "The sum of the negative contrast coefficients must be -1")
})

test_that("Unreasonable probability intervals must be stopped",{
  expect_error(dfba_beta_contrast(n1_vec = n1_vec,
                                  n2_vec = n2_vec,
                                  contrast_vec = ABcontrast,
                                  prob_interval = 7),
               "The probability for the interval estimate must be between 0 and 1.")
})


# Function tests

  Acon<-dfba_beta_contrast(n1_vec = n1_vec,
                           n2_vec = n2_vec,
                           contrast_vec = ABcontrast)

  test_that("Posterior contrast mean is correct",{
    expect_lte(abs(Acon$mean-.1785714), 3e-05)
  })

  test_that("Posterior contrast median is correct",{
    expect_lte(abs(Acon$delta_quantiles[51]-.17924), 0.00904)
  })

  test_that("Equal-tail lower limit is correct",{
    expect_lte(abs(Acon$lower_limit-.030875), 0.0176)
  })

  test_that("Equal-tail upper limit is correct",{
    expect_lte(abs(Acon$upper_limit-.32255), 0.0114)
  })

  test_that("Prior probability of positive contrast is correct",{
    expect_lte(abs(Acon$prior_positive_delta-.50013), 0.0343)
  })

  test_that("Posterior probability of positive contrast is correct",{
    expect_lte(abs(Acon$prob_positive_delta-.991), 0.0063)
  })

  test_that("Bayes Factor is correct",{
    expect_lte(abs(Acon$bayes_factor-111.034), 250)
  })

  test_that("Giant BF values = samples",{
    n1_vec<-c(22, 0, 0, 21)
    n2_vec<-c(18, 25, 27, 19)
    Bcon<-dfba_beta_contrast(n1_vec = n1_vec,
                             n2_vec = n2_vec,
                             contrast_vec = ABcontrast)
    expect_equal(Bcon$bayes_factor,
                 Bcon$samples)
  })
