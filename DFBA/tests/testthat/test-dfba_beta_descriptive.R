# DFBA Beta Descriptive Tests

# Error tests
test_that("Unreasonable probability intervals must be stopped",{
  expect_error(dfba_beta_descriptive(a = 38,
                                     b = 55,
                                     prob_interval = 900),
               "The probability for the interval estimate must be between 0 and 1.")
})

test_that("Missing a parameter produces stop error",{
  expect_error(dfba_beta_descriptive(a = NA,
                                     b = 55,
                                     prob_interval = .90),
               "Both the a and b shape parameters for a beta must be positive and finite")
})

test_that("Missing b parameter produces stop error",{
  expect_error(dfba_beta_descriptive(a = 38,
                                     b = NA,
                                     prob_interval = .90),
               "Both the a and b shape parameters for a beta must be positive and finite")
})

# Function tests
  ADes<-dfba_beta_descriptive(a = 38, b = 55, prob_interval=.90)

  test_that("Beta mean is correct",{
    expect_lte(abs(ADes$x_mean - 0.4086022), 3e-05)
  })

  test_that("Beta median is correct",{
    expect_lte(abs(ADes$x_median - 0.4079442), 3e-05)
  })

  test_that("Beta mode is correct",{
    expect_lte(abs(ADes$x_mode - 0.4065934), 3e-05)
  })

  test_that("Equal-tail lower limit is correct",{
    expect_lte(abs(ADes$eti_lower - 0.3262818), 3e-05)
  })

  test_that("Equal-tail upper limit is correct",{
    expect_lte(abs(ADes$eti_upper - 0.4931701), 3e-05)
  })

  test_that("Highest-density lower limit is correct",{
    expect_lte(abs(ADes$hdi_lower - 0.3250019), 3e-05)
  })

  test_that("Highest-density upper limit is correct",{
    expect_lte(abs(ADes$hdi_upper - 0.4918332), 3e-05)
  })

  test_that("Spot-check of density vector",{
    expect_lte(abs(ADes$outputdf$density[83] - 7.793301), 3e-05)
  })

  test_that("Spot-check of cumulative vector",{
    expect_lte(abs(ADes$outputdf$cumulative_prob[83] - 0.5160382), 3e-05)
  })

  test_that("mode is undefined when a and b == 1 or a or b < 1",{
    BDes<-dfba_beta_descriptive(a = 1,
                                b = 1,
                                prob_interval=.90)
    expect_equal(is.na(BDes$x_mode), TRUE)
  })

  test_that("interval ll undefined when a and b == 1 or a or b < 1",{
    BDes<-dfba_beta_descriptive(a = 1,
                                b = 1,
                                prob_interval=.90)
    expect_equal(is.na(BDes$hdi_lower), TRUE)
  })

  test_that("interval ul undefined when a and b == 1 or a or b < 1",{
    BDes<-dfba_beta_descriptive(a = 1,
                                b = 1,
                                prob_interval=.90)
    expect_equal(is.na(BDes$hdi_upper), TRUE)
  })







