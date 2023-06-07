# Binomial Test

  Abin<-dfba_binomial(n1 = 16,
                      n2 = 2)

  # Error Tests

  test_that("Unreasonable probability intervals must be stopped",{
    expect_error(dfba_binomial(n1 = 16,
                               n2 = 2,
                               prob_interval = 23),
                 "prob_interval must be greater than 0 and less than 1")
  })

  test_that("Missing a0 parameter produces stop error",{
    expect_error(dfba_binomial(a0 = NA,
                               n1 = 16,
                               n2 = 2),
                 "Both a0 and b0 must be positive and finite")
  })

  test_that("Missing b0 parameter produces stop error",{
    expect_error(dfba_binomial(b0 = NA,
                               n1 = 16,
                               n2 = 2),
                 "Both a0 and b0 must be positive and finite")
  })

  test_that("n's cannot be negative",{
    expect_error(dfba_binomial(n1 = -16,
                               n2 = 2),
                 "Neither n1 nor n2 can be negative")
  })

  test_that("n's must be integers",{
    expect_error(dfba_binomial(n1 = 16.666,
                               n2 = 2),
                 "n1 and n2 must be integers")
  })

  # Function Tests
  test_that("Posterior a parameter is correct",{
    expect_equal(floor(Abin$apost+.1), 17)
  })

  test_that("Posterior b parameter is correct",{
    expect_equal(floor(Abin$bpost+.1), 3)
  })

  test_that("Posterior phi mean is correct",{
    expect_lte(abs(Abin$phimean-0.85), 3e-05)
  })

  test_that("Posterior phi median is correct",{
    expect_lte(abs(Abin$phimedian-0.8617288), 3e-05)
  })

  test_that("Posterior phi mode is correct",{
    expect_lte(abs(Abin$phimode-0.8888889), 3e-05)
  })

  test_that("ETI lower limit is correct",{
    expect_lte(abs(Abin$eti_lower-0.6686233), 3e-05)
  })

  test_that("ETI upper limit is correct",{
    expect_lte(abs(Abin$eti_upper-0.9661738), 3e-05)
  })

  test_that("HDI lower limit is correct",{
    expect_lte(abs(Abin$hdi_lower-0.6973879), 3e-05)
  })

  test_that("HDI upper limit is correct",{
    expect_lte(abs(Abin$hdi_upper-0.9801174), 3e-05)
  })

