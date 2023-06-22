# Bivariate concordance tests

  mt1 <- c(45, 40, 48, 42, 45, 44, 40, 37, 27, 45, 51, 44, 44, 29, 27, 48, 33, 39, 47, 54, 32)
  mt2 <- c(37, 41, 50, 46, 31, 39, 37, 42, 27, 48, 49, 52, 27, 36, 29, 44, 44, 43, 34, 48, 33)

  ABiCon<-dfba_bivariate_concordance(x = mt1,
                                     y = mt2)

  # Error Tests

  test_that("Missing data throws warning",{
    expect_warning(dfba_bivariate_concordance(x = c(NA, mt1[-1]),
                                              y = mt2)
                   )
  })

  test_that("Missing a0 parameter produces stop error",{
    expect_error(dfba_bivariate_concordance(a0 = NA,
                                            x = mt1,
                                            y = mt2),
                 "Both the a0 and b0 shape parameters must be positive and finite")
  })

  test_that("Missing b0 parameter produces stop error",{
    expect_error(dfba_bivariate_concordance(b0 = NA,
                                            x = mt1,
                                            y = mt2),
                 "Both the a0 and b0 shape parameters must be positive and finite")
  })

  # Function Tests
  test_that("Tau is correct",{
    expect_lte(abs(ABiCon$tau - 0.3807107), 3e-05)
  })

  test_that("Number of concordant pairs is correct",{
    expect_equal(floor(ABiCon$nc + 0.1), 136)
  })

  test_that("Number of discordant pairs is correct",{
    expect_equal(floor(ABiCon$nd + 0.1), 61)
  })

  test_that("Sample proportion is correct",{
    expect_lte(abs(ABiCon$sample.p - 0.6903553), 3e-05)
  })

  test_that("Posterior shape parameter a is correct",{
    expect_lte(abs(ABiCon$a.post - 137), 3e-05)
  })

  test_that("Posterior shape parameter b is correct",{
    expect_lte(abs(ABiCon$b.post - 62), 3e-05)
  })

  test_that("Posterior median is correct",{
    expect_lte(abs(ABiCon$post.median - 0.6890746), 3e-05)
  })

  test_that("Equal-tail lower limit is correct",{
    expect_lte(abs(ABiCon$post.eti.lower - 0.6225501), 3e-05)
  })

  test_that("Equal-tail upper limit is correct",{
    expect_lte(abs(ABiCon$post.eti.upper - 0.7507466), 3e-05)
  })

# GOF tests (phi_star)

  p <- seq(.05,.95,.05)
  ypred <- 17.332-(50.261*p) + (48.308*p^2)

#  # Note the coefficients in the ypred equation were found first
#  # via a polynomial regression
  yobs <- c(19.805, 10.105, 9.396, 8.219, 6.110, 4.543, 5.864, 4.861, 6.136,
             5.789,  5.443, 5.548, 4.746, 6.484, 6.185, 6.202, 9.804, 9.332,
            14.408)

  AFit <- dfba_bivariate_concordance(x = yobs,
                                     y = ypred,
                                     fitting.parameters = 3)

  test_that("Tau star is correct",{
    expect_lte(abs(AFit$tau_star - 0.516667), 3e-05)
  })

  test_that("Adjusted number of concordant pairs is correct",{
    expect_equal(floor(AFit$nc_star + 0.1), 91)
  })

  test_that("Adjusted number of discordant pairs is correct",{
    expect_equal(floor(AFit$nd_star + 0.1), 29)
  })

  test_that("Adjusted sample proportion is correct",{
    expect_lte(abs(AFit$sample.p_star - 0.7583333), 3e-05)
  })

  test_that("Adjusted posterior shape parameter a is correct",{
    expect_lte(abs(AFit$a.post_star - 92), 3e-05)
  })

  test_that("Adjusted posterior shape parameter b is correct",{
    expect_lte(abs(AFit$b.post_star - 30), 3e-05)
  })

  test_that("Adjusted posterior median is correct",{
    expect_lte(abs(AFit$post.median_star - 0.7554904), 3e-05)
  })

  test_that("Adjusted equal-tail lower limit is correct",{
    expect_lte(abs(AFit$post.eti.lower_star - 0.6742621), 3e-05)
  })

  test_that("Adjusted equal-tail upper limit is correct",{
    expect_lte(abs(AFit$post.eti.upper_star - 0.8260471), 3e-05)
  })
