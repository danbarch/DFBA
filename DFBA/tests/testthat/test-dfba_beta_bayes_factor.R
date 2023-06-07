# Interval Tests

# Error tests

test_that("Stop for unexpected method argument",{
  expect_error(dfba_beta_bayes_factor(a = 17,
                                      b = 5,
                                      method = "intervalll",
                                      H0 = c(0, .5)),
               "method must be either 'point' or 'interval'")
})

test_that("Missing a0 produces stop error",{
  expect_error(dfba_beta_bayes_factor(a0 = NA,
                                      a = 17,
                                      b = 5,
                                      method = "interval",
                                      H0 = c(0, .5)),
               "Both a0 and b0 must be positive and finite.")
})

test_that("Missing b0 produces stop error",{
  expect_error(dfba_beta_bayes_factor(b0 = NA,
                                      a = 17,
                                      b = 5,
                                      method = "interval",
                                      H0 = c(0, .5)),
               "Both a0 and b0 must be positive and finite.")
})

test_that("a < a0 produces stop error",{
  expect_error(dfba_beta_bayes_factor(a0 = 77,
                                      a = 17,
                                      b = 5,
                                      method = "interval",
                                      H0 = c(0, .5)),
               "Both a and b cannot be less than the respective a0 and b0 values")
})

test_that("b < b0 produces stop error",{
  expect_error(dfba_beta_bayes_factor(b0 = 55,
                                      a = 17,
                                      b = 5,
                                      method = "interval",
                                      H0 = c(0, .5)),
               "Both a and b cannot be less than the respective a0 and b0 values")
})

test_that("Point H0 > 1 produces stop error",{
  expect_error(dfba_beta_bayes_factor(a = 17,
                                      b = 5,
                                      method = "point",
                                      H0 = 2),
               "H0 must be greater than or equal to 0 and less than or equal to 1")
})

test_that("Error for point method when H0 is interval",{
  expect_error(dfba_beta_bayes_factor(a = 17,
                                      b = 5,
                                      method = "point",
                                      H0 = c(0, 0.5)),
               "'H0' must be a single numeric value when method = 'point'")
})
test_that("Interval H0 > 1 produces stop error",{
  expect_error(dfba_beta_bayes_factor(a = 17,
                                      b = 5,
                                      method = "interval",
                                      H0 = c(0, 2)),
               "H0 interval limits must be greater than or equal to 0 and less than or equal to 1")
})

test_that("Interval length > 2 produces stop error",{
  expect_error(dfba_beta_bayes_factor(a = 17,
                                      b = 5,
                                      method = "interval",
                                      H0 = c(0, 0.5, 1))
               )
})

test_that("Lower H0 limit > upper limit produces stop error",{
  expect_error(dfba_beta_bayes_factor(a = 17,
                                      b = 5,
                                      method = "interval",
                                      H0 = c(1, 0)),
               "When method = 'interval', H0 upper limit must be greater than H0 lower limit")
})

# Function tests

# Interval method

  ABF<-dfba_beta_bayes_factor(a = 17,b = 5,method = "interval",H0 = c(0, .5))

  test_that("[Interval] Prior p(H1) is correct",{
    expect_lte(abs(ABF$pH1 - 0.5), 3e-05)
  })

  test_that("[Interval] Posterior p(H1) is correct",{
    expect_lte(abs(ABF$postH1 - 0.9964013), 3e-05)
  })

  test_that("[Interval] Bayes Factor 1/0 is correct",{
    expect_lte(abs(ABF$BF10 - 276.8789), 2.4)
  })

  test_that("[Interval] Bayes Factor 0/1 is correct",{
    expect_lte(abs(ABF$BF01 - 0.003611687), 4e-05)
  })

# Point Tests

  BBF<-dfba_beta_bayes_factor(a = 377,
                              b = 123,
                              method = "point",
                              H0 = .75)

  test_that("[Point] Prior density p(H0) is correct",{
    expect_lte(abs(BBF$dpriorH0-1), 3e-05)
  })

  test_that("[Point] Posterior density p(H0) is correct",{
    expect_lte(abs(BBF$dpostH0-20.04153), 3e-05)
  })

  test_that("[Point] Bayes Factor 1/0 is correct",{
    expect_lte(abs(BBF$BF10-0.04989638), 3e-05)
  })

  test_that("[Point] Bayes Factor 0/1 is correct",{
    expect_lte(abs(BBF$BF01-20.04153), 5e-04)
  })
