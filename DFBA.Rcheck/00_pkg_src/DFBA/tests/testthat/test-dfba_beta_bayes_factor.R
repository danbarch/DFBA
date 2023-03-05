# Interval Tests

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
