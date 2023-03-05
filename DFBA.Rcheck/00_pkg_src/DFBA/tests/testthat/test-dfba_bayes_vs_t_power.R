  Apow1 <- dfba_bayes_vs_t_power(n = 20,
                                 model = "weibull",
                                 delta = 0.8,
                                 shape1 = 0.8,
                                 shape2 = 0.8,
                                 design = "paired",
                                 samples = 300)

  bayespower=Apow1$outputdf[[2]]
  tpower=Apow1$outputdf[[3]]
  bayes70c=bayespower[11]
  t70c=tpower[11]
#  bayestest=abs(bayes70c-.99422)

  test_that("Bayesian estimate is correct",{
    expect_lte(abs(bayes70c-.99422), 0.0483)
  })

  test_that("Classical power estimate is correct",{
    expect_lte(abs(t70c-.94205), 0.1170)
  })

