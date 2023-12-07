# Bayes v t Power Tests

  Apow1 <- dfba_bayes_vs_t_power(n_min = 20,
                                 model = "weibull",
                                 delta = 0.8,
                                 shape1 = 0.8,
                                 shape2 = 0.8,
                                 design = "paired",
                                 samples = 300)

  bayespower <- Apow1$outputdf[[2]]
  tpower <- Apow1$outputdf[[3]]
  bayes70c <- bayespower[11]
  t70c <- tpower[11]


  # Test the errors

  test_that("Negative delta throws stop error",{
    expect_error(dfba_bayes_vs_t_power(n_min = 20,
                                       model = "weibull",
                                       delta = - 0.8,
                                       shape1 = 0.8,
                                       shape2 = 0.8,
                                       design = "paired",
                                       samples = 300),
                 "The function requires a nonnegative value for delta.")
  })

  test_that("stop n is a non-integer",{
    expect_error(dfba_bayes_vs_t_power(n_min = 20.1,
                                       model = "weibull",
                                       delta = 0.8,
                                       shape1 = 0.8,
                                       shape2 = 0.8,
                                       design = "paired",
                                       samples = 300),
                 "n_min must be an integer that is 20 or larger")
  })

  test_that("stop if n < 20",{
    expect_error(dfba_bayes_vs_t_power(n_min = 19,
                                       model = "weibull",
                                       delta = 0.8,
                                       shape1 = 0.8,
                                       shape2 = 0.8,
                                       design = "paired",
                                       samples = 300),
                 "n_min must be an integer that is 20 or larger")
  })

  test_that("Negative a0 produces stop error",{
    negative_a0 <- -77
    expect_error(dfba_bayes_vs_t_power(n_min = 20,
                                       model = "weibull",
                                       delta = 0.8,
                                       shape1 = 0.8,
                                       shape2 = 0.8,
                                       design = "paired",
                                       samples = 300,
                                       a0 = negative_a0),
                 "Both a0 and b0 must be positive and finite.")
  })

  test_that("Negative b0 produces stop error",{
    negative_b0 <- -77
    expect_error(dfba_bayes_vs_t_power(n_min = 20,
                                       model = "weibull",
                                       delta = 0.8,
                                       shape1 = 0.8,
                                       shape2 = 0.8,
                                       design = "paired",
                                       samples = 300,
                                       b0 = negative_b0),
                 "Both a0 and b0 must be positive and finite.")
  })

  test_that("model is not in the list",{
    mlist<-c("normal",
             "weibull",
             "cauchy",
             "lognormal",
             "chisquare",
             "logistic",
             "exponential",
             "gumbel",
             "pareto")
    modelstop <- paste0("The set of distributions for model are:"," ","\n",
                        "\t\t", paste0(mlist, collapse = "\n\t\t"), "\n",
                        "The stipulated model is not on the list")
    expect_error(dfba_bayes_vs_t_power(n_min = 20,
                                       model = "hansel",
                                       delta = 0.8,
                                       shape1 = 0.8,
                                       shape2 = 0.8,
                                       design = "paired",
                                       samples = 300),
                 modelstop)
  })


  test_that("design is not in the list",{
    designlist<-c("paired",
                  "independent")
    designstop <- paste0("The set of distributions for design are:"," ","\n",
                         "\t\t", paste0(designlist, collapse = "\n\t\t"), "\n",
                         "The stipulated design is not on the list")
    expect_error(dfba_bayes_vs_t_power(n_min = 20,
                                       model = "weibull",
                                       delta =  0.8,
                                       shape1 = 0.8,
                                       shape2 = 0.8,
                                       design = "paireddd",
                                       samples = 300),
                 designstop)
  })

  test_that("critical effect is between 0 and 1",{
    expect_error(dfba_bayes_vs_t_power(n_min = 20,
                                       model = "weibull",
                                       delta = 0.8,
                                       shape1 = 0.8,
                                       shape2 = 0.8,
                                       design = "paired",
                                       samples = 300,
                                       effect_crit = 42),
                 "The effect_crit value must be a number nonzero number less than 1.")
  })
  # Test function output
    test_that("Bayesian estimate is correct",{
    expect_lte(abs(bayes70c-.99422), 0.0483)
  })

  test_that("Classical power estimate is correct",{
    expect_lte(abs(t70c-.94205), 0.1170)
  })

