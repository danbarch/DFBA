# sim data tests

# Error tests

test_that("Missing a0 parameter produces stop error",{
  expect_error(dfba_sim_data(a0 = NA,
                             n = 450,
                             model = "normal",
                             design = "independent",
                             delta = 0.4,
                             block.max = 0),
               "Both a0 and b0 must be positive and finite")
})

test_that("Missing b0 parameter produces stop error",{
  expect_error(dfba_sim_data(b0 = NA,
                             n = 450,
                             model = "normal",
                             design = "independent",
                             delta = 0.4,
                             block.max = 0),
               "Both a0 and b0 must be positive and finite")
})

test_that("negative delta stops the show",{
  expect_error(dfba_sim_data(n = 450,
                             model = "normal",
                             design = "independent",
                             delta = -0.4,
                             block.max = 0),
               "The function requires a positive difference in the location of the two conditions.")
})

test_that("negative block.max stops the show",{
  expect_error(dfba_sim_data(n = 450,
                             model = "normal",
                             design = "independent",
                             delta = 0.4,
                             block.max = -100),
               "block.max must be nonnegative")
})

test_that("n too small",{
  expect_error(dfba_sim_data(n = 4,
                             model = "normal",
                             design = "independent",
                             delta = 0.4,
                             block.max = 0),
               "The function requires an integer that is 20 or larger for sample size")
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
  expect_error(dfba_sim_data(n = 450,
                             model = "T",
                             design = "independent",
                             delta = 0.4,
                             block.max = 0),
               modelstop)
})


test_that("design is not in the list",{
  designlist<-c("paired",
                "independent")
  designstop <- paste0("The set of distributions for design are:"," ","\n",
                       "\t\t", paste0(designlist, collapse = "\n\t\t"), "\n",
                       "The stipulated design is not on the list")
  expect_error(dfba_sim_data(n = 450,
                             model = "normal",
                             design = "independenttttt",
                             delta = 0.4,
                             block.max = 0),
               designstop)
})

## Model-specific error tests

### Normal

test_that("negative shape stops the show (normal)",{
  expect_error(dfba_sim_data(n = 450,
                             model = "normal",
                             design = "independent",
                             delta = 0.4,
                             shape1 = -77,
                             block.max = 100),
               "shape1 and shape2 are standard deviations and must be positive values.")
})

### Weibull

test_that("negative shape stops the show (weibull)",{
  expect_error(dfba_sim_data(n = 450,
                             model = "weibull",
                             design = "independent",
                             delta = 0.4,
                             shape1 = -77,
                             block.max = 100),
               "shape1 and shape2 are the Weibull shape parameters for the respective control and experiment conditions and must be positive values.")
})

### Cauchy

test_that("negative shape stops the show (cauchy)",{
  expect_error(dfba_sim_data(n = 450,
                             model = "cauchy",
                             design = "independent",
                             delta = 0.4,
                             shape1 = -77,
                             block.max = 100),
               "shape1 and shape2 are the Cauchy distribution scale factors for the respective control and experiment conditions and must be positive values.")
})

### lognormal

test_that("negative shape stops the show (lognormal)",{
  expect_error(dfba_sim_data(n = 450,
                             model = "lognormal",
                             design = "independent",
                             delta = 0.4,
                             shape1 = -77,
                             block.max = 100),
               "shape1 and shape2 are the sdlog values for the respective control and experiment conditions and must be positive values.")
})

### chi-squared

test_that("negative shape stops the show (chi-squared)",{
  expect_error(dfba_sim_data(n = 450,
                             model = "chisquare",
                             design = "independent",
                             delta = 0.4,
                             shape1 = -77,
                             block.max = 100),
               "shape1 and shape2 are df for respectively the control and experiment conditions and must be positive values.")
})

### logistic

test_that("negative shape stops the show (logistic)",{
  expect_error(dfba_sim_data(n = 450,
                             model = "logistic",
                             design = "independent",
                             delta = 0.4,
                             shape1 = -77,
                             block.max = 100),
               "shape1 and shape2 are scale factors and must be positive values.")
})

### exponential

test_that("negative shape stops the show (exponential)",{
  expect_error(dfba_sim_data(n = 450,
                             model = "exponential",
                             design = "independent",
                             delta = 0.4,
                             shape1 = -77,
                             block.max = 100),
               "shape1 and shape2 are scale factors and must be positive values.")
})

### gumbel

test_that("negative shape stops the show (gumbel)",{
  expect_error(dfba_sim_data(n = 450,
                             model = "gumbel",
                             design = "independent",
                             delta = 0.4,
                             shape1 = -77,
                             block.max = 100),
               "shape1 and shape2 are scale factors and must be positive values.")
})

### pareto

test_that("negative shape stops the show (pareto)",{
  expect_error(dfba_sim_data(n = 450,
                             model = "pareto",
                             design = "independent",
                             delta = 0.4,
                             shape1 = -77,
                             block.max = 100),
               "shape1 and shape2 must be greater than or equal to 1.")
})

# Function tests

## Normal Distribution Tests

  Tsim1<-dfba_sim_data(n = 450,
                       model = "normal",
                       design = "independent",
                       delta = 0.4,
                       block.max = 0)


  test_that("Posterior probability of H1 is correct [normal, independent]",{
    expect_gte(Tsim1$prH1, 0.72347)
  })

  test_that("Frequentist p-value for H1 is correct [normal, independent]",{
    expect_lte(Tsim1$pvalue, 0.13385)
  })

  Tsim2<-dfba_sim_data(n = 600,
                       model = "normal",
                       design = "paired",
                       delta = 0.4,
                       block.max = 0)

  test_that("Posterior probability of H1 is correct [normal, paired]",{
    expect_gte(Tsim2$prH1, 0.933)
  })

  test_that("Frequentist p-value for H1 is correct [normal, paired]",{
    expect_lte(Tsim2$pvalue, 0.069)
  })

  Tsim3<-dfba_sim_data(n = 600,
                       model = "normal",
                       design = "independent",
                       delta = 0.4,
                       block.max = 16)

  test_that("Mean of C values is correct [normal, independent]",{
    expect_lte(abs(mean(Tsim3$C)-8.000), 1.264)
  })

## Exponential Distribution Test

  Aexpf <- dfba_sim_data(n = 600,
                         model = "exponential",
                         design = "paired",
                         delta = 0.4,
                         block.max = 0)
  Cw <- Aexpf$C
  Cwor <- sort(Cw)

  Uact <- 1-exp(-Cwor)
  Upred <- seq(1,600,1)/600

  RsqExp <- (cor(Uact,Upred,method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Exponential Distribution]",{
    expect_gte(RsqExp, 0.9645)
  })

## Weibull Distribution Test

  AWeibull<-dfba_sim_data(n = 600,
                          model = "weibull",
                          design = "paired",
                          delta = 0.4,
                          shape1 = 0.8,
                          shape2 = 0.8)
  shape1 <- 0.8
  Cw2 <- AWeibull$C
  Cwor2 <- sort(Cw2)
  Uact2 <- 1-exp(-Cwor2^shape1)
  Upred2 <- seq(1,
                600,
                1)/600
  RsqW <- (cor(Uact2, Upred2, method = "pearson"))^2

  test_that("Test R^2 GOF for Group C [Weibull Distribution]",{
    expect_gte(RsqW, 0.969)
  })

## Logistic Distribution Test

  Alogis <- dfba_sim_data(n = 600,
                          model = "logistic",
                          design = "paired",
                          delta = 0.4,
                          shape1 = 0.551,
                          shape2 = 0.551)
  shape1 <- 0.551
  Cw <- Alogis$C
  Cwor <- sort(Cw)
  Uact <- 1/(1+exp(-(Cwor/shape1)))
  Upred <- seq(1,600,1)/600
  RsqLogistic <- (cor(Uact,Upred,method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Logistic Distribution]",{
    expect_gte(RsqLogistic, 0.9794)
  })

## Gumbel Distribution Test
  Agum <- dfba_sim_data(n = 600,
                        model = "gumbel",
                        design = "paired",
                        delta = 0.4,
                        shape1 = 1,
                        shape2 = 1)
  Cw <- Agum$C
  Cwor <- sort(Cw)
  Uact <- exp(-exp(-Cwor))
  Upred <- seq(1,600,1)/600
  RsqGum <- (cor(Uact,Upred,method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Gumbel Distribution]",{
    expect_gte(RsqGum, 0.9794)
  })

## Cauchy Distribution Test

  Acauchy<-dfba_sim_data(n = 600,
                         model = "cauchy",
                         design = "paired",
                         delta = 0.4,
                         shape1 = 1,
                         shape2 = 1)
  shape1 <- 1
  Cw <- Acauchy$C
  Cwor <- sort(Cw)
  Uact <- .5+atan(Cwor/shape1)
  Upred <- seq(1,600,1)/600
  RsqCauchy <- (cor(Uact, Upred, method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Cauchy Distribution]",{
    expect_gte(RsqCauchy, 0.9765)
  })

## Pareto Distribution Test

  Apareto<-dfba_sim_data(n = 600,
                         model = "pareto",
                         design = "paired",
                         delta = 0.4,
                         shape1 = 1,
                         shape2 = 1)
  shape1 <- 1
  alpha1 <- 1.16*shape1
  Cw <- Apareto$C
  Cwor <- sort(Cw)
  Uact <- 1-(1/(Cwor^alpha1))
  Upred <- seq(1,600,1)/600
  RsqPareto <- (cor(Uact,Upred,method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Pareto Distribution]",{
    expect_gte(RsqPareto, 0.9693719)
  })

## Chi-squared Distribution Test

  D <- dfba_sim_data(n = 600,
                     model = "chisquare",
                     design = "paired",
                     delta = 0.4,
                     shape1 = 10,
                     shape2 = 10)

  test_that("Mean of C values is correct [chi-squared distribution]",{
    expect_lte(abs(mean(D$C)-10.00), 1.0694)
  })

## Lognormal Distribution Test

  Dlogn<-dfba_sim_data(n = 2000,
                       model = "lognormal",
                       design = "paired",
                       delta = 0.4,
                       shape1 = 1,
                       shape2 = 1)

  test_that("Median of C values is correct [lognormal distribution]",{
    expect_lte(abs(median(Dlogn$C)-1.000), 0.23024)
  })




