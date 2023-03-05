# Normal Distribution Tests

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

# Exponential Distribution Test

  Aexpf <- dfba_sim_data(n = 600,
                         model = "exponential",
                         design = "paired",
                         delta = 0.4,
                         block.max = 0)
  Cw=Aexpf$C
  Cwor=sort(Cw)

  Uact=1-exp(-Cwor)
  Upred=(seq(1,600,1))/600

  RsqExp=(cor(Uact,Upred,method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Exponential Distribution]",{
    expect_gte(RsqExp, 0.9645)
  })

# Weibull Distribution Test

  AWeibull<-dfba_sim_data(n = 600,
                          model = "weibull",
                          design = "paired",
                          delta = 0.4,
                          shape1 = 0.8,
                          shape2 = 0.8)
  shape1 = 0.8
  Cw2=AWeibull$C
  Cwor2=sort(Cw2)
  Uact2=1-exp(-Cwor2^shape1)
  Upred2=(seq(1,600,1))/600
  RsqW=(cor(Uact2, Upred2, method = "pearson"))^2

  test_that("Test R^2 GOF for Group C [Weibull Distribution]",{
    expect_gte(RsqW, 0.969)
  })

# Logistic Distribution Test

  Alogis <- dfba_sim_data(n = 600,
                          model = "logistic",
                          design = "paired",
                          delta = 0.4,
                          shape1 = 0.551,
                          shape2 = 0.551)
  shape1 = 0.551
  Cw=Alogis$C
  Cwor=sort(Cw)
  Uact=1/(1+exp(-(Cwor/shape1)))
  Upred=(seq(1,600,1))/600
  RsqLogistic=(cor(Uact,Upred,method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Logistic Distribution]",{
    expect_gte(RsqLogistic, 0.9794)
  })

# Gumbel Distribution Test
  Agum <- dfba_sim_data(n = 600,
                        model = "gumbel",
                        design = "paired",
                        delta = 0.4,
                        shape1 = 1,
                        shape2 = 1)
  Cw=Agum$C
  Cwor=sort(Cw)
  Uact=exp(-exp(-Cwor))
  Upred=(seq(1,600,1))/600
  RsqGum=(cor(Uact,Upred,method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Gumbel Distribution]",{
    expect_gte(RsqGum, 0.9794)
  })

# Cauchy Distribution Test

  Acauchy<-dfba_sim_data(n = 600,
                         model = "cauchy",
                         design = "paired",
                         delta = 0.4,
                         shape1 = 1,
                         shape2 = 1)
  shape1=1
  Cw=Acauchy$C
  Cwor=sort(Cw)
  Uact=.5+atan(Cwor/shape1)
  Upred=(seq(1,600,1))/600
  RsqCauchy=(cor(Uact, Upred, method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Cauchy Distribution]",{
    expect_gte(RsqCauchy, 0.9765)
  })

# Pareto Distribution Test

  Apareto<-dfba_sim_data(n = 600,
                         model = "pareto",
                         design = "paired",
                         delta = 0.4,
                         shape1 = 1,
                         shape2 = 1)
  shape1=1
  alpha1=1.16*shape1
  Cw=Apareto$C
  Cwor=sort(Cw)
  Uact=1-(1/(Cwor^alpha1))
  Upred=(seq(1,600,1))/600
  RsqPareto=(cor(Uact,Upred,method="pearson"))^2

  test_that("Test R^2 GOF for Group C [Pareto Distribution]",{
    expect_gte(RsqPareto, 0.9693719)
  })

# Chi-squared Distribution Test

  D <- dfba_sim_data(n = 600,
                     model = "chisquare",
                     design = "paired",
                     delta = 0.4,
                     shape1 = 10,
                     shape2 = 10)

  test_that("Mean of C values is correct [chi-squared distribution]",{
    expect_lte(abs(mean(D$C)-10.00), 1.0694)
  })

# Lognormal Distribution Test

  Dlogn<-dfba_sim_data(n = 2000,
                       model = "lognormal",
                       design = "paired",
                       delta = 0.4,
                       shape1 = 1,
                       shape2 = 1)

  test_that("Median of C values is correct [lognormal distribution]",{
    expect_lte(abs(median(Dlogn$C)-1.000), 0.23024)
  })




