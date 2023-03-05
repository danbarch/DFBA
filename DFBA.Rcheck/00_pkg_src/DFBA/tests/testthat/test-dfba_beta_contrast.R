
  n1_vec<-c(22, 15, 13, 21)
  n2_vec<-c(18, 25, 27, 19)
  ABcontrast<-c(.5, -.5, -.5, .5)

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
