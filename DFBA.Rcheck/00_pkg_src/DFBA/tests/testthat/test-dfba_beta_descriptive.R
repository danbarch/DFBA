
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







