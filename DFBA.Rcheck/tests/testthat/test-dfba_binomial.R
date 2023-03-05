
  dfba_bin=rep(0,9)
  Abin<-dfba_binomial(n1 = 16,n2 =2)

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

