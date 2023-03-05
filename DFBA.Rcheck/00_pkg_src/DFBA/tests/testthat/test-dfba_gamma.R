
  AGam <- dfba_gamma(matrix(c(38, 4, 5, 0, 6, 40, 1, 2, 4, 8, 20, 30),
                            ncol = 4,
                            byrow = TRUE))

  test_that("Gamma estimate is correct",{
    expect_lte(abs(AGam$gamma - 0.8417668), 3e-05)
  })

  test_that("Sample proportion is correct",{
    expect_lte(abs(AGam$sample.p - 0.9208834), 3e-05)
  })

  test_that("Number of concordant pairs is correct",{
    expect_equal(floor(AGam$nc + 0.1), 6588)
  })

  test_that("Number of discordant pairs is correct",{
    expect_equal(floor(AGam$nd + 0.1), 566)
  })

  test_that("Posterior shape parameter a is correct",{
    expect_equal(floor(AGam$a.post + 0.1), 6589)
  })

  test_that("Posterior shape parameter b is correct",{
    expect_equal(floor(AGam$b.post + 0.1), 567)
  })

  test_that("Posterior median is correct",{
    expect_lte(abs(AGam$post.median - 0.920805), 3e-05)
  })

  test_that("Equal-tail interval lower limit is correct",{
    expect_lte(abs(AGam$post.eti.lower - 0.9143977), 3e-05)
  })

  test_that("Equal-tail interval upper limit is correct",{
    expect_lte(abs(AGam$post.eti.upper - 0.926912), 3e-05)
  })

