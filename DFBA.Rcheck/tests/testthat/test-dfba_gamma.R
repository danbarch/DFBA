
  gamma_test_matrix <- matrix(c(38, 4, 5, 0, 6, 40, 1, 2, 4, 8, 20, 30),
                              ncol = 4,
                              byrow = TRUE)
  AGam <- dfba_gamma(gamma_test_matrix)

 # test_that("No error messages",{
 #   expect_no_error(dfba_gamma(matrix(c(38, 4, 5, 0, 6, 40, 1, 2, 4, 8, 20, 30),
 #                                     ncol = 4,
 #                                     byrow = TRUE)))
 # })
  test_that("Gamma estimate is correct",{
    expect_lte(abs(AGam$gamma - 0.8417668), 3e-05)
  })

  test_that("Sample proportion is correct",{
    expect_lte(abs(AGam$sample_p - 0.9208834), 3e-05)
  })

  test_that("Number of concordant pairs is correct",{
    expect_equal(floor(AGam$nc + 0.1), 6588)
  })

  test_that("Number of discordant pairs is correct",{
    expect_equal(floor(AGam$nd + 0.1), 566)
  })

  test_that("Posterior shape parameter a is correct",{
    expect_equal(floor(AGam$a_post + 0.1), 6589)
  })

  test_that("Posterior shape parameter b is correct",{
    expect_equal(floor(AGam$b_post + 0.1), 567)
  })

  test_that("Posterior median is correct",{
    expect_lte(abs(AGam$post_median - 0.920805), 3e-05)
  })

  test_that("Equal-tail interval lower limit is correct",{
    expect_lte(abs(AGam$eti_lower - 0.9143977), 3e-05)
  })

  test_that("Equal-tail interval upper limit is correct",{
    expect_lte(abs(AGam$eti_upper - 0.926912), 3e-05)
  })



  test_that("Non-matrix produces stop error",{
    MatrixTestArray <- c(38, 4, 5, 0, 6, 40, 1, 2, 4, 8, 20, 30)
    expect_error(dfba_gamma(MatrixTestArray), "input must be in matrix or table format")
  })

  test_that("Negative a0 produces stop error",{
    negative_a0 <- -77
    expect_error(dfba_gamma(gamma_test_matrix, a0 = negative_a0), "Both the a0 and b0 shape parameters must be positive and finite.")
  })

  test_that("Negative b0 produces stop error",{
    negative_b0 <- -77
    expect_error(dfba_gamma(gamma_test_matrix, b0 = negative_b0), "Both the a0 and b0 shape parameters must be positive and finite.")
  })
