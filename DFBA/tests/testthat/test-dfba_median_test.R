# Tests of median test function

# Test data

group1 <- c(12.90, 10.84, 22.67, 10.64, 10.67, 10.79, 13.55, 10.95, 12.19,
            12.76, 10.89, 11.02, 14.27, 13.98, 11.52, 13.49, 11.22, 15.07,
            15.74, 19.00)

group2 <- c(4.63, 58.64, 5.07, 4.66, 4.13, 3.92, 3.39, 3.57, 3.56, 3.39)

# Error Tests

test_that("Missing a0 parameter produces stop error",{
  expect_error(dfba_median_test(a0 = NA,
                                E = group1,
                                C = group2),
               "Both a0 and b0 must be positive and finite")
})

test_that("Missing b0 parameter produces stop error",{
  expect_error(dfba_median_test(b0 = NA,
                                E = group1,
                                C = group2),
               "Both a0 and b0 must be positive and finite")
})



# Function Tests


  AMed<-dfba_median_test(E = group1,
                         C = group2)

  test_that("Observed median is correct",{
    expect_lte(abs(AMed$median-10.985), 3e-05)
  })

  test_that("n of E vector is correct",{
    expect_equal(floor(AMed$nE + 0.1), 20)
  })

  test_that("n of C vector is correct",{
    expect_equal(floor(AMed$nC + 0.1), 10)
  })

  test_that("Base rate of E vector is correct",{
    expect_lte(abs(AMed$Ebaserate - 0.6666667), 3e-05)
  })

  test_that("Base rate of C vector is correct",{
    expect_lte(abs(AMed$Cbaserate - 0.3333333), 3e-05)
  })

  test_that("n of E values above median is correct",{
    expect_equal(floor(AMed$nEabove + 0.1), 14)
  })

  test_that("n of C values above median is correct",{
    expect_equal(floor(AMed$nCabove + 0.1), 1)
  })

  test_that("n of E values below median is correct",{
    expect_equal(floor(AMed$nEbelow + 0.1), 6)
  })

  test_that("n of C values below median is correct",{
    expect_equal(floor(AMed$nCbelow + 0.1), 9)
  })

  test_that("Posterior shape parameter a is correct",{
    expect_lte(abs(AMed$a.post - 15), 3e-05)
  })

  test_that("Posterior shape parameter b is correct",{
    expect_lte(abs(AMed$b.post - 2), 3e-05)
  })

  test_that("Posterior probability of > median for E vector is correct",{
    expect_lte(abs(AMed$postEhi - 0.9862981), 3e-05)
  })

  test_that("Posterior probability of > median for C vector is correct",{
    expect_lte(abs(AMed$postChi - 0.01370195), 3e-05)
  })

  test_that("Prior probability of > median for E vector is correct",{
    expect_lte(abs(AMed$priorEhi - 0.3333333), 3e-05)
  })

  test_that("Prior probability of > median for C vector is correct",{
    expect_lte(abs(AMed$priorChi - 0.6666667), 3e-05)
  })

  test_that("Bayes Factor 1/0 for E vector is correct",{
    expect_lte(abs(AMed$BF10E-143.9646), 0.03)
  })

  test_that("Bayes Factor 1/0 for C vector is correct",{
    expect_lte(abs(AMed$BF10C-0.006946151), 3e-05)
  })
