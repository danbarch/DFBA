  measure_1 <- c(1.49, 0.64, 0.96, 2.34, 0.78, 1.29, 0.72, 1.52, 0.62, 1.67, 1.19, 0.86)
  measure_2 <- c(0.53, 0.55, 0.58, 0.97, 0.60, 0.22, 0.05, 13.14, 0.63, 0.33, 0.91, 0.37)

    ASgn<-dfba_sign_test(Y1 = measure_1,
                         Y2 = measure_2)

    test_that("n of positive values is correct",{
      expect_equal(floor(ASgn$n_pos + 0.1), 10)
    })

    test_that("n of negative values is correct",{
      expect_equal(floor(ASgn$n_neg + 0.1), 2)
    })

    test_that("Posterior shape parameter a is correct",{
      expect_equal(floor(ASgn$a.post + 0.1), 11)
    })

    test_that("Posterior shape parameter b is correct",{
      expect_equal(floor(ASgn$b.post + 0.1), 3)
    })

    test_that("Posterior mean for positivity rate phi is correct",{
      expect_lte(abs(ASgn$phimean - 0.7857143), 3e-05)
    })

    test_that("Posterior median for positivity rate phi is correct",{
      expect_lte(abs(ASgn$phimedian - 0.7995514), 3e-05)
    })

    test_that("Posterior mode for positivity rate phi is correct",{
      expect_lte(abs(ASgn$phimode - 0.8333333), 3e-05)
    })

    test_that("Equal-tail interval lower limit is correct",{
      expect_lte(abs(ASgn$eti_lower - 0.5455289), 3e-05)
    })

    test_that("Equal-tail interval upper limit is correct",{
      expect_lte(abs(ASgn$eti_upper - 0.9496189), 3e-05)
    })

    test_that("Highest-density interval lower limit is correct",{
      expect_lte(abs(ASgn$hdi_lower - 0.5789457), 3e-05)
    })

    test_that("Highest-density interval upper limit is correct",{
      expect_lte(abs(ASgn$hdi_upper - 0.9677091), 3e-05)
    })

    test_that("Posterior probability of H1 is correct",{
      expect_lte(abs(ASgn$post_H1 - 0.9887695), 3e-05)
    })

    test_that("Bayes Factor 1/0 is correct",{
      expect_lte(abs(ASgn$BF10 - 88.04348), 0.4)
    })

    test_that("Bayes Factor 0/1 is correct",{
      expect_lte(abs(ASgn$BF01 - 0.01135802), 6e-05)
    })

