
# Small method tests

# data for small method tests

dfba_wil=rep(0,22)
Y1<-c(2.286,0.621,0.967,3.782,18.960,5.473,4.274,0.605)
Y2<-c(1.114,0.002,0.382,1.251,0.003,8.413,7.947,0.050)
Awil<-dfba_wilcoxon(Y1,Y2,samples=10000)


test_that("T_positive for small method is correct",{
  expect_equal(floor(Awil$T_pos + 0.1), 23)
})

test_that("T_negative for small method is correct",{
  expect_equal(floor(Awil$T_neg + 0.1), 13)
})

test_that("Phibar for small method is correct",{
  expect_lte(abs(Awil$phibar - 0.612888), 0.00568)
})

test_that("Prior H1 for small method is correct",{
  expect_lte(abs(Awil$prH1 - 0.75174), 0.020536)
})

test_that("Equal tail lower limit for small method is correct",{
  expect_lte(abs(Awil$qLv - 0.282248), 0.01515)
})

test_that("Equal tail upper limit for small method is correct",{
  expect_lte(abs(Awil$qHv - 0.893838), 0.00708)
})

test_that("Bayes Factor 10 for small method is correct",{
  expect_lte(abs(Awil$BF10 - 3.028392), 0.5)
})

test_that("Posterior distribution for Phi for small method is correct",{
  expect_equal(floor(sum(Awil$phipost)+.0001), 1)
})

test_that("Total cumulative probability for Phi for small method is correct",{
  expect_equal(floor(Awil$cumulative_phi[200]+.0001), 1)
})

# Large method tests

# data vectors for the large sample cases.
  Y1L<-rep(Y1, 4)
  Y2L<-rep(Y2, 4)
  Bwil<-dfba_wilcoxon(Y1L,Y2L)

  test_that("T_positive for large method is correct",{
    expect_equal(floor(Bwil$T_pos + 0.1), 332)
  })

  test_that("T_negative for large method is correct",{
    expect_equal(floor(Bwil$T_neg + 0.1), 196)
  })

  test_that("n value for large method is correct",{
    expect_equal(floor(Bwil$n + 0.1), 32)
  })

  test_that("posterior a parameter for large method is correct",{
    expect_lte(abs(Bwil$apost - 15.84091), 0.0001)
  })

  test_that("posterior b parameter for large method is correct",{
    expect_lte(abs(Bwil$bpost - 9.659091), 0.0001)
  })

  test_that("posterior mean for large method is correct",{
    expect_lte(abs(Bwil$postmean - 0.6212121), 0.0001)
  })

  test_that("posterior median for large method is correct",{
    expect_lte(abs(Bwil$postmedian - 0.6244281), 0.0001)
  })

  test_that("Posterior probability of H1 for large method is correct",{
    expect_lte(abs(Bwil$prH1 - 0.8945555), 0.0001)
  })

  test_that("Bayes Factor 1/0 for large method is correct",{
    expect_lte(abs(Bwil$BF10 - 8.483664), 0.02)
  })

  test_that("Lower Limit for Equal-Tail Interval for large method is correct",{
    expect_lte(abs(Bwil$qlequal - 0.4293161), 0.0001)
  })

  test_that("Upper Limit for Equal-Tail Interval for large method is correct",{
    expect_lte(abs(Bwil$qhequal - 0.7950966), 0.0001)
  })

  test_that("Lower Limit for Highest-Density Interval for large method is correct",{
    expect_lte(abs(Bwil$qLmin - 0.4361713), 0.0001)
  })

  test_that("Upper Limit for Highest-Density Interval for large method is correct",{
    expect_lte(abs(Bwil$qHmax - 0.8010602), 0.0001)
  })
