test_mcnemar<-function(dfba_mcn=rep(0,9)){
  ## This function tests the dfba_mcnemar function.
  # There are nine tests on a canned example. Tests 1
  # and 2 check the values for a.post and b.post.
  # Tests 3 and 4 check the values for post_mean
  # and post_median which are the
  # respective posterior mean and median for the rate
  # change of a 0 score on the initial measurement to
  # a 1 score on the second measurement relative to
  # the overall changes between the two measurements.
  # Tests 5 and 6 check the values for the 95 percent
  # equal-tail interval limits. Tests 7 and 8 check
  # the BF10 Bayes factor for an interval and point
  # null hypothesis. Test 9 checks the posterior
  # probability for the hypothesis that the change
  # rate is greater than .5.
  ## The output of the function is a vector of 9
  # values, which are either a 0 or 1. These values
  # correspond to the 9 tests. If a test passes, it
  # is a 0, and if a test fails, it is a 1.
  ## The output vector is dfba_mcn.

  dfba_mcn=rep(0,9)
  Amc<-dfba_mcnemar(n_01=17,n_10=2)

  test_a.post=Amc$a.post +.1
  if (floor(test_a.post)!=18){dfba_mcn[1]=1}

  test_b.post=Amc$b.post+.1
  if (floor(test_b.post)!=3){dfba_mcn[2]=1}

  test_post_mean=abs(Amc$post_mean-.8571429)
  if (test_post_mean>3e-05){dfba_mcn[3]=1}

  test_post_median=abs(Amc$post_median-.8685263)
  if (test_post_median>3e-05){dfba_mcn[4]=1}

  test_post_eti_lower=abs(Amc$post_eti_lower-.6830173)
  if (test_post_eti_lower>3e-05){dfba_mcn[5]=1}

  test_post_eti_upper=abs(Amc$post_eti_upper-.9679291)
  if (test_post_eti_upper>3e-05){dfba_mcn[6]=1}

  test_BF10point=abs(Amc$BF10point-153.3006)
  if (test_BF10point>0.4){dfba_mcn[7]=1}

  test_BF10interval=abs(Amc$BF10interval-4968.555)
  if (test_BF10interval>31.0){dfba_mcn[8]=1}

  test_postH1=abs(Amc$postH1-.9997988)
  if (test_postH1>3e-05){dfba_mcn[9]=1}

  failtot=sum(dfba_mcn)
  if (failtot==0){
    cat("dfba_mcnemar passes", " ","\n") } else {
      cat("dfba_mcnemar fails. There are","\n")
      cat(failtot," failed subtests of the function.","\n")
    }
  test_sign_list=list(dfba_mcn=dfba_mcn)

}

