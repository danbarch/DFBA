test_beta_contrast<-function(dfba_contrast=rep(0,7)){
  ## This function has seven tests for the dfba_beta_contrast function
  # The tests are based on an included data set and a contrast among
  # four indepedent binomial conditions.
  # Tests 1 and 2 check on the mean and median of the posterior
  # contrast. Tests 3 and 4 check on the lower and upper 95 percent
  # equal-tail interval limits for the posterior contrast. Test 5 and 6
  # are checks on the prior and posterior probability values that the
  # contrast is positive. Test 7 checks the Bayes factor BF10 value.
  ## The output of this function is the vector dfba_contrast that contains
  # either a 0 or 1 for the seven tests where 1 corresponds to a failure
  # and 0 is corresponds to a pass.

  dfba_contrast=rep(0,7)
  n1_vec<-c(22,15,13,21)
  n2_vec<-c(18,25,27,19)
  ABcontrast<-c(.5,-.5,-.5,.5)

  Acon<-dfba_beta_contrast(n1_vec=n1_vec,n2_vec=n2_vec,contrast_vec=ABcontrast)

  test_mean=abs(Acon$mean-.1785714)
  if (test_mean>3e-05){dfba_contrast[1]=1}

  test_median=abs(Acon$delta_quantiles[51]-.17924)
  if (test_median>.00904){dfba_contrast[2]=1}

  test_lower=abs(Acon$lower_limit-.030875)
  if (test_lower>.0176){dfba_contrast[3]=1}

  test_upper=abs(Acon$upper_limit-.32255)
  if (test_upper>.0114){dfba_contrast[4]=1}

  test_prior_positive=abs(Acon$prior_positive_delta-.50013)
  if (test_prior_positive>.0343){dfba_contrast[5]=1}

  test_post_positive=abs(Acon$prob_positive_delta-.991)
  if (test_post_positive>.0063){dfba_contrast[6]=1}

  test_BF=abs(Acon$bayes_factor-111.034)
  if (test_BF>250){dfba_contrast[7]=1}

  failtot=sum(dfba_contrast)
  if (failtot==0){
    cat("dfba_beta_contrast passes", " ","\n") } else {
      cat("dfba_beta_contrast fails. There are","\n")
      cat(failtot," failed subtests of the function.","\n")
    }

  dfba_beta_contrast_list=list(dfba_contrast=dfba_contrast)


}
