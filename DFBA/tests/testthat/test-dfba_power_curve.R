test_power2<-function(dfba_pow2=rep(0,2)){
  ## The function tests the Bayes and t power
  # as found with the dfba_power_curve function.
  # There are two tests to see if the Bayes and
  # t power are near the mean power for the specific
  # case when n=70 with paired samples and when
  # the data are sampled from normal distributions
  # and separation between the two distributions is .7.
  ## The output of the function is the vector dfba_pow2,
  # which has each a 0 or 1 for the two tests where
  # the 0 is if the test passes and 1 if it fails.
  # Tests 1 and 2 check the respective Bayes and t power.

  dfba_pow2=rep(0,2)
  Apow2<-dfba_power_curve(n=70,model="normal",design="paired",samples=150)
  bayespower=Apow2$outputdf[[2]]
  tpower=Apow2$outputdf[[3]]
  bayes70c=bayespower[15]
  t70c=tpower[15]
  bayestest=abs(bayes70c-.99034)
  if (bayestest>.07){dfba_pow2[1]=1}
  ttest=abs(t70c-.99249)
  if (ttest>.07){dfba_pow2[2]=1}

  failtot=sum(dfba_pow2)
  if (failtot==0){
    cat("dfba_power_curve passes", " ","\n") } else {
      cat("dfba_power_curve fails. There are","\n")
      cat(failtot," failed subtests of the function.","\n")
    }

  dfba_power_curve_list=list(dfba_pow2=dfba_pow2)
}
