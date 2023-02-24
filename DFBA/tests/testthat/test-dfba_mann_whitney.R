


test_mann<-function(dfba_mann=rep(0,30)){
  dfba_mann=rep(0,30)
  ## The first 13 tests are for the small-sample
  # case. These test are : Tests 1 and 2 are
  # the E and C means, Tests 3 and 4 are for n_E and n_C,
  # Tests 5 and 6 are for U_E and U_C, Test 7 checks
  # that the total of the discrete posterior
  # probabilities is 1, Test 8 that the last value in
  # cumulative probability vector is 1, Test 9 checks
  # the posterior prH1 value, Test 10 checks the
  # BF10 value, Test 11 checks the posterior mean,
  # Test 12 and 13 check the equal-tail 95 percent limits.

  ## Following are the data for small-sample tests.
  E<-c(1.61,2.02,2.34,2.89,4.51,4.72,4.86,6.21,9.50,33.39)
  C<-c(1.11,1.13,1.32,1.82,1.99,4.12,6.28,6.21,8.24,44.55)
  Amann<-dfba_mann_whitney(E,C,samples=10000)
  Emeantest=abs(Amann$Emean - 7.205)
  if (Emeantest>.001){dfba_mann[1]=1}
  Cmeantest=abs(Amann$Cmean - 7.677)
  if (Cmeantest>.001){dfba_mann[2]=1}
  n_Etest=Amann$n_E+.1
  if (floor(n_Etest)!=10){dfba_mann[3]=1}
  n_Ctest=Amann$n_C+.1
  if (floor(n_Ctest)!=10){dfba_mann[4]=1}
  U_Etest=Amann$U_E+.1
  if (floor(U_Etest)!=60){dfba_mann[5]=1}
  U_Ctest=Amann$U_C+.1
  if (floor(U_Ctest)!=39){dfba_mann[6]=1}
  probtotalsm=sum(Amann$omegapost)+.0001
  if (floor(probtotalsm)!=1){dfba_mann[7]=1}
  cumprobtotalsm=Amann$cumulative_omega[200]
  if (floor(.001+cumprobtotalsm)!=1){dfba_mann[8]=1}
  prH1smalltest=abs(Amann$prH1-.7778633)
  if (prH1smalltest>.02116715){dfba_mann[9]=1}
  BF10mansmtest=abs(Amann$BF10-3.502526)
  if (BF10mansmtest>.6){dfba_mann[10]=1}
  omegabarsmalltest=abs(Amann$omegabar-.5911166)
  if (omegabarsmalltest>.005174341){dfba_mann[11]=1}
  qLvsmalltest=abs(Amann$qLv-.3524778)
  if (qLvsmalltest>.013732592){dfba_mann[12]=1}
  qHvsmalltest=abs(Amann$qHv-.8073713)
  if (qHvsmalltest>.013191075){dfba_mann[13]=1}
  ## The next tests are for the large-sample case.
  # Tests 14 and 15 check the sample mean for E and C,
  # Tests 16 and 17 check the values for n_E and n_C
  # Tests 18 and 19 check the values for U_E and U_C,
  # Tests 20 and 21 check the values for the
  # posterior values for a and b shape parameters,
  # Tests 22 and 23 ckecks value for posterior mean
  # and median, Test 24 checks omegabar value,
  # Tests 25 and 26 check the equal-tail 95 percent
  # interval limits, Tests 27 and 28 check the 95
  # percent HDI limits, Test 29 checks the value
  # for the posterior prH1, Test 30 checks the
  # value for BF10.
  ## The output of function is a vector of 30 values
  # which are 0 if the test passes and 1 if it fails.
  # The output vector is called dfba_mann.

  ## Following are the data for the large-sample tests
  Elarge<-c(E,E,E)
  Clarge<-c(C,C,C)
  Bmann<-dfba_mann_whitney(Elarge,Clarge)
  Emeanlargetest=abs(Bmann$Emean-7.205)
  if (Emeanlargetest>.0005){dfba_mann[14]=1}
  Cmeanlargetest=abs(Bmann$Cmean-7.677)
  if (Cmeanlargetest>.0005){dfba_mann[15]=1}
  n_Elargetest=Bmann$n_E +.1
  if (floor(n_Elargetest)!=30){dfba_mann[16]=1}
  n_Clargetest=Bmann$n_C+.1
  if (floor(n_Clargetest)!=30){dfba_mann[17]=1}
  U_Elargetest=Bmann$U_E +.1
  if (floor(U_Elargetest)!=540){dfba_mann[18]=1}
  U_Clargetest=Bmann$U_C+.1
  if (floor(U_Clargetest)!=351){dfba_mann[19]=1}
  apostlargetest=abs(Bmann$apost-27.90098)
  if (apostlargetest>.0005){dfba_mann[20]=1}
  bpostlargetest=abs(Bmann$bpost-18.57538)
  if (bpostlargetest>.0005){dfba_mann[21]=1}
  postmeanlargetest=abs(Bmann$postmean-.6003262)
  if (postmeanlargetest>.0005){dfba_mann[22]=1}
  postmedianlargetest=abs(Bmann$postmedian-.6017773)
  if (postmedianlargetest>.0005){dfba_mann[23]=1}
  omegabarlargetest=abs(Bmann$omegabar-.6003262)
  if (omegabarlargetest>.0005){dfba_mann[24]=1}
  qlequallargetest=abs(Bmann$qlequal-.4576042)
  if (qlequallargetest>.0005){dfba_mann[25]=1}
  qhequalllargetest=abs(Bmann$qhequal-.7348651)
  if (qhequalllargetest>.0005){dfba_mann[26]=1}
  qLminlargetest=abs(Bmann$qLmin-.4606209)
  if (qLminlargetest>.0005){dfba_mann[27]=1}
  qHmaxlargetest=abs(Bmann$qHmax -.7376452)
  if (qHmaxlargetest>.0005){dfba_mann[28]=1}
  prH1largetest=abs(Bmann$prH1-.9169977)
  if (prH1largetest>.0005){dfba_mann[29]=1}
  BF10largetest=abs(Bmann$BF10-11.04786)
  if (BF10largetest>.08){dfba_mann[30]=1}
  failtot3=sum(dfba_mann)
  if (failtot3==0){
    cat("dfba_mann_whitney passes", " ","\n") } else {
      cat("dfba_mann_whitney fails. There are","\n")
      cat(failtot3," failed subtests of the function","\n")
    }


  dfba_mann_list=list(dfba_mann=dfba_mann)

}



