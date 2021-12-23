# Beta Interval Bayes Factor
#
#
#
#
#
#
#
#

#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


Beta_intervalBayesfactor<-function(n1,n2,lowerH0value=0,upperH0value=.5,a0=1,b0=1){
  if ((lowerH0value<0)|(lowerH0value>1)|(lowerH0value>=upperH0value)|(upperH0value>1)) {lowerH0value=0
  upperH0value=.5} else {}
#  print("H0 range is:")
  rangeH0<-c(lowerH0value,upperH0value)
#  print(rangeH0)
  if ((a0<=0)|(b0<=0)) {
    a0=1
    b0=1} else {}
  if ((n1<0)|(n2<0)){
    stop("Error. Both n1 and n2 must be nonnegative; please redo")} else {
      a=n1+a0
      b=n2+b0
      pH0up=pbeta(upperH0value,a0,b0)
      pH0lo=pbeta(lowerH0value,a0,b0)
      pH0=pH0up-pH0lo
      pH1=1-pH0
#      print("prior probability for H0 is:")
#      print(pH0)
#      print("prior probability for H1 is:")
#      print(pH1)
      postH0up=pbeta(upperH0value,a,b)
      postH0lo=pbeta(lowerH0value,a,b)
      postH0=postH0up-postH0lo
#      print("posterior probability for H0 is")
#      print(postH0)
      postH1=1-postH0
#      print("posterior probability for H1 is:")
#      print(postH1)
      if (pH0>=postH0){
        Direction<-"Bayes factor is for alternative over null"
#        print("Bayes factor is for alternative over null")
        if(postH0==0) {print("BF10 approaches infinity")} else
        {BF10=((1/postH0)-1)/((1/pH0)-1)
#        print(BF10)
        BF<-BF10}} else {
          Direction<-"Bayes factor is for null over alternative"
#          print("Bayes factor is for null over alternative")
          if (postH0==1){print("BF01 is near infinity")} else
          {BF01=((1/pH0)-1)/((1/postH0)-1)
#          print(BF01)
          BF<-BF01}}
    }
  list(H0_Range=rangeH0,
       H0_prior=pH0,
       H1_prior=pH1,
       H0_posterior=postH0,
       h1_posterior=postH1,
       Direction=Direction,
       BF=BF
       )
}
