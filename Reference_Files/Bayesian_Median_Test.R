# Bayesian Median Test
#
#
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


Bayes_mediantest<-function(Y1,Y2,a0=1,b0=1){
  Y<-c(Y1,Y2)
  med=median(Y)
#  print("Overall median is:")
#  print(med)
  l1=length(Y1)
  l2=length(Y2)
  A1above=0
  A1below=0
  for (I in 1:l1){
    if (Y1[I]>med){
      A1above=A1above+1
      } else {A1below=A1below+1}
    }
  A2above=0
  A2below=0
  for (I in 1:l2){
    if (Y2[I]>med){
      A2above=A2above+1
      } else {A2below=A2below+1}
    }
#  print("Respective frequencies of Y1 and Y2 above the median are:")
#  abovefreq<-c(A1above, A2above)
#  print(abovefreq)
#  print("Respective frequencies of Y1 and Y2 at or below median are:")
#  belowfreq<-c(A1below,A2below)
#  print(belowfreq)
  a=A1above+a0
  b=A2above+b0
  Y1baserate=l1/(l1+l2)
  probY1hi=1-pbeta(Y1baserate,a,b)
#  Y2baserate=1-Y1baserate
#  print("Respective base rates for Y1 and Y2 responses are:")
#  Y2baserate=1-Y1baserate
#  baserate<-c(Y1baserate,Y2baserate)
#  print(baserate)
#  print("Following is the analysis of the responses above the median:")
  probY2hi=1-pbeta(Y2baserate,b,a)
  if (probY1hi>=probY2hi) {
    prob<-probY1hi
#    print("Posterior probability that Y1 exceeds base rate is:")
    Direction<-"Y1 exceeds base rate"
#    print(probY1hi)
    postodds=probY1hi/(1-probY1hi)
    priorY1hi=1-pbeta(Y1baserate,a0,b0)
    priorodds=priorY1hi/(1-priorY1hi)
    BF=postodds/priorodds
#    print("Prior probability that Y1 exceeds base rate is:")
#    print(priorY1hi)
    prior<-priorY1hi
#    print("Bayes factor is:")
#    print(BF)
    } else {
      Direction<-"Y2 exceeds base rate"
      prob<-probY2hi
#      print("Posterior probability that Y2 exceeds base rate is:")
#      print(probY2hi)
      postodds=probY2hi/(1-probY2hi)
      priorY2hi=1-pbeta(Y2baserate,b0,a0)
      priorodds=priorY2hi/(1-priorY2hi)
      BF=postodds/priorodds
#      print("Prior probability that Y2 exceeds base rate is:")
      prior<-priorY2hi
#      print(priorY2hi)
#      print("Bayes factor is:")
#      print(BF)
    }
  list(Overall_median=med,
       Y1_freq_above=A1above,
       Y2_freq_above=A2above,
       Y1_freq_below=A1below,
       Y2_freq_below=A2below,
       Y1_base_rate=Y1baserate,
       Y2_base_rate=1-Y1baserate,
       Direction=Direction,
       Posterior_Probability=prob,
       Prior_Probability=prior,
       Bayes_Factor=BF
       )
}
