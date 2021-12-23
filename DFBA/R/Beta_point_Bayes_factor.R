# Beta Point Bayes Factor
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


Beta_pointBayesfactor<-function(n1,n2,ptH0=.5,a0=1,b0=1){
  if ((a0<=0)|(b0<=0)){
    a0=1
    b0=1} else {}
  if ((ptH0>1)|(ptH0<0)){
    ptH0=.5} else {}
#  print("H0 is the point:")
#  print(ptH0)
  if ((n1<0)|(n2<0)){
    stop("Error. Both n1 and n2 must be nonnegative; please redo")} else
    {
      a=n1+a0
      b=n2+b0
      dpriorH0=dbeta(ptH0,a0,b0)
      dpostH0=dbeta(ptH0,a,b)
      if (dpriorH0>=dpostH0){
        Direction<-"Bayes factor is for alternative over null"
#        print("Bayes factor is for alternative over null")
        if(dpostH0==0){print("BF10 approaches infinity")} else
        {BF<-dpriorH0/dpostH0
#        print(BF10)
        }} else {
          Direction<-"Bayes factor is for null over alternative"
#          print("Bayes factor is for null over alternative")
          BF<-dpostH0/dpriorH0
#          print(BF01)
          }
    }
  list(H0_point=H0,
       Direction=Direction,
       BF=BF)
}
