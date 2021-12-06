# U Stats
#
# This function takes two (2) data vectors as input
# and outputs U statistics

#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Function based on code from p. 346

Mann_Whitney_U_Stats<-function(A, B){
  UA_vector<-rep(NA, length(A)) # UA counter
  UB_vector<-rep(NA, length(B)) # UB counter
  for (i in 1:length(A)){
    UA_vector[i]<-sum(A[i]>B)
  }
  for (j in 1:length(B)){
    UB_vector[j]<-sum(B[j]>A)
  }

  list(UA=sum(UA_vector),
       UB=sum(UB_vector))
}

